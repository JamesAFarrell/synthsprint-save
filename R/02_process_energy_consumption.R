library(tidyverse)
library(DBI)
library(duckdb)
library(glue)

#--------------------------------------------------
# HELPERS
#--------------------------------------------------

validate_rowcounts <- function(pattern, con) {
  tables <- dbGetQuery(con, glue("
    SELECT table_name
    FROM information_schema.tables
    WHERE table_name LIKE '{pattern}'
    ORDER BY table_name
  "))
  
  purrr::map_dfr(tables$table_name, \(tbl) {
    dbGetQuery(con, glue("SELECT '{tbl}' AS table_name, COUNT(*) AS row_count FROM {tbl}"))
  })
}

#--------------------------------------------------
# COMBINED INTERPOLATION + PROCESSING FUNCTION
#--------------------------------------------------

process_level <- function(suffix, con) {
  
  message("Processing: level_", suffix)
  
  input_table  <- glue("energy_consumption_raw_{suffix}")
  output_table <- glue("energy_consumption_processed_{suffix}")
  
  tryCatch({
    
    dbExecute(con, glue("
      CREATE OR REPLACE TABLE {output_table} AS

      WITH

      normalized AS (
        SELECT
          SUBSTRING(bmg_id, 3)  AS bmg_id,
          source_level,
          received_timestamp,
          recorded_timestamp,
          energy
        FROM {input_table}
      ),

      gaps AS (
        SELECT
          bmg_id,
          source_level,
          recorded_timestamp                              AS gap_start,
          LEAD(recorded_timestamp) OVER (
            PARTITION BY bmg_id ORDER BY recorded_timestamp
          )                                               AS gap_end,
          energy                                          AS energy_start,
          LEAD(energy) OVER (
            PARTITION BY bmg_id ORDER BY recorded_timestamp
          )                                               AS energy_end,
          LEAD(received_timestamp) OVER (
            PARTITION BY bmg_id ORDER BY recorded_timestamp
          )                                               AS received_timestamp
        FROM normalized
      ),

      real_gaps AS (
        SELECT *,
          ((gap_end - gap_start) / 900)::BIGINT           AS n_missing_intervals
        FROM gaps
        WHERE gap_end IS NOT NULL
          AND (gap_end - gap_start) > 900
      ),

      imputed AS (
        SELECT
          g.bmg_id,
          g.source_level,
          g.received_timestamp,
          (g.gap_start + (s.step * 900))                  AS recorded_timestamp,
          TRUE                                             AS is_imputed,
          ROUND(
            g.energy_start + (g.energy_end - g.energy_start)
              * (s.step::DOUBLE / g.n_missing_intervals)
          )::BIGINT                                        AS energy
        FROM real_gaps g
        JOIN generate_series(1::BIGINT, (g.n_missing_intervals - 1)::BIGINT) AS s(step)
          ON TRUE
      ),

      combined AS (
        SELECT
          bmg_id,
          source_level,
          received_timestamp,
          recorded_timestamp,
          energy,
          FALSE                                            AS is_imputed
        FROM normalized

        UNION ALL

        SELECT
          bmg_id,
          source_level,
          received_timestamp,
          recorded_timestamp,
          energy,
          is_imputed
        FROM imputed
      ),

      ts_converted AS (
        SELECT *,
          epoch_ms(CAST(recorded_timestamp AS BIGINT) * 1000) AS ts
        FROM combined
      )

      SELECT
        source_level,
        bmg_id,
        received_timestamp,
        recorded_timestamp,
        energy,
        is_imputed,
        energy - LAG(energy) OVER (
          PARTITION BY bmg_id ORDER BY recorded_timestamp
        )                                                  AS energy_use,
        received_timestamp != LAG(received_timestamp) OVER (
          PARTITION BY bmg_id ORDER BY recorded_timestamp
        )                                                  AS online,
        CAST(ts AS DATE)                                   AS date,
        DAYOFWEEK(CAST(ts AS TIMESTAMP))                   AS day_of_week,
        STRFTIME(CAST(ts AS TIMESTAMP), '%H:%M')           AS time_of_day
      FROM ts_converted
      ORDER BY bmg_id, recorded_timestamp
    "))
    
    message("  Done: ", output_table)
    
  }, error = function(e) {
    message("  ERROR on ", input_table, ":\n  ", conditionMessage(e))
  })
}

#--------------------------------------------------
# CONNECTION
#--------------------------------------------------

db_path <- here::here("data", "duckdb", "save_pipeline.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
on.exit(dbDisconnect(con, shutdown = TRUE))

#--------------------------------------------------
# DERIVE SUFFIXES FROM EXISTING RAW TABLES
#--------------------------------------------------

suffixes <- dbGetQuery(con, "
  SELECT table_name
  FROM information_schema.tables
  WHERE table_name LIKE 'energy_consumption_raw_%'
  ORDER BY table_name
") |>
  pull(table_name) |>
  stringr::str_remove("energy_consumption_raw_")

#--------------------------------------------------
# RUN PIPELINE
#--------------------------------------------------

purrr::walk(suffixes, process_level, con = con)

#--------------------------------------------------
# VALIDATION
#--------------------------------------------------

message("\n--- Processed tables ---")
print(validate_rowcounts("energy_consumption_processed_%", con))

message("\n--- Sample output ---")
print(
  dbGetQuery(con, "
    SELECT *
    FROM energy_consumption_processed_1
    ORDER BY bmg_id, recorded_timestamp
    LIMIT 10
  ")
)