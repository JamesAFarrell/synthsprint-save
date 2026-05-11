library(tidyverse)
library(DBI)
library(duckdb)
library(glue)

#--------------------------------------------------
# PATHS
#--------------------------------------------------
base_raw <- here::here("data", "raw")

db_path <- here::here("data", "duckdb", "save_pipeline.duckdb")
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

dbExecute(con, "INSTALL icu;")
dbExecute(con, "LOAD icu;")

#--------------------------------------------------
# CONFIG
#--------------------------------------------------
raw_data_config <- list(
  
  level_1 = list(
    root = "Synthetic_SAVE_Admin_Proxy_Level1/Synthetic_SAVE_Admin_Proxy_Level1/synthetic_household_and_consumption_data",
    consumption_folders = c(
      "synthetic_save_consumption_data_2017_1_v0-1",
      "synthetic_save_consumption_data_2017_2_v0-1",
      "synthetic_save_consumption_data_2018_1_v0-1",
      "synthetic_save_consumption_data_2018_2_v0-1"
    ),
    survey_folder = "synthetic_save_household_survey_data",
    survey_file = "save_household_survey_data_v0-3.csv"
  ),
  
  level_2 = list(
    root = "Synthetic_SAVE_Admin_Proxy_Level2/Synthetic_SAVE_Admin_Proxy_Level2/synthetic_household_and_consumption_data_level_2",
    consumption_folders = c(
      "save_consumption_data_2017_1_v0-1",
      "save_consumption_data_2017_2_v0-1",
      "save_consumption_data_2018_1_v0-1",
      "save_consumption_data_2018_2_v0-1"
    ),
    survey_folder = "save_household_survey_data",
    survey_file = "save_household_survey_data.csv"
  ),
  
  level_3 = list(
    root = "Synthetic_SAVE_Admin_Proxy_Level3/Synthetic_SAVE_Admin_Proxy_Level3",
    consumption_folders = c(
      "electricity_consumption_data/save_consumption_data_2017_1_v0-1",
      "electricity_consumption_data/save_consumption_data_2017_2_v0-1",
      "electricity_consumption_data/save_consumption_data_2018_1_v0-1",
      "electricity_consumption_data/save_consumption_data_2018_2_v0-1"
    ),
    survey_folder = "synthetic_household_level_3/save_household_survey_data",
    survey_file = "save_household_survey_data.csv"
  ),
  
  level_4 = list(
    root = "8676_csv_7dffa539543e5739c4a6c8e78563ab07/csv",
    consumption_folders = c(
      "save_consumption_data_2017_1_v0-1/save_consumption_data_2017_1_v0-1",
      "save_consumption_data_2017_2_v0-1/save_consumption_data_2017_2_v0-1",
      "save_consumption_data_2018_1_v0-1/save_consumption_data_2018_1_v0-1",
      "save_consumption_data_2018_2_v0-1/save_consumption_data_2018_2_v0-1"
    ),
    survey_folder = "save_household_survey_data",
    survey_file = "save_household_survey_data_v0-3.csv"
  )
)

#--------------------------------------------------
# INGEST FUNCTION
#--------------------------------------------------
ingest_level <- function(cfg, level) {
  
  message("Ingesting: ", level)
  suffix <- stringr::str_remove(level, "level_")
  
  #==================================================
  # ENERGY CONSUMPTION DATA
  #==================================================
  
  union_sql <- cfg$consumption_folders |>
    purrr::map_chr(\(folder) {
      full_path <- file.path(base_raw, cfg$root, folder)
      glue("SELECT '{level}' AS source_level, * 
           FROM read_csv_auto('{full_path}/*.csv', union_by_name = true)")
    }) |>
    paste(collapse = "\nUNION ALL\n")
  
  dbExecute(con, glue("
    CREATE OR REPLACE TABLE energy_consumption_raw_{suffix} AS
    {union_sql}
  "))
  
  #==================================================
  # SURVEY DATA
  #==================================================
  survey_path <- file.path(base_raw, cfg$root, cfg$survey_folder, cfg$survey_file)
  
  dbExecute(con, glue("
    CREATE OR REPLACE TABLE household_survey_raw_{suffix} AS
    SELECT
      '{level}' AS source_level,
      *
    FROM read_csv_auto('{survey_path}')
  "))
}

#--------------------------------------------------
# RUN INGESTION
#--------------------------------------------------
purrr::imap(raw_data_config, ingest_level)

#--------------------------------------------------
# VALIDATION WITH ROW COUNTS
#--------------------------------------------------
validate_rowcounts <- function(pattern) {
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

message("\n--- Consumption tables ---")
print(validate_rowcounts("energy_consumption_raw_%"))

message("\n--- Survey tables ---")
print(validate_rowcounts("household_survey_raw_%"))

#--------------------------------------------------
# CLEAN UP
#--------------------------------------------------
dbDisconnect(con, shutdown = TRUE)