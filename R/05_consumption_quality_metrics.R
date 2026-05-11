library(DBI)
library(tidyverse)
library(lubridate)

#--------------------------------------------------
# CONNECTION
#--------------------------------------------------
db_path <- here::here("data", "duckdb", "save_pipeline.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

#--------------------------------------------------
# QUERY FUNCTIONS
#--------------------------------------------------
fetch_dq_raw <- function(con, level) {
  dbGetQuery(con, glue::glue("
    WITH lagged AS (
      SELECT
        bmg_id,
        received_timestamp,
        recorded_timestamp,
        LAG(recorded_timestamp) OVER (PARTITION BY bmg_id ORDER BY recorded_timestamp) AS prev_timestamp,
        energy,
        LAG(energy)             OVER (PARTITION BY bmg_id ORDER BY recorded_timestamp) AS prev_energy
      FROM energy_consumption_raw_{level}
    )
    SELECT
      ROUND(100.0 * SUM(CASE WHEN received_timestamp  IS NOT NULL THEN 1 ELSE 0 END) / COUNT(*),                2) AS pct_received_timestamp_notnull,
      ROUND(100.0 * SUM(CASE WHEN recorded_timestamp  IS NOT NULL THEN 1 ELSE 0 END) / COUNT(*),                2) AS pct_recorded_timestamp_notnull,
      ROUND(100.0 * SUM(CASE WHEN energy              IS NOT NULL THEN 1 ELSE 0 END) / COUNT(*),                2) AS pct_energy_notnull,
      ROUND(100.0 * SUM(CASE WHEN recorded_timestamp - prev_timestamp = 900 THEN 1 ELSE 0 END) / COUNT(prev_timestamp), 2) AS pct_15min_intervals,
      ROUND(100.0 * SUM(CASE WHEN energy - prev_energy >= 0              THEN 1 ELSE 0 END) / COUNT(prev_energy),      2) AS pct_positive_energy,
      ROUND(100.0 * SUM(CASE WHEN recorded_timestamp <= received_timestamp THEN 1 ELSE 0 END) / COUNT(*),               2) AS pct_timestamp_valid
    FROM lagged
  "))
}

fetch_dq_processed <- function(con, level) {
  dbGetQuery(con, glue::glue("
    WITH survey_ids AS (
      SELECT DISTINCT CAST(BMG_ID AS VARCHAR) AS bmg_id
      FROM household_survey_raw_{level}
    )
    SELECT
      ROUND(100.0 * SUM(CASE WHEN s.bmg_id IS NOT NULL                       THEN 1 ELSE 0 END) / COUNT(*), 2) AS pct_survey_linked,
      ROUND(100.0 * SUM(CASE WHEN p.online = TRUE                             THEN 1 ELSE 0 END) / COUNT(*), 2) AS pct_imputed,
      ROUND(100.0 * SUM(CASE WHEN p.energy_use >= 0 AND p.energy_use < 10000  THEN 1 ELSE 0 END) / COUNT(*), 2) AS pct_realistic_energy
    FROM energy_consumption_processed_{level} p
    LEFT JOIN survey_ids s ON CAST(p.bmg_id AS VARCHAR) = s.bmg_id
  "))
}

#--------------------------------------------------
# LOOP ACROSS LEVELS
#--------------------------------------------------

LEVELS <- c(1, 2, 4)

dq_combined <- map_dfr(LEVELS, function(level) {
  
  message("Processing Level", level)
  
  bind_rows(
    fetch_dq_raw(con, level) %>%
      pivot_longer(everything(), names_to = "metric", values_to = "pct") %>%
      mutate(facet = "Raw data"),
    fetch_dq_processed(con, level) %>%
      pivot_longer(everything(), names_to = "metric", values_to = "pct") %>%
      mutate(facet = "Processed data")
  ) %>%
    mutate(level = paste("Level", level))
})

#--------------------------------------------------
# RESHAPE
#--------------------------------------------------

METRIC_LABELS <- c(
  pct_received_timestamp_notnull = "Non-null received timestamp",
  pct_recorded_timestamp_notnull = "Non-null recorded timestamp",
  pct_energy_notnull             = "Non-null energy reading",   
  pct_15min_intervals            = "Regular 15-min intervals",
  pct_positive_energy            = "Non-negative energy consumption",
  pct_timestamp_valid            = "Recorded before received",    
  pct_survey_linked              = "Linked to survey",
  pct_imputed                    = "Live measurement (not imputed)", 
  pct_realistic_energy           = "Realistic consumption (0–10,000 Wh)"  
)

dq_combined <- dq_combined %>%
  mutate(
    facet  = factor(facet,  levels = c("Raw data", "Processed data")),
    level  = factor(level,  levels = paste("Level", LEVELS)) %>% 
      fct_recode(
        "Synthetic - Level 1" = "Level 1",
        "Synthetic - Level 2" = "Level 2",
        "Real" = "Level 4"
      ),
    metric = factor(recode(metric, !!!METRIC_LABELS), levels = rev(METRIC_LABELS))
  )

#--------------------------------------------------
# PLOT
#--------------------------------------------------

plot_quality <- dq_combined %>%
  ggplot(aes(x = pct, y = metric)) +
  geom_col(fill = "#4C9BE8", width = 0.5) +
  geom_text(
    data = \(x) filter(x, pct > 30),
    aes(label = paste0(pct, "%")),
    hjust  = 1.15,
    colour = "white",
    size   = 3
  ) +
  geom_text(
    data = \(x) filter(x, pct <= 30),
    aes(label = paste0(pct, "%")),
    hjust  = -0.15,
    colour = "black",
    size   = 3
  ) +
  facet_grid(facet ~ level, scales = "free_y", space = "free_y") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title    = "Data Quality Summary: Energy Consumption",
    subtitle = "By data fidelity level — raw and processed",
    x        = "% of records",
    y        = NULL,
    caption  = "Note: Level 3 data was not provided."
  ) +
  theme_bw() +
  theme(
    strip.text         = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.subtitle      = element_text(colour = "grey40", margin = margin(b = 8)),
    axis.text.x        = element_text(size = 8)
  )

ggsave(
  filename = here::here("results", "consumption_data_quality.png"),
  height = 8,
  width = 12,
  dpi = 200
)
