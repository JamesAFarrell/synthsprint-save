library(DBI)
library(tidyverse)
library(lubridate)

#--------------------------------------------------
# HELPERS
#--------------------------------------------------
MONTH_LABELS <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

DAY_LABELS <- c("Sunday","Monday","Tuesday","Wednesday",
                "Thursday","Friday","Saturday")

fetch_energy_data <- function(con, table) {
  dbGetQuery(con, glue::glue("
    SELECT
      time_of_day,
      day_of_week,
      AVG(energy_use) AS energy_use,
      EXTRACT(MONTH FROM date) AS month_num
    FROM {table}
    GROUP BY time_of_day, day_of_week, month_num
  "))
}

prepare <- function(df) {
  df %>%
    mutate(
      hour_continuous = period_to_seconds(hm(time_of_day)) / 3600,
      month_num       = factor(month_num, levels = 1:12, labels = MONTH_LABELS),
      day_of_week     = factor(day_of_week, levels = 0:6, labels = DAY_LABELS)
    )
}

#--------------------------------------------------
# CONNECTION
#--------------------------------------------------
db_path <- here::here("data", "duckdb", "save_pipeline.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)


#--------------------------------------------------
# FETCH DATA
#--------------------------------------------------
energy_real    <- fetch_energy_data(con, "energy_consumption_processed_4")
energy_level_1 <- fetch_energy_data(con, "energy_consumption_processed_1")
energy_level_2 <- fetch_energy_data(con, "energy_consumption_processed_2")

#--------------------------------------------------
# CREATE PLOT
#--------------------------------------------------
combined <- bind_rows(
  mutate(energy_level_1, level = "Synthetic - Level 1"),
  mutate(energy_level_2, level = "Synthetic - Level 2"),
  mutate(energy_real,    level = "Real")
) %>%
  mutate(
    level = factor(
      level, levels = c("Synthetic - Level 1", "Synthetic - Level 2", "Real")
    ))


plot_average_energy_consumption <- combined %>%
  prepare() %>%
  ggplot(aes(hour_continuous, energy_use,
             colour = day_of_week, group = day_of_week)) +
  geom_line() +
  facet_grid(level ~ month_num, scales = "free_y") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(
    name   = "Time of day",
    breaks = c(0, 6, 12, 18, 24),
    labels = c("00", "06", "12", "18", "24"),
    limits = c(0, 24)
  ) +
  labs(
    title    = "Average Energy Consumption Throughout the Day",
    subtitle = "By month, day of the week and data fidelity",
    y       = "Energy consumption (Wh per 15 min)",
    colour  = "Day of week",
    caption = "Note: Level 3 synthetic energy consumption data was not provided."
  ) +
  scale_colour_viridis_d(name = "Day of week") +
  theme_bw()

ggsave(
  filename = here::here("results", "consumption_daily_averages.png"),
  height = 8,
  width = 12,
  dpi = 200
)
