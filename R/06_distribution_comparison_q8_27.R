library(DBI)
library(tidyverse)
library(lubridate)

#--------------------------------------------------
# CONNECTION
#--------------------------------------------------

db_path <- here::here("data", "duckdb", "save_pipeline.duckdb")

connect_db <- function(path) {
  if (exists("con") && dbIsValid(con)) dbDisconnect(con, shutdown = TRUE)
  dbConnect(duckdb::duckdb(), dbdir = path)
}

con <- connect_db(db_path)

#--------------------------------------------------
# LABELS
#--------------------------------------------------

LEVELS <- 1:4

fidelity_labels <- c(
  "1" = "Synthetic - Level 1",
  "2" = "Synthetic - Level 2",
  "3" = "Synthetic - Level 3",
  "4" = "Real"
)

income_labels <- c(
  "1"  = "Under £10,000",
  "2"  = "£10,000–£12,500",
  "3"  = "£12,501–£15,000",
  "4"  = "£15,001–£17,500",
  "5"  = "£17,501–£20,000",
  "6"  = "£20,001–£22,500",
  "7"  = "£22,501–£25,000",
  "8"  = "£25,001–£27,500",
  "9"  = "£27,501–£30,000",
  "10" = "£30,001–£35,000",
  "11" = "£35,001–£40,000",
  "12" = "£40,001–£50,000",
  "13" = "£50,001–£60,000",
  "14" = "£60,001–£80,000",
  "15" = "£80,001–£100,000",
  "16" = "Over £100,000",
  "17" = "Don't know",
  "18" = "Refused"
)

#--------------------------------------------------
# FUNCTIONS
#--------------------------------------------------

fetch_survey <- function(con, level) {
  dbGetQuery(con, glue::glue("SELECT * FROM household_survey_raw_{level}"))
}

#--------------------------------------------------
# FETCH & EXTRACT Q8_27 ACROSS ALL LEVELS
#--------------------------------------------------

q8_27_combined <- map_dfr(
  LEVELS,
  ~ fetch_survey(con, .x) %>%
    select(Q8_27) %>%
    mutate(
      level    = as.character(.x),
      fidelity = fidelity_labels[as.character(.x)]
    )
) %>%
  mutate(
    fidelity = factor(fidelity, levels = fidelity_labels),
    income_band = factor(
      coalesce(income_labels[as.character(Q8_27)], "Unknown / Missing"),
      levels = c(income_labels, "Unknown / Missing")
    )
  )

#--------------------------------------------------
# PLOT
#--------------------------------------------------

plot_q8_27 <- ggplot(
  q8_27_combined,
  aes(x = income_band, fill = fidelity)
) +
  geom_bar(
    position = "dodge",
    colour   = "white",
    linewidth = 0.2
  ) +
  scale_fill_brewer(palette = "Dark2", name = "Fidelity Level") +
  scale_x_discrete(drop = FALSE) +
  labs(
    title    = "Marginal Distributions Vary Across Synthetic Data Fidelity Levels",
    subtitle = "Q8.27 — Total monthly or annual gross household income (before tax)",
    x        = "Income Band",
    y        = "Count"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(colour = "grey50"),
    panel.grid.major.x = element_blank()
  ) +
  geom_vline(
    xintercept = seq(1.5, nlevels(q8_27_combined$income_band) - 0.5, by = 1),
    colour     = "grey85",
    linewidth  = 0.3
  ) 

#--------------------------------------------------
# SAVE
#--------------------------------------------------

ggsave(
  filename = here::here("results", "survey_q8_27_income.png"),
  plot     = plot_q8_27,
  height   = 7,
  width    = 7,
  dpi      = 200
)

