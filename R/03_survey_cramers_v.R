library(DBI)
library(tidyverse)
library(lubridate)
library(greybox)
library(patchwork)

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
# FUNCTIONS
#--------------------------------------------------

fetch_survey <- function(con, level) {
  dbGetQuery(con, glue::glue("SELECT * FROM household_survey_raw_{level}"))
}

clean_survey <- function(df) {
  df %>%
    select(starts_with("Q"), "Intervention") %>%
    mutate(across(everything(), ~ na_if(.x, 0)))
}

compute_cramer <- function(df) {
  result <- assoc(df, method = "cramer")
  v_mat  <- result$value
  
  as.data.frame(v_mat) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "cramers_v") %>%
    mutate(
      var1 = factor(var1, levels = colnames(v_mat)),
      var2 = factor(var2, levels = colnames(v_mat))
    ) %>%
    filter(as.integer(var1) > as.integer(var2))
}

make_cramer_plot <- function(v_df, level) {
  level_label <- switch(as.character(level),
                        "1" = "Synthetic - Level 1",
                        "2" = "Synthetic - Level 2",
                        "3" = "Synthetic - Level 3",
                        "4" = "Real"
  )
  
  ggplot(v_df, aes(x = var2, y = var1, fill = cramers_v)) +
    geom_tile(color = "grey", linewidth = 0.5) +
    scale_fill_gradient(
      low    = "white",
      high   = "blue",
      limits = c(0, 1),
      name   = "Cramér's V",
      guide  = guide_colorbar(
        direction      = "vertical",
        title.position = "top",
        barwidth       = 1,
        barheight      = 20
      )
    ) +
    coord_fixed() +
    labs(title = level_label, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text       = element_text(size = 7, face = "bold"),
      axis.text.x     = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.grid      = element_blank(),
      plot.title      = element_text(face = "bold"),
      legend.title    = element_text(face = "bold")
    )
}

#--------------------------------------------------
# FETCH & CLEAN ALL LEVELS
#--------------------------------------------------

LEVELS <- 1:4

surveys_clean <- map(
  LEVELS,
  ~ fetch_survey(con, .x) %>%
    clean_survey()
  ) %>%
  set_names(LEVELS)

#--------------------------------------------------
# VALID QUESTIONS FROM LEVEL 4 (REAL DATA)
#--------------------------------------------------

questions_valid <- surveys_clean[[4]] %>%
  pivot_longer(cols = everything(), names_to = "field") %>%
  group_by(field) %>%
  summarise(invalid = sum(is.na(value))) %>%
  filter(invalid < 100) %>%
  pull(field)

#--------------------------------------------------
# BUILD PANELS
#--------------------------------------------------

plots <- imap(surveys_clean, function(df, level) {
  message("Processing Level ", level)
  df %>%
    select(all_of(questions_valid)) %>%
    compute_cramer() %>%
    make_cramer_plot(level)
})

#--------------------------------------------------
# COMBINE
#--------------------------------------------------

plot_survey_cramer <- wrap_plots(plots, ncol = 4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Household Survey — Association Matrix",
    subtitle = "Cramér's V across selected question fields, by data fidelity level",
    caption  = "Fields selected based on having 100+ non-null records in the real data.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey50")
    )
  )

#--------------------------------------------------
# SAVE
#--------------------------------------------------

ggsave(
  filename = here::here("results", "survey_cramers_v.png"),
  plot     = plot_survey_cramer,
  height   = 8,
  width    = 18,
  dpi      = 200
)
