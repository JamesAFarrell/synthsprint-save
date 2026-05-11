

#--------------------------------------------------
# SETUP PACKAGES
#--------------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

cat("\n[1/3] Restoring renv environment...\n")
renv::restore(prompt = FALSE)

#--------------------------------------------------
# CREATE PROJECT DIRECTORIES
#--------------------------------------------------


cat("\n[2/3] Creating project directories...\n")

dirs <- c(
  here::here("data"),
  here::here("data", "raw"),
  here::here("data", "processed"),
  here::here("data", "duckdb"),
  here::here("results")
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("  Created:", dir, "\n")
  } else {
    cat("  Exists:", dir, "\n")
  }
}


#--------------------------------------------------
# DUCKDB DATABASE
#--------------------------------------------------

cat("\n[3/3] Initialising DuckDB database...\n")

db_path <- here::here(
  "data",
  "duckdb",
  "save_pipeline.duckdb"
)

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = db_path,
  read_only = FALSE
)

cat("  DuckDB database ready at:\n")
cat("  ", db_path, "\n")

DBI::dbDisconnect(con, shutdown = TRUE)

