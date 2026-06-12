message("Running Stjordal data extraction")

repo_dir <- normalizePath(getwd(), mustWork = TRUE)
report_dir <- file.path(repo_dir, "reports", "stjordal")
docs_dir <- file.path(repo_dir, "docs", "stjordal")

fetch_script <- file.path(report_dir, "fetch-stjordal-history.R")
if (!file.exists(fetch_script)) {
  stop("Missing script: ", fetch_script, call. = FALSE)
}

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

source(fetch_script, local = new.env(parent = globalenv()))

files_to_copy <- c(
  "stjordal.html",
  "stjordal-history.json",
  "stjordal-history.js"
)

source_files <- file.path(report_dir, files_to_copy)
missing_files <- source_files[!file.exists(source_files)]
if (length(missing_files)) {
  stop("Missing generated/source files: ", paste(missing_files, collapse = ", "), call. = FALSE)
}

copied <- file.copy(
  from = source_files,
  to = file.path(docs_dir, files_to_copy),
  overwrite = TRUE
)

if (!all(copied)) {
  stop("Could not copy all Stjordal files to docs/stjordal", call. = FALSE)
}

message("Copied Stjordal files to ", docs_dir)
