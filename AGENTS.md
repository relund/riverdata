# Repository Guidelines

## Project Structure & Module Organization
Top-level `script_*.R` files drive hourly data updates and report rendering for each river system (`script_skjern.R`, `script_karup.R`, `script_mv-lf.R`). Reusable logic lives in `riverdata-package/R/`, with generated documentation in `riverdata-package/man/`. Source reports are under `reports/<area>/` and render into `docs/`, which is committed output for GitHub Pages. Raw and derived CSV data lives in `data/`. Shiny app sources are in `shiny/`.

## Build, Test, and Development Commands
Use `renv` to match the project library:

```r
R -e 'renv::restore()'
R -e 'install.packages("here"); install.packages("riverdata-package", repos = NULL, type = "source")'
```

Run the data/report pipelines with `Rscript script_skjern.R`, `Rscript script_karup.R`, or `Rscript script_mv-lf.R`. Start the Shiny app from RStudio or with `R -e 'shiny::runApp("shiny")'`.

## Coding Style & Naming Conventions

- use `snake_case` for functions and variables
- document exported package functions with roxygen, never update .Rd files, and add runnable examples when practical. 
- Match the existing style in `riverdata-package/R/`: short pipelines, two-space indentation in package code, and clear section comments in long scripts. Keep filenames descriptive and area-specific, for example `reports/skjern/skjern-waterlevel.Rmd` and `data/data_skjern_waterlevel_2026.csv`.


## Generated Files
Treat `docs/`, `riverdata-package/man/`, and many files in `data/` as generated artifacts. Edit source code in `reports/`, `riverdata-package/R/`, or `script_*.R` first, then regenerate outputs instead of patching rendered files by hand.

Do not run `roxygen2` or update generated package documentation unless explicitly requested by the user.
