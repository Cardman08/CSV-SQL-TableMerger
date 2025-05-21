# CSV/SQL Merger for Data Preprocessing

This Shiny application allows users to merge two tabular data sources—such as CSV or TXT files, optionally with SQL dump support—into a single, harmonized data frame. It is designed for pre-processing fragmented lab data before importing into systems like [eLabFTW](https://www.elabftw.net/).

## Screenshot

![screenshot_sql_merger](https://github.com/user-attachments/assets/97f57dc7-94c0-4208-807d-f0a4b6302f0f)


## Features

- Upload two files in `.csv` or `.txt` format
- Extract column headers from `CREATE TABLE` SQL dump files (if headers are missing)
- Configure key columns for merging
- Join types supported:
  - Inner Join
  - Outer Join
  - Left Join
  - Right Join
- Automatic renaming of columns to avoid duplication
- Preview of input files and final merged result
- Export to `.csv` or `.xlsx` format with custom filename

## Intended Use

This tool is useful for consolidating legacy inventory files from multiple departments or sources. It reduces the manual effort of combining and cleaning data prior to ELN import and supports transparent, structured data workflows.

## Installation and Usage

1. Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/)
2. Install dependencies:

```r
install.packages(c("shiny", "readr", "writexl", "stringr", "DT", "shinyjs"))
```

3. Launch the application:

```r
shiny::runApp("path/to/csv-sql-merger")
```

## Dependencies
R ≥ 4.0
Packages:
- shiny
- readr
- writexl
- stringr
- DT
- shinyjs

## File Structure
- app.R – Main application code
- screenshot_sql_merger.png – Placeholder for interface screenshot
- readme.html – Optional help documentation
