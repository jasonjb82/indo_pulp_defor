## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Small helpers shared by the analysis targets
## ---------------------------------------------------------

#' Write a character vector to a text file, creating the folder if needed
#' @param lines Character vector, one element per line
#' @param file_path Destination path
#' @return file_path, as required by format = "file"
save_text_lines <- function(lines, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file_path)
  file_path
}

#' Write a table to CSV, creating the folder if needed
#'
#' Writes with the same function the standalone analysis script used, so SI
#' tables keep the same layout: write.csv() without row names (which quotes
#' every field, preserving line breaks inside cells) or readr::write_csv().
#' @param df Data frame to write
#' @param file_path Destination path
#' @param writer "base" for write.csv() or "readr" for readr::write_csv()
#' @return file_path, as required by format = "file"
save_csv_table <- function(df, file_path, writer = c("base", "readr")) {
  writer <- match.arg(writer)
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  if (writer == "base") {
    utils::write.csv(df, file_path, row.names = FALSE)
  } else {
    readr::write_csv(df, file_path)
  }
  file_path
}
