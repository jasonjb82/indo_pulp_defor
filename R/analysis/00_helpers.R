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
