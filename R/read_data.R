## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Data reader helper functions for targets pipeline
## Author: Robert Heilmayr and Jason Jon Benedict
## ---------------------------------------------------------

#' Read spatial shapefile for Kabupaten (Districts)
read_kab_data <- function(filepath) {
  st_read(filepath, quiet = TRUE)
}

#' Read spatial shapefile for HTI Concessions
read_hti_data <- function(filepath) {
  st_read(filepath, quiet = TRUE)
}

#' Read wood supply / timber plot dataset
read_ws_data <- function(filepath) {
  read_csv(filepath, show_col_types = FALSE)
}

#' Read annual pulp production / capacity Excel file
read_cap_df <- function(filepath) {
  read_excel(filepath)
}

#' Read HTI and Non-HTI conversion dataset
read_hti_nonhti_conv <- function(filepath) {
  read_csv(filepath, show_col_types = FALSE)
}