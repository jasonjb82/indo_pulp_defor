## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Check the numbers printed by calc_paper_stats() against
##   the numbers reported in the manuscript
## Notes: The manuscript values live in manuscript/manuscript_values.csv, one
##   row per number, written exactly as the manuscript prints it. Numbers are
##   compared as printed (after rounding), since that is where a mismatch with
##   the manuscript shows up.
##
## Pipeline inputs (targets in _targets.R)
##        1) paper_stats: Output of calc_paper_stats() (R/05_paper_stats.R).
##        2) manuscript_values_file -> manuscript/manuscript_values.csv: Each
##               number as printed in the manuscript, with its section. Kept
##               by hand; update it whenever the manuscript text changes.
##
## Pipeline outputs
##        1) manuscript_check: One row per manuscript value with its status
##               (match, mismatch or pending). Mismatches also raise a warning
##               during tar_make().
##        2) manuscript_check_csv -> outputs/text/manuscript_check.csv
## ---------------------------------------------------------

#' Compare paper stats sentences with the manuscript's numbers
#'
#' @param stats_list Output of calc_paper_stats()
#' @param values_csv Path to manuscript/manuscript_values.csv, with columns
#'   block (a text_* name in stats_list), section, value and note
#' @return A tibble with one row per manuscript value and a status of
#'   "match", "mismatch", "pending: sentence not produced" (an input is
#'   incomplete) or "pending: block not in pipeline yet" (no code or data)
check_manuscript_values <- function(stats_list, values_csv) {
  vals <- read_csv(
    values_csv,
    col_types = cols(.default = col_character())
  )

  strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x)
  block_text <- vapply(
    vals$block,
    function(b) {
      x <- stats_list[[b]]
      if (is.null(x)) {
        NA_character_
      } else {
        strip_ansi(paste(x, collapse = " "))
      }
    },
    character(1),
    USE.NAMES = FALSE
  )

  # Match the value as a whole number: "7" must not match inside "733,700"
  escape_regex <- function(s) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s)
  patterns <- paste0(
    "(?<![0-9.,])",
    escape_regex(vals$value),
    "(?![0-9]|[.,][0-9])"
  )
  found <- mapply(
    function(text, pattern) {
      !is.na(text) && nzchar(text) && grepl(pattern, text, perl = TRUE)
    },
    block_text,
    patterns,
    USE.NAMES = FALSE
  )

  # For mismatches, list what the pipeline printed so the cause is visible
  numbers_in <- function(text) {
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    nums <- regmatches(
      text,
      gregexpr("[0-9][0-9,]*(\\.[0-9]+)?|TRUE|FALSE", text)
    )[[1]]
    paste(unique(sub(",$", "", nums)), collapse = " ")
  }

  result <- vals %>%
    mutate(
      status = case_when(
        is.na(block_text) ~ "pending: block not in pipeline yet",
        !nzchar(block_text) ~ "pending: sentence not produced",
        found ~ "match",
        TRUE ~ "mismatch"
      ),
      pipeline_numbers = ifelse(
        status == "mismatch",
        vapply(block_text, numbers_in, character(1), USE.NAMES = FALSE),
        NA_character_
      )
    )

  mismatches <- result %>% filter(status == "mismatch")
  if (nrow(mismatches) > 0) {
    warning(
      "Paper stats differ from the manuscript for ",
      nrow(mismatches),
      " value(s):\n  ",
      paste0(
        mismatches$section,
        ": manuscript ",
        mismatches$value,
        " (",
        mismatches$note,
        ")",
        collapse = "\n  "
      ),
      call. = FALSE
    )
  }

  result
}

#' Write the manuscript check report to CSV
#' @param check_df Output of check_manuscript_values()
#' @param file_path Destination path
#' @return file_path, as required by format = "file"
save_manuscript_check <- function(check_df, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(check_df, file_path)
  file_path
}
