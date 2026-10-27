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

#' ggsave() with showtext switched off for the duration of the call
#'
#' R/02_fig2_defor_timing.R enables showtext at load time (for DM Sans in
#' Figures 1 and 2), and tar_source() loads it into every pipeline session.
#' showtext replaces system fonts (e.g. Courier New) and renders text at its
#' own dpi, so the analysis figures - drawn without showtext in the standalone
#' scripts - are saved with it switched off, then it is switched back on.
#' @param ... Passed to ggplot2::ggsave()
#' @return The value of ggsave(), invisibly
ggsave_without_showtext <- function(...) {
  if (isNamespaceLoaded("showtext")) {
    showtext::showtext_auto(FALSE)
    on.exit(showtext::showtext_auto(TRUE), add = TRUE)
  }
  invisible(ggsave(...))
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


#' Write a modelsummary table, falling back to markdown if pandoc is missing
#'
#' .docx output goes through pandoc, which is an external program rather than
#' an R dependency, so a replicator can have every package installed and still
#' be unable to produce it. Failing the whole pipeline over that is harsh, but
#' silently skipping the table is worse. This writes the same table as markdown
#' instead and warns loudly, so the contents are still available and the
#' omission is impossible to miss. The returned path is what was actually
#' written, which keeps the calling format = "file" target valid.
#'
#' @param write_fn Function of one argument (the output path) that writes the
#'   table.
#' @param path Intended output path, normally .docx.
#' @return Path actually written: `path`, or the markdown fallback.
write_table_or_warn <- function(write_fn, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  err <- NULL
  tryCatch(write_fn(path), error = function(e) err <<- conditionMessage(e))
  if (is.null(err)) {
    return(path)
  }

  fallback <- sub("\\.docx$", ".md", path)
  md_err <- NULL
  tryCatch(write_fn(fallback), error = function(e) md_err <<- conditionMessage(e))
  if (!is.null(md_err)) {
    stop(
      "Could not write ", path, " (", err, ") ",
      "nor its markdown fallback (", md_err, ")",
      call. = FALSE
    )
  }

  warning(
    "\n",
    strrep("!", 70), "\n",
    "!! ", basename(path), " WAS NOT PRODUCED.\n",
    "!! Reason: ", err, "\n",
    "!! .docx output requires the pandoc program on the PATH; it is not an\n",
    "!! R package, so renv::restore() does not supply it.\n",
    "!! The same table has been written as ", basename(fallback), " instead.\n",
    strrep("!", 70), "\n",
    call. = FALSE,
    immediate. = TRUE
  )
  fallback
}
