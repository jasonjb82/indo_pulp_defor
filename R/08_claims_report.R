## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Build a single report of every claim in the paper and
##   SM, with the live values, the check against the manuscript, and the
##   provenance of each claim (the functions and input files behind it).
## Notes: targets does not allow a running pipeline to inspect its own graph
##   (tar_network() inside a target recurses), so _targets.R records the
##   dependency graph when the pipeline is defined, using
##   pipeline_dependency_graph(), and passes it to the report target as data.
##   A change to any target's command changes the graph and rebuilds the
##   report, so the provenance cannot fall out of date.
##
## Pipeline inputs (targets in _targets.R)
##        1) paper_stats: Main text and SM Section 4 sentences.
##               Produced by calc_paper_stats() (R/05_paper_stats.R)
##        2) manuscript_check: Status of each manuscript number.
##               Produced by check_manuscript_values() (R/07_manuscript_check.R)
##        3) mai_results, elast_results, rf_results, scenario_results: The SI
##               statements written by analysis scripts 02-05.
##        4) The dependency graph recorded by pipeline_dependency_graph().
##
## Pipeline outputs
##        1) claims_report_md -> outputs/text/claims_report.md
## ---------------------------------------------------------

#' Record the dependency graph of a list of targets
#'
#' Reads each target's name, storage format, command and the symbols its
#' command uses. These live in the target objects' internal fields, so the
#' function checks they exist and returns NULL (the report then omits
#' provenance) if a future version of targets changes them.
#' @param targets_list The list of tar_target() objects defined in _targets.R
#' @return A tibble with name, format, command and deps (list column), or NULL
pipeline_dependency_graph <- function(targets_list) {
  # Field locations as of targets 1.12 (name and deps on the target itself),
  # falling back to where earlier versions kept them
  get_field <- function(...) {
    for (getter in list(...)) {
      value <- tryCatch(getter(), error = function(e) NULL)
      if (!is.null(value)) return(value)
    }
    NULL
  }
  rows <- lapply(targets_list, function(t) {
    name <- get_field(function() t$name, function() t$settings$name)
    format <- get_field(function() t$settings$format)
    expr <- get_field(function() t$command$expr)
    deps <- get_field(function() t$deps, function() t$command$deps)
    if (is.null(name) || is.null(expr) || is.null(deps)) {
      return(NULL)
    }
    tibble(
      name = name,
      format = format %||% "rds",
      command = paste(deparse(expr), collapse = " "),
      deps = list(as.character(deps))
    )
  })
  if (any(vapply(rows, is.null, logical(1)))) {
    return(NULL)
  }
  bind_rows(rows)
}

#' Map each function defined in R/ to the file that defines it
#' @param dir Folder to scan
#' @return Named character vector: function name -> file path
r_function_files <- function(dir = "R") {
  files <- list.files(dir, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  out <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    defs <- regmatches(
      lines,
      regexpr("^[A-Za-z_.][A-Za-z0-9_.]* <- function", lines)
    )
    fns <- sub(" <- function$", "", defs)
    out <- c(out, stats::setNames(rep(f, length(fns)), fns))
  }
  out
}

#' Trace the functions and input files upstream of a target
#' @param graph Output of pipeline_dependency_graph()
#' @param target Name of the target to trace from
#' @param fn_files Output of r_function_files()
#' @return A list: functions (named by file), input_files (paths) and
#'   targets (all upstream target names)
trace_provenance <- function(graph, target, fn_files) {
  seen <- character(0)
  queue <- target
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    if (current %in% seen || !current %in% graph$name) {
      next
    }
    seen <- c(seen, current)
    deps <- graph$deps[[match(current, graph$name)]]
    queue <- c(queue, setdiff(intersect(deps, graph$name), seen))
  }

  # Input files: file targets whose command names a literal path (outputs are
  # written by save_* functions and are never upstream of a claim)
  file_rows <- graph[graph$name %in% seen & graph$format == "file", ]
  quoted <- regmatches(
    file_rows$command,
    gregexpr('"[^"]+[.][A-Za-z0-9]+"', file_rows$command)
  )
  input_files <- unique(gsub('"', "", unlist(quoted)))
  input_files <- input_files[!grepl("^outputs/|zenodo_record", input_files)]

  used_fns <- unique(unlist(graph$deps[match(seen, graph$name)]))
  used_fns <- intersect(used_fns, names(fn_files))
  list(
    functions = fn_files[used_fns],
    input_files = sort(input_files),
    targets = seen
  )
}

#' Render a provenance list as Markdown lines
provenance_markdown <- function(prov) {
  if (is.null(prov)) {
    return("*Provenance unavailable: the dependency graph could not be read.*")
  }
  fn_by_file <- split(names(prov$functions), unname(prov$functions))
  c(
    "**Functions** (by file):",
    "",
    unlist(lapply(names(fn_by_file), function(f) {
      sprintf(
        "- `%s`: %s",
        f,
        paste0("`", sort(fn_by_file[[f]]), "()`", collapse = ", ")
      )
    })),
    "",
    "**Input files** (relative to `data/01_data_replication/` unless a folder is given):",
    "",
    paste0("- `", prov$input_files, "`"),
    ""
  )
}

#' Build the claims report
#'
#' @param paper_stats Output of calc_paper_stats()
#' @param manuscript_check Output of check_manuscript_values()
#' @param si_sources Named list; each element a list with title, target (the
#'   producing target's name) and text (character vector)
#' @param graph Output of pipeline_dependency_graph(), or NULL
#' @param file_path Destination .md path
#' @return file_path, as required by format = "file"
build_claims_report <- function(
  paper_stats,
  manuscript_check,
  si_sources,
  graph,
  file_path
) {
  fn_files <- r_function_files("R")
  prov_for <- function(target) {
    if (is.null(graph)) NULL else trace_provenance(graph, target, fn_files)
  }

  # ANSI bold markers in the sentences become Markdown bold
  to_md <- function(x) {
    x <- gsub("\033\\[1m", "**", x)
    gsub("\033\\[0m", "**", x)
  }

  status_counts <- table(manuscript_check$status)
  mismatches <- manuscript_check[manuscript_check$status == "mismatch", ]

  # --- Summary --------------------------------------------------------------
  lines <- c(
    "# Claims report",
    "",
    paste0(
      "Every quantitative claim in the main text and SM that the pipeline ",
      "reproduces, with its current value, its check against the manuscript ",
      "(`manuscript/manuscript_values.csv`) and its provenance. Generated by ",
      "`build_claims_report()` (`R/08_claims_report.R`); rebuilt automatically ",
      "by `targets::tar_make()` whenever a claim or its inputs change."
    ),
    "",
    "## Summary",
    "",
    "| Status | Numbers |",
    "|---|---|",
    sprintf("| %s | %d |", names(status_counts), as.integer(status_counts)),
    ""
  )
  if (nrow(mismatches) > 0) {
    # One row per sentence: the manuscript numbers that differ, next to the
    # numbers the pipeline prints in that sentence
    by_block <- split(mismatches, mismatches$block)
    by_block <- by_block[unique(mismatches$block)]
    lines <- c(
      lines,
      "**Sentences whose numbers differ from the manuscript:**",
      "",
      "| Section | Sentence | Manuscript | Pipeline prints |",
      "|---|---|---|---|",
      vapply(
        by_block,
        function(m) {
          sprintf(
            "| %s | `%s` | %s | %s |",
            m$section[1],
            m$block[1],
            paste(m$value, collapse = ", "),
            m$pipeline_numbers[1]
          )
        },
        character(1)
      ),
      ""
    )
  }

  # --- Main text and SM -----------------------------------------------------
  text_keys <- grep("^text_", names(paper_stats), value = TRUE)
  lines <- c(
    lines,
    "## Main text and SM Section 4",
    "",
    "All sentences below are produced by `calc_paper_stats()` (`R/05_paper_stats.R`, target `paper_stats`).",
    "",
    "<details><summary>Provenance of these sentences</summary>",
    "",
    provenance_markdown(prov_for("paper_stats")),
    "</details>",
    ""
  )
  mark <- c(match = "✓", mismatch = "✗")
  for (key in text_keys) {
    block <- to_md(paper_stats[[key]])
    block_lines <- strsplit(paste(block, collapse = ""), "\n")[[1]]
    block_lines <- block_lines[nzchar(trimws(block_lines))]
    heading <- sub("^\\[(.*)\\]$", "\\1", block_lines[1])
    body <- block_lines[-1]
    checks <- manuscript_check[manuscript_check$block == key, ]
    check_line <- if (nrow(checks) == 0) {
      "*Not in the manuscript check.*"
    } else {
      paste0(
        "Manuscript check: ",
        paste0(
          checks$value,
          " ",
          ifelse(checks$status %in% names(mark), mark[checks$status], "…"),
          collapse = " · "
        )
      )
    }
    lines <- c(
      lines,
      paste0("### ", heading, " (`", key, "`)"),
      "",
      paste0("> ", body),
      "",
      check_line,
      ""
    )
  }

  # Claims in the manuscript check with no code yet
  no_code <- manuscript_check[
    manuscript_check$status == "pending: block not in pipeline yet",
  ]
  if (nrow(no_code) > 0) {
    lines <- c(
      lines,
      "### Claims not yet reproduced by the pipeline",
      "",
      sprintf(
        "- %s: %s (%s)",
        no_code$section,
        no_code$value,
        no_code$note
      ),
      ""
    )
  }

  # --- SI statements from the analysis scripts ------------------------------
  lines <- c(lines, "## SI statements from the analysis scripts", "")
  for (src in si_sources) {
    lines <- c(
      lines,
      paste0("### ", src$title, " (target `", src$target, "`)"),
      "",
      "<details><summary>Provenance</summary>",
      "",
      provenance_markdown(prov_for(src$target)),
      "</details>",
      "",
      "```text",
      src$text,
      "```",
      ""
    )
  }

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file_path, useBytes = TRUE)
  file_path
}
