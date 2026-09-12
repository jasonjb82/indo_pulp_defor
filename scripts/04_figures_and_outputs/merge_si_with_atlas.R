## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Append the SI section 9 concession atlas to the exported SI PDF
## Author: Robert Heilmayr and Jason Jon Benedict
## ---------------------------------------------------------
##
## Run after exporting the Word SI to PDF (export with "Best for electronic
## distribution and accessibility" so the SI's own internal hyperlinks survive).
## The atlas is recompiled here so its page numbers continue the SI's, which is
## why this is a submission-time script rather than a targets target: it depends
## on a manually produced export at a machine-specific path.
##
## Usage:
##   Rscript scripts/04_figures_and_outputs/merge_si_with_atlas.R <si_pdf> [out_pdf]

source("R/06_si_concession_atlas.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop(
    "Usage: Rscript scripts/04_figures_and_outputs/merge_si_with_atlas.R ",
    "<si_pdf> [out_pdf]",
    call. = FALSE
  )
}

si_pdf <- args[[1]]
out_pdf <- if (length(args) >= 2) {
  args[[2]]
} else {
  file.path(
    "data/01_data_replication/04_results/atlas",
    paste0(tools::file_path_sans_ext(basename(si_pdf)), "_with_atlas.pdf")
  )
}

merge_si_pdf(si_pdf, out_pdf)
