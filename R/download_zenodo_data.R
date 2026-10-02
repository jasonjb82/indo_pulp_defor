#' Download and extract Zenodo replication data if missing or outdated
#'
#' A marker file (.zenodo_record) in output_dir records which Zenodo record
#' the local data came from. The zip is downloaded only when output_dir is
#' missing or empty, or when the marker names a different record, so changing
#' zenodo_record_id to a new Zenodo version fetches that version.
#'
#' A new version is unzipped into a staging folder next to output_dir and
#' swapped in only once extraction succeeds, so a failed download never leaves
#' a mix of old and new files. The swap replaces the whole folder, including
#' any local edits made to the previous version's files.
#'
#' @param zenodo_record_id Zenodo record ID, e.g. "21542417"
#' @param output_dir Folder the replication data are extracted into
#' @return output_dir
download_zenodo_data <- function(
  zenodo_record_id,
  output_dir = "data/01_data_replication"
) {
  zenodo_record_id <- as.character(zenodo_record_id)
  marker_file <- file.path(output_dir, ".zenodo_record")

  # list.files() skips dotfiles, so the marker alone does not count as data
  has_data <- dir.exists(output_dir) && length(list.files(output_dir)) > 0
  local_record <- if (file.exists(marker_file)) {
    trimws(readLines(marker_file, n = 1, warn = FALSE))
  } else {
    NA_character_
  }

  # 1. Data present and from the requested record: nothing to do
  if (has_data && identical(local_record, zenodo_record_id)) {
    message("Replication data already present locally. Skipping download.")
    return(output_dir)
  }

  # 2. Data present but downloaded before markers existed: assume it is the
  #    requested record rather than re-downloading, and record that
  if (has_data && is.na(local_record)) {
    writeLines(zenodo_record_id, marker_file)
    message(
      "Replication data present without a record marker; marking it as ",
      "Zenodo record ",
      zenodo_record_id,
      "."
    )
    return(output_dir)
  }

  # 3. Data missing, or from a different record: download and swap in
  if (has_data) {
    message(
      "Local replication data are from Zenodo record ",
      local_record,
      "; fetching record ",
      zenodo_record_id,
      "..."
    )
  } else {
    message(
      "Data missing! Fetching raw replication data directly from Zenodo..."
    )
  }

  # Set 15-minute timeout for large download
  old_options <- options(timeout = 900)
  on.exit(options(old_options), add = TRUE)

  zip_dest <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_dest), add = TRUE)
  zenodo_url <- paste0(
    "https://zenodo.org/api/records/",
    zenodo_record_id,
    "/files/01_data_replication.zip/content"
  )
  download.file(url = zenodo_url, destfile = zip_dest, mode = "wb")

  # Stage beside output_dir (same drive) so the final swap is a fast rename
  staging_dir <- paste0(output_dir, "_staging_", zenodo_record_id)
  unlink(staging_dir, recursive = TRUE)
  dir.create(staging_dir, recursive = TRUE)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)
  unzip(zip_dest, exdir = staging_dir)

  # If the zip contained a top-level directory (e.g., '01_data_replication'),
  # its contents are the data
  extracted_items <- list.files(staging_dir, full.names = TRUE)
  new_root <- if (length(extracted_items) == 1 && dir.exists(extracted_items[1])) {
    extracted_items[1]
  } else {
    staging_dir
  }
  writeLines(zenodo_record_id, file.path(new_root, ".zenodo_record"))

  # Swap: move the old folder aside, move the new one in, then delete the old
  old_dir <- paste0(output_dir, "_previous")
  if (dir.exists(output_dir)) {
    unlink(old_dir, recursive = TRUE)
    if (!file.rename(output_dir, old_dir)) {
      stop("Could not move ", output_dir, " aside to install the new data.")
    }
  }
  if (!file.rename(new_root, output_dir)) {
    if (dir.exists(old_dir)) {
      file.rename(old_dir, output_dir)
    }
    stop("Could not move the downloaded data into ", output_dir, ".")
  }
  unlink(old_dir, recursive = TRUE)

  message("Zenodo data downloaded and extracted successfully!")
  output_dir
}
