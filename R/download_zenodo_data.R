#' Download and extract Zenodo replication data if missing
download_zenodo_data <- function(
  zenodo_record_id,
  output_dir = "data/01_data_replication"
) {
  # 1. Only run if output_dir doesn't exist or is empty
  if (!dir.exists(output_dir) || length(list.files(output_dir)) == 0) {
    message(
      "Data missing! Fetching raw replication data directly from Zenodo..."
    )

    # 2. Set 15-minute timeout for large download
    old_options <- options(timeout = 900)
    on.exit(options(old_options))

    # 3. Download zip to a temporary file
    zip_dest <- tempfile(fileext = ".zip")
    zenodo_url <- paste0(
      "https://zenodo.org/api/records/",
      zenodo_record_id,
      "/files/01_data_replication.zip/content"
    )

    download.file(url = zenodo_url, destfile = zip_dest, mode = "wb")

    # 4. Unzip into a temporary staging folder
    temp_extract <- tempfile("zenodo_staging_")
    dir.create(temp_extract)
    unzip(zip_dest, exdir = temp_extract)
    unlink(zip_dest) # Delete zip file

    # 5. Ensure output_dir exists
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # 6. Move extracted contents into output_dir
    extracted_items <- list.files(temp_extract, full.names = TRUE)

    # If zip contained a top-level directory (e.g., '01_data_replication'), move its contents
    if (length(extracted_items) == 1 && dir.exists(extracted_items[1])) {
      inner_items <- list.files(extracted_items[1], full.names = TRUE)
      file.copy(inner_items, output_dir, recursive = TRUE)
    } else {
      file.copy(extracted_items, output_dir, recursive = TRUE)
    }

    # Clean up staging folder
    unlink(temp_extract, recursive = TRUE)

    message("Zenodo data downloaded and extracted successfully!")
  } else {
    message("Replication data already present locally. Skipping download.")
  }

  return(output_dir)
}
