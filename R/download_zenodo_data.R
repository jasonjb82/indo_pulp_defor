#' Download and extract Zenodo replication data if not already present
download_zenodo_data <- function(zenodo_record_id, output_dir) {
  if (!dir.exists(output_dir) || length(list.files(output_dir)) == 0) {
    message("Data missing! Fetching raw replication data directly from Zenodo...")
    
    # Direct Zenodo file URL using the Record ID
    zip_url <- paste0("https://zenodo.org/records/", zenodo_record_id, "/files/data_replication.zip?download=1")
    dest_file <- tempfile(fileext = ".zip")
    
    # Download and unzip
    download.file(zip_url, destfile = dest_file, mode = "wb")
    unzip(dest_file, exdir = output_dir)
    unlink(dest_file)
    message("Zenodo data downloaded and extracted successfully!")
  } else {
    message("Replication data already present locally. Skipping download.")
  }
  
  return(output_dir)
}