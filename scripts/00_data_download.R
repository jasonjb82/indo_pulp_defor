options(timeout = max(300, 900))
zenodo_url <- "https://zenodo.org/api/records/20448607/files/01_data_replication.zip/content"
download.file(url = zenodo_url, destfile = "data/01_data_replication.zip", mode = "wb")
unzip("data/01_data_replication.zip", exdir = "data/")
