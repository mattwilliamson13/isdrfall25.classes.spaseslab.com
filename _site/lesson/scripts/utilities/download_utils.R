#' Download a file from a web destination 
#'
#' @description
#' `download_file` silently downloads a file and returns the destination
#'
#' @details
#' This function works within the `download_and_unzip` function to download a file and return the destination to the target pipeline
#'
#' @param url a character string containing the weblink for the dataset to be downloaded
#' @param dest a character string containing the destination for the downloaded file

download_file <- function(url, dest) {
  download.file(url, dest)
  dest
}

#' Unzip a downloaded file into a user specified directory
#'
#' @description
#' `download_and_unzip` downloads a compressed file and extracts it to `extdir`. If `extdir` does not exist, `download_and_unzip` creates it.
#'
#' @details
#' Returns the path of the extracted files
#'
#' @param url a character string containing the weblink for the dataset to be downloaded
#' @param extdir a character string containing the destination for the extracted files
#' @param pat a character string identifying the file extension type of the extracted files to be returned

download_and_unzip <- function(url, extdir, pat) {
  downloaded_zip <- tempfile()
  download_file(url, downloaded_zip)
  unzip(downloaded_zip, exdir = dir_create(extdir))
  file.remove(downloaded_zip)
  list.files(extdir, pattern=pat, recursive = TRUE, full.names = TRUE)
}


gdrive_folder_download <- function(filelst, dstfldr){
  gdid <- as_id(filelst$id)
  fname <- sub("\\_.*", "", tools::file_path_sans_ext(filelst$name))
  ext   <- tools::file_ext(filelst$name)
  
  homedir <- file.path("data/original", dstfldr)
  if (!dir.exists(homedir)) dir.create(homedir, recursive = TRUE)
  
  outpath <- file.path(homedir, paste0(fname, ".", ext))
  drive_download(gdid, path = outpath, overwrite = TRUE)
  
  return(outpath)
}
