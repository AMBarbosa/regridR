#' Download files if missing or incomplete
#'
#' @description
#' Checks a set of file download links and downloads each file into a destination
#' directory if it is not already present or if the existing file appears
#' incomplete. File completeness is assessed by comparing the local file size
#' with the remote file size obtained via an HTTP HEAD request.
#'
#' @param links Character vector of file download URLs.
#' @param destdir Path to the directory where files should be saved.
#'
#' @details
#' For each URL in `links`, this function:
#' \itemize{
#'   \item Determines the expected file name using `basename()`.
#'   \item Checks whether the file already exists in `destdir`.
#'   \item If it exists, compares its size to the remote file size obtained
#'         from the `content-length` header of an HTTP HEAD request.
#'   \item Downloads the file only if it is missing or appears incomplete.
#' }
#'
#' For large files, consider increasing the download timeout, e.g.:
#' \code{options(timeout = 6000)}.
#'
#' @return Invisibly returns `NULL`. Files are written to `destdir` as a side effect.
#'
#' @examples
#' \dontrun{
#' links <- c(
#'   paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/",
#'   "1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif"),
#'   paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/";
#'   "1981-2010/bio/CHELSA_bio12_1981-2010_V.2.1.tif")
#' )
#'
#' downloadif(links, destdir = tempdir())
#'
#'
#' links <- c(links,
#'   paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/",
#'   "1981-2010/bio/CHELSA_bio8_1981-2010_V.2.1.tif"))
#'
#' downloadif(links, destdir = tempdir())
#' }
#'
#' @importFrom httr HEAD
#' @importFrom utils download.file

#' @export

downloadif <- function(links, destdir) {

  for (l in 1:length(links)) {
    link <- links[l]
    file_name <- basename(link)
    message(l, "\n", file_name)
    file_path <- paste(destdir, file_name, sep = "/")
    download <- TRUE

    if (file.exists(file_path)) {
      # check if file is corrupted and needs new download
      file_size <- file.info(file_path)$size
      download_size <- as.numeric(httr::HEAD(link)$headers$`content-length`)  # https://stackoverflow.com/questions/63852146/how-to-determine-online-file-size-before-download-in-r
      if (all.equal(file_size, download_size))  download <- FALSE
    }  # end if file.exists

    if (isTRUE(download))
      utils::download.file(link, destfile = file_path, mode = "wb")
  }  # end if download
}
