#' Build file download links
#'
#' Constructs full download URLs (currently by default, for CHELSA v2
#' bioclimatic variables from the 1981-2010 climatologies) by pasting together
#' a prefix, variable names, and a suffix.
#'
#' @param vars Character vector of variable identifiers. Defaults to
#'   `paste0("bio", 1:19)`.
#' @param prefix Character string. URL prefix pointing to the CHELSA file
#'   location. Defaults to
#'   `"https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_"`.
#' @param suffix Character string appended after each variable name.
#'   Defaults to `"_1981-2010_V.2.1.tif"`.
#'
#' @return A character vector of full URLs.
#'
#' @examples
#' linkbuild()
#'
#' linkbuild(c("bio1", "bio12", "scd"))
#'
#' @author A. Marcia Barbosa
#' @export


linkbuild <- function(vars = paste0("bio", 1:19),
                      prefix = "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",
                      suffix = "_1981-2010_V.2.1.tif") {

  # version 1.0 (23 Mar 2026)

  paste0(prefix, vars, suffix)
}
