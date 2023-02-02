#' saveAsFile
#'
#' @param fileName HTML-Filename.
#' @param fileNameOut Filename of image, either with .pdf or .png extension.
#' @param zoom Zoom factor, default = 3.
#' @param vwidth Width of frame to capture, default = 430.
#' @param ... Other potential parameters to be passed to webshot2::webshot()
#'
#' @import webshot2
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'   saveAsFile("webFile.html", "outputFile.png")}
saveAsFile <- function(fileName, fileNameOut, zoom = 3, vwidth = 430, ...) {
  webshot2::webshot(
    url = fileName,
    file = fileNameOut,
    selector = "svg",
    zoom = zoom,
    vwidth = vwidth,
    ...)
}