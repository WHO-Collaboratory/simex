#' Attach resource path for logo
#'
#' @noRd
#'
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "simex",
    system.file("assets", package = "simex")
  )
}
