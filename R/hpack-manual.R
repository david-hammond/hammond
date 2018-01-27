#' create package manual
#'
#' This function calculates combinations for efficient correlation calculations
#'
#' @param pack name of package
#'
#' @import devtools
#' @export

hpack.manual <- function(pack="hammond") {
  devtools::document()
  path <- find.package(pack)
  if (file.exists(file.path(getwd(), paste0(pack, '.pdf')))) {
    file.remove(file.path(getwd(), paste0(pack, '.pdf')))
  }
  system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf", shQuote(path)))
}
