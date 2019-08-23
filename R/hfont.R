#' hfonts
#'
#' This function loads hammond fonts
#'
#' @param df
#'
#' @examples
#' x = hfont()
#'
#' @export
hfont = function(libname = find.package("hammond"), pkgname = "hammond"){
    require("extrafont")
    require(grDevices)
    font_import(paths = system.file("extdata", "graphik", package = "hammond"),
                pattern = "Graphik", prompt = FALSE)
    loadfonts("pdf", quiet = T)
    loadfonts("postscript", quiet = T)
    try(loadfonts("win", quiet = T))
}

