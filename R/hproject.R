#' hcreate.project
#'
#' This function creates a minimal project folder structure
#'
#'
#'
#' @export
#'

hproject = function(dir = getwd()){
  paths = list.dirs(system.file("extdata", package = "hammond"))
  paths = paths[-1]
  file.copy(paths, dir, recursive = T)
}
