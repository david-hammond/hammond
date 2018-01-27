#' hcreate.project
#'
#' This function creates a minimal project folder structure
#'
#'
#'
#' @import ProjectTemplate
#' @export
#'

hproject = function(){
  require(ProjectTemplate)
  mydir = "tmp86402tmp"
  create.project(mydir, minimal = T)
  file.copy(from=list.dirs(mydir, full.names = T, recursive = T),
            to=getwd(),
            overwrite = TRUE, recursive = TRUE,
            copy.mode = TRUE)
  unlink(mydir, recursive=TRUE)
}
