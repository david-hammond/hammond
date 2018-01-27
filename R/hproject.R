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
  create.project("tmp", minimal = T)
  file.copy(from="tmp", to=".",
            overwrite = TRUE, recursive = TRUE,
            copy.mode = TRUE)
  file.remove(tmp)
}
