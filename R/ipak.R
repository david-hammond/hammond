#' install dependencies
#'
#' easy install for R packages, sourced from
#' https://gist.github.com/stevenworthington/3178163
#' @param pkg a string or list of packages to install
#'
#' @export
#'
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
