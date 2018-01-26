#' install dependencies
#'
#' easy install for R packages, sourced from
#' https://gist.github.com/stevenworthington/3178163
#'
#'
#' @examples ipak("tidyverse")
#'
#' @export
#'
ipak <- function(pkg.name) {
  args.check <- check.arguments(pkg.name)
  if (!exit.success(args.check)) {
    return(1)
  }

  new.pkg <- pkg.name[!(pkg.name %in% utils::installed.packages()[, 'Package'])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  tryCatch({
    suppressWarnings(sapply(pkg.name, require, character.only = T))
  }, warning = function(w) {
    message(w)
  }, error = function(e) {
    message(e)
  })
}

check.arguments <- function(...) {
  arg.supplied <- F
  tryCatch({
    arg.supplied <- all(sapply(c(...), function(x) !missing(x)))
  }, error = function(e) {
    message(e)
  })
  if (!arg.supplied) {
    message("\nPlease provide all required arguments")
    return(1)
  } else {
    return(0)
  }
}

exit.success <- function(obj) {
  return(identical(obj, 0))
}
