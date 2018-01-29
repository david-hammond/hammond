#' Save object to clipboard
#'
#' This function save an object to a clipboard.
#'
#' @param obj the object to be written to the file
#' @param size optional argument used for clipping very large objects
#'
#' @examples df <- data.frame(a = I("a \ quote"), b = pi)
#' hclip(df)
#'
#' @keywords utilities internal review
#' @export

hclip <- function(obj, size = 4096) {
  clip <- paste("clipboard-", size, sep = "")
  f <- file(description = clip, open = "w")
  write.table(obj, f, row.names = FALSE, sep = "\t")
  close(f)
}
