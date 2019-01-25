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
htrim.png = function(folder){
  require(magick)
  fnames = list.files(folder, full.names = T)
  pos = grepl("dont-clip", fnames)
  fnames = fnames[!pos]
  pblapply(fnames, function(i){
    image_read(i) %>%
      image_trim() %>% image_write(i, 'png')
  })
}
