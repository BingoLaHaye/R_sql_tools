#' Convert array to sql code
#' @description  This allows you to copy and array to your clipboard as a comma delimited and surrounded by string stuff.
#' This makes it easy to just copy lists of things for quick SQL queries.
#' @param string An array or list of items you wish to delimit.
#'
#' @return A list delimeted by , and each block surrounded by ' '.. This is auto copied to your clip board.
#' @export
#'
#' @examples string_to_sql(mtcars$mpg)
string_to_sql <- function(string){
  library(clipr)
 s <- paste0("'",paste(string, collapse = "', '"), "'")
  clipr::write_clip(s)
}
