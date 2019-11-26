#' @title getSQL
#' @description
#' this makes getting sql easier into R, use this instead of copying the sql file
#' Just make sure to SET NOCOUNT ON if you are using temp tables
#'
#' @param filepath the file path to a .sql file you would like to use
#'
#' @return The text from the .sql file
#' @export
#' @examples
#' getSQL("blep.sql")
getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}
