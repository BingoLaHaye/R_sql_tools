#' @title Insert characters and convert strings to dates
#' @description This function is used to clean up bad date formats. Many times during the census I tried to automate the picking up and dating of files.
#' There were problems in this that Nathanial or just general humans don't put 0's in front of their single dates. EX: 9102019 as opposed to 09102019.
#' This caused a lot of date format problems when you try to grab everything at the same time. This functions solves it by inserting a 0 lets say at a certain index.
#' So we can use this function with some other logic to clean up the character strings before converting to dates.
#'
#' @param census_files Path to the census file.
#' @param after_index The index in the character string where you want to add a number.
#' @param thing_insert The number you wish to insert into the string.
#' @param convert_date Would you like the function to return a date or return a raw character string. Use FALSE for a string and TRUE for a date.
#' @param date_format Define the Date format you would like to convert from.
#'
#' @return The \code{census_files} string where the letter from \code{thing_insert} is put after the \code{after_index}.
#' This is then either converted to a date if \code{convert_date} is TRUE and converts the string using the \code{date_format}.
#' If \code{convert_date} is FALSE it will just return the raw string instead of converting to a date type.
#' @export
#' @examples
#' get_date_pathV3(census_files = "CNMC ER 8052019",
#' after_index = 0,
#' thing_insert = 0, convert_date = TRUE, date_format = "%m%d%Y")
get_date_pathV3 <- function(census_files, after_index, thing_insert, convert_date = FALSE, date_format = "%m%d%Y") {
  library(stringr)
  library(dplyr)
  search_algo<- paste0('^([0-9]{',after_index ,'})([0-9]+)$')
  insert_algo <- paste0('\\1',thing_insert ,'\\2')
  dates <- paste(gsub("[^[:digit:]]", "\\1", census_files))
  dates2 <- if_else(str_length(dates) == 7, true = gsub(search_algo, insert_algo, dates), false = dates)
  if (convert_date == TRUE) {
    dates2 <- as.Date(dates2, format = date_format)
    return(dates2)
  } else {
    return(dates2)
  }
}
