#' @name tidy_inflection_of
#' @title Organize Word Inflections into a Data Frame
#' 
#' @description A word may have multiple inflections.  This function 
#'   organizes them into a data frame that may be joined with other 
#'   lexical entry information.
#'   
#' @param inflection A list containing inflections of a word.
#' 
# UNEXPORTED

tidy_inflection_of <- function(inflection)
{
  inflection <- unlist(inflection)
  
  data.frame(inflection_of_id = inflection[which(names(inflection) == "id")],
             inflection_of = inflection[which(names(inflection) == "text")],
             stringsAsFactors = FALSE)
}