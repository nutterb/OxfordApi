#' @name tidy_lexical_entry 
#' @title Organize a Lexical Entry into a Data Frame
#' 
#' @description A word may have multiple lexical entries. This function 
#'   gathers information about the entries into a data frame that is 
#'   suitable for indexing, filtering, etc.
#'   
#' @param le A list containing a lexical entry
#' 
# UNEXPORTED

tidy_lexical_entry <- function(le)
{
  gf <- tidy_grammatical_features(le$grammaticalFeatures)
  
  inflection <- tidy_inflection_of(le$inflectionOf)
  
  le <- data.frame(search_term = le$text,
                   language = le$language,
                   lexical_category = le$lexicalCategory,
                   stringsAsFactors = FALSE)
  
  le_inflect <- cbind(le, inflection)
  
  le_gf <- cbind(le, gf)
  
  merge(x = le_gf,
        y = le_inflect,
        by = c("search_term", "language", "lexical_category"),
        all = TRUE)
}