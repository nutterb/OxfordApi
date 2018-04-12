#' @name tidy_grammatical_features
#' @title Organize Grammatical Features into a Data Frame
#' 
#' @description Lexical entries may have multiple grammatical features.
#'   This function organizes these into a data frame with one row per 
#'   features.  This is a necessary step toward organizing a lexical entry.
#'   
#' @param gf a list of grammatical features.
#' 
#' @return A data frame with columns \code{grammatical_feature} and 
#' \code{grammatical_feature_type}.
#' 
# UNEXPORTED

tidy_grammatical_features <- function(gf)
{
  gf <- unlist(gf)
  
  data.frame(grammatical_feature = gf[which(names(gf) == "text")],
             grammatical_feature_type = gf[which(names(gf) == "type")],
             stringsAsFactors = FALSE)
}