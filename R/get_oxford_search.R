#' @name get_oxford_search 
#' @title Retrieve Possible Matches to Input
#' 
#' @description Retrieve possible headword matches for a given string of text.
#'   The results are culcated using headword matching, fuzzing matching, 
#'   and lemmatization. (Oxford Dictionaries API Documentation)
#'   
#' @param query \code{character(1)}. The string to search for.
#' @param prefix \code{logical(1)}. If \code{TRUE}, search only for terms 
#'   that start with \code{query}. Defaults to \code{FALSE}.
#' @param region One of \code{c("gb", "us")}. If searching in English, the region
#'   in which to search.
#' @param limit \code{integerish(1)} on the interval (1, 5000]. The limit of
#'   the number of response to return.
#' @param offset \code{integerish(1)}. Offset the start number of the result.
#'   Defaults to 0.
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language in which to search.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a data frame
#' \itemize{
#'  \item \code{match_id } The word ID of the matched term.
#'  \item \code{match_word } The word of the matched term.
#'  \item \code{match_string } The search term used in the API call.
#'  \item \code{match_type } The grammatical type of the match.
#'  \item \code{score } The match score.
#'  \item \code{region } The region for the search.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame with the results of the call.
#'  \item Casts an error if \code{query} is not \code{character(1)}.
#'  \item Casts an error if \code{prefix} is not \code{logical(1)}.
#'  \item Casts an error if \code{region} is not one of \code{c("gb", "us")}.
#'  \item Casts an error if \code{limit} is not \code{integerish(1)} on the
#'    interval (0, 5000].
#'  \item Casts an error if \code{offset} is not \code{integerish(1)} on the
#'    interval [0, 5000].
#'  \item Cast an error if the API returns an error.
#'  \item Cast an error if \code{app_id} is not \code{character(1)}.
#'  \item Cast an error if \code{app_id} is \code{NULL}.
#'  \item Cast an error if \code{app_key} is not \code{character(1)}.
#'  \item Cast an error if \code{app_key} is \code{NULL}.
#'  \item Cast an error if \code{language} is not one of 
#'    \code{oxford_languages}.
#'  \item Cast an error if \code{url_base} is not \code{character(1)}.
#' }
#' 
#' @examples 
#' \dontrun{
#' get_oxford_search("eye")
#' get_oxford_search("eye", limit = 10)
#' get_oxford_search("eye", limit = 10, 
#'                          offset = 11)
#' get_oxford_search("eye", limit = 10, 
#'                          offset = 11, 
#'                          region = "us")
#' get_oxford_search("eye", prefix = TRUE, 
#'                          limit = 10, 
#'                          offset = 11, 
#'                          region = "us")
#' }
#' 
#' @export

get_oxford_search <- function(query, prefix = FALSE, region = c("gb", "us"),
                                 limit = 5000, offset = 0)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = query,
                              len = 1,
                              add = coll)
  
  checkmate::assert_logical(x = prefix,
                            len = 1,
                            add = coll)
  
  region <- checkmate::matchArg(x = region,
                                choices = c("gb", "us"),
                                add = coll)
  
  checkmate::assert_integerish(x = limit,
                               len = 1,
                               lower = 1,
                               upper = 5000,
                               add = coll)
  
  checkmate::assert_integerish(x = limit,
                               len = 1,
                               lower = 0,
                               upper = 5000,
                               add = coll)
  
  language <- validate_params(app_id = app_id,
                              app_key = app_key,
                              url_base = url_base,
                              language = language,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  url <- 
    sprintf("%s/search/%s?q=%s&prefix=%s&regions=%s&limit=%s&offset=%s",
            url_base,
            language,
            query,
            tolower(prefix),
            region,
            limit,
            offset)
  
  response <- httr::GET(url = url,
                        httr::add_headers(app_id = app_id,
                                          app_key = app_key))
  
  oxford_error(response)
  
  response <- as.character(response)
  response <- rjson::fromJSON(response)
  
  response <- lapply(response[["results"]],
                     function(x){
                       data.frame(match_id = utils::URLdecode(x[["id"]]),
                                  match_word = x[["word"]],
                                  match_string = x[["matchString"]],
                                  match_type = x[["matchType"]],
                                  score = x[["score"]],
                                  region = x[["region"]],
                                  stringsAsFactors = FALSE)
                     })
  do.call("rbind", response)
}