#' @name get_oxford_thesaurus
#' @title Get Thesaurus Entries from the Oxford Dictionaries API
#' 
#' @description Retrieve words that are similar or opposite a given word.
#' 
#' @param word_id \code{character(1)}. The word to search in the dictionary.
#' @param synonym \code{logical(1)}. If \code{TRUE}, return synonyms for 
#'   \code{word_id}.
#' @param antonym \code{logical(1)}. If \code{FALSE}, return antonyms for
#'   \code{word_id}.
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language in which to search.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a data frame:
#' \itemize{
#'  \item \code{search_term } The term searched in the dictionary.
#'  \item \code{thesaurus_entry } A term returned from the dictionary.
#'  \item \code{type } Character value indicating if \code{thesaurus_entry}
#'    is a synonym or an antonym.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame with the results of the API call.
#'  \item Cast an error if \code{word_id} is not \code{character(1)}.
#'  \item Cast an error if \code{synonym} is not \code{logical(1)}.
#'  \item Cast an error if \code{antonym} is not \code{logical(1)}.
#'  \item Return an empty data frame with a warning if both \code{synonym}
#'    and \code{antonym} are \code{FALSE}.
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
#' @export

get_oxford_thesaurus <- function(word_id, synonym = TRUE, antonym = FALSE,
                                 app_id = getOption("oxford_api_app_id"),
                                 app_key = getOption("oxford_api_app_key"),
                                 language = getOption("oxford_api_language"),
                                 url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = word_id,
                              len = 1,
                              add = coll)
  
  checkmate::assert_logical(x = synonym,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = antonym,
                            len = 1,
                            add = coll)
  
  language <- validate_params(app_id = app_id,
                              app_key = app_key,
                              url_base = url_base,
                              language = language,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!synonym && !antonym)
  {
    warning("Both `synonym` and `antonym` are FALSE.  No call made to the API.")
    return(NULL)
  }
  
  url <- sprintf("%s/entries/%s/%s/%s%s%s",
                 url_base,
                 language,
                 word_id,
                 if (synonym) "synonyms" else "",
                 if (synonym && antonym) ";" else "",
                 if (antonym) "antonyms" else "")
  
  response <- httr::GET(url = url,
                        httr::add_headers(app_id = app_id,
                                          app_key = app_key))
  
  oxford_error(response)
  
  response <- as.character(response)
  response <- rjson::fromJSON(response)
  response
}