#' @name get_oxford_inflections
#' @title Check if a Word Exists in the Dictionary and Retrieve its Root Form
#' 
#' @description "Use this to check if a word exists in the dictionary, or what
#' 'root' form it links to (e.g., swimming > swim). The response tells you
#' the possible lemmas for a given inflected word. This can then be combined
#' with other endpoints to retrieve more information." (Source: Oxford 
#' Dictionaries API Documentation)
#' 
#' @param word_id \code{character(1)} The word to search for in the dictionary.
#' @param ... Named arguments of character vectors giving additional filters
#'   to apply to the search.
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language in which to search.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a data frame
#' \itemize{
#'  \item \code{search_term_id } The search term's word ID
#'  \item \code{search_term } The text of the search term.
#'  \item \code{language } The language for which the search was conducted
#'  \item \code{lexical_category } The lexical category of the inflection.
#'  \item \code{grammatical_feature } The grammatical feature of the inflection.
#'  \item \code{grammatical_feature_type } The type of grammatical feature.
#'  \item \code{inflection_id } The word ID of the word for which the search
#'    term is an inflection.
#'  \item \code{inflection_text } The text of the word for which the search 
#'    term is an inflection.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame with the results of the API call.
#'  \item Cast an error if \code{word_id} is not \code{character(1)}.
#'  \item Cast an error if any argument to \code{...} is not named.
#'  \item Cast an error if any argument to \code{...} is not a character 
#'    vector.
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
#' get_oxford_inflections(word_id = "swimming")
#' 
#' get_oxford_inflections(word_id = "change",
#'                        grammaticalFeatures = c("singular", "past"),
#'                        lexicalCategory = "noun")
#' }
#' 
#' @export

get_oxford_inflections <- function(word_id, ...,
                                   app_id = getOption("oxford_api_app_id"),
                                   app_key = getOption("oxford_api_app_key"),
                                   language = getOption("oxford_api_language"),
                                   url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = word_id,
                              len = 1,
                              add = coll)
  
  filters <- list(...)
  
  checkmate::assert_list(x = filters,
                         types = "character",
                         names = "named",
                         add = coll)
  
  language <- validate_params(app_id = app_id,
                              app_key = app_key,
                              url_base = url_base,
                              language = language,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  filters <- 
    mapply(FUN = 
             function(nms, fls){
               paste0(nms, "=", paste0(fls, collapse = ","))
             },
           names(filters),
           filters,
           SIMPLIFY = FALSE)
  
  filters <- paste0(unlist(filters), collapse = ";")
  if (filters != "") filters <- paste0("/", filters)
  
  url <- sprintf("%s/inflections/%s/%s%s",
                 url_base,
                 language,
                 word_id,
                 filters)
  
  response <- httr::GET(url = url,
                        httr::add_headers(app_id = app_id,
                                          app_key = app_key))
  
  oxford_error(response)
  
  response <- as.character(response)
  response <- rjson::fromJSON(response)
  
  entry <- 
    do.call("rbind",
            lapply(response$results[[1]]$lexicalEntries,
                   tidy_lexical_entry))

  data.frame(search_term_id = response$results[[1]]$id, 
             entry,
             stringsAsFactors = FALSE)
}