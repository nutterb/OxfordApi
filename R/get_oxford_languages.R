#' @name get_oxford_languages
#' @title Get the Languages Supported by the Oxford API
#' 
#' @description Returns a data frame of the languages supported by the
#'   Oxford Dictionaries API.
#'   
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a data frame describing the languages available in the API.
#' 
#' \itemize{
#'  \item \code{id } The language ID of the source dictionary.
#'  \item \code{language } The language name for the source dictionary.
#'  \item \code{source } The source dictionary name.
#'  \item \code{type } Identifier for whether the source dictionary is 
#'    monolingual or bilingual.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame describing the languages available in the API.
#'  \item Cast an error if the API returns an error.
#'  \item Cast an error if \code{app_id} is not \code{character(1)}.
#'  \item Cast an error if \code{app_id} is \code{NULL}.
#'  \item Cast an error if \code{app_key} is not \code{character(1)}.
#'  \item Cast an error if \code{app_key} is \code{NULL}.
#'  \item Cast an error if \code{url_base} is not \code{character(1)}.
#' }
#' 
#' @examples 
#' \dontrun{
#' set_application_access(app_id = [your_app_id],
#'                        app_key = [your_app_key])
#' get_oxford_languages()
#' }
#' 
#' @export

get_oxford_languages <- function(app_id = getOption("oxford_api_app_id"),
                                 app_key = getOption("oxford_api_app_key"),
                                 url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  language <- validate_params(app_id = app_id,
                              app_key = app_key,
                              url_base = url_base,
                              language = NULL,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  url <- sprintf("%s/languages",
                 url_base)
  
  response <- 
    httr::GET(url = url,
              httr::add_headers(app_id = app_id,
                                app_key = app_key))
  
  oxford_error(response)
  
  response <- as.character(response)
  response <- rjson::fromJSON(response)
  
  response <- 
    lapply(X = response[["results"]],
           FUN = function(x){
             data.frame(id = x[["sourceLanguage"]][["id"]],
                        language = x[["sourceLanguage"]][["language"]],
                        source = x[["source"]],
                        type = x[["type"]],
                        stringsAsFactors = FALSE)
           })
  
  do.call("rbind", response)
}
