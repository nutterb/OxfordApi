#' @name get_oxford_grammatical_features
#' @title Get Grammatical Features for a Chosen Language
#' 
#' @description Returns the grammatical features available for the 
#'   selected language.
#'   
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language for which to look up 
#'   domains.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a data frame mapping region codes to labels.
#' 
#' \itemize{
#'  \code{category } The category of the grammatical feature.
#'  \code{feature } The name of the grammatical feature.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame of regions in the chosen language.
#'  \item Cast an error if the API returns an error.
#'  \item Cast an error if \code{app_id} is not \code{character(1)}.
#'  \item Cast an error if \code{app_id} is \code{NULL}.
#'  \item Cast an error if \code{app_key} is not \code{character(1)}.
#'  \item Cast an error if \code{app_key} is \code{NULL}.
#'  \item Cast an error if \code{language} is not \code{character(1)}.
#'  \item Cast an error if \code{url_base} is not \code{character(1)}.
#' }
#' 
#' @examples 
#' \dontrun{
#' set_application_access(app_id = [your_app_id],
#'                        app_key = [your_app_key])
#' get_oxford_grammatical_features()
#' }
#' 
#' @export

get_oxford_grammatical_features <- function(app_id = getOption("oxford_api_app_id"),
                                          app_key = getOption("oxford_api_app_key"),
                                          language = getOption("oxford_api_language"),
                                          url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  language <- validate_params(app_id = app_id,
                              app_key = app_key,
                              url_base = url_base,
                              language = language,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  url <- sprintf("%s/grammaticalfeatures/%s",
                 url_base,
                 language)
  
  response <- 
    httr::GET(url = url,
              httr::add_headers(app_id = app_id,
                                app_key = app_key))
  
  oxford_error(response)
  
  response <- rjson::fromJSON(as.character(response))
  
  features <- 
    mapply(
      FUN = function(name, content){
        data.frame(category = rep(name, length(unlist(content))),
                   feature = unlist(content),
                   stringsAsFactors = FALSE)
      }, 
      names(response[["results"]]),
      response[["results"]],
      SIMPLIFY = FALSE
    )
  
  features <- do.call("rbind", 
                      features)
  
  rownames(features) <- NULL
  features
}