#' @name get_oxford_filters
#' @title Get the Languages Supported by the Oxford API
#' 
#' @description Returns a data frame of the filters supported by the 
#'   Oxford Dictionaries API>
#'   
#' @param endpoint \code{character(1)} or \code{NULL}. If given, the filters
#'   for that endpoint are retrieved.
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a data frame describing the filters available in the API.
#' 
#' \itemize{
#'  \item \code{endpoint } The feature for which the filter is available.
#'  \item \code{filter } The name of the filter.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame describing the filters available in the API.
#'  \item Correctly returns only the filters for the indicated endpoint
#'    when \code{endpoint} is not \code{NULL}.
#'  \item Cast an error if the API returns an error.
#'  \item Cast an error if \code{endpoint} is not \code{character(1)} or
#'    \code{NULL}.
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

get_oxford_filters <- function(endpoint = NULL, 
                               app_id = getOption("oxford_api_app_id"),
                               app_key = getOption("oxford_api_app_key"),
                               url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = endpoint,
                              len = 1,
                              null.ok = TRUE,
                              add = coll)
  
  language <- validate_params(app_id = app_id,
                              app_key = app_key,
                              url_base = url_base,
                              language = NULL,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  endpoint <- if (is.null(endpoint)) ""
              else paste0("/", endpoint)
  
  url <- sprintf("%s/filters%s",
                 url_base,
                 endpoint)
  
  response <- 
    httr::GET(url = url,
              httr::add_headers(app_id = app_id,
                                app_key = app_key))
  
  oxford_error(response)
  
  response <- as.character(response)
  response <- rjson::fromJSON(response)
  
  filters <- 
    mapply(
      FUN = function(name, content){
        data.frame(endpoint = rep(name, length(content)),
                   filter = content,
                   stringsAsFactors = FALSE)
      }, 
      names(response[["results"]]),
      response[["results"]],
      SIMPLIFY = FALSE
    )
  
  filters <- do.call("rbind", 
                     filters)
  
  rownames(filters) <- NULL
  filters
}
