#' @name get_oxford_registers
#' @title Get Domains for a Chosen Language
#' 
#' @description Returns the lexical categories available for the 
#'   selected language.
#'   
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language for which to look up 
#'   domains.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @return Returns a character vector of lexical categories in the 
#'   chosen language.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a character vector of domains in the chosen language.
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
#' get_oxford_registers()
#' }
#' 
#' @export

get_oxford_registers <- function(app_id = getOption("oxford_api_app_id"),
                                          app_key = getOption("oxford_api_app_key"),
                                          language = getOption("oxford_api_language"),
                                          url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = app_id,
                              len = 1,
                              add = coll)
  
  if (any(is.null(app_id)))
  {
    coll$push("`app_id` is set to NULL. Consider using `set_application_access`")
  }
  
  checkmate::assert_character(x = app_key,
                              len = 1,
                              add = coll)
  
  if (is.null(app_key))
  {
    coll$push("`app_key` is set to NULL. Consider using `set_application_access`")
  }

  language <- 
    checkmate::matchArg(x = language,
                        choices = oxford_languages,
                        add = coll)
  
  checkmate::assert_character(x = url_base,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  url <- sprintf("%s/registers/%s",
                 url_base,
                 language)
  
  response <- 
    httr::GET(url = url,
              httr::add_headers(app_id = app_id,
                                app_key = app_key))
  
  oxford_error(response)
  
  response <- as.character(response)
  response <- rjson::fromJSON(response)
  
  response <- unname(unlist(response[["results"]]))
  sub("%27", "'", response)
}