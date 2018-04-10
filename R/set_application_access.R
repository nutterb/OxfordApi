#' @name set_application_access
#' @title Set Application Access Parameters
#' 
#' @description Access to the Oxford Dictionaries API requires that 1) the 
#'   URL to the API is properly constructed, 2) the call utilizes a valid
#'   application ID, and 3) the call utilizes a valid application key.
#'   
#' @param url_base \code{character(1)}. Sets the base of the URL to the 
#'   Oxford Dictionaries API.
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The desired language for API calls.
#' 
#' @section Functional Requirements:
#' 
#' \code{set_application_access}
#' 
#' \enumerate{
#'  \item Successfully sets the value of \code{options("oxford_api_app_id")}
#'  \item Successfully sets the value of \code{options("oxford_api_app_key")}
#'  \item Casts an error if \code{app_id} is not a \code{character(1)}
#'  \item Casts an error if \code{app_id} does not have exactly eight characters.
#'  \item Casts an error if \code{app_key} is not a \code{character(1)}
#'  \item Casts an error if \code{app_key} does not have exactly 32 characters.
#' }
#' 
#' \code{set_oxford_url_base}
#' 
#' \enumerate{
#'  \item Successfully sets the value of \code{options("oxford_api_url_base")}
#'  \item Casts an error if \code{url_base} is not a \code{character(1)}
#' }
#' 
#' \code{set_application_id}
#' 
#' \enumerate{
#'  \item Successfully sets the value of \code{options("oxford_api_app_id")}
#'  \item Casts an error if \code{app_id} is not a \code{character(1)}
#'  \item Casts an error if \code{app_id} does not have exactly eight characters.
#' }
#' 
#' \code{set_application_key}
#' 
#' \enumerate{
#'  \item Successfully sets the value of \code{options("oxford_api_app_key")}
#'  \item Casts an error if \code{app_key} is not a \code{character(1)}
#'  \item Casts an error if \code{app_key} does not have exactly 32 characters.
#' }
#' 
#' \code{set_application_language}
#' 
#' \enumerate{
#'  \item Successfully sets the value of \code{options("oxford_api_language")}
#'  \item Casts an error if \code{language} is not one of 
#'    \code{oxford_languages} (See `?oxford_languages`)
#' }
#' 
#' \code{get_oxford_url_base}
#' 
#' \enumerate{
#'  \item Successfully retrieves the value of \code{options("oxford_api_url_base")}
#' }
#' 
#' \code{get_application_id}
#' 
#' \enumerate{
#'  \item Successfully retrieves the value of \code{options("oxford_api_app_id")}
#' }
#' 
#' \code{get_application_key}
#' 
#' \enumerate{
#'  \item Successfully retrieves the value of \code{options("oxford_api_app_key")}
#' }
#' 
#' \enumerate{
#'  \item Successfully retrieves the value of \code{options("oxford_api_language")}
#' }
#' 
#' @examples 
#' 
#' set_application_access(app_id = "myAppId1",
#'                        app_key = "This_is_not_a_real_key_123456789")
#' 
#' @export

set_application_access <- function(app_id, app_key)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = app_id,
                              len = 1,
                              add = coll)
  
  if (any(nchar(app_id) != 8))
  {
    coll$push("`app_id` must be eight (8) characters.")
  }
  
  checkmate::assert_character(x = app_key,
                              len = 1,
                              add = coll)
  
  if (any(nchar(app_key) != 32))
  {
    coll$push("`app_key` must be 32 characters.")
  }
  
  checkmate::reportAssertions(coll)
  
  set_application_id(app_id)
  set_application_key(app_key)
  
}

#' @rdname set_application_access
#' @export

set_oxford_url_base <- function(url_base)
{
  checkmate::assert_character(x = url_base,
                              len = 1)
  
  options(oxford_api_url_base = url_base)
}

#' @rdname set_application_access
#' @export

set_application_id <- function(app_id)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = app_id,
                              len = 1,
                              add = coll)
  
  if (any(nchar(app_id) != 8))
  {
    coll$push("`app_id` must be eight (8) characters.")
  }
  
  checkmate::reportAssertions(coll)
  
  options(oxford_api_app_id = app_id)
}

#' @rdname set_application_access
#' @export

set_application_key <- function(app_key)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = app_key,
                              len = 1,
                              add = coll)
  
  if (any(nchar(app_key) != 32))
  {
    coll$push("`app_key` must be 32 characters.")
  }
  
  checkmate::reportAssertions(coll)
  
  options(oxford_api_app_key = app_key)
}

#' @rdname set_application_access
#' @export

set_application_language <- function(language = getOption("oxford_api_language"))
{
  checkmate::matchArg(x = language,
                      choices = oxford_languages)
  
  options(oxford_api_language = language)
}

#' @rdname set_application_access
#' @export

get_application_id <- function()
{
  getOption("oxford_api_app_id")
}

#' @rdname set_application_access
#' @export

get_application_key <- function()
{
  getOption("oxford_api_app_key")
}

#' @rdname set_application_access
#' @export

get_oxford_url_base <- function()
{
  getOption("oxford_api_url_base")
}

#' @rdname set_application_access
#' @export

get_application_language <- function()
{
  getOption("oxford_api_language")
}