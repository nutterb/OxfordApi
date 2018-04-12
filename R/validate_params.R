#' @name validate_params
#' @title Validate Common Parameters
#' 
#' @description All of the API calls require the Application ID, Application 
#'   Key, and URL base.  Many of them also require a language designation.
#'   All of these may be set as global options, which the functions will
#'   reference.  These parameters are validated in each call. 
#'   \code{validate_params} allows the validations to be performed 
#'   consistently between functions, and avoids duplication of effort.
#'   
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language for which to look up 
#'   domains.
#' @param url_base \code{character(1)}. The url stem for the API.
#' @param add An object of class \code{assertCollection}.
#' 
#' @details \code{validate_params} violates one commonly accepted best 
#'   practice in that it both creates side effects (potential errors) 
#'   \emph{and} returns a value.  Since the \code{language} argument is 
#'   matched against allowable values, it is necessary to return the 
#'   language so that the language code passed to the API call has 
#'   length 1.
#'   
#' @return Returns a \code{character(1)} with the \code{language} code.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Cast an error if \code{app_id} is not \code{character(1)}.
#'  \item Cast an error if \code{app_id} is \code{NULL}.
#'  \item Cast an error if \code{app_key} is not \code{character(1)}.
#'  \item Cast an error if \code{app_key} is \code{NULL}.
#'  \item Cast an error if \code{language} is not \code{character(1)}.
#'  \item Cast an error if \code{url_base} is not \code{character(1)}.
#'  \item Cast an error if \code{url_base} is \code{NULL}
#' }

# UNEXPORTED

validate_params <- function(app_id, app_key, url_base, language, add){
  checkmate::assert_character(x = app_id,
                              len = 1,
                              add = add)
  
  if (any(is.null(app_id)))
  {
    add$push("`app_id` is set to NULL. Consider using `set_application_access`")
  }
  
  checkmate::assert_character(x = app_key,
                              len = 1,
                              add = add)
  
  if (is.null(app_key))
  {
    add$push("`app_key` is set to NULL. Consider using `set_application_access`")
  }
  
  if (!is.null(language))
  {
    language <- 
      checkmate::matchArg(x = language,
                          choices = oxford_languages,
                          add = add)
  }
  
  checkmate::assert_character(x = url_base,
                              len = 1,
                              add = add)
  
  if (is.null(url_base))
  {
    add$push("`url_base` is set to NULL. Consider using `set_oxford_url_base`")
  }
  
  language
}
