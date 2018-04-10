#' @name get_oxford_wordlist
#' @title Get a List of Words
#' 
#' @description Get a list of words for a particular domain, lexical 
#'   category, register, and/or region.
#'   
#' @param ... named arguments of character vectors to pass as filters to 
#'   the API.  The API requires that at least one filter be given. See
#'   'Details'
#' @param app_id \code{character(1)}. The user's application ID.
#' @param app_key \code{character(1)}. The user's application key.
#' @param language \code{character(1)}. The language for which to look up 
#'   domains.
#' @param url_base \code{character(1)}. The url stem for the API.
#' 
#' @details The response only includes headwords, not all their possible 
#' inflections.
#' 
#' Filters must be given as named arguments in \code{...}. Each argument is a 
#' character vector. Examples include \code{registers = "Rare"} or 
#' \code{domains = c("Art", "Science")}.
#' 
#' @return 
#' A data frame with the following columns:
#' 
#' \itemize{
#'  \item \code{id } Word ID
#'  \item \code{word } The plain text of the word.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a data frame with the results of the API call.
#'  \item Cast an error if the API returns an error.
#'  \item Cast an error if any element in \code{...} is unnamed.
#'  \item Cast an error if any element in \code{...} is not a character vector.
#'  \item Cast an error if \code{app_id} is not \code{character(1)}.
#'  \item Cast an error if \code{app_id} is \code{NULL}.
#'  \item Cast an error if \code{app_key} is not \code{character(1)}.
#'  \item Cast an error if \code{app_key} is \code{NULL}.
#'  \item Cast an error if \code{language} is not \code{character(1)}.
#'  \item Cast an error if \code{url_base} is not \code{character(1)}.
#'  \item Cast an error if any element in \code{advanced_filter} is unnamed.
#'  \item Cast an error if any element in \code{advanced_filter} is not a character vector.
#' }
#' 
#' @export

get_oxford_wordlist <- function(...,
                                advanced_filter = list(),
                                app_id = getOption("oxford_api_app_id"),
                                app_key = getOption("oxford_api_app_key"),
                                language = getOption("oxford_api_language"),
                                url_base = getOption("oxford_api_url_base"))
{
  coll <- checkmate::makeAssertCollection()
  
  filters <- list(...)
  
  checkmate::assert_list(x = filters,
                         types = "character",
                         names = "named",
                         add = coll)
  
  checkmate::assert_list(x = advanced_filter,
                         types = "character",
                         names = "named",
                         add = coll)
  
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
  
  filters <- 
    mapply(FUN = 
             function(nms, fls){
               paste0(nms, "=", paste0(fls, collapse = ","))
             },
           names(filters),
           filters,
           SIMPLIFY = FALSE)
  
  filters <- paste0(unlist(filters), collapse = ";")
  
  advanced_filter <- 
    mapply(FUN = 
             function(nms, fls){
               paste0(nms, "=", paste0(fls, collapse = ","))
             },
           names(advanced_filter),
           advanced_filter,
           SIMPLIFY = FALSE)
  
  advanced_filter <- paste0(unlist(advanced_filter), collapse = ";")
  if (advanced_filter != "")
    advanced_filter <- paste0("?", advanced_filter)
  
  url <- sprintf("%s/wordlist/%s/%s%s",
                 url_base,
                 language,
                 filters,
                 advanced_filter)
  
  response <- 
    httr::GET(url = url,
              httr::add_headers(app_id = app_id,
                                app_key = app_key))
  
  oxford_error(response)
  
  response <- rjson::fromJSON(as.character(response))
  
  wlist <- 
    lapply(X = response[["results"]],
           FUN = function(x){
             data.frame(id = x[["id"]],
                        word = x[["word"]],
                        stringsAsFactors = FALSE)
           })
  
  wlist <- do.call("rbind", wlist)
  wlist[["id"]] <- vapply(X = wlist[["id"]],
                          FUN = utils::URLdecode,
                          FUN.VALUE = character(1))
  wlist
}