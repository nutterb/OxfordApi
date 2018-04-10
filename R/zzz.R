packageStartupMessage(
  "In order to begin using `OxfordApi`, remember to set your \n",
  "Application ID and Application Key using `set_application_access`")

.onLoad <- function(libname,pkgname)
{
  options(oxford_api_url_base = "https://od-api.oxforddictionaries.com/api/v1",
          oxford_api_app_id   = NULL,
          oxford_api_app_key  = NULL,
          oxford_api_language = "en")
}

.onUnload <- function(libPath)
{
  options(oxford_api_url_base = NULL,
          oxford_api_app_id   = NULL,
          oxford_api_app_key  = NULL,
          oxford_api_language = NULL)
}