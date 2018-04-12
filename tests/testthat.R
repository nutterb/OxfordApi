library("testthat")
library("OxfordApi")

# This file must be kept private to prevent access to my app_id and key.
if (file.exists("/home/benjamin")) {
  load("/home/benjamin/GitHub/OxfordApi/OxfordApiKeys.Rdata")
} else{
  app_id <- "12345678"
  app_key <- paste0(c(letters, letters[1:6]), collapse = "")
}

language <- "en"
url_base <- "https://od-api.oxforddictionaries.com/api/v1"
set_application_access(app_id = app_id,
                       app_key = app_key)

invalid_language <- "piglatin"
invalid_app_id <- "12345678"
invalid_app_key <- paste0(c(letters, letters[1:6]), collapse = "")
invalid_url_base <- "http://nowhere.net"

test_check("OxfordApi")

# USE THIS CODE TO REESTABLISH NORMAL WORKING PARAMETERS AFTER TESTING 
# FOR ERRORS
# set_application_language("en")
# set_application_access(app_id = app_id,
#                        app_key = app_key)
# set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")