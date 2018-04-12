context("set_application_language.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Successfully sets the value of options('oxford_api_language')",
  {
    set_application_language(language = "gu")
    expect_equal(
      getOption("oxford_api_language"),
      "gu"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if `language` is not one of `oxford_languages`",
  {
    expect_error(
      set_application_language(language = "fr")
    )
  }
)

test_that(
  "Casts an error if `language` is not a character(1)",
  {
    expect_error(
      set_application_language(url_base = c("en", "es"))
    )
  }
)

set_application_language("en")
set_application_access(app_id = app_id,
                       app_key = app_key)
set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")