context("set_application_key.R")

# Functional Requirement 1 ------------------------------------------

# invalid_app_key is not an app_key the API will recognize, but is
# the correct length, so it is useful for testing functionality 
test_that(
  "Successfully sets the value of options('oxford_api_app_key')",
  {
    set_application_key(app_key = invalid_app_key)
    expect_equal(
      getOption("oxford_api_app_key"),
      invalid_app_key
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if `app_key` is not a character(1)",
  {
    expect_error(
      set_application_key(app_key = 12345678)
    )
  }
)

test_that(
  "Casts an error if `app_key` is not a character(1)",
  {
    expect_error(
      set_application_key(app_key = c(app_key, app_key))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if `app_key` does not have exactly 32 characters",
  {
    expect_error(
      set_application_key(app_key = substr(app_key, 1, 31))
    )
  }
)

test_that(
  "Casts an error if `app_key` does not have exactly 32 characters",
  {
    expect_error(
      set_application_key(app_key = paste0(app_key, "a",
                                           collapse = ""))
    )
  }
)

set_application_language("en")
set_application_access(app_id = app_id,
                       app_key = app_key)
set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")