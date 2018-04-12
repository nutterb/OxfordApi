context("set_application_id.R")

# Functional Requirement 1 ------------------------------------------

# invalid_app_id is not an app_id the API will recognize, but is
# the correct length, so it is useful for testing functionality 
test_that(
  "Successfully sets the value of options('oxford_api_app_id')",
  {
    set_application_id(app_id = invalid_app_id)
    expect_equal(
      getOption("oxford_api_app_id"),
      invalid_app_id
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if `app_id` is not a character(1)",
  {
    expect_error(
      set_application_id(app_id = 12345678)
    )
  }
)

test_that(
  "Casts an error if `app_id` is not a character(1)",
  {
    expect_error(
      set_application_id(app_id = c(app_id, app_id))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if `app_id` does not have exactly eight characters",
  {
    expect_error(
      set_application_id(app_id = c("abcd"))
    )
  }
)

test_that(
  "Casts an error if `app_id` does not have exactly eight characters",
  {
    expect_error(
      set_application_id(app_id = c("abcdefg"))
    )
  }
)

test_that(
  "Casts an error if `app_id` does not have exactly eight characters",
  {
    expect_error(
      set_application_id(app_id = c("abcdefghi"))
    )
  }
)

set_application_language("en")
set_application_access(app_id = app_id,
                       app_key = app_key)
set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")