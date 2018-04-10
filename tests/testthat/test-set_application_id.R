context("set_application_id.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Successfully sets the value of options('oxford_api_app_id')",
  {
    set_application_id(app_id = paste0(letters[1:8], 
                                       collapse = ""))
    expect_equal(
      getOption("oxford_api_app_id"),
      paste0(letters[1:8], collapse = "")
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
      set_application_id(app_id = c("12345678", "12345678"))
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
