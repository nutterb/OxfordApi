context("set_application_key.R")

valid_key <- paste(sample(letters, 32, replace = TRUE),
                   collapse = "")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Successfully sets the value of options('oxford_api_app_key')",
  {
    set_application_key(app_key = valid_key)
    expect_equal(
      getOption("oxford_api_app_key"),
      valid_key
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
      set_application_key(app_key = c(valid_key, valid_key))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if `app_key` does not have exactly eight characters",
  {
    expect_error(
      set_application_key(app_key = paste0(sample(letters, 31, replace = TRUE),
                                           collapse = ""))
    )
  }
)

test_that(
  "Casts an error if `app_key` does not have exactly eight characters",
  {
    expect_error(
      set_application_key(app_key = paste0(sample(letters, 33, replace = TRUE),
                                           collapse = ""))
    )
  }
)
