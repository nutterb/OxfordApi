context("set_application_access.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Successfully sets the value of options('oxford_api_app_id')",
  {
    set_application_access(app_id = paste0(letters[1:8], 
                                           collapse = ""),
                           app_key = paste0(c(letters, letters[1:6]), 
                                            collapse = ""))
    expect_equal(
      getOption("oxford_api_app_id"),
      paste0(letters[1:8], collapse = "")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Successfully sets the value of options('oxford_api_app_key)",
  {
    set_application_access(app_id = paste0(letters[1:8], 
                                           collapse = ""),
                           app_key = paste0(c(letters, letters[1:6]),
                                            collapse = ""))
    expect_equal(
      getOption("oxford_api_app_key"),
      paste0(c(letters, letters[1:6]), collapse = "")
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if `app_id` is not a character(1)",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    expect_error(
      set_application_access(app_id = 12345678,
                             app_key = key)
    )
  }
)

test_that(
  "Casts an error if `app_id` is not a character(1)",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    expect_error(
      set_application_access(app_id = c("12345678", "12345678"),
                             app_key = key)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if `app_id` does not have exactly eight characters",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    expect_error(
      set_application_access(app_id = c("abcd"),
                             app_key = key)
    )
  }
)

test_that(
  "Casts an error if `app_id` does not have exactly eight characters",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    expect_error(
      set_application_access(app_id = c("abcdefg"),
                             app_key = key)
    )
  }
)

test_that(
  "Casts an error if `app_id` does not have exactly eight characters",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    expect_error(
      set_application_access(app_id = c("abcdefghi"),
                             app_key = key)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if `app_key` is not a character(1)",
  {
    expect_error(
      set_application_access(app_id = "abcdefgh",
                             app_key = 01234567891011121314151617181920)
    )
  }
)

test_that(
  "Casts an error if `app_key` is not a character(1)",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    expect_error(
      set_application_access(app_id = "abcdefgh",
                             app_key = c(key, key))
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if `app_key` does not have exactly 32 characters",
  {
    key <- paste0(sample(letters, 31, replace = TRUE))
    expect_error(
      set_application_access(app_id = "abcdefgh", 
                             app_key = key)
    )
  }
)

test_that(
  "Casts an error if `app_key` does not have exactly 32 characters",
  {
    key <- paste0(sample(letters, 33, replace = TRUE))
    expect_error(
      set_application_access(app_id = "abcdefgh",
                             app_key = key)
    )
  }
)

