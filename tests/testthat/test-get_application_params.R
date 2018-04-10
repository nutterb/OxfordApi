context("Get application parameters")

set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")

# get_application_url_base ------------------------------------------

test_that(
  "Successfully retrieves the value of options('oxford_api_url_base')",
  {
    expect_equal(
      get_oxford_url_base(),
      "https://od-api.oxforddictionaries.com/api/v1"
    )
  }
)

# get_application_id ------------------------------------------------

test_that(
  "Successfully retrieves the value of options('oxford_api_app_id')",
  {
    set_application_id("abcdefgh")
    expect_equal(
      get_application_id(),
      "abcdefgh"
    )
  }
)

# get_application_key -----------------------------------------------

test_that(
  "Successfully retrieves the value of options('oxford_api_app_id')",
  {
    key <- paste0(sample(letters, 32, replace = TRUE),
                  collapse = "")
    set_application_key(key)
    expect_equal(
      get_application_key(),
      key
    )
  }
)