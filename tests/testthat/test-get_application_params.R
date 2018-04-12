context("Get application parameters")

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
    set_application_id(invalid_app_id)
    expect_equal(
      get_application_id(),
      invalid_app_id
    )
  }
)

# get_application_key -----------------------------------------------

test_that(
  "Successfully retrieves the value of options('oxford_api_app_id')",
  {
    set_application_key(invalid_app_key)
    expect_equal(
      get_application_key(),
      invalid_app_key
    )
  }
)

# get_application_key -----------------------------------------------

test_that(
  "Successfully retreives the value of options('oxford_api_language')",
  {
    set_application_language("gu")
    expect_equal(
      get_application_language(),
      "gu"
    )
  }
)

set_application_language("en")
set_application_access(app_id = app_id,
                       app_key = app_key)
set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")