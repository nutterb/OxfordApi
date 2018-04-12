context("set_oxford_url_base.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Successfully sets the value of options('oxford_api_url_base')",
  {
    set_oxford_url_base(url_base = "some_url")
    expect_equal(
      getOption("oxford_api_url_base"),
      "some_url"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if `url_base` is not a character(1)",
  {
    expect_error(
      set_oxford_url_base(url_base = 12345678)
    )
  }
)

test_that(
  "Casts an error if `url_base` is not a character(1)",
  {
    expect_error(
      set_oxford_url_base(url_base = c("some_url", "other_url"))
    )
  }
)

set_application_language("en")
set_application_access(app_id = app_id,
                       app_key = app_key)
set_oxford_url_base("https://od-api.oxforddictionaries.com/api/v1")