context("get_oxford_filters.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a data frame",
  {
    skip_on_cran()
    expect_equal(
      is.data.frame(get_oxford_filters()),
      TRUE
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Return a data frame with only the filters for an indicated endpoint",
  {
    skip_on_cran()
    expect_equal(
      all(get_oxford_filters(endpoint = "entries")[["endpoint"]] %in% "entries"),
      TRUE
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if the API returns an error",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_id = invalid_app_id)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if endpoint is not character(1) or NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(endpoint = 123)
    )
  }
)

test_that(
  "Cast an error if endpoint is not character(1) or NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(endpoint = c("entries", "wordlist"))
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if app_id is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_id = 123)
    )
  }
)

test_that(
  "Cast an error if app_id is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_id = rep(get_application_id(), 2))
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if app_id is NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_id = NULL)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if app_key is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_key = 123)
    )
  }
)

test_that(
  "Cast an error if app_key is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_key = rep(get_application_key(), 2))
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if app_key is NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(app_key = NULL)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if url_base is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(url_base = 123)
    )
  }
)

test_that(
  "Cast an error if url_base is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_filters(url_base = rep(get_oxford_url_base(), 2))
    )
  }
)