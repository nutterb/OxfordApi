context("get_oxford_lexical_categories.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a character vector",
  {
    skip_on_cran()
    expect_equal(
      is.character(get_oxford_lexical_categories()),
      TRUE
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if the API returns an error",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_id = invalid_app_id)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if app_id is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_id = 123)
    )
  }
)

test_that(
  "Cast an error if app_id is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_id = rep(get_application_id(), 2))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if app_id is NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_id = NULL)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if app_key is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_key = 123)
    )
  }
)

test_that(
  "Cast an error if app_key is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_key = rep(get_application_key(), 2))
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if app_key is NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(app_key = NULL)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if language is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(language = 123)
    )
  }
)

test_that(
  "Cast an error if language is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(language = rep(get_application_language(), 2))
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if url_base is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(url_base = 123)
    )
  }
)

test_that(
  "Cast an error if url_base is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_lexical_categories(url_base = rep(get_oxford_url_base(), 2))
    )
  }
)