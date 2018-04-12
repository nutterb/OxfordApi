context("get_oxford_wordlist.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a data frame",
  {
    skip_on_cran()
    expect_equal(
      is.data.frame(get_oxford_wordlist(domains = c("Art"))),
      TRUE
    )
  }
)

test_that(
  "Return a data frame",
  {
    skip_on_cran()
    expect_equal(
      is.data.frame(get_oxford_wordlist(domains = c("Art"),
                                        advanced_filter = list(word_length = ">5"))),
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
      get_oxford_wordlist(app_id = invalid_app_id)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if any element of ... is unnamed",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist("Art")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if any element of ... is not a character vector",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(domains = 1)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if app_id is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(app_id = 123)
    )
  }
)

test_that(
  "Cast an error if app_id is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(app_id = rep(get_application_id(), 2))
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if app_id is NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(app_id = NULL)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if app_key is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(app_key = 123)
    )
  }
)

test_that(
  "Cast an error if app_key is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(app_key = rep(get_application_key(), 2))
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if app_key is NULL",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(app_key = NULL)
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if language is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(language = 123)
    )
  }
)

test_that(
  "Cast an error if language is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(language = rep(get_application_language(), 2))
    )
  }
)

# Functional Requirement 10 ------------------------------------------

test_that(
  "Cast an error if url_base is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(url_base = 123)
    )
  }
)

test_that(
  "Cast an error if url_base is not character(1)",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(url_base = rep(get_oxford_url_base(), 2))
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if any element of advanced_filter is unnamed",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(advanced_filter = list(c(">5", "< 10")))
    )
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if any element of ... is not a character vector",
  {
    skip_on_cran()
    expect_error(
      get_oxford_wordlist(advanced_filter = list(word_length = 5))
    )
  }
)