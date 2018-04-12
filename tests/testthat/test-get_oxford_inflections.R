context("get_oxford_inflections.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a data frame with the results of the API call",
  {
    skip_on_cran()
    expect_equal(
      is.data.frame(get_oxford_inflections("swimming")),
      TRUE
    )
  }
)

test_that(
  "Return a data frame with the results of the API call with filters",
  {
    skip_on_cran()
    expect_equal(
      is.data.frame(get_oxford_inflections(word_id = "change",
                                           grammaticalFeatures = c("singular", "past"),
                                           lexicalCategory = "noun")),
      TRUE
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if word_id is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = 123)
    )
  }
)

test_that(
  "Cast an error if word_id is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = c("swimming", "change"))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if any argument to ... is not named",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming", "singular")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if any argument to ... is not a character vector",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             grammaticalFeatures = 123)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if the API returns an error",
  {
    skip_on_cran()
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_id = "abcdefgh")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if app_id is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_id = 123)
    )
  }
)

test_that(
  "Cast an error if app_id is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_id = c(app_id, app_id))
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if app_id is NULL",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_id = NULL)
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if app_key is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_key = 123)
    )
  }
)

test_that(
  "Cast an error if app_key is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_key = c(app_key, app_key))
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if app_key is NULL",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             app_key = NULL)
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if langauge is not one of oxford_languages",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             language = "zz")
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if urls_base is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             url_base = 123)
    )
  }
)

test_that(
  "Cast an error if urls_base is not character(1)",
  {
    expect_error(
      get_oxford_inflections(word_id = "swimming",
                             url_base = c(url_base, url_base))
    )
  }
)