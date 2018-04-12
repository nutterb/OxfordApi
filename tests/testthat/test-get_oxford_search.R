context("get_oxford_search.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a data frame with the results of the call",
  {
    skip_on_cran()
    expect_true(
      is.data.frame(
        get_oxford_search("eye")
      )
    )
  }
)

test_that(
  "Return a data frame with the results of the call",
  {
    skip_on_cran()
    expect_true(
      is.data.frame(
        get_oxford_search("eye",
                          limit = 10)
      )
    )
  }
)

test_that(
  "Return a data frame with the results of the call",
  {
    skip_on_cran()
    expect_true(
      is.data.frame(
        get_oxford_search("eye", 
                          limit = 10,
                          offset = 11)
      )
    )
  }
)

test_that(
  "Return a data frame with the results of the call",
  {
    skip_on_cran()
    expect_true(
      is.data.frame(
        get_oxford_search("eye", 
                          limit = 10,
                          offset = 11,
                          region = "us")
      )
    )
  }
)

test_that(
  "Return a data frame with the results of the call",
  {
    skip_on_cran()
    expect_true(
      is.data.frame(
        get_oxford_search("eye", 
                          prefix = TRUE,
                          limit = 10,
                          offset = 11,
                          region = "us")
      )
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if query is not character(1)",
  {
    expect_error(
      get_oxford_search(123)
    )
  }
)

test_that(
  "Casts an error if query is not character(1)",
  {
    expect_error(
      get_oxford_search(c("eye", "ball"))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if prefix is not logical(1)",
  {
    expect_error(
      get_oxford_search("eye", 
                        prefix = "true")
    )
  }
)

test_that(
  "Casts an error if prefix is not logical(1)",
  {
    expect_error(
      get_oxford_search("eye", 
                        prefix = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts and error if region is not one of c('gb', 'us')",
  {
    expect_error(
      get_oxford_search("eye",
                        region = "gs")
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if limit is not integerish on the interval (0, 5000]",
  {
    expect_error(
      get_oxford_search("eye",
                        limit = pi)
    )
  }
)

test_that(
  "Casts an error if limit is not integerish on the interval (0, 5000]",
  {
    expect_error(
      get_oxford_search("eye",
                        limit = 0)
    )
  }
)

test_that(
  "Casts an error if limit is not integerish on the interval (0, 5000]",
  {
    expect_error(
      get_oxford_search("eye",
                        limit = 5001)
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if offset is not integerish on the interval [0, 5000]",
  {
    expect_error(
      get_oxford_search("eye",
                        offset = pi)
    )
  }
)

test_that(
  "Casts an error if offset is not integerish on the interval [0, 5000]",
  {
    expect_error(
      get_oxford_search("eye",
                        limit = -1)
    )
  }
)

test_that(
  "Casts an error if offset is not integerish on the interval [0, 5000]",
  {
    expect_error(
      get_oxford_search("eye",
                        offset = 50001)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if the API returns an error",
  {
    skip_on_cran()
    expect_error(
      get_oxford_search(query = "eye", 
                        app_id = invalid_app_id)
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if app_id is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", app_id = 123)
    )
  }
)

test_that(
  "Cast an error if app_id is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", app_id = rep(get_application_id(), 2))
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if app_id is NULL",
  {
    expect_error(
      get_oxford_search(query = "eye", app_id = NULL)
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if app_key is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", app_key = invalid_app_key)
    )
  }
)

test_that(
  "Cast an error if app_key is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", app_key = rep(get_application_key(), 2))
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if app_key is NULL",
  {
    expect_error(
      get_oxford_search(query = "eye", app_key = NULL)
    )
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if language is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", language = 123)
    )
  }
)

test_that(
  "Cast an error if language is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", language = rep(get_application_language(), 2))
    )
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if url_base is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", url_base = 123)
    )
  }
)

test_that(
  "Cast an error if url_base is not character(1)",
  {
    expect_error(
      get_oxford_search(query = "eye", url_base = rep(get_oxford_url_base(), 2))
    )
  }
)