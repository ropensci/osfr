context("Test creating")

# Set the test PAT before testing
# Make sure to have object OSF_PAT_TEST in environment
# I usually keep it in ~/.Rprofile
Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))

test_that("Creating projects and components", {
  x <- create_project(title = "This is an automated test",
                             description = "This is an automated test",
                             test = TRUE)

  expect_that(x, matches("[A-Za-z0-9]{5}"))

  y <- create_component(id = x,
    title = "Component test",
    description = "jaldskfjlafk",
    category = 'hypothesis', test = TRUE)
  expect_that(y, matches("[A-Za-z0-9]{5}"))
  expect_error(create_component(id = x, category = 'hypoehtesis'))
})

logout()
