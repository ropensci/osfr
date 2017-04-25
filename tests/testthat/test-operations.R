context("Test creating")

# Instead of testing on a test server, why not test live, and at the end,
# test the delete functions and finally delete the entire test project.
# To do all of this, the file uploading needs to work, so several tests
# are commented out until that is taken care of.

# Set the test PAT before testing
# Make sure to have object OSF_PAT_TEST in environment
# On Travis, this can be set as an environment variable
Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))

proj_id <- NULL

test_that("Creating projects and components", {
  # skip_on_travis()
  x <- create_project(
    title = "This is an automated test",
    description = "This is an automated test")

  expect_that(x, matches("[A-Za-z0-9]{5}"))
  proj_id <<- x

  y <- create_component(id = x,
    title = "Component test",
    description = "jaldskfjlafk",
    category = "hypothesis")
  expect_that(y, matches("[A-Za-z0-9]{5}"))
  expect_error(create_component(id = x, category = "hypothesis"))
})

# context("Test upload functions")

# Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))

# test_that("Uploading files", {
#   expect_error(upload_osf())
#   Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
#   expect_error(upload_osf(id = "kjh47"))
#   getwd()
#   expect_true(upload_revision(id = "kjh47", filename = "test_upload1.png"))

#   # expect_true(upload_osf(id = "djxwq", filename = "test_upload1.png"))
#   # expect_true(upload_osf(id = "djxwq", filename = "test_upload1.png"))
# })

# test_that("Uploading comments", {
#   # skip_on_travis()
#   expect_error(comment())
#   # Forcing as.character to make test consistent across machines
#   expect_true(comment(id = "djxwq", txt = "Tesdsat"))
# })

# context("test users functions")

# test_that("users are retrieved", {
#   expect_that(class(get_users()), matches("list"))
# })

# context("Test downloading")

# # Set the test PAT before testing
# # Make sure to have object OSF_PAT_TEST in environment
# # I usually keep it in ~/.Rprofile
# Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))

# test_that("copying is not theft", {
#   # Note that those status codes had to be forced to make testing consistent
#   expect_that(
#     as.character(download(id = "bzm73", file = "test.file.remove")$status_code),
#     matches("200"))
#   expect_that(
#     as.character(download(id = "bzm73")$status_code),
#     matches("200"))
#   expect_that(
#     as.character(download(id = "2pf6w", file = "test.file.remove",
#       private = TRUE)$status_code),
#     matches("200"))
# })

# test_that("process_type functions", {
#   # skip_on_travis()
#   expect_that(process_type(id = proj_id), matches("nodes"))
#   expect_that(process_type(id = "f9q8r"), matches("files"))
#   # Also checkable for private nodes/files
# })

test_that("Deleting projects", {
  # skip_on_travis()
  expect_error(delete_project(proj_id)) # we added a component so it can't be deleted
  delete_project(proj_id, recursive = TRUE)
})

logout()
