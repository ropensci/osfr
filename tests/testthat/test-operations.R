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

test_that("Creating a project", {
  x <- create_project(
    title = "This is an automated test",
    description = "This is an automated test")

  expect_that(x, matches("[A-Za-z0-9]{5}"))
  proj_id <<- x
})

# view_project(proj_id)

context("Test component functions")

comp_id <- NULL

test_that("Creating a component", {
  comp_id <<- create_component(id = proj_id,
    title = "Component test",
    description = "Component test",
    category = "hypothesis")
  expect_that(comp_id, matches("[A-Za-z0-9]{5}"))
  expect_error(create_component(id = proj_id, category = "hypothesis"))
})

pcomp_id <- NULL

test_that("Creating a private component", {
  pcomp_id <<- create_component(id = proj_id,
    title = "Private component test",
    description = "Private component test",
    category = "analysis",
    private = TRUE)
  expect_that(pcomp_id, matches("[A-Za-z0-9]{5}"))
  tmp <- get_nodes(pcomp_id, private = TRUE)
  expect_equal(tmp$data$attributes$public, FALSE)
})

context("Test file functions")

test_that("Uploading files", {
  expect_error(upload_file())
  expect_error(upload_file(id = proj_id))
  expect_type(
    upload_file(id = proj_id, path = "test_upload1.png"),
    "character")
  # Revision:
  expect_type(
    upload_file(id = proj_id, path = "test_upload1_rev.png",
      dest = "test_upload1.png"),
    "character")
  # Try uploading to a different destination name
  expect_type(
    upload_file(id = proj_id, path = "test_upload1.png", dest = "test_upload2.png"),
    "character")
})

test_that("Get info of files in project", {
  expect_error(get_file_info())
  fi <- get_file_info(proj_id)
  expect_is(fi, "data.frame")
  expect_equal(nrow(fi), 2)
})

test_that("Deleting files", {
  expect_error(delete_file())
  # This should work with a file guid but we can't test because files don't get
  # guids for some reason until you do something with them in the browser...
  # But it will fail with a project guid:
  expect_error(delete_file(id = proj_id))
  expect_true(delete_file(proj_id, "test_upload2.png"))
})

test_that("Uploading folder zip", {
  upload_zip(proj_id, ".", "test.zip")
})

# test_that("Downloading files", {
#   download_file(id, "test_upload1.png")
# })

context("Test folder functions")

test_that("Creating folders", {
  expect_error(create_folder())
  expect_error(create_folder(id = proj_id))
  expect_type(
    create_folder(id = proj_id, path = "test_folder"),
    "character"
  )
  # Can't create_folder if it's already there...
  expect_error(create_folder(id = proj_id, path = "test_folder"))
  # Create sub-folder
  expect_type(
    create_folder(id = proj_id, path = "test_folder/sub_folder"),
    "character"
  )
  # Can't create sub folder if it's already there...
  expect_error(create_folder(id = proj_id, path = "test_folder/sub_folder"))
  # Can't create sub folder if parent folder doesn't exist... (should we allow recursive?)
  expect_error(create_folder(id = proj_id, path = "tessst_folder/sub_folder"))
})

# test_that("Deleting folders", {
#   a <- create_folder(id = proj_id, folder = "dummy_folder")

#   # ...
# })

# test_that("Uploading folders", {
# })

test_that("Uploading files in private components", {
  expect_type(
    upload_file(id = pcomp_id, path = "test_upload1.png",
      dest = "test.png"),
    "character")
})

test_that("Getting file info for private components", {
  expect_null(get_file_info(pcomp_id))
  fi <- get_file_info(pcomp_id, private = TRUE)
  expect_is(fi, "data.frame")
})

test_that("Uploading files in subdirectories", {
  expect_error(upload_file())
  expect_error(upload_file(id = proj_id))
  expect_type(
    upload_file(id = proj_id, path = "test_upload1.png",
      dest = "test_folder/test.png"),
    "character")
})



# http://waterbutler.readthedocs.io/en/latest/api.html
# http://help.osf.io/m/files/l/482002-upload-files

# a <- recurse_node(id = proj_id)

# test_that("Uploading comments", {
#   # skip_on_travis()
#   expect_error(comment_osf())
#   # Forcing as.character to make test consistent across machines
#   expect_true(comment_osf(id = "3xr76", txt = "Tesdsat"))
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
  expect_error(delete_project(proj_id)) # we added a component so it can't be deleted
  delete_project(proj_id, recursive = TRUE)
})

test_that("Logging out...", {
  logout()
})

# maybe now do some testing when logged out...
