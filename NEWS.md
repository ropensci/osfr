Version 0.1
----------------------------------------------------------------------

This update is an overhaul of the 0.0.99 version (link) and mainly provides a more robust set of operations and gets the internals of the package more in shape for a CRAN release when ready.

## Outward changes

Most every outward-facing function remains the same in this version with a few minor exceptions. A few functions have been renamed to have consistent style or to more clearly describe the operation.

- `get.nodes()` is now `get_nodes()`
- `get.users()` is now `get_users()`
- `download()` is now `download_file()`
- `delete()` and `delete_empty()` are now a single function `delete_component()`

Also, the `test` argument has been removed from functions to avoid messy `...` arguments in other functions that need this argument passed on. Instead, if you want to run the operations on the test environment, you can specify `Sys.setenv(OSF_USE_TEST_SERVER = TRUE)` in your R session or set this as a system environment variable. This cleans up the functions since real users will never use this option and it also helps avoid problems when the user might misspecify an argument which gets passed to `...` instead of throwing an error.

Another change for consistency is to always use the argument `private` where in the previous version some functions used `private` and some used `public`.

## Internal changes

- Organized functions into files: accounts, comments, components, files, folders, nodes, projects, users
- Cleaned up coding style to be consistent and added a lintr unit test
- Reorganized unit tests to create a test project and run through multiple operations on that project, deleting the project at the end.
- Note: unit tests run on the production osf.io since I don't have a PAT for the test server.
- Added code coverage checking to TravisCI so that code coverage badge now works.
- Unit tests can be run on TravisCI by specifying a TravisCI environment variable `OSF_PAT_TEST`.
- Unit
- Documentation has been updated so that the package passes CRAN check preparatory for a CRAN release.
- Use a valid open source software license (chose MIT -- CC0 is not appropriate for software -- see http://kbroman.org/pkg_primer/pages/licenses.html).
- In general,

## New functionality

`view_project()`: given a project/component ID, simply opens the project in the web browser
`get_file_info()`: given a project/component ID, returns a data frame of all files in a project with various meta data about the files
`create_folder()`: create a folder or sub-folder
`upload_zip()`: zip up a local directory and upload it to OSF
