# Create directory w/ files and subdirectories for upload tests with structure:
# ├── a.txt
# ├── b.txt
# ├── c.txt
# ├── subdir1
# │   ├── d.txt
# │   └── subdir1_1
# │       ├── e.txt
# │       └── f.txt
# └── subdir2
#     └── g.txt

create_upload_test_files <- function(path) {
  stopifnot("'path' already exists" = !dir.exists(path))

  # directories
  subdir1 <- file.path(path, "subdir1")
  subdir2 <- file.path(path, "subdir2")
  subdir1_1 <- file.path(subdir1, "subdir1_1")
  dir.create(subdir2, showWarnings = FALSE, recursive = TRUE)
  dir.create(subdir1_1, showWarnings = FALSE, recursive = TRUE)

  # files
  brio::writeLines("1", file.path(path, "a.txt"))
  brio::writeLines("2", file.path(path, "b.txt"))
  brio::writeLines("3", file.path(path, "c.txt"))
  brio::writeLines("4", file.path(subdir1, "d.txt"))
  brio::writeLines("5", file.path(subdir1_1, "e.txt"))
  brio::writeLines("6", file.path(subdir1_1, "f.txt"))
  brio::writeLines("7", file.path(subdir2, "g.txt"))

  invisible(path)
}
