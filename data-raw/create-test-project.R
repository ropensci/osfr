# Create OSF projects for unit tests
# NOTE: these must be created on test.osf.io and publicly accessible
# GUIDs for OSF entities required by the tests are recorded in:
guid_file <- "tests/testthat/test-files/test-guids.dcf"

library(osfr)

proj_root <- osf_create_project(
  title = "osfr-testthat-project",
  description = "This project is for osfr's unit tests and should not be deleted.",
  public = TRUE
)


# Create component with lots of sub-components in its top level -----------
c_paged <- osf_create_component(
  proj_root,
  "osfr-testthat-paginated-component",
  "A component with many sub-components in its top level for pagination tests",
  public = TRUE
)

lapply(
  paste(c_paged$name, formatC(1:30, width = 2, flag = "0"), sep = "-"),
  osf_create_component,
  x = c_paged,
  public = TRUE
)



# create a directory with lots of files for listing tests -----------------
d_files <- osf_mkdir(proj_root, "many-files")

# add text files
dir_tmp <- tempdir()
files_txt <- file.path(dir_tmp, sprintf("text-file-%02.0f.txt", 1:20))
random_nums <- replicate(length(files_txt), runif(100), simplify = FALSE)

dev.null <- mapply(
  writeLines,
  text = lapply(random_nums, as.character),
  con = files_txt
)

osf_upload(d_files, path = files_txt, verbose = TRUE)

# add png files
files_pngs <- file.path(dir_tmp, sprintf("image-file-%02.0f.png", seq_along(files_txt)))

for (i in seq_along(files_pngs)) {
  png(files_pngs[i])
    plot(density(random_nums[[1]]))
    rug(random_nums[[1]])
  dev.off()
}

osf_upload(d_files, path = files_pngs, verbose = TRUE)


# add a top-level file ----------------------------------------------------
file_pdf <- sub("png", "pdf", files_pngs[1])

pdf(file_pdf)
plot(density(random_nums[[1]]))
dev.off()

f_pdf <- osf_upload(proj_root, file_pdf)



# create a nested directory structure for recurse tests -------------------
d_nested <- osf_mkdir(proj_root, "nested-dir")

osf_upload(d_nested, files_txt[1:2])

d_nested %>%
  osf_mkdir(path = "d01") %>%
  osf_upload(path = files_txt[3:4])

d_nested %>%
  osf_mkdir(path = "d01/d02") %>%
  osf_upload(path = files_txt[3:4])

d_nested %>%
  osf_mkdir(path = "d01/d02/d03") %>%
  osf_upload(path = files_txt[5:6])


# export GUIDS ------------------------------------------------------------
guids <-
  list(
    p1 = proj_root,
    c1 = c_paged,
    f1 = f_pdf,
    d1 = d_files,
    d2 = d_nested
  )

write.dcf(
  lapply(guids, function(x) x$id),
  file = guid_file
)
