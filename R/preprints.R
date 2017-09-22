download_preprints <- function (id, path) {
httr::GET(sprintf('https://osf.io/%s/download', id),
                      httr::write_disk(paste0(id, '.pdf'), overwrite = TRUE))
}