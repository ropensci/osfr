# osfr 0.1.0.9000

## Major Changes

### Authentication

* Most `osfr` functions now require the user to be logged into OSF using 
a personal authentication token (PAT). This will break previously used code 
where the user logged in using an empty string for the PAT. For consistent use, 
make sure that you log in using a PAT prior to running any `osfr` functions. 
The `download_files()` and `path_file()` functions are an exception to the login 
requirement.
* `login()` no longer automatically saves the user's PAT to a file. If you wish 
to have the PAT saved, the `store` argument can be used to save the PAT in the 
.Renviron file.
* `logout()` now clears the PAT from R environment, but does not remove the PAT 
from the .Renviron file if it has been stored. Removing the PAT from the 
.Renviron file must be done manually.

### Files

* `download_files()` no longer uses the `private` argument to check whether a 
file is public or private. All files are initially assumed to be public. If the 
file cannot be accessed, the file is assumed to be private and is downloaded 
using the user's login information.
* `download_files()` can now accept view-only links as additional credentials 
when downloading private files.
* `upload_files()` can now be used to upload a new file to a sub-folder in the 
OSF directory.
* `path_file()` is a new function that acts similarly to `download_files()`, but 
returns the file download link instead of the file itself.

### Vignettes

* A "Getting Started with osfr" vignette is now available. This vignette covers 
information on how to log into OSF using `osfr`, how to use the basic 
functionality of `osfr`, and a brief overview of other provided functionality. 
The vignette also contains links to relevant guides from the OSF support page.

## Removed Features

* `welcome()` has been removed.
* The search functions `search_nodes()`, `search_users()`, and `search_osf()` 
have been removed and will be returned in a future release.
* `get_users()` has been removed and will be returned in a future release.

## Minor Fixes

* `create_folder()` now allows for the creation of sub-folders. 
* `get_files_info()` should now return a GUID for every file that has been 
previously viewed on OSF using a browser.
* `get_nodes()` with the `files` argument set to `TRUE` now returns information 
on all of the nodes in osfstorage instead of a listing of possible storage 
providers.
