# osfr 0.1.0.9000

## Major Changes

### Authentication

* Most `osfr` functions now require the user to be logged into OSF using 
a personal authentication token (PAT). This will break previously used code 
where the user logged in using an empty string for the PAT. For consistent use, 
make sure that you log in using a PAT prior to running any `osfr` functions. **
The `download_files()` and `path_file()` functions are an exception to the login 
requirement.**
* `login()` no longer automatically saves the user's PAT to a file. If you wish 
to have the PAT saved, the `store` argument can be used to save the PAT in the 
.Renviron file.
* `logout()` now clears the PAT from R environment, but does not remove the PAT 
from the .Renviron file if it has been stored. Removing the PAT from the 
.Renviron file must be done manually.

### Files

## New Features

## Removed Features

## Minor Fixes
