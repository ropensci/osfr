# Creates an object for the OSF_PAT environment variable during setup
test_pat <- Sys.getenv("OSF_PAT")

# Creates an object for the OSF_USE_SERVER environment variable during setup
osf_use_server_status <- Sys.getenv("OSF_USE_SERVER")
Sys.setenv(OSF_USE_SERVER = "test")
