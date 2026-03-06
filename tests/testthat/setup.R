# Cassettes are recorded against the test server so ensure OSF_SERVER is set
if (!nzchar(Sys.getenv("OSF_SERVER"))) {
  withr::local_envvar(OSF_SERVER = "test", .local_envir = teardown_env())
}
