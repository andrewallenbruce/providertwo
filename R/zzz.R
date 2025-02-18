# .api__public <- .api__provider <- .api__openpay <- NULL

.onLoad <- function(libname, pkgname) {

  if (httr2::is_online()) {
    .api__public   <<- load_public()
    .api__provider <<- load_provider()
    .api__openpay  <<- load_openpayments()
  }

  S7::methods_register()
}

.onUnload <- function(libpath) {
  remove(
    list = c(
      ".api__public",
      ".api__provider",
      ".api__openpay"
      ),
    envir = .GlobalEnv)
}
