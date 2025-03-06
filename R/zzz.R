.onLoad <- function(libname, pkgname) {

  if (httr2::is_online()) {
    .api__public   <<- load_public()
    .api__provider <<- load_provider()
    .api__openpay  <<- load_openpayments()
  }

  catalog_provider <<- memoise::memoise(catalog_provider)
  open_catalog     <<- memoise::memoise(open_catalog)

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
