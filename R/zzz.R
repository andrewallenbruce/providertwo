.onLoad <- function(libname, pkgname) {

  rlang::run_on_load()

  catalogs        <<- memoise::memoise(catalogs)
  MainCurrent     <<- memoise::memoise(MainCurrent)
  OpenCurrent     <<- memoise::memoise(OpenCurrent)
  ProviderCurrent <<- memoise::memoise(ProviderCurrent)

  S7::methods_register()

}

# .onUnload <- function(libpath) {
#
#   remove(
#     list = c("CATALOG"),
#     envir = .GlobalEnv
#   )
#
# }


