.onLoad <- function(libname, pkgname) {

  rlang::run_on_load()

  catalogs <<- memoise::memoise(catalogs)
  careMain <<- memoise::memoise(careMain)
  openMain <<- memoise::memoise(openMain)
  proMain  <<- memoise::memoise(proMain)

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


