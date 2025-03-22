.onLoad <- function(libname, pkgname) {

  catalogs        <<- memoise::memoise(catalogs)
  CurrentMain     <<- memoise::memoise(CurrentMain)
  CurrentOpen     <<- memoise::memoise(CurrentOpen)
  CurrentProvider <<- memoise::memoise(CurrentProvider)

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


