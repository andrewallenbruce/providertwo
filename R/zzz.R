.onLoad <- function(libname, pkgname) {

  S7::methods_register()

  list_resources <<- memoise::memoise(list_resources)
  base_request   <<- memoise::memoise(base_request)
  quick          <<- memoise::memoise(quick)

}

# rlang::run_on_load()
# .onUnload <- function(libpath) {remove(list = c("CATALOG"), envir = .GlobalEnv)}


