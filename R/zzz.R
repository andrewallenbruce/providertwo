.onLoad <- function(libname, pkgname) {

  rlang::run_on_load()
  S7::methods_register()

  catalogs       <<- memoise::memoise(catalogs)
  care_endpoint  <<- memoise::memoise(care_endpoint)
  pro_endpoint   <<- memoise::memoise(pro_endpoint)
  open_endpoint  <<- memoise::memoise(open_endpoint)
  caid_endpoint  <<- memoise::memoise(caid_endpoint)
  hgov_endpoint  <<- memoise::memoise(hgov_endpoint)
  care_temporal  <<- memoise::memoise(care_temporal)
  open_temporal  <<- memoise::memoise(open_temporal)
  hgov_temporal  <<- memoise::memoise(hgov_temporal)
  care_group     <<- memoise::memoise(care_group)
  pro_group      <<- memoise::memoise(pro_group)
  open_group     <<- memoise::memoise(open_group)
  caid_group     <<- memoise::memoise(caid_group)
  care_troup     <<- memoise::memoise(care_troup)
  open_troup     <<- memoise::memoise(open_troup)
  quick          <<- memoise::memoise(quick)

}

# .onUnload <- function(libpath) {remove(list = c("CATALOG"), envir = .GlobalEnv)}


