.onLoad <- function(libname, pkgname) {

  S7::methods_register()

  # op_old <- options()
  # op_new <- list(nthreads = 4L, fastplyr.inform = FALSE, fastplyr.optimise = FALSE)
  # op_set <- !(names(op_new) %in% names(op_old))
  # if (any(op_set)) options(op_new[op_set])
  # invisible()

  # list_resources <<- memoise::memoise(list_resources)
  # base_request   <<- memoise::memoise(base_request)
  # quick          <<- memoise::memoise(quick)

}
