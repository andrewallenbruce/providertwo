#' @include query.R
NULL

#' @autoglobal
#' @noRd
keys <- S7::new_generic("keys", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(keys, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(keys)
}

S7::method(keys, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> keys()
}

S7::method(keys, class_endpoint) <- function(obj) {
  S7::prop(obj, "fields") |> keys()
}

S7::method(keys, class_fields) <- function(obj) {
  S7::prop(obj, "keys")
}

#' @autoglobal
#' @noRd
meta <- S7::new_generic("meta", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(meta, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(meta)
}

S7::method(meta, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> meta()
}

S7::method(meta, class_endpoint) <- function(obj) {
  S7::prop(obj, "metadata")
}

#' @autoglobal
#' @noRd
dims <- S7::new_generic("dims", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(dims, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(dims)
}

S7::method(dims, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> dims()
}

S7::method(dims, class_endpoint) <- function(obj) {
  S7::prop(obj, "dimensions")
}

#' @autoglobal
#' @noRd
params <- S7::new_generic("params", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(params, class_query) <- function(obj) {
  S7::prop(obj, "params")
}
