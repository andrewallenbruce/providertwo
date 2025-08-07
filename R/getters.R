#' @autoglobal
#' @noRd
parameters <- S7::new_generic("parameters", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(parameters, class_query) <- function(obj) {
  S7::prop(obj, "params")
}

#' @autoglobal
#' @noRd
field_keys <- S7::new_generic("field_keys", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(field_keys, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(field_keys)
}

S7::method(field_keys, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> field_keys()
}

S7::method(field_keys, class_endpoint) <- function(obj) {
  S7::prop(obj, "fields") |> field_keys()
}

S7::method(field_keys, class_fields) <- function(obj) {
  S7::prop(obj, "keys")
}

#' @autoglobal
#' @noRd
title <- S7::new_generic("title", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(title, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(title)
}

S7::method(title, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> title()
}

S7::method(title, class_endpoint) <- function(obj) {
  S7::prop(obj, "metadata") |> _$title
}
