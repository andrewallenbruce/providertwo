#' @noRd
#' @autoglobal
`:=` <- function(left, right) {
  name <- substitute(left)
  if (!is.symbol(name))
    stop("left hand side must be a symbol")

  right <- substitute(right)
  if (!is.call(right))
    stop("right hand side must be a call")

  if (is.symbol(cl <- right[[1L]]) &&
      as.character(cl) %in% c("function", "new.env")) {
    # attach "name" attr for usage like:
    # foo := function(){}
    # foo := new.env()
    right <- eval(right, parent.frame())
    attr(right, "name") <- as.character(name)
  } else {
    # for all other usage,
    # inject name as a named arg, so that
    #   foo := new_class(...)
    # becomes
    #   foo <- new_class(..., name = "foo")

    right <- as.call(c(as.list(right), list(name = as.character(name))))

    ## skip check; if duplicate 'name' arg is an issue the call itself will signal an error.
    # if (hasName(right, "name")) stop("duplicate `name` argument.")

    ## alternative code path that injects `name` as positional arg instead
    # right <- as.list(right)
    # right <- as.call(c(right[[1L]], as.character(name), right[-1L]))
  }
  eval(call("<-", name, right), parent.frame())
}

#' @noRd
#' @autoglobal
class_dimensions := new_class(
  package = NULL,
  properties = list(
    limit = class_integer,
    rows  = class_integer,
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, self@limit)
    ),
    fields = new_property(
      class_character,
      getter = function(self)
        as.list(self@fields) |>
        set_names(self@fields),
      setter = function(self, value) {
        self@fields <- value
        self
      }
    )
  )
)

#' @noRd
#' @autoglobal
class_endpoint := new_class(
  package       = NULL,
  properties    = list(
    identifier  = class_character,
    metadata    = class_list,
    dimensions  = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_group := new_class(
  package    = NULL,
  properties = list(
    group    = class_character,
    members  = class_list
  )
)

#' @noRd
#' @autoglobal
class_temporal := new_class(
  package       = NULL,
  properties    = list(
    metadata    = class_list,
    dimensions  = class_dimensions,
    endpoints   = class_list
  )
)
