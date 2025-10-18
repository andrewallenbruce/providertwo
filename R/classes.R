#' @include fields.R
NULL

#' @noRd
#' @autoglobal
`%:=%` <- function(sym, val) {
  lhs  <- substitute(sym)
  rhs  <- substitute(val)
  stopifnot("left hand side must be a symbol" = is.symbol(lhs))
  cl   <- call('<-', lhs, rhs)

  if (is.call(rhs)) {
    cl[[3L]]$name     <- as.character(lhs)
  } else if (S7::S7_inherits(val)) {
    val@name          <- as.character(lhs)
    cl[[3L]]          <- val
  } else {
    attr(val, "name") <- as.character(lhs)
    cl[[3L]]          <- val
  }
  eval.parent(cl)
}

#' @noRd
#' @autoglobal
class_fields %:=% S7::new_class(
  package = NULL,
  properties = list(key = S7::class_character))

#' @noRd
#' @autoglobal
class_fields_list %:=% S7::new_class(
  parent     = class_fields,
  package    = NULL,
  properties = list(set = S7::class_data.frame)
)

#' @noRd
#' @autoglobal
fields_df <- function(x) {

  key <- collapse::funique(unlist(x, use.names = FALSE))
  set <- purrr::map(x, function(x) collapse::fmatch(x, key))
  idx <- cheapr::cheapr_rep_each(rlang::names2(set), cheapr::list_lengths(set))

  split <- rlang::set_names(vctrs::vec_split(unlist(set, use.names = FALSE), idx), c("year", "index"))
  split <- cheapr::col_c(split, group = cheapr::attrs_rm(vctrs::vec_identify_runs(split$index)))

  index <- vctrs::vec_unique(split[c("group", "index")])
  year  <- rlang::set_names(vctrs::vec_split(split$year, split$group), c("group", "year"))

  class_fields_list(
    key = key,
    set = collapse::join(index, year, on = "group", verbose = 0L))
}


#' @noRd
#' @autoglobal
class_dimensions %:=% S7::new_class(
  package    = NULL,
  properties = list(
    limit    = S7::class_integer,
    total    = S7::class_integer,
    pages    = S7::new_property(S7::class_integer,
      getter = function(self) page_count(self@total, self@limit)),
    simple   = S7::new_property(S7::class_logical,
      getter = function(self) length(self@pages) == 1L && self@pages == 1L)),
  validator  = function(self) {
    if (length(self@limit) != 1L) "@limit must be length 1"
    if (self@limit < 1L) "@limit must be greater than 0"
  }
)

#' @noRd
#' @autoglobal
class_endpoint %:=% S7::new_class(
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    alias       = S7::class_character,
    title       = S7::class_character,
    modified    = S7::class_Date,
    identifier  = S7::class_character,
    dimensions  = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_current %:=% S7::new_class(class_endpoint, package = NULL,
  properties = list(fields = class_fields)
)

#' @noRd
#' @autoglobal
class_temporal %:=% S7::new_class(class_endpoint, package = NULL,
  properties = list(
    fields = class_fields_list,
    year = S7::class_integer
  )
)

#' @noRd
#' @autoglobal
care_current %:=% S7::new_class(class_current, package = NULL,
  properties = list(resources = S7::class_character)
)

#' @noRd
#' @autoglobal
care_temporal %:=% S7::new_class(class_temporal, package = NULL,
  properties = list(resources = S7::class_character)
)

#' @noRd
#' @autoglobal
class_catalog %:=% S7::new_class(
  package = NULL,
  abstract = TRUE,
  properties = list(
    access = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_care %:=% S7::new_class(class_catalog, package = NULL,
  properties = list(access = care_current | care_temporal)
)

#' @noRd
#' @autoglobal
class_prov %:=% S7::new_class(class_catalog, package = NULL,
  properties = list(access = class_current)
)
#' @noRd
#' @autoglobal
class_caid %:=% S7::new_class(class_catalog, package = NULL)

#' @noRd
#' @autoglobal
class_open %:=% S7::new_class(class_catalog, package = NULL)

#' @noRd
#' @autoglobal
class_hgov %:=% S7::new_class(class_catalog, package = NULL)

#' @noRd
#' @autoglobal
class_group %:=% S7::new_class(
  package = NULL,
  properties = list(
    title = S7::class_character,
    members = S7::class_list
  )
)

#' @noRd
#' @autoglobal
class_collection %:=% S7::new_class(class_group, package = NULL)

#' @noRd
#' @autoglobal
class_response %:=% S7::new_class(
  package = NULL,
  properties = list(
    alias = S7::class_character,
    param = S7::class_character | S7::class_list,
    year = S7::class_integer,
    string = S7::class_character,
    limit = S7::class_integer,
    found = S7::class_integer,
    total = S7::class_integer,
    pages = S7::new_property(S7::class_integer,
      getter = function(self) page_count(self@found, self@limit)),
    error = S7::new_property(S7::class_logical,
      getter = function(self) {
        if (empty(self@param)) FALSE else self@found == self@total
      })
    )
  )
