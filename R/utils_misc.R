#' @autoglobal
#' @noRd
flatten_column <- function(i) {
  map_chr(i, function(x) paste0(delist(x), collapse = ", "))
}

#' @autoglobal
#' @noRd
map_na_if <- function(x) {
  map_if(x, is.character, function(x) na_if(x, y = ""))
}

#' @autoglobal
#' @noRd
get_data_elem <- function(x) {
  delist(map(x, function(i) get_elem(as.list(i), "data")))
}

#' @autoglobal
#' @noRd
delist_elem <- function(x, el) {
  delist(get_elem(x, el, DF.as.list = TRUE))
}

#' @autoglobal
#' @noRd
smush_elem <- function(i, el) {
  map_chr(get_elem(i, el), function(x) paste0(x, collapse = ", "))
}

#' @autoglobal
#' @noRd
clean_names <- function(x) {
  gsub("\\(|\\)", "",
       gsub("\\s|-", "_", tolower(x), perl = TRUE),
       perl = TRUE)
}

#' @autoglobal
#' @noRd
set_clean <- function(i, x) {
  set_names(i, clean_names(x))
}

#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n)
}

#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n))
}

#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}

#' @noRd
empty <- function(x) vec_is_empty(x)

#' @autoglobal
#' @noRd
if_empty_null <- function(x) if (empty(x)) NULL else x

#' @autoglobal
#' @noRd
na_if <- function(x, y = "") {
  vctrs::vec_slice(x, vec_in(x, y, needles_arg = "x", haystack_arg = "y")) <- NA
  x
}

#' @noRd
yank <- function(x) x[[1]]

#' @noRd
yank_index_name <- function(x, nm, i = 1L) get_elem(x[[i]], elem = rlang::ensym(nm))

#' @noRd
null_to_na <- function(x) if (is.null(x)) NA_character_ else x

#' @noRd
na <- function(x) is_na(x)

#' @noRd
not_na <- function(x) !is_na(x)

#' @noRd
delist <- function(x) unlist(x, use.names = FALSE)

#' @noRd
null <- function(x) is.null(x)

#' @noRd
not_null <- function(x) !is.null(x)

#' @noRd
true <- function(x) isTRUE(x)

#' @noRd
false <- function(x) isFALSE(x)

#' @noRd
as_date <- function(x, ..., fmt = "%Y-%m-%d") as.Date(x, ..., format = fmt)

#' @autoglobal
#' @noRd
roundup <- function(x, d = 2) {
  d  <- 10^d
  z  <- abs(x) * d
  z  <- z + 0.5 + sqrt(.Machine[["double.eps"]])
  z  <- trunc(z)
  z  <- z / d
  z * sign(x)
}

#' @autoglobal
#' @noRd
convert_epoch <- function(x) {
  as.Date(as.POSIXct.numeric(as.numeric(x) / 1000L, origin = "1970-01-01"))
}

#' @autoglobal
#' @noRd
convert_entity <- function(x) {
  factor_(val_match(x, "NPI-1" ~ "I", "NPI-2" ~ "O", .default = "X"))
}

# This will be exported by S7 next release.
#' Assignment operator that allows for a more convenient syntax
#' @param left `<symbol>` name of the object to assign
#' @param right `<call>` the value to assign to the object
#' @returns `<symbol>` the name of the object assigned
#' @export
#' @autoglobal
`%:=%` <- function(left, right) {
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
