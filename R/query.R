#' @noRd
#' @autoglobal
class_query <- S7::new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    # input    = S7::class_list,
    params   = S7::class_list,
    groups   = S7::class_list
  )
)

#' Create a Query Object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions
#' @returns S7 `<class_query>` object.
#' @name query
NULL

#' @rdname query
#' @examples
#' query(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of("CA", "GA", "NY"),
#'   state_own   = c("GA", "MD"),
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#' @autoglobal
#' @export
query <- function(...) {
  class_query(
    params = purrr::compact(
      rlang::dots_list(
        ...,
        .homonyms = "error",
        .named = TRUE,
        .check_assign = TRUE
      ))
  )
}

#' @noRd
#' @autoglobal
query2 <- function(...) {
  x <- list(
    groups = purrr::keep(rlang::enexprs(...), \(x) is_junc(x)),
    params = purrr::discard(rlang::enexprs(...), \(x) is_junc(x))
  )

  g_names <- purrr::map(x$groups, function(x)
    S7::prop(eval(x), "members")) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  ok <- all(g_names %in_% rlang::names2(x$params))

  if (!ok) {
    cli::cli_abort(
      c("x" = "All {.field group} members must be {.field query} field names"),
      call = caller_env())
  }

  class_query(
    params = purrr::map(x$params, eval),
    groups = rlang::set_names(
      purrr::map(x$groups, eval),
      paste0("g", seq_along(x$groups)))
  )
}

# TODO Explore chunking params with 10+ elements
# Submitting too many params at once can cause API errors
# TODO Consider the case sensitivity of each endpoint's fields' values
# Some are all uppercase and won't match anything but uppercase values

#' @autoglobal
#' @noRd
as_equal <- function(x) {
  purrr::map(x, function(i) str2lang(paste0("equal(", deparse1(i), ")")))
}

#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = rlang::caller_env()) {

  idx <- rlang::names2(x) %!=% ""
  prn <- rlang::names2(x[idx])

  if (collapse::any_duplicated(prn)) {

    dupe <- prn[cheapr::which_(collapse::fduplicated(prn))]

    cli::cli_abort(
      c("x" = "{.field Query} names must be unique",
        "!" = "Field{?s} {.field {dupe}} appea{?rs/rs/r} multiple times."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_bare_unnamed <- function(x, call = rlang::caller_env()) {

  idx <- rlang::names2(x) %==% ""

  if (any(idx)) {

    cli::cli_abort(
      c("x" = "{.field Query} values must be named",
        "!" = "Unnamed value{?s}: {.field {x[idx]}}."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_group_members <- function(x, call = rlang::caller_env()) {
  # TODO
  # check no duplicate groups
  # check no nested groups

  grps <- purrr::map(x$grps, as.character) |>
    purrr::map(\(x) x[-1])

  # check no empty/single-member groups
  if (any(cheapr::list_lengths(grps) < 2L)) {

    cli::cli_abort(
      c("x" = "{.field Query} groups must have 2 or more members"),
      call  = call)
  }

  mems <- purrr::list_c(grps)

  # check no duplicate group members
  if (collapse::any_duplicated(mems)) {

    dupe <- mems[cheapr::which_(collapse::fduplicated(mems))]

    cli::cli_abort(
      c("x" = "{.field Query} group members must be unique",
        "!" = "Member{?s} {.field {dupe}} appear{?s/rs/r} in multiple groups."),
      call  = call)
  }

  # check group members are param names
  if (!any(mems %in% names(c(x$bare, x$mods)))) {

    bad <- mems[!mems %in% names(c(x$bare, x$mods))]

    cli::cli_abort(
      c("x" = "{.field Query} group members must be {.field query} field names",
        "!" = "Member{?s} {.field {bad}} do{?es/s} not match any {.field query} field names."),
      call  = call)
  }
}

#' @rdname query
#' @examples
#' query3(
#'   state = any_of("CA", "GA", "NY"),
#'   state_owner = c("GA", "MD"),
#'   or("state", "state_owner"))
#'
#' try(query3(
#'   first_name  = starts_with("And"),
#'   ccn         = "01256",
#'   and("ccn", "npii")))
#' @autoglobal
#' @export
query3 <- function(...) {

  x <- purrr::compact(rlang::enexprs(...))

  check_names_unique(x)

  x <- list(
    grps = purrr::keep(x, \(x) is_junc(x)),
    mods = purrr::keep(x, \(x) is_mod(x)),
    bare = purrr::keep(x, \(x) !is_junc(x) & !is_mod(x))
  )

  check_bare_unnamed(x$bare)
  check_group_members(x)

  g <- map_eval(x$j) |> rlang::set_names(paste0("g", seq_along(x$j)))
  x <- rlang::list2(!!!x$m, !!!as_equal(x$p)) |> map_eval()

  g_m  <- purrr::map(g, function(x) S7::prop(x, "members"))
  g_i <- purrr::map(g_m, \(y) rlang::names2(x) %iin% y)

  x[unlist(g_i, use.names = FALSE)] <- purrr::imap(g_i, function(g, i)
    purrr::map2(x[g], i, function(x, i)
      S7::set_props(x, member_of = i))) |>
    purrr::list_flatten(name_spec = "{inner}")

  grps_valid <- all(collapse::funique(unlist(g_m, use.names = FALSE)) %in_% rlang::names2(x))

  if (!grps_valid) {
    cli::cli_abort(
      c("x" = "All {.field group} members must be {.field query} field names"),
      call = rlang::caller_env())
  }

  class_query(params = x, groups = g)
}
