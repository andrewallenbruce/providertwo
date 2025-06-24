#' @autoglobal
#' @noRd
alias_info <- function(x) {
  flist(
    alias   = x,
    api     = api_type(x),
    catalog = catalog_type(x),
    regex   = alias_regex(x),
    str     = str2lang(glue("the$catalog${catalog}${api}"))
  )
}

#' API Endpoints
#' @param alias `<chr>` endpoint alias
#' @returns An S7 object
#' @examplesIf rlang::is_interactive()
#' new_endpoint("care_dial_end")
#' new_endpoint("care_dial_tmp")
#' new_endpoint("quality_payment")
#' new_endpoint("managed_longterm")
#' new_endpoint("state_drug_util")
#' new_endpoint("hgov_ab_reg_comp")
#' new_endpoint("hgov_puf_rate")
#' new_endpoint("profile_covered")
#' new_endpoint("grouped_state_nature")
#' new_endpoint("asc_facility")
#' new_endpoint("prov_dialysis")
#' @autoglobal
#' @export
new_endpoint <- function(alias) {

  check_required(alias)
  check_character(alias, allow_na = FALSE, allow_null = FALSE)

  x   <- alias_info(alias)

  res <- select_alias(eval(x$str), x$regex)

  if (is_empty(res))  cli_abort(c("x" = "{.val {x$regex}} has 0 matches."))
  if (nrow(res) > 1L) cli_abort(c("x" = "{.val {x$regex}} has multiple matches."))

  # end <- yank(get_elem(res, "endpoints"))
  #
  # if (all_na(gv(end, "resources"))) gv(end, "resources") <- NULL

  a <- switch(
    x$api,
    end = c(res),
    tmp = flist(!!!c(slt(res, -endpoints)),
                endpoints = yank(get_elem(res, "endpoints")),
                identifier = yank(get_elem(endpoints, "identifier")))
  )

  switch(
    x$catalog,
    care = class_care(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(slt(a$endpoints, -resources)),
      resources  = if (x$api == "end")
        class_endpoint(a$resources)
      else
        class_temporal(slt(a$endpoints, year, resources))
    ),
    caid = class_caid(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
    prov = class_prov(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
    open = class_open(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
    hgov = class_hgov(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
  )
}

#' Create Collection of Endpoints
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param call `<env>` Environment to use for error reporting.
#' @returns S7 `class_collection` object.
#' @examplesIf rlang::is_interactive()
#' new_collection("caid_demographics")
#' new_collection("caid_unwind")
#' new_collection("caid_services")
#' @autoglobal
#' @export
new_collection <- function(alias, call = caller_env()) {

  check_required(alias)
  check_character(alias, allow_na = FALSE, allow_null = FALSE)

  x <- group_regex(alias)

  if (is_empty(x)) {
    cli_abort(
      c("x" = "Collection {.emph alias} {.val {alias}} is invalid."),
      call = call)
  }

  class_collection(
    group = "Collection",
    set_names(map(x, new_endpoint), x))
}

#' Create Groups of Endpoints
#'
#' @param members `<chr>` Alias representing the CMS data endpoint.
#' @param name    `<chr>` Name of the group.
#' @param quick   `<lgl>` call `quick_()` method on group if `TRUE`.
#' @param offset  `<int>` Offset for pagination.
#' @param limit   `<int>` Limit for pagination.
#' @returns S7 `class_group` object.
#' @examplesIf rlang::is_interactive()
#' new_group(c("hgov_local_help", "hgov_qhp_business"))
#' new_group(c("hgov_local_help", "hgov_qhp_business"), quick = TRUE)
#' @autoglobal
#' @export
new_group <- function(members,
                      name = NULL,
                      quick = FALSE,
                      offset = 0,
                      limit = 10) {

  check_required(members)
  check_character(name, allow_na = FALSE, allow_null = FALSE)

  obj <- ifelse_(
    length(members) == 1L,
    new_endpoint(name),
    class_group(
      name = name %||% paste0(members, collapse = " | "),
      members = set_names(map(members, new_endpoint), members)))

  check_bool(quick)
  if (!quick) return(ob)

  check_number_whole(offset, min = 0)
  check_number_whole(limit, min = 1)
  quick_(obj, offset = offset, limit = limit)
}
