#' @noRd
#' @autoglobal
null_if <- function(x) {
  if (is.null(x)) return(NULL)
  if (is_na(x)) NULL else x
}

#' @noRd
#' @autoglobal
unlist_if <- function(x) {
  if (is.list(x)) unlist(x, use.names = FALSE) else x
}

#' @noRd
#' @autoglobal
get_metadata <- function(x) {
  compact(list(
    api         = null_if(x$api),
    title       = null_if(x$title),
    description = null_if(x$description),
    modified    = null_if(x$modified),
    group       = null_if(x$group),
    issued      = null_if(x$issued),
    released    = null_if(x$released),
    temporal    = null_if(x$temporal),
    periodicity = null_if(x$periodicity),
    download    = null_if(x$download),
    resources   = unlist_if(null_if(x$resources)),
    dictionary  = null_if(x$dictionary),
    site        = null_if(x$site),
    references  = null_if(x$references)
  ))
}

#' @noRd
#' @autoglobal
class_dimensions <- new_class(
  name       = "class_dimensions",
  package    = NULL,
  properties = list(
    limit = class_integer,
    rows  = class_integer,
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, self@limit)
    ),
    fields = new_property(
      class_character | class_list,
      getter = function(self)
        set_names(
          new_list(
            length(self@fields),
            character(0)),
          self@fields),
      setter = function(self, value) {
        self@fields <- value
        self
      }
    )
  )
)

#' @noRd
#' @autoglobal
class_backend <- new_class(
  name = "class_backend",
  package = NULL,
  abstract = TRUE)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name          = "class_endpoint",
  package       = NULL,
  parent        = class_backend,
  properties    = list(
    identifier  = class_character,
    metadata    = class_list,
    dimensions  = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name          = "class_temporal",
  package       = NULL,
  parent        = class_backend,
  properties    = list(
    metadata    = class_list,
    dimensions  = class_dimensions,
    endpoints   = class_list
  )
)

#' @noRd
#' @autoglobal
class_group <- new_class(
  name       = "class_group",
  package    = NULL,
  properties = list(
    group    = class_character,
    members  = class_list
  ),
  validator = function(self) {
    if (!all(rlang::have_name(self@members))) "all @members must be named"
    if (!all(map_lgl(self@members, S7_inherits, class_backend))) "all @members must be a `class_backend` object"
  }
)

#' @autoglobal
#' @noRd
get_dimensions <- function(x, call = caller_env()) {

  api <- x$api

  limit <- switch(
    api,
    `Open Payments`            = ,
    `Open Payments [Temporal]` = ,
    `HealthcareGov`            = ,
    `HealthcareGov [Temporal]` = 500L,
    `Provider`                 = 2000L,
    `Medicare`                 = ,
    `Medicare [Temporal]`      = 5000L,
    `Medicaid`                 = ,
    `Medicaid [Temporal]`      = 8000L,
    cli::cli_abort(c("x"       = "{.val {x$api}} is an invalid API."), call = call)
  )

  req <- request(x$identifier) |>
    req_url_query(offset = 0L) |>
    req_error(body = \(resp) resp_body_json(resp)$meta$message)

  req <- switch(
    api,
    `Medicare` = ,
    `Medicare [Temporal]` = req_url_query(req, size = 1L),
    req_url_query(req, limit = 1L, results = "false")
    )

  x <- switch(
    api,
    `Medicare`            = perform_simple(req)$meta |> get_elem(c("total_rows", "headers")),
    `Medicare [Temporal]` = list(headers = perform_simple(req) |> names(), total_rows = req_url_path_append(req, "stats") |> perform_simple() |> get_elem("total_rows")),
    perform_simple(req)
  )

  switch(
    api,
    `Medicare` = ,
    `Medicare [Temporal]` = class_dimensions(limit = limit, rows = x$total_rows, fields = x$headers),
    class_dimensions(limit = limit, rows = x$count, fields = x$query$properties)
  )
}

# select_caid("managed_care_bene_year") |> get_dimensions()
# select_caid_temp("caid_drug_rebate_week") |> get_dimensions()
# select_care("lab_fee_schedule") |> get_dimensions()
# select_care_temp("procedure_summary") |> get_dimensions()
# select_hgov_temp("hie_network") |> get_dimensions()
# select_hgov("rolling_draft_ecp") |> get_dimensions()
# select_open("summary_specialty") |> get_dimensions()
# select_open_temp("payment_general") |> get_dimensions()
# select_pro("hospital_general") |> get_dimensions()
