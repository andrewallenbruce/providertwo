#' @autoglobal
#' @noRd
Care <- new_class(name = "Care", package = NULL)

#' Medicare Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<careMain>` object.
#'
#' @examples
#' careMain("enrollees")
#' careMain("opt_out")
#' careMain("order_refer")
#' careMain("reassignments")
#' # careMain("hospitals")
#' careMain("laboratories")
#' careMain("crosswalk")
#' careMain("rbcs")
#' careMain("facilities")
#' careMain("home_health")
#' careMain("hospice")
#' careMain("dialysis")
#' careMain("skilled_nursing")
#' @autoglobal
#' @rdname careMain
#' @export
careMain <- new_class(
  parent     = Care,
  name       = "careMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    description = class_character,
    modified    = class_character | class_Date,
    identifier  = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    download    = class_character,
    temporal    = class_character,
    periodicity = class_character,
    resources   = class_character,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  ),
  constructor = function(alias) {

    x <- care_main(alias)
    q <- dims_care(x$identifier)

    new_object(
      Care(),
      title       = x$title,
      description = x$description,
      modified    = x$modified,
      periodicity = x$periodicity,
      temporal    = x$temporal,
      identifier  = x$identifier,
      resources   = x$resources,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages,
      download    = x$download,
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references
    )
  }
)

#' Medicare Endpoint Group
#'
#' @param alias `<chr>` title alias
#'
#' @returns An S7 `<careGroup>` object.
#'
#' @examples
#' careGroup("pending")
#' careGroup("rhc")
#' careGroup("fqhc")
#' careGroup("hospitals")
#' @autoglobal
#' @rdname careGroup
#' @export
careGroup <- new_class(
  parent     = Care,
  name       = "careGroup",
  package    = NULL,
  properties = list(
    title       = NULL | class_character,
    description = NULL | class_character,
    modified    = NULL | class_character | class_Date,
    periodicity = NULL | class_character,
    temporal    = NULL | class_character,
    groups      = class_list
  ),
  constructor = function(alias) {

    x <- care_group(alias)

    q <- map(x$identifier, dims_care) |>
      set_names(gsub("^pending_initial_logging_and_tracking_", "", cclean(x$title), perl = TRUE))

    i <- rsplit(x, ~ title) |>
      set_names(gsub("^pending_initial_logging_and_tracking_", "", cclean(x$title), perl = TRUE))

    template <- glue(
      "
      {group} = list(
        rows      = q${group}$rows,
        pages     = q${group}$pages,
        fields    = q${group}$fields,
        endpoints = i${group}
        )
      ",
      group = names(q)) |>
      glue_collapse(sep = ",\n")

    new_object(
      Care(),
      title  = x$title[1],
      groups = glue("list({template})") |>
        parse_expr() |>
        eval_bare()
    )
  }
)

#' Medicare Temporal Endpoint
#'
#' @param alias `<chr>` title alias
#'
#' @returns An S7 `<careTemp>` object.
#'
#' @examples
#' careTemp("quality_payment")
#' @autoglobal
#' @rdname careTemp
#' @export
careTemp <- new_class(
  parent     = Care,
  name       = "careTemp",
  package    = NULL,
  properties = list(
    title       = class_character,
    description = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    periodicity = class_character,
    dictionary  = class_character,
    site        = class_character,
    endpoints   = class_list
  ),
  constructor = function(alias) {

    x <- care_temp(alias)
    d <- get_elem(x, "data")[[1]]
    q <- dims_care_temp(d$identifier[1])

    new_object(
      Care(),
      title       = x$title,
      description = x$description,
      rows        = q$rows,
      pages       = q$pages,
      fields      = q$fields,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site,
      endpoints   = slt(d, year, modified, identifier, download, resources)
    )
  }
)

#' Group of Medicare Temporal Endpoints
#'
#' @param alias `<chr>` title alias
#'
#' @returns An S7 `<careTempGroup>` object.
#'
#' @examples
#' careTempGroup("inpatient")
#' careTempGroup("outpatient")
#' careTempGroup("prescribers")
#' careTempGroup("suppliers")
#' careTempGroup("utilization")
#' @autoglobal
#' @rdname careTempGroup
#' @export
careTempGroup <- new_class(
  parent     = Care,
  name       = "careTempGroup",
  package    = NULL,
  properties = list(
    title       = class_character,
    periodicity = class_character,
    years       = class_integer,
    groups      = class_list
  ),
  constructor = function(alias) {

    x <- care_temp_group(alias)

    template <- glue(
      "
      {group} = list(
        description = x${group}$description,
        dictionary  = x${group}$dictionary,
        site        = x${group}$site,
        rows        = x${group}$rows,
        pages       = x${group}$pages,
        fields      = x${group}$fields,
        endpoints   = x${group}$endpoints
        )
      ",
      group = x$groups) |>
      glue_collapse(sep = ",\n")

    new_object(
      Care(),
      title        = x$title,
      periodicity  = x$periodicity,
      years        = x$years,
      groups       = glue("list({template})") |>
        parse_expr() |>
        eval_bare()
    )
  }
)
