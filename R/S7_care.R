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
#' careMain("hospitals")
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
  parent = Care,
  name = "careMain",
  package = NULL,
  properties = list(
    # common
    title       = class_character,
    description = class_character,
    contact     = class_character,
    modified    = class_character | class_Date,
    identifier  = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    download    = class_character,
    # unique
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
      contact     = x$contact,
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
  parent = Care,
  name = "careTemp",
  package = NULL,
  properties = list(
    # common
    title       = class_character,
    description = class_character,
    contact     = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    endpoints   = class_list,
    # unique
    periodicity = class_character,
    dictionary  = class_character,
    site        = class_character
  ),
  constructor = function(alias) {

    x   <- care_temp(alias)
    dat <- get_elem(x, "data")[[1]]
    q   <- dims_care_temp(dat$identifier[1])

    new_object(
      Care(),
      title       = x$title,
      description = x$description,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site,
      contact     = x$contact,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages,
      endpoints   = slt(dat, year, modified, identifier, download, filetype, resources)
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
#' careTempGroup("utilization")
#' careTempGroup("prescribers")
#' careTempGroup("suppliers")
#' careTempGroup("outpatient")
#' careTempGroup("inpatient")
#' @autoglobal
#' @rdname careTemp
#' @export
careTempGroup <- new_class(
  parent  = Care,
  name    = "careTempGroup",
  package = NULL,
  properties = list(
    # common
    title       = class_character,
    contact     = class_character,
    groups      = class_list,
    # unique
    periodicity = class_character,
    years       = class_integer
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
      contact      = x$contact,
      periodicity  = x$periodicity,
      years        = x$years,
      groups       = glue("list({template})") |>
        parse_expr() |>
        eval_bare()
    )
  }
)
