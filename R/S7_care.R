#' @autoglobal
#' @noRd
Care <- new_class(name = "Care", package = NULL)

#' Medicare Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<careMain>` object.
#'
#' @examplesIf interactive()
#' careMain("contact")
#' careMain("crosswalk")
#' careMain("dialysis")
#' careMain("enrollees")
#' careMain("facilities")
#' careMain("hospice_acute")
#' careMain("IQIES")
#' careMain("laboratories")
#' careMain("long_term")
#' careMain("opt_out")
#' careMain("order_refer")
#' careMain("rbcs")
#' careMain("transparency")
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

#' Medicare Class Metadata
#' @param x `<list>` metadata list
#' @returns An S7 `<careMeta>` object.
#' @export
#' @keywords internal
#' @autoglobal
careMeta <- new_class(
  name = "careMeta",
  package = NULL,
  properties = list(
    description = class_character,
    temporal    = class_character,
    periodicity = class_character,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character,
    modified    = new_union(class_character, class_Date),
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      temporal    = x$temporal,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references,
      modified    = x$modified,
      download    = x$download
    )
  }
)

#' Medicare Class Dimensions
#' @param x `<list>` dimensions list
#' @returns An S7 `<careDim>` object.
#' @export
#' @keywords internal
#' @autoglobal
careDim <- new_class(
  name = "careDim",
  package = NULL,
  properties = list(
    rows   = class_integer,
    pages  = class_integer,
    fields = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      rows   = x$rows,
      pages  = x$pages,
      fields = x$fields
    )
  }
)

#' Medicare Endpoint 2
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<careMain>` object.
#'
#' @examples
#' careMain2("contact")
#' careMain2("crosswalk")
#' careMain2("dialysis")
#' careMain2("enrollees")
#' careMain2("facilities")
#' careMain2("hospice_acute")
#' careMain2("IQIES")
#' careMain2("laboratories")
#' careMain2("long_term")
#' careMain2("opt_out")
#' careMain2("order_refer")
#' careMain2("rbcs")
#' careMain2("transparency")
#' @autoglobal
#' @rdname careMain
#' @export
careMain2 <- new_class(
  parent     = Care,
  name       = "careMain2",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = careMeta,
    dimensions  = careDim,
    identifier  = class_character,
    resources   = class_character
  ),
  constructor = function(alias) {

    x <- care_main(alias)
    q <- dims_care(x$identifier)

    new_object(
      S7_object(),
      title       = x$title,
      metadata    = careMeta(x),
      dimensions  = careDim(q),
      identifier  = x$identifier,
      resources   = x$resources
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


#' Medicare Endpoint Group
#'
#' @param alias `<chr>` title alias
#'
#' @returns An S7 `<careGroup>` object.
#'
#' @examplesIf interactive()
#' careGroup("home_health")
#' careGroup("hospice")
#' careGroup("hospitals")
#' careGroup("rural_health")
#' careGroup("fqhc")
#' careGroup("pending")
#' careGroup("reassignment")
#' careGroup("skilled_nursing")
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
      set_clean(gsub("^pending_initial_logging_and_tracking_", "", x$title, perl = TRUE))

    i <- rsplit(x, ~ title) |>
      set_clean(gsub("^pending_initial_logging_and_tracking_", "", x$title, perl = TRUE))

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

#' Group of Medicare Temporal Endpoints
#'
#' @param alias `<chr>` title alias
#'
#' @returns An S7 `<careTempGroup>` object.
#'
#' @examplesIf interactive()
#' careTempGroup("inpatient")
#' careTempGroup("outpatient")
#' careTempGroup("utilization")
#' careTempGroup("suppliers")
#' careTempGroup("prescribers")
#' careTempGroup("staffing")
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
      years        = order(x$years),
      groups       = glue("list({template})") |> parse_expr() |> eval_bare()
    )
  }
)
