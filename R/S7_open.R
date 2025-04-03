#' @autoglobal
#' @noRd
Open <- new_class(name = "Open", package = NULL)

#' Open Payments Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<openMain>` object.
#'
#' @examples
#' openMain("prof_cov")
#' openMain("prof_phys")
#' openMain("prof_info")
#' openMain("prof_map")
#' openMain("prof_entity")
#' openMain("prof_teach")
#' openMain("dashboard")
#' openMain("pay_state_total")
#' openMain("pay_state_group")
#' openMain("pay_nat_group")
#' openMain("pay_nat_total")
#' @autoglobal
#' @rdname Open
#' @export
openMain <- new_class(
  parent = Open,
  name = "openMain",
  package = NULL,
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    modified    = class_character | class_Date,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    download    = class_character,
    uuid        = class_character,
    identifier = new_property(
      class_character,
      getter = function(self) open_url(self@uuid),
      setter = NULL
    )
  ),
  constructor = function(alias) {

    x <- open_main(alias)
    q <- dims_open(x$identifier)

    new_object(
      Open(),
      title       = x$title,
      description = x$description,
      contact     = x$contact,
      modified    = x$modified,
      uuid        = x$identifier,
      # identifier  = x$identifier,
      download    = x$download,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages
    )
  }
)

#' Open Payments Temporal Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<openTemp>` object.
#'
#' @examples
#' openTemp("general")
#' openTemp("ownership")
#' openTemp("research")
#' @autoglobal
#' @rdname Open
#' @export
openTemp <- new_class(
  parent = Open,
  name = "openTemp",
  package = NULL,
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    years       = class_integer,
    endpoints   = class_list
  ),
  constructor = function(alias) {

    x <- open_temp(alias)
    q <- dims_open(x$identifier[1])

    new_object(
      Open(),
      title       = x$title[1],
      description = x$description[1],
      contact     = x$contact[1],
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages,
      years       = x$year,
      endpoints   = slt(x, year, modified, identifier, download)
    )
  }
)

#' Group of Open Payments Temporal Endpoints
#' @autoglobal
#' @noRd
openTempGroup <- new_class(
  parent  = Open,
  name    = "openTempGroup",
  package = NULL,
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    years       = class_integer,
    endpoints   = class_list
  )
)
