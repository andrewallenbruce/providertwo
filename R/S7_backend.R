#' API Current Endpoint Class
#'
#' @param title       `<chr>` title
#' @param description `<chr>` description
#' @param contact     `<chr>` contact name and email address
#' @param modified    `<chr>` date last modified
#' @param identifier  `<chr>` uuid
#' @param rows        `<int>` number of rows
#' @param fields      `<chr>` field names
#' @param pages       `<int>` number of pages
#' @param download    `<chr>` download URL
#'
#' @returns An S7 `<Current>` object.
#'
#' @autoglobal
#' @keywords internal
#' @export
Current <- new_class(
  name          = "Current",
  properties    = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    modified    = class_character | class_Date,
    identifier  = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    download    = class_character
  )
)

#' Main API Endpoint Resources
#' @param url `<chr>` resources URL
#' @returns An S7 `<Resources>` object.
#' @autoglobal
#' @keywords internal
#' @export
Resources <- new_class(
  name = "Resources",
  properties = list(
    url = class_character,
    files = new_property(
      class_list,
      getter = function(self) {
        fload(self@url,
              query = "/data") |>
          fcompute(
            file = name,
            size = roundup(fileSize / 1e6),
            ext = file_ext(downloadURL),
            download = downloadURL) |>
          roworder(ext, -size)
      }
    )
  ),
  validator = function(self)
    if (length(self@url) != 1L)
      "must be length 1"
)

#' Main API Endpoint (Current)
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<CurrentMain>` object.
#'
#' @examples
#' CurrentMain("enrollees")
#' CurrentMain("opt_out")
#' CurrentMain("order_refer")
#' CurrentMain("reassignments")
#' CurrentMain("hospitals")
#' CurrentMain("laboratories")
#' CurrentMain("crosswalk")
#' CurrentMain("rbcs")
#' CurrentMain("facilities")
#' CurrentMain("home_health")
#' CurrentMain("hospice")
#' CurrentMain("dialysis")
#' CurrentMain("snf")
#'
#' @autoglobal
#' @export
CurrentMain <- new_class(
  parent = Current,
  name   = "CurrentMain",
  properties = list(
    temporal    = class_character,
    periodicity = class_character,
    resources   = Resources,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  ),
  constructor = function(alias) {

    x <- catalog_main()$current |>
      subset_detect(
        title,
        alias_main_current(alias)) |>
      c()

    q <- main_nrows_fields(x$identifier)

    new_object(
      Current(),
      title       = x$title,
      description = x$description,
      contact     = x$contact,
      modified    = x$modified,
      periodicity = x$periodicity,
      temporal    = x$temporal,
      identifier  = x$identifier,
      resources   = Resources(x$resources),
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages,
      download    = x$download,
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references)
  }
)

#' Provider API Endpoint (Current)
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<CurrentProvider>` object.
#'
#' @examples
#' CurrentProvider("affiliations")
#' CurrentProvider("clinicians")
#' CurrentProvider("utilization")
#'
#' @autoglobal
#'
#' @export
CurrentProvider <- new_class(
  parent = Current,
  name = "CurrentProvider",
  properties = list(
    issued     = class_character | class_Date,
    released   = class_character | class_Date,
    group      = class_character,
    uuid       = class_character,
    site       = class_character,
    identifier = new_property(
      class_character,
      getter = function(self)
        prov_uuid_url(self@uuid))
    ),
  constructor = function(alias) {

    x <- subset_detect(
      catalog_provider(),
      title,
      alias_provider_current(alias)) |>
      c()

    q <- prov_nrows_fields(x$identifier)

    new_object(
      Current(),
      title       = x$title,
      description = x$description,
      group       = x$group,
      contact     = x$contact,
      modified    = x$modified,
      uuid        = x$identifier,
      download    = x$download,
      issued      = x$issued,
      released    = x$released,
      site        = x$site,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages)
  }
)

#' Open Payments API Endpoint (Current)
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<CurrentOpen>` object.
#'
#' @examples
#' CurrentOpen("prof_cov")
#' CurrentOpen("prof_phys")
#' CurrentOpen("prof_info")
#' CurrentOpen("prof_map")
#' CurrentOpen("prof_entity")
#' CurrentOpen("prof_teach")
#' CurrentOpen("dashboard")
#' CurrentOpen("pay_state_total")
#' CurrentOpen("pay_state_group")
#' CurrentOpen("pay_nat_group")
#' CurrentOpen("pay_nat_total")
#'
#' @autoglobal
#' @export
CurrentOpen <- new_class(
  parent = Current,
  name   = "CurrentOpen",
  properties = list(
    uuid       = class_character,
    identifier = new_property(
      class_character,
      getter = function(self) open_url(self@uuid)
      )
    ),
  constructor = function(alias) {

    x <- catalog_open()$current |>
      subset_detect(
        title,
        alias_open_current(alias)) |>
      c()

    q <- open_nrows_fields(x$identifier)

    new_object(
      Current(),
      title       = x$title,
      description = x$description,
      contact     = x$contact,
      modified    = x$modified,
      uuid        = x$identifier,
      download    = x$download,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages)
    }
  )

#' Temporal Endpoint Object
#'
#' @param title       `<chr>` title
#' @param description `<chr>` description
#' @param contact     `<chr>` contact name and email address
#' @param rows        `<int>` number of rows
#' @param pages       `<int>` number of pages
#' @param fields      `<chr>` field names
#' @param years       `<int>` years available
#' @param endpoints   `<data.frame>` endpoints
#'
#' @returns An S7 `<Temporal>` object.
#'
#' @autoglobal
#' @keywords internal
#' @export
Temporal <- new_class(
  name = "Temporal",
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

#' Main API Endpoint (Temporal)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalMain>` object.
#'
#' @autoglobal
#' @export
TemporalMain <- new_class(
  parent = Temporal,
  name   = "TemporalMain"
)

#' Open Payments API Endpoint (Temporal)
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<TemporalOpen>` object.
#'
#' @examples
#' TemporalOpen("general")
#' TemporalOpen("ownership")
#' TemporalOpen("research")
#' TemporalOpen("recipient_nature")
#' TemporalOpen("recipient_entity")
#' TemporalOpen("entity_nature")
#' TemporalOpen("entity_recipient_nature")
#' TemporalOpen("state_nature")
#'
#' @autoglobal
#' @export
TemporalOpen <- new_class(
  parent = Temporal,
  name   = "TemporalOpen",
  constructor = function(alias) {

    x <- catalog_open()$temporal |>
      subset_detect(
        title,
        alias_open_temporal(alias))

    q <- open_nrows_fields(x$identifier[1])

    new_object(
      Temporal(),
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

#' Main API Endpoint Group (Temporal)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalMainGroup>` object.
#'
#' @autoglobal
#' @export
TemporalMainGroup <- new_class(
  parent = Temporal,
  name   = "TemporalMainGroup"
)

#' Open Payments API Endpoint Group (Temporal)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalOpenGroup>` object.
#'
#' @autoglobal
#' @export
TemporalOpenGroup <- new_class(
  parent = Temporal,
  name   = "TemporalOpenGroup"
)
