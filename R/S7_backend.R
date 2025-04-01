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
  name = "Current",
  package = NULL,
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

#' Current Main API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<MainCurrent>` object.
#'
#' @examples
#' MainCurrent("enrollees")
#' MainCurrent("opt_out")
#' MainCurrent("order_refer")
#' MainCurrent("reassignments")
#' MainCurrent("hospitals")
#' MainCurrent("laboratories")
#' MainCurrent("crosswalk")
#' MainCurrent("rbcs")
#' MainCurrent("facilities")
#' MainCurrent("home_health")
#' MainCurrent("hospice")
#' MainCurrent("dialysis")
#' MainCurrent("skilled_nursing")
#' @autoglobal
#' @rdname Main
#' @export
MainCurrent <- new_class(
  parent = Current,
  name = "MainCurrent",
  package = NULL,
  properties = list(
    temporal    = class_character,
    periodicity = class_character,
    resources   = class_character,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  ),
  constructor = function(alias) {

    if (!exists("catalog")) catalog <- catalogs()

    x <- c(select_alias(
      catalog$main$current,
      alias_main(alias)))

    q <- dims_main(x$identifier)

    new_object(
      Current(),
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

#' Current Provider API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<proCurr>` object.
#'
#' @examples
#' proCurr("affiliations")
#' proCurr("clinicians")
#' proCurr("utilization")
#' @autoglobal
#' @rdname Provider
#' @export
proCurr <- new_class(
  parent = Current,
  name = "proCurr",
  package = NULL,
  properties = list(
    issued     = class_character | class_Date,
    released   = class_character | class_Date,
    uuid       = class_character,
    site       = class_character,
    identifier = new_property(
      class_character,
      getter = function(self) pro_url(self@uuid),
      setter = NULL
    )
  ),
  constructor = function(alias) {

    if (!exists("catalog")) catalog <- catalogs()

    x <- c(select_alias(catalog$pro, alias_pro(alias)))

    q <- dims_pro(x$identifier)

    new_object(
      Current(),
      title       = x$title,
      description = x$description,
      contact     = x$contact,
      modified    = x$modified,
      uuid        = x$identifier,
      download    = x$download,
      issued      = x$issued,
      released    = x$released,
      site        = x$site,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages
    )
  }
)

#' Provider API Group Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns `<data.frame>`
#'
#' @examples
#' pro_group("mips")
#' @autoglobal
#' @export
pro_group <- function(alias) {

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$pro, alias_pro_group(alias))
}

#' Current Open Payments API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<openCurr>` object.
#'
#' @examples
#' openCurr("prof_cov")
#' openCurr("prof_phys")
#' openCurr("prof_info")
#' openCurr("prof_map")
#' openCurr("prof_entity")
#' openCurr("prof_teach")
#' openCurr("dashboard")
#' openCurr("pay_state_total")
#' openCurr("pay_state_group")
#' openCurr("pay_nat_group")
#' openCurr("pay_nat_total")
#' @autoglobal
#' @rdname Open
#' @export
openCurr <- new_class(
  parent = Current,
  name = "openCurr",
  package = NULL,
  properties = list(
    uuid       = class_character,
    identifier = new_property(
      class_character,
      getter = function(self)
        open_url(self@uuid)
    )
  ),
  constructor = function(alias) {

    if (!exists("catalog")) catalog <- catalogs()

    x <- c(select_alias(catalog$open$current, alias_open(alias)))

    q <- dims_open(x$identifier)

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
      pages       = q$pages
    )
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

#' Temporal Main API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<mainTemp>` object.
#'
#' @examples
#' mainTemp("quality_payment")
#' @autoglobal
#' @rdname Main
#' @export
mainTemp <- new_class(
  parent = Temporal,
  name = "mainTemp",
  package = NULL,
  properties = list(
    periodicity = class_character,
    dictionary  = class_character,
    site        = class_character
  ),
  constructor = function(alias) {

    if (!exists("catalog")) catalog <- catalogs()

    x <- select_alias(catalog$main$temporal, alias_main_temp(alias))

    dat <- get_elem(x, "data")[[1]]

    q <- dims_main_temp(dat$identifier[1])

    new_object(
      Temporal(),
      title       = x$title,
      description = x$description,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site,
      contact     = x$contact,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages,
      years       = dat$year,
      endpoints   = slt(dat, year, modified, identifier, download, filetype, resources)
    )
  }
)

#' Temporal Open Payments API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<openTemp>` object.
#'
#' @examples
#' openTemp("general")
#' openTemp("ownership")
#' openTemp("research")
#' openTemp("recipient_nature")
#' openTemp("recipient_entity")
#' openTemp("entity_nature")
#' openTemp("entity_recipient_nature")
#' openTemp("state_nature")
#' @autoglobal
#' @rdname Open
#' @export
openTemp <- new_class(
  parent = Temporal,
  name = "openTemp",
  package = NULL,
  constructor = function(alias) {

    if (!exists("catalog")) catalog <- catalogs()

    x <- select_alias(catalog$open$temporal, alias_open_temp(alias))

    q <- dims_open(x$identifier[1])

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

#' Temporal Group Endpoint
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalGroup>` object.
#'
#' @autoglobal
#' @keywords internal
#' @export
TemporalGroup <- new_class(
  name = "TemporalGroup",
  package = NULL,
  properties = list(
    title       = class_character,
    contact     = class_character,
    endpoints   = class_list
  )
)

#' Main API Endpoint Temporal Group
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<mainTempGroup>` object.
#'
#' @examples
#' mainTempGroup("utilization")
#' mainTempGroup("prescribers")
#' mainTempGroup("suppliers")
#' mainTempGroup("outpatient")
#' mainTempGroup("inpatient")
#' @autoglobal
#' @rdname Main
#' @export
mainTempGroup <- new_class(
  parent  = TemporalGroup,
  name    = "mainTempGroup",
  package = NULL,
  properties = list(
    periodicity = class_character,
    years       = class_integer
  ),
  constructor = function(alias) {

    x <- main_temp_group(alias)

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
      TemporalGroup(),
      title        = x$title,
      contact      = x$contact,
      periodicity  = x$periodicity,
      years        = x$years,
      endpoints    = glue("list({template})") |>
        parse_expr() |>
        eval_bare()
    )
  }
)

#' Open Payments API Endpoint Temporal Group
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<openTempGroup>` object.
#'
#' @autoglobal
#' @rdname Open
#' @export
openTempGroup <- new_class(
  parent  = Temporal,
  name    = "openTempGroup",
  package = NULL
)
