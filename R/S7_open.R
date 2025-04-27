#' @autoglobal
#' @noRd
Open <- new_class(name = "Open", package = NULL)

#' @noRd
#' @autoglobal
open_dimensions <- new_class(
  name = "open_dimensions",
  package = NULL,
  properties = list(
    limit = class_integer,
    rows = class_integer,
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows,
                    self@limit)
    ),
    fields = new_property(
      class_character,
      getter = function(self)
        as.list(self@fields) |>
        set_names(self@fields)
    )
  ),
  constructor = function(x) {

    x <- x$identifier |>
      request() |>
      req_url_query(
        schema  = "false",
        keys    = "false",
        results = "false",
        count   = "true",
        format  = "json",
        rowIds  = "false",
        offset  = 0L,
        limit   = 1L
      ) |>
      perform_simple()

    new_object(
      S7_object(),
      limit  = 500L,
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
open_metadata <- new_class(
  name = "open_metadata",
  package = NULL,
  properties = list(
    description = class_character,
    modified    = new_union(class_character, class_Date),
    download    = new_union(NULL, class_character)
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      modified    = x$modified,
      download    = x$download
    )
  }
)

#' Open Payments Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<openMain>` object.
#' @examples
#' openMain("PROF_covered")
#' openMain("PROF_physician")
#' openMain("PROF_information")
#' openMain("PROF_mapping")
#' openMain("PROF_entity")
#' openMain("PROF_teaching")
#' openMain("SUMM_dashboard")
#' openMain("SUMM_state_all")
#' openMain("SUMM_state_group")
#' openMain("SUMM_nation_all")
#' openMain("SUMM_nation_group")
#' @autoglobal
#' @rdname openpayments
#' @export
openMain <- new_class(
  parent     = Open,
  name       = "openMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    identifier  = class_character,
    metadata    = open_metadata,
    dimensions  = open_dimensions
  ),
  constructor = function(alias) {

    x <- open_main(alias)

    new_object(
      Open(),
      title       = x$title,
      identifier  = x$identifier,
      metadata    = open_metadata(x),
      dimensions  = open_dimensions(x)
    )
  }
)

#' Open Payments Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<openGroup>` object.
#' @examples
#' openGroup("profile")
#  openGroup("summary")
#' @autoglobal
#' @rdname openpayments
#' @export
openGroup <- new_class(
  parent     = Open,
  name       = "openGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, openMain) |>
        set_names(self@members)
      )
    ),
  constructor = function(alias) {

    x <- open_group(alias)

    new_object(
      Open(),
      group  = x$group,
      members = x$alias
    )
  }
)

#' Open Payments Temporal Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<openTemp>` object.
#' @examples
#' openTemp("DATA_general")
#' openTemp("DATA_ownership")
#' openTemp("DATA_research")
#' openTemp("GROUP_recip_nature")
#' openTemp("GROUP_recip_entity")
#' openTemp("GROUP_entity_nature")
#' openTemp("GROUP_all")
#' @autoglobal
#' @rdname openpayments
#' @export
openTemp <- new_class(
  parent     = Open,
  name       = "openTemp",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = open_metadata,
    dimensions  = open_dimensions,
    endpoints   = class_list
    ),
  constructor = function(alias) {

    x <- open_temp(alias)

    x <- list(
      title       = x$title[1],
      description = x$description[1],
      modified    = x$modified[1],
      identifier  = x$identifier[1],
      endpoints   = slt(x, year, identifier, download)
    )

    new_object(
      Open(),
      title       = x$title,
      metadata    = open_metadata(x),
      dimensions  = open_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)

#' Group of Open Payments Temporal Endpoints
#' @param alias `<chr>` title alias
#' @returns An S7 `<openTempGroup>` object.
#' @examples
#' openTempGroup("grouped_payment")
#' openTempGroup("detailed_payment")
#' @autoglobal
#' @rdname openpayments
#' @export
openTempGroup <- new_class(
  parent     = Open,
  name       = "openTempGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, openTemp) |>
        set_names(self@members)
    )
  ),
  constructor = function(alias) {

    x <- open_temp_group(alias)

    new_object(
      Open(),
      group   = x$group,
      members = x$alias
    )
  }
)
