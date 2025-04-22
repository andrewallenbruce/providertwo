#' @autoglobal
#' @noRd
Open <- new_class(name = "Open", package = NULL)

#' @noRd
#' @autoglobal
openDim <- new_class(
  name = "openDim",
  package = NULL,
  properties = list(
    rows = new_property(
      class_integer,
      default = 0L
    ),
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, 500L)
    ),
    fields = class_character
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
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
openMeta <- new_class(
  name = "openMeta",
  package = NULL,
  properties = list(
    description = class_character,
    modified    = new_union(class_character, class_Date),
    download    = class_character
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
#' #openMain("SUMM_dashboard")
#' openMain("SUMM_state_all")
#' openMain("SUMM_state_group")
#' openMain("SUMM_nation_all")
#' openMain("SUMM_nation_group")
#' @autoglobal
#' @rdname openMain
#' @export
openMain <- new_class(
  parent     = Open,
  name       = "openMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = openMeta,
    dimensions  = openDim,
    identifier  = class_character
  ),
  constructor = function(alias) {

    x <- open_main(alias)

    new_object(
      Open(),
      title       = x$title,
      metadata    = openMeta(x),
      dimensions  = openDim(x),
      identifier  = x$identifier
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
#' @rdname openGroup
#' @export
openGroup <- new_class(
  parent     = Open,
  name       = "openGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = class_list),
  constructor = function(alias) {

    x <- open_group(alias)

    new_object(
      Open(),
      group  = x$group,
      members = map(x$alias, openMain) |> set_names(x$alias)
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
#' @rdname openTemp
#' @export
openTemp <- new_class(
  parent     = Open,
  name       = "openTemp",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = openMeta,
    dimensions  = openDim,
    endpoints   = class_list
    ),
  constructor = function(alias) {

    x <- open_temp(alias)

    m <- list(
      description = x$description[1],
      modified = x$modified[1],
      download = character(0),
      identifier = x$identifier[1]
    )

    new_object(
      Open(),
      title       = x$title[1],
      metadata    = openMeta(m),
      dimensions  = openDim(m),
      endpoints   = slt(x, year, modified, identifier, download)
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
#' @rdname openTempGroup
#' @export
openTempGroup <- new_class(
  parent     = Open,
  name       = "openTempGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    # years = class_integer,
    members = class_list
  ),
  constructor = function(alias) {

    x <- open_temp_group(alias)

    new_object(
      Care(),
      group  = x$group,
      members = map(x$alias, openTemp) |>
        set_names(x$alias)
    )
  }
)
