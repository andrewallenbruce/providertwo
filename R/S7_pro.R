#' @autoglobal
#' @noRd
Pro <- new_class(name = "Pro", package = NULL)


#' @noRd
#' @autoglobal
pro_dimensions <- new_class(
  name = "pro_dimensions",
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
      limit  = 2000L,
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
pro_metadata <- new_class(
  name = "pro_metadata",
  package = NULL,
  properties = list(
    description = class_character,
    issued      = new_union(class_character, class_Date),
    modified    = new_union(class_character, class_Date),
    released    = new_union(class_character, class_Date),
    site        = class_character,
    dictionary  = class_character,
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      issued      = x$issued,
      modified    = x$modified,
      released    = x$released,
      site        = x$site,
      dictionary  = x$dictionary,
      download    = x$download
    )
  }
)

#' Provider Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<proMain>` object.
#' @examples
#' proMain("PDC_affiliations")
#' proMain("PDC_clinicians")
#' proMain("PDC_utilization")
#' @autoglobal
#' @rdname pro
#' @export
proMain <- new_class(
  parent     = Pro,
  name       = "proMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = pro_metadata,
    dimensions  = pro_dimensions,
    identifier  = class_character
  ),
  constructor = function(alias) {

    x <- pro_main(alias)

    new_object(
      Pro(),
      title       = x$title,
      metadata    = pro_metadata(x),
      dimensions  = pro_dimensions(x),
      identifier  = x$identifier
    )
  }
)

#' Provider Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<proGroup>` object.
#' @examples
#' proGroup("PDC")
#' proGroup("MIPS")
#' @autoglobal
#' @rdname pro
#' @export
proGroup <- new_class(
  parent     = Pro,
  name       = "proGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, \(x) proMain(x)) |>
          set_names(self@members)
      )
    ),
  constructor = function(alias) {

    x <- pro_group(alias)

    new_object(
      Pro(),
      group   = x$group,
      members = x$alias
    )
  }
)
