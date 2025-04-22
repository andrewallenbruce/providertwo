#' @autoglobal
#' @noRd
Pro <- new_class(name = "Pro", package = NULL)

#' @noRd
#' @autoglobal
proDim <- new_class(
  name = "proDim",
  package = NULL,
  properties = list(
    rows = new_property(
      class_integer,
      default = 0L
    ),
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, 2000L)
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
proMeta <- new_class(
  name = "proMeta",
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
#' proMain("affiliations")
#' proMain("clinicians")
#' proMain("utilization")
#' @autoglobal
#' @rdname pro
#' @export
proMain <- new_class(
  parent     = Pro,
  name       = "proMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = proMeta,
    dimensions  = proDim,
    identifier  = class_character
  ),
  constructor = function(alias) {

    x <- pro_main(alias)

    new_object(
      Pro(),
      title       = x$title,
      metadata    = proMeta(x),
      dimensions  = proDim(x),
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
    members = class_list),
  constructor = function(alias) {

    x <- pro_group(alias)

    new_object(
      Pro(),
      group  = x$group,
      members = map(x$alias, proMain) |> set_names(x$alias)
    )
  }
)
