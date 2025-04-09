#' @autoglobal
#' @noRd
Pro <- new_class(name = "Pro", package = NULL)

#' Provider Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns An S7 `<proMain>` object.
#'
#' @examples
#' proMain("affiliations") |> str()
#' proMain("clinicians") |> str()
#' proMain("utilization") |> str()
#' @autoglobal
#' @rdname proMain
#' @export
proMain <- new_class(
  parent     = Pro,
  name       = "proMain",
  package    = NULL,
  properties = list(
    # common
    title       = class_character,
    description = class_character,
    # contact     = class_character,
    modified    = class_character | class_Date,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    download    = class_character,
    # unique
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

    x <- pro_main(alias)
    q <- dims_pro(x$identifier)

    new_object(
      Pro(),
      title       = x$title,
      description = x$description,
      # contact     = x$contact,
      uuid        = x$identifier,
      download    = x$download,
      issued      = x$issued,
      modified    = x$modified,
      released    = x$released,
      site        = x$site,
      rows        = q$rows,
      fields      = q$fields,
      pages       = q$pages
    )
  }
)
