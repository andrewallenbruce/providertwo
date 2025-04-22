proMain <- new_class(
  parent     = Pro,
  name       = "proMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    description = class_character,
    modified    = class_character | class_Date,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    download    = class_character,
    issued     = class_character | class_Date,
    released   = class_character | class_Date,
    site       = class_character,
    identifier = class_character
  ),
  constructor = function(alias) {

    x <- pro_main(alias)
    q <- dims_pro(x$identifier)

    new_object(
      Pro(),
      title       = x$title,
      description = x$description,
      identifier  = x$identifier,
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

#' API Endpoint Current Object (Provider)
#'
#' @inheritParams Current
#'
#' @param issued   `<chr>` date issued
#' @param released `<chr>` date released
#' @param uuid     `<chr>` uuid
#' @param site     `<chr>` endpoint landing site
#'
#' @returns An S7 `<CurrentProvider>` object.
#'
#' @autoglobal
#' @export
CurrentProvider <- new_class(
  parent = Current,
  name = "CurrentProvider",
  properties = list(
    issued     = class_character | class_Date,
    released   = class_character | class_Date,
    uuid       = class_character,
    identifier = new_property(class_character, getter = function(self) prov_uuid_url(self@uuid)),
    site       = class_character
  )
)

#' Load `<CurrentProvider>` API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns `<CurrentProvider>` object
#'
#' @examples
#' provider_current("affiliations")
#' provider_current("clinicians")
#' provider_current("utilization")
#' @autoglobal
#'
#' @export
provider_current <- function(alias) {

  x <- catalog_provider()$doctors_and_clinicians |>
    subset_detect(
      title,
      alias_provider_current(alias)) |>
    c()

  q <- prov_nrows_fields(x$identifier)

  CurrentProvider(
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

#' @include props.R
NULL

#' Identifier Class
#' @param url `<chr>` identifier url
#' @returns `<S7_class>` Identifier object
#' @family classes
#' @autoglobal
#' @export
Identifier <- new_class(
  name       = "Identifier",
  package    = "provider",
  properties = list(
    url      = class_character,
    rows     = new_property(
      class = class_integer,
      getter = function(self)
        get_nrows(self@url)
    ),
    fields   = new_property(
      class = class_character,
      getter = function(self)
        get_fields(self@url)
    )
  ),
  validator  = function(self)
    if (length(self@url) != 1L)
      "must be length 1"
)

#' Contact Class
#' @param name `<chr>` Contact name
#' @param email `<chr>` Contact email
#' @returns `<S7_class>` Contact object
#' @family classes
#' @autoglobal
#' @export
Contact <- new_class(
  name       = "Contact",
  package    = "provider",
  properties = list(
    name     = class_character,
    email    = class_character),
  validator  = function(self) {
    if (length(self@name)  != 1L) "must be length 1"
    if (length(self@email) != 1L) "must be length 1"
  })

#' Publisher Class
#' @param type `<chr>` Publisher type; default is `org:Organization`
#' @param name `<chr>` Publisher name; default is `Centers for Medicare & Medicaid Services`
#' @returns `<S7_class>` Publisher object
#' @family classes
#' @autoglobal
#' @export
Publisher <- new_class(
  name    = "Publisher",
  package = "provider",
  properties = list(
    type = new_property(class_character, default = "org:Organization"),
    name = new_property(class_character, default = "Centers for Medicare & Medicaid Services")
  ),
  validator = function(self) {
    if (length(self@type) != 1L)
      "must be length 1"
    if (length(self@name) != 1L)
      "must be length 1"
  }
)

#' Resources Class
#' @param url `<chr>` `resourcesAPI` url; default is `NA`
#' @returns `<S7_class>` Resources object
#' @family classes
#' @autoglobal
#' @export
Resources <- new_class(
  name       = "Resources",
  package    = "provider",
  properties = list(
    url      = new_property(class_character, default = NA_character_),
    files    = new_property(class_character | class_list,
                            getter = function(self) get_resources(self@url), default = NA_character_)),
  validator  = function(self) if (length(self@url) != 1L) "must be length 1"
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
        fload(self@url, query = "/data") |>
          fcompute(
            file = name,
            size = roundup(fileSize / 1e6),
            ext = file_ext(downloadURL),
            download = downloadURL
          ) |>
          roworder(ext, -size)
      }
    )
  ),
  validator = function(self)
    if (length(self@url) != 1L)
      "must be length 1"
)

#' Dataset Class
#' @name Dataset
#' @param type `<chr>` Schema type; default is `dcat:Dataset`
#' @param access `<chr>` Dataset access level; default is `public`
#' @param bureau `<chr>` Dataset bureau code; default is `009:38`
#' @param program `<chr>` Dataset program code; default is `009:000`
#' @param contact `<S7_class>` Dataset contact
#' @param identifier `<S7_class>` dcat:Dataset url and nrows in dataset
#' @param publisher `<S7_class>` Dataset publisher
#' @param resources `<S7_class>` `data.frame` of available supplemental resource files; default is `NA`
#' @param modified `<dbl> | <Date>` Date Dataset was last modified
#' @param title `<chr>` Dataset title
#' @param periodicity `<chr>` Dataset update frequency
#' @param dictionary `<chr>` Hyperlink to Data dictionary
#' @param description `<chr>` Dataset description
#' @param keyword `<chr>` Hyperlink to API landing page
#' @param landingpage `<chr>` Hyperlink to API landing page
#' @param references `<chr>` Dataset references
#' @param temporal `<chr>` Date range the Current dataset covers
#' @param theme `<chr>` Dataset theme
#' @returns `<S7_class>` Dataset object
#' @family classes
#' @autoglobal
#' @export
Dataset <- new_class(
  name    = "Dataset",
  package = "provider",
  properties = list(
    # type        = new_property(class_character, default = "dcat:Dataset"),
    # access      = new_property(class_character, default = "public"),
    # bureau      = new_property(class_character, default = "009:38"),
    # program     = new_property(class_character, default = "009:000"),
    contact     = class_character,
    identifier  = Identifier,
    # publisher   = Publisher,
    resources   = Resources,
    modified    = class_Date,
    title       = class_character,
    periodicity = class_character,
    dictionary  = class_character,
    description = class_character,
    # keyword     = class_character,
    site = class_character,
    references  = class_character,
    temporal    = class_character,
    theme       = class_character
  )
)

#' Distribution Class
#' @name Distribution
#' @param type `<chr>` Schema type; default is `dcat:Distribution`
#' @param access `<chr>` Dataset access level; default is `public`
#' @param bureau `<chr>` Dataset bureau code; default is `009:38`
#' @param program `<chr>` Dataset program code; default is `009:000`
#' @param contact `<S7_class>` Dataset contact
#' @param identifier `<S7_class>` dcat:Dataset url and nrows in dataset
#' @param publisher `<S7_class>` Dataset publisher
#' @param resources `<S7_class>` `data.frame` of available supplemental resource files; default is `NA`
#' @param distributions `<S7_class>` `data.frame` of available distributions
#' @param modified `<dbl> | <Date>` Date Dataset was last modified
#' @param title `<chr>` Dataset title
#' @param periodicity `<chr>` Dataset update frequency
#' @param dictionary `<chr>` Hyperlink to Data dictionary
#' @param description `<chr>` Dataset description
#' @param keyword `<chr>` Hyperlink to API landing page
#' @param landingpage `<chr>` Hyperlink to API landing page
#' @param references `<chr>` Dataset references
#' @param temporal `<chr>` Date range the Current dataset covers
#' @param theme `<chr>` Dataset theme
#' @returns `<S7_class>` Distribution object
#' @autoglobal
#' @export
Distribution <- new_class(
  name           = "Distribution",
  parent         = Dataset,
  package        = "provider",
  properties     = list(
    type = new_property(class_character, default = "dcat:Distribution"),
    distributions = class_list
  )
)
