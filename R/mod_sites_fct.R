#' Create new site row with defaults
#'
#' @param site_number Integer. Site number for generating the site code. Default is 1.
#' @param base_code Character. Base code prefix for the site code. If empty or NULL,
#'   defaults to "SITE_". Default is "".
#' @param username Character. Username of the person entering the site data.
#'
#' @return A tibble with one row containing the new site with default values.
#'
#' @examples
#' \dontrun{
#' create_new_site(site_number = 1, base_code = "LAKE_", username = "jdoe")
#' create_new_site(site_number = 42, username = "jsmith")
#' }
#'
#' @export
create_new_site <- function(site_number = 1, base_code = "", username) {
  # Generate site code
  if (base_code == "" || is.null(base_code)) {
    site_code <- paste0("SITE_", sprintf("%03d", site_number))
  } else {
    site_code <- paste0(base_code, sprintf("%03d", site_number))
  }

  initialise_sites_tibble() |>
    add_row(
      SITE_CODE = site_code,
      SITE_NAME = "",
      SITE_GEOGRAPHIC_FEATURE = "",
      SITE_GEOGRAPHIC_FEATURE_SUB = "",
      SITE_COORDINATE_SYSTEM = "WGS 84",
      LATITUDE = NA,
      LONGITUDE = NA,
      COUNTRY_ISO = "",
      OCEAN_IHO = "",
      ALTITUDE_VALUE = NA,
      ALTITUDE_UNIT = "m",
      ENTERED_BY = username %|truthy|% "",
      ENTERED_DATE = as.character(Sys.Date()),
      SITE_COMMENT = ""
    )
}
