#' Get parameter names for a specific parameter type
#'
#' @param param_type Character string specifying the parameter type
#' @param dummy_parameters Dataframe containing base parameters
#' @param session_parameters Optional list containing session-specific parameters
#'
#' @return Character vector of parameter names
#' @importFrom dplyr filter pull
#' @export
get_parameters_of_types <- function(param_type, dummy_parameters, session_parameters = NULL) {
  # Get base parameters from dummy_parameters dataframe
  base_params <- dummy_parameters |>
    filter(PARAMETER_TYPE == param_type) |>
    pull(PARAMETER_NAME)

  # Get session parameters if provided
  session_params <- if (!is.null(session_parameters)) {
    names(session_parameters[[param_type]] %||% list())
  } else {
    character(0)
  }

  all_params <- c("-- New Parameter --", base_params, session_params)
  return(all_params)
}

#' Create parameter row from existing parameter data
#'
#' @param param_type Character string specifying the parameter type
#' @param param_name Character string specifying the parameter name
#' @param dummy_parameters Dataframe containing base parameters
#' @param session_parameters Optional dataframe containing session-specific parameters
#'
#' @return Data.frame with parameter information or NULL if not found
#' @importFrom dplyr filter slice
#' @export
create_existing_parameter <- function(param_type, param_name, dummy_parameters, session_parameters = NULL) {
  # Check dummy parameters first
  param_row <- dummy_parameters |>
    filter(PARAMETER_NAME == param_name)

  # If not found in dummy_parameters, check session_parameters
  if (nrow(param_row) == 0 && !is.null(session_parameters)) {
    param_row <- session_parameters |>
      filter(PARAMETER_TYPE == param_type, PARAMETER_NAME == param_name)
  }

  # Return NULL if parameter not found
  if (nrow(param_row) == 0) {
    return(NULL)
  }

  # Extract first matching row and ensure all required columns exist
  param_data <- param_row |> slice(1)

  data.frame(
    PARAMETER_TYPE = param_type,
    PARAMETER_TYPE_SUB = param_data$PARAMETER_TYPE_SUB %||% "",
    MEASURED_TYPE = param_data$MEASURED_TYPE %||% "Concentration",
    PARAMETER_NAME = param_data$PARAMETER_NAME %||% param_name,
    PARAMETER_NAME_SUB = param_data$PARAMETER_NAME_SUB %||% "",
    INCHIKEY_SD = param_data$INCHIKEY_SD %||% "",
    PUBCHEM_CID = param_data$PUBCHEM_CID %||% "",
    CAS_RN = param_data$CAS_RN %||% "",
    stringsAsFactors = FALSE
  )
}

#' Create new blank parameter row
#'
#' @param param_type Character string specifying the parameter type
#'
#' @return Data.frame with blank parameter template
#' @export
create_new_parameter <- function(param_type) {
  data.frame(
    PARAMETER_TYPE = param_type,
    PARAMETER_TYPE_SUB = "",
    MEASURED_TYPE = "Concentration",
    PARAMETER_NAME = "",
    PARAMETER_NAME_SUB = "",
    INCHIKEY_SD = "",
    PUBCHEM_CID = "",
    CAS_RN = "",
    stringsAsFactors = FALSE
  )
}
