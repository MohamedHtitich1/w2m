#' @title Panel Data Imputation using GAM Smoothing
#'
#' @description
#' Imputes missing values in panel data using Generalized Additive Models (GAM)
#' with cubic regression splines. Fits a smooth curve within each group (e.g., country)
#' and uses it to predict missing values.
#'
#' @param data Data frame in long format with unit, time, and value columns
#' @param value_col Name of the column containing values to impute
#' @param unit_col Name of the grouping column (e.g., "country")
#' @param time_col Name of the time column (e.g., "year")
#' @param min_obs Minimum number of non-NA observations required per unit (default 3)
#'
#' @return Data frame with imputed values and additional columns:
#' \describe{
#'   \item{pred}{Predicted/imputed values}
#'   \item{imputed}{Logical indicating whether the value was imputed}
#' }
#'
#' @details
#' This function requires the 'mgcv' package. It will only impute values for
#' units that have at least \code{min_obs} non-missing observations to ensure
#' reliable curve fitting.
#'
#' @examples
#' \dontrun{
#' # Create panel data with missing values
#' panel <- data.frame(
#'   country = rep(c("A", "B"), each = 10),
#'   year = rep(2010:2019, 2),
#'   gdp = c(100, 105, NA, 115, 120, NA, 130, 135, 140, 145,
#'           50, NA, 55, 60, NA, 70, 75, NA, 85, 90)
#' )
#'
#' imputed <- gam_impute(panel, "gdp", "country", "year")
#' }
#'
#' @export
gam_impute <- function(data, value_col, unit_col = "country",
                       time_col = "year", min_obs = 3) {

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for gam_impute(). Please install it.")
  }

  # Input validation
  required_cols <- c(unit_col, time_col, value_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Helper function to fit GAM and predict
  fit_and_predict <- function(df) {
    n_obs <- sum(!is.na(df[[value_col]]))

    if (n_obs < min_obs) {
      df$pred <- df[[value_col]]
      df$imputed <- is.na(df[[value_col]])
      return(df)
    }

    # Determine k for spline (can't exceed number of observations)
    k <- min(n_obs, 10)

    tryCatch({
      # Fit GAM with cubic regression spline
      formula_str <- paste0(value_col, " ~ s(", time_col, ", bs = 'cr', k = ", k, ")")
      model <- mgcv::gam(stats::as.formula(formula_str), data = df)

      # Predict for all rows
      df$pred <- stats::predict(model, newdata = df, type = "response")
      df$imputed <- is.na(df[[value_col]])

      # Use original values where available
      df$pred[!df$imputed] <- df[[value_col]][!df$imputed]

    }, error = function(e) {
      # If GAM fails, return original data
      df$pred <- df[[value_col]]
      df$imputed <- is.na(df[[value_col]])
    })

    df
  }

  # Apply to each unit
  result <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(unit_col))) %>%
    dplyr::group_modify(~ fit_and_predict(.x)) %>%
    dplyr::ungroup()

  return(result)
}


#' @title Linear Interpolation with Constant Endpoint Extension
#'
#' @description
#' Performs linear interpolation for missing values, with constant value
#' extension at the endpoints (LOCF/NOCB style).
#'
#' @param y Numeric vector with potential NA values
#'
#' @return List with:
#' \describe{
#'   \item{value}{Interpolated/extended values}
#'   \item{flag}{Integer vector: 1 if the value was imputed, 0 if original}
#' }
#'
#' @examples
#' result <- interp_const_ends(c(NA, 1, NA, NA, 5, NA))
#' result$value
#' # Returns interpolated values with constant extension at ends
#'
#' @export
interp_const_ends <- function(y) {
  n <- length(y)
  idx <- which(!is.na(y))
  was_na <- is.na(y)

  if (length(idx) == 0) {
    return(list(value = y, flag = as.integer(was_na)))
  }

  if (length(idx) == 1) {
    return(list(value = rep(y[idx], n), flag = as.integer(was_na)))
  }

  # Linear interpolation with constant extension at endpoints
  v <- stats::approx(
    x = idx,
    y = y[idx],
    xout = seq_len(n),
    method = "linear",
    rule = 2  # Constant extension beyond range
  )$y

  list(value = v, flag = as.integer(was_na))
}


#' @title Last Observation Carried Forward
#'
#' @description
#' Fills NA values by carrying forward the last non-NA observation.
#' Alternative name for \code{\link{repeat_before}}.
#'
#' @param x Vector with potential NA values
#'
#' @return Vector with NAs filled forward
#'
#' @examples
#' locf(c(1, NA, NA, 4, NA, 6))
#' # Returns: 1, 1, 1, 4, 4, 6
#'
#' @export
locf <- repeat_before


#' @title Next Observation Carried Backward
#'
#' @description
#' Fills NA values by carrying backward the next non-NA observation.
#'
#' @param x Vector with potential NA values
#'
#' @return Vector with NAs filled backward
#'
#' @examples
#' nocb(c(1, NA, NA, 4, NA, 6))
#' # Returns: 1, 4, 4, 4, 6, 6
#'
#' @export
nocb <- function(x) {
  rev(repeat_before(rev(x)))
}
