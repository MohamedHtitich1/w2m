#' @title Repeat Last Non-NA Value Forward
#'
#' @description
#' Fills NA values by carrying forward the last observed value.
#' Useful for panel data imputation where carrying forward is appropriate.
#'
#' @param x Vector (numeric, character, or factor)
#'
#' @return Vector with NA values filled forward
#'
#' @examples
#' repeat_before(c(NA, 1, NA, 3, NA, NA, 5))
#' # Returns: NA, 1, 1, 3, 3, 3, 5
#'
#' @export
repeat_before <- function(x) {
  ind <- which(!is.na(x))
  if (length(ind) == 0) return(x)
  if (is.na(x[1])) ind <- c(1, ind)
  rep(x[ind], times = diff(c(ind, length(x) + 1)))
}


#' @title Rescale Vector to 0-100 Range
#'
#' @description
#' Performs min-max normalization to scale values to a 0-100 range.
#' This is the standard normalization used in the Social Progress Index.
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values when calculating range (default TRUE)
#'
#' @return Numeric vector scaled to 0-100
#'
#' @examples
#' rescale_0_100(c(10, 20, 30, 40, 50))
#' # Returns: 0, 25, 50, 75, 100
#'
#' @export
rescale_0_100 <- function(x, na.rm = TRUE) {
  r <- range(x, na.rm = na.rm)
  if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) {
    return(rep(50, length(x)))  # Return midpoint if no variance
  }
  (x - r[1]) / (r[2] - r[1]) * 100
}


#' @title Rescale with Custom Bounds (SPI-style)
#'
#' @description
#' Rescales values to 0-100 using specified dystopia and utopia reference points,
#' following the Social Progress Index methodology.
#'
#' @param x Numeric vector
#' @param dystopia Lower bound (worst value, maps to 0)
#' @param utopia Upper bound (best value, maps to 100)
#' @param invert Logical, if TRUE treats lower values as better (default FALSE)
#'
#' @return Numeric vector scaled to approximately 0-100 (may exceed bounds)
#'
#' @examples
#' # Life expectancy: dystopia = 40 years, utopia = 90 years
#' rescale_spi(c(50, 60, 70, 80), dystopia = 40, utopia = 90)
#'
#' # Mortality rate: lower is better
#' rescale_spi(c(5, 10, 15, 20), dystopia = 25, utopia = 0, invert = TRUE)
#'
#' @export
rescale_spi <- function(x, dystopia, utopia, invert = FALSE) {
  if (invert) {
    (dystopia - x) / (dystopia - utopia) * 100
  } else {
    (x - dystopia) / (utopia - dystopia) * 100
  }
}


#' @title Safe Logarithm
#'
#' @description
#' Computes logarithm that handles non-positive values gracefully by returning NA.
#'
#' @param x Numeric vector
#' @param base Logarithm base (default 10)
#'
#' @return Numeric vector of log values, with NA for non-positive inputs
#'
#' @examples
#' safe_log(c(-1, 0, 1, 10, 100))
#' # Returns: NA, NA, 0, 1, 2
#'
#' @export
safe_log <- function(x, base = 10) {
  ifelse(is.na(x) | x <= 0, NA_real_, log(x, base = base))
}


#' @title Geometric Mean
#'
#' @description
#' Computes the geometric mean of a numeric vector.
#' Used in composite indices like HDI and SPI components.
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values (default TRUE)
#'
#' @return Geometric mean value
#'
#' @examples
#' geometric_mean(c(1, 2, 4, 8))
#' # Returns: 2.828...
#'
#' # Compare to arithmetic mean
#' mean(c(1, 2, 4, 8))
#' # Returns: 3.75
#'
#' @export
geometric_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0 || any(x <= 0)) return(NA_real_)
  exp(mean(log(x)))
}


#' @title Harmonic Mean
#'
#' @description
#' Computes the harmonic mean of a numeric vector.
#' Useful when averaging rates or ratios.
#'
#' @param x Numeric vector (must be positive)
#' @param na.rm Logical, remove NA values (default TRUE)
#'
#' @return Harmonic mean value
#'
#' @examples
#' harmonic_mean(c(1, 2, 4, 8))
#' # Returns: 2.133...
#'
#' @export
harmonic_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0 || any(x <= 0)) return(NA_real_)
  length(x) / sum(1/x)
}


#' @title Filter EU27 Countries from NUTS Data
#'
#' @description
#' Filters a data frame to keep only EU27 countries (plus optional extras)
#' based on NUTS geo codes. Useful for Eurostat data processing.
#'
#' @param df Data frame with 'geo' column containing NUTS codes
#' @param extra Additional country codes to include (default: c("NO", "IS"))
#' @param geo_col Name of the geo column (default "geo")
#'
#' @return Filtered data frame
#'
#' @examples
#' \dontrun{
#' eurostat_data %>% keep_eu27()
#' eurostat_data %>% keep_eu27(extra = c("NO", "IS", "CH"))
#' }
#'
#' @export
keep_eu27 <- function(df, extra = c("NO", "IS"), geo_col = "geo") {
  eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL",
            "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
            "SI","ES","SE")
  keep <- union(eu27, extra)

  df %>%
    dplyr::mutate(
      .ctry = substr(.data[[geo_col]], 1, 2),
      .ctry = ifelse(.data$.ctry == "GR", "EL", .data$.ctry)
    ) %>%
    dplyr::filter(.data$.ctry %in% keep) %>%
    dplyr::select(-".ctry")
}


#' @title Coefficient of Variation
#'
#' @description
#' Calculates the coefficient of variation (CV) as a measure of relative dispersion.
#' Useful for comparing variability across indicators with different scales.
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values (default TRUE)
#'
#' @return Coefficient of variation (SD / mean)
#'
#' @examples
#' cv(c(10, 20, 30, 40, 50))
#'
#' @export
cv <- function(x, na.rm = TRUE) {
  stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}


#' @title Winsorize Extreme Values
#'
#' @description
#' Limits extreme values to specified percentiles, reducing the influence
#' of outliers while preserving rank order.
#'
#' @param x Numeric vector
#' @param probs Two-element vector of lower and upper percentile bounds.
#'   Default is c(0.01, 0.99).
#'
#' @return Numeric vector with extreme values capped
#'
#' @examples
#' set.seed(42)
#' x <- c(rnorm(98), -10, 10)  # Normal data with outliers
#' winsorize(x)
#'
#' @export
winsorize <- function(x, probs = c(0.01, 0.99)) {
  limits <- stats::quantile(x, probs = probs, na.rm = TRUE)
  x[x < limits[1]] <- limits[1]
  x[x > limits[2]] <- limits[2]
  x
}
