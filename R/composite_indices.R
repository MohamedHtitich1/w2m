#' @title Calculate Carbon Efficiency of Social Progress (CESP)
#'
#' @description
#' Computes the CESP score measuring how efficiently countries convert
#' CO2 emissions into social progress. Higher CESP indicates that a country
#' achieves more social progress per unit of carbon emitted.
#'
#' @details
#' The CESP methodology:
#' 1. Adjusts CO2 using an equalization constant to ensure comparability with SPI
#' 2. Calculates efficiency ratio: adjusted_CO2 / SPI
#' 3. Normalizes to 0-100 scale using utopia/dystopia reference points
#'
#' Reference points based on global analysis:
#' - Utopia (best): 0.1758 (lowest CO2/SPI ratio)
#' - Dystopia (worst): 1.5 (highest CO2/SPI ratio)
#'
#' @param data Data frame with SPI scores and CO2 data
#' @param spi_col Name of SPI score column
#' @param co2_col Name of CO2 per capita column
#' @param year_col Name of year column (optional, for panel data)
#' @param utopia Best possible ratio (default 0.1758)
#' @param dystopia Worst possible ratio (default 1.5)
#'
#' @return Data frame with added columns:
#' \describe{
#'   \item{adj_co2}{CO2 adjusted by equalization constant}
#'   \item{cesp_raw}{Raw efficiency ratio}
#'   \item{score_cesp}{CESP score on 0-100 scale}
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   country = c("Norway", "USA", "Chad"),
#'   score_spi = c(90, 85, 45),
#'   co2_percap = c(8, 15, 0.05)
#' )
#'
#' result <- calculate_cesp(data, "score_spi", "co2_percap")
#' }
#'
#' @references
#' Htitich, M. (2024). Carbon Efficiency of Social Progress.
#' \emph{Geography and Sustainability} (under revision).
#'
#' @export
calculate_cesp <- function(data, spi_col = "score_spi", co2_col = "co2_percap",
                           year_col = NULL, utopia = 0.1758, dystopia = 1.5) {

  # Input validation
  if (!spi_col %in% names(data)) {
    stop("Column '", spi_col, "' not found in data")
  }
  if (!co2_col %in% names(data)) {
    stop("Column '", co2_col, "' not found in data")
  }

  # Calculate equalization constant
  # This ensures CO2 and SPI are on comparable scales
  stats_df <- data %>%
    dplyr::summarise(
      spi_mean = mean(.data[[spi_col]], na.rm = TRUE),
      spi_sd = stats::sd(.data[[spi_col]], na.rm = TRUE),
      co2_mean = mean(.data[[co2_col]], na.rm = TRUE),
      co2_sd = stats::sd(.data[[co2_col]], na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      const_cesp = ((.data$co2_sd * .data$spi_mean) / .data$spi_sd) - .data$co2_mean
    )

  # Calculate CESP
  result <- data %>%
    dplyr::mutate(
      adj_co2 = .data[[co2_col]] + stats_df$const_cesp,
      cesp_raw = .data$adj_co2 / .data[[spi_col]],
      score_cesp = (dystopia - .data$cesp_raw) / (dystopia - utopia) * 100,
      score_cesp = pmax(0, pmin(100, .data$score_cesp))
    )

  return(result)
}


#' @title Calculate Material Footprint Efficiency (MFSP)
#'
#' @description
#' Computes efficiency of social progress relative to material footprint.
#' Similar methodology to CESP but using material consumption instead of CO2.
#'
#' @param data Data frame with SPI scores and material footprint data
#' @param spi_col Name of SPI score column
#' @param matfoot_col Name of material footprint per capita column
#'
#' @return Data frame with MFSP scores added
#'
#' @export
calculate_mfsp <- function(data, spi_col = "score_spi",
                           matfoot_col = "matfoot_percap") {

  stats_df <- data %>%
    dplyr::summarise(
      spi_mean = mean(.data[[spi_col]], na.rm = TRUE),
      spi_sd = stats::sd(.data[[spi_col]], na.rm = TRUE),
      mf_mean = mean(.data[[matfoot_col]], na.rm = TRUE),
      mf_sd = stats::sd(.data[[matfoot_col]], na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      const_mfsp = ((.data$mf_sd * .data$spi_mean) / .data$spi_sd) - .data$mf_mean
    )

  data %>%
    dplyr::mutate(
      adj_mf = .data[[matfoot_col]] + stats_df$const_mfsp,
      mfsp_raw = .data$adj_mf / .data[[spi_col]],
      score_mfsp = rescale_0_100(-.data$mfsp_raw)
    )
}


#' @title Calculate Just Transition Score (JTS)
#'
#' @description
#' Computes the Just Transition Score combining both carbon efficiency (CESP)
#' and material footprint efficiency (MFSP) into a single metric measuring
#' overall environmental efficiency of social progress.
#'
#' @param data Data frame with SPI, CO2, and material footprint data
#' @param spi_col Name of SPI score column
#' @param co2_col Name of CO2 per capita column
#' @param matfoot_col Name of material footprint per capita column
#'
#' @return Data frame with CESP, MFSP, and JTS scores
#'
#' @examples
#' \dontrun{
#' jts <- calculate_jts(
#'   data = country_data,
#'   spi_col = "score_spi",
#'   co2_col = "co2_percap",
#'   matfoot_col = "material_footprint"
#' )
#' }
#'
#' @export
calculate_jts <- function(data, spi_col = "score_spi",
                          co2_col = "co2_percap",
                          matfoot_col = "matfoot_percap") {

  # Calculate CESP
  data <- calculate_cesp(data, spi_col = spi_col, co2_col = co2_col)

  # Calculate MFSP
  data <- calculate_mfsp(data, spi_col = spi_col, matfoot_col = matfoot_col)

  # Combine into JTS (simple average)
  data %>%
    dplyr::mutate(
      score_jts = (.data$score_cesp + .data$score_mfsp) / 2
    )
}


#' @title Calculate Mazziotta-Pareto Index (MPI)
#'
#' @description
#' Computes the MPI which penalizes imbalance across indicators.
#' Countries with uneven development across dimensions receive lower scores.
#'
#' @param data Data frame with indicators
#' @param indic_cols Column indices or names of indicators
#' @param polarity Vector of "POS" or "NEG" for each indicator
#' @param method MPI variant: "original" (default), "adjusted"
#'
#' @return Numeric vector of MPI scores
#'
#' @details
#' The MPI formula:
#' MPI = M - S * cv
#'
#' Where:
#' - M is the arithmetic mean of normalized indicators
#' - S is the sign (+ for positive penalty, - for negative)
#' - cv is the coefficient of variation
#'
#' @export
calculate_mpi <- function(data, indic_cols, polarity, method = "original") {

  # Get indicator data
  if (is.numeric(indic_cols)) {
    indic_data <- data[, indic_cols]
  } else {
    indic_data <- data[, indic_cols]
  }

  n_indic <- ncol(indic_data)

  if (length(polarity) != n_indic) {
    stop("Length of 'polarity' must match number of indicators")
  }

  # Normalize to 0-100
  indic_norm <- as.data.frame(lapply(seq_len(n_indic), function(i) {
    x <- indic_data[[i]]
    if (toupper(polarity[i]) == "NEG") {
      # Invert negative polarity
      x <- max(x, na.rm = TRUE) - x
    }
    rescale_0_100(x)
  }))

  # Standardize (mean = 100, SD = 10)
  indic_std <- as.data.frame(lapply(indic_norm, function(x) {
    (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE) * 10 + 100
  }))

  # Calculate MPI
  M <- rowMeans(indic_std, na.rm = TRUE)
  S <- apply(indic_std, 1, stats::sd, na.rm = TRUE)
  cv <- S / M

  # Apply penalty (positive penalty means imbalance reduces score)
  mpi <- M - S * cv

  return(mpi)
}
