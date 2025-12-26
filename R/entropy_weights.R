#' @title Static Entropy Weights (Single Time Point)
#'
#' @description
#' Calculates entropy-based weights for indicators at a single point in time.
#' For temporal analysis, use \code{\link{modified_entropy}} instead.
#'
#' @details
#' The entropy weighting method assigns higher weights to indicators that
#' have greater discriminating power (more spread/diversity across units).
#' An indicator where all units have similar values provides little information
#' for differentiation and receives lower weight.
#'
#' @param data Data frame or matrix with units as rows and indicators as columns.
#'   All columns should be numeric.
#' @param neg_vars Character vector of variable names with negative polarity
#'   (higher values are worse). These will be inverted before weighting.
#' @param pos_vars Character vector of variable names with positive polarity
#'   (higher values are better). Default treatment if not specified.
#' @param normalize Logical, whether to normalize indicators to 0-1 range first.
#'   Default is TRUE.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{indicator}{Name of the indicator}
#'   \item{entropy}{The entropy value (0-1, lower = more concentrated)}
#'   \item{diversity}{Degree of diversification (1 - entropy)}
#'   \item{weight}{Normalized weight (sums to 1)}
#'   \item{polarity}{The polarity used for this indicator}
#' }
#'
#' @examples
#' # Create example data
#' indicators <- data.frame(
#'   gdp = c(10000, 15000, 20000, 25000, 30000),
#'   education = c(80, 85, 75, 90, 88),
#'   mortality = c(5, 8, 12, 3, 6)  # Negative polarity
#' )
#'
#' # Calculate weights
#' weights <- entropy_weights(
#'   data = indicators,
#'   neg_vars = "mortality",
#'   pos_vars = c("gdp", "education")
#' )
#'
#' print(weights)
#'
#' @seealso \code{\link{modified_entropy}} for temporal entropy analysis,
#'   \code{\link{calculate_composite}} for creating weighted composite indices
#'
#' @export
entropy_weights <- function(data, neg_vars = NULL, pos_vars = NULL, normalize = TRUE) {

  # Input validation
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }

  data <- as.data.frame(data)

  # Ensure all columns are numeric
  non_numeric <- !sapply(data, is.numeric)
  if (any(non_numeric)) {
    warning("Non-numeric columns removed: ", paste(names(data)[non_numeric], collapse = ", "))
    data <- data[, !non_numeric, drop = FALSE]
  }

  n_units <- nrow(data)
  n_indicators <- ncol(data)
  indicator_names <- names(data)

  # Determine polarity for each variable
  polarity <- rep("positive", n_indicators)
  names(polarity) <- indicator_names

  if (!is.null(neg_vars)) {
    if (!all(neg_vars %in% indicator_names)) {
      stop("Some neg_vars not found in data: ",
           paste(setdiff(neg_vars, indicator_names), collapse = ", "))
    }
    polarity[neg_vars] <- "negative"
  }

  # Handle polarity - invert negative indicators
  data_adj <- data
  for (var in names(polarity)[polarity == "negative"]) {
    data_adj[[var]] <- max(data[[var]], na.rm = TRUE) - data[[var]]
  }

  # Normalize to 0-1 range if requested
  if (normalize) {
    data_adj <- as.data.frame(lapply(data_adj, function(x) {
      rng <- range(x, na.rm = TRUE)
      if (rng[1] == rng[2]) return(rep(0.5, length(x)))
      (x - rng[1]) / (rng[2] - rng[1])
    }))
  }

  # Calculate proportions (adding small constant to avoid log(0))
  p_ij <- apply(data_adj, 2, function(x) {
    x <- x + 1e-10
    x / sum(x, na.rm = TRUE)
  })

  # Calculate entropy for each indicator
  k <- 1 / log(n_units)

  entropy_vals <- apply(p_ij, 2, function(p) {
    p <- p[!is.na(p) & p > 0]
    -k * sum(p * log(p))
  })

  # Degree of diversification
  diversity <- 1 - entropy_vals

  # Normalize to weights
  weights <- diversity / sum(diversity)

  # Create result data frame
  result <- data.frame(
    indicator = indicator_names,
    entropy = entropy_vals,
    diversity = diversity,
    weight = weights,
    polarity = polarity,
    row.names = NULL
  )

  # Sort by weight descending
  result <- result[order(-result$weight), ]

  class(result) <- c("mwmm_weights", "data.frame")
  return(result)
}


#' @title Calculate Weighted Composite Index
#'
#' @description
#' Applies entropy weights (or custom weights) to create a composite index.
#'
#' @param data Data frame with indicator columns
#' @param weights Either a data frame from \code{entropy_weights()}, a named vector
#'   of weights, or NULL to use equal weights
#' @param indicators Character vector of indicator column names to use.
#'   If NULL, uses all numeric columns.
#' @param normalize_first Logical, whether to normalize indicators to 0-100 before
#'   aggregating. Default is TRUE.
#' @param aggregation Method of aggregation: "arithmetic" (default), "geometric",
#'   or "harmonic"
#'
#' @return A numeric vector of composite index values, one per row
#'
#' @examples
#' # Create example data
#' data <- data.frame(
#'   country = c("A", "B", "C", "D", "E"),
#'   health = c(80, 75, 90, 85, 70),
#'   education = c(70, 80, 75, 90, 85),
#'   environment = c(60, 65, 70, 55, 75)
#' )
#'
#' # Calculate weights
#' weights <- entropy_weights(data[, 2:4])
#'
#' # Calculate composite index
#' data$composite <- calculate_composite(
#'   data = data[, 2:4],
#'   weights = weights,
#'   aggregation = "arithmetic"
#' )
#'
#' print(data)
#'
#' @export
calculate_composite <- function(data, weights = NULL, indicators = NULL,
                                normalize_first = TRUE, aggregation = "arithmetic") {

  # Select indicators
  if (is.null(indicators)) {
    data_subset <- data[, sapply(data, is.numeric), drop = FALSE]
  } else {
    data_subset <- data[, indicators, drop = FALSE]
  }

  n_indicators <- ncol(data_subset)

  # Process weights
  if (is.null(weights)) {
    # Equal weights
    w <- rep(1 / n_indicators, n_indicators)
    names(w) <- names(data_subset)
  } else if (inherits(weights, "mwmm_weights")) {
    # Extract weights from entropy_weights result
    w <- weights$weight
    names(w) <- weights$indicator
    # Reorder to match data
    w <- w[names(data_subset)]
  } else if (is.numeric(weights)) {
    if (is.null(names(weights))) {
      if (length(weights) != n_indicators) {
        stop("Length of weights must match number of indicators")
      }
      names(weights) <- names(data_subset)
    }
    w <- weights[names(data_subset)]
  } else {
    stop("'weights' must be NULL, a data frame from entropy_weights(), or a named numeric vector")
  }

  # Check for missing weights
  if (any(is.na(w))) {
    stop("Missing weights for some indicators")
  }

  # Normalize weights to sum to 1
  w <- w / sum(w)

  # Normalize indicators if requested
  if (normalize_first) {
    data_subset <- as.data.frame(lapply(data_subset, function(x) {
      rng <- range(x, na.rm = TRUE)
      if (rng[1] == rng[2]) return(rep(50, length(x)))
      (x - rng[1]) / (rng[2] - rng[1]) * 100
    }))
  }

  # Aggregate based on method
  composite <- switch(aggregation,
    "arithmetic" = {
      rowSums(sweep(as.matrix(data_subset), 2, w, "*"), na.rm = TRUE)
    },
    "geometric" = {
      # Geometric mean (requires positive values)
      data_positive <- data_subset + 1  # Shift to ensure positive
      exp(rowSums(sweep(log(as.matrix(data_positive)), 2, w, "*"), na.rm = TRUE)) - 1
    },
    "harmonic" = {
      # Harmonic mean
      1 / rowSums(sweep(1 / as.matrix(data_subset + 1), 2, w, "*"), na.rm = TRUE) - 1
    },
    stop("Unknown aggregation method. Use 'arithmetic', 'geometric', or 'harmonic'")
  )

  return(composite)
}
