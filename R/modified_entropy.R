#' @title Modified Entropy Weighting with Temporal Decay
#'
#' @description
#' The core function of the mwmm package. Calculates entropy-based indicator weights
#' that evolve over time, identifying which indicators "matter most" by measuring
#' how their information diversity (discriminating power) changes across time periods.
#'
#' @details
#' This method extends traditional entropy weighting by incorporating temporal dynamics:
#'
#' **The Entropy-Probability Relationship:**
#' Entropy measures the diversity or uncertainty in indicator values across units.
#' When all countries have similar values on an indicator, entropy is low (the indicator

#' doesn't discriminate well). When values are spread out, entropy is high (the indicator
#' carries more information for differentiating countries).
#'
#' **Temporal Decay Weighting:**
#' The `alpha` parameter controls how much weight is given to recent vs. older time periods.
#' With `alpha < 1`, more recent periods receive higher weight, reflecting the intuition
#' that current discriminating power matters more than historical patterns.
#'
#' **Interpretation:**
#' - Higher entropy weight = indicator has more discriminating power (matters more)
#' - Weights that increase over time = indicator becoming more important
#' - Weights that decrease over time = convergence occurring on that dimension
#'
#' @param data Data frame containing indicator columns (numeric). Should NOT include
#'   ID columns like country names or codes - only the indicators to be weighted.
#' @param time Vector of time period identifiers (e.g., years) corresponding to rows in data.
#'   Must be the same length as `nrow(data)`.
#' @param alpha Decay parameter controlling temporal weighting. Default is 0.95.
#'   - `alpha = 1`: All time periods weighted equally
#'   - `alpha < 1`: Recent periods weighted more heavily (recommended: 0.90-0.99)
#'   - `alpha > 1`: Earlier periods weighted more heavily (rarely used)
#'   - For minimal decay, use `exp(-0.0001)` ≈ 0.9999
#'
#' @return An object of class "mwmm_entropy" containing:
#' \describe{
#'   \item{weights}{Matrix of entropy weights with indicators as rows and time periods as columns}
#'   \item{weights_long}{Tidy data frame with columns: indicator, time, weight}
#'   \item{summary}{Summary statistics for each indicator across all time periods}
#'   \item{alpha}{The decay parameter used}
#'   \item{call}{The function call}
#' }
#'
#' @examples
#' # Create example panel data
#' set.seed(42)
#' n_countries <- 50
#' years <- 2000:2020
#'
#' panel_data <- expand.grid(
#'   country = paste0("Country_", 1:n_countries),
#'   year = years
#' )
#'
#' # Add indicators with different temporal patterns
#' panel_data$health <- rnorm(nrow(panel_data), 70, 10) +
#'   (panel_data$year - 2000) * 0.5  # Gradual improvement
#' panel_data$education <- rnorm(nrow(panel_data), 60, 15)  # Stable variance
#' panel_data$environment <- rnorm(nrow(panel_data), 50, 5) +
#'   (panel_data$year - 2000) * rnorm(nrow(panel_data), 0, 0.5)  # Converging
#'
#' # Calculate modified entropy weights
#' result <- modified_entropy(
#'   data = panel_data[, c("health", "education", "environment")],
#'   time = panel_data$year,
#'   alpha = 0.95
#' )
#'
#' # View weights over time
#' print(result$weights)
#'
#' # Plot the evolution of weights
#' plot(result)
#'
#' @references
#' Htitich, M. (2025). The Evolving Importance of Social and Governance
#' Measures Over Time. \emph{Working Paper}.
#'
#' Shannon, C. E. (1948). A Mathematical Theory of Communication.
#' \emph{Bell System Technical Journal}, 27(3), 379-423.
#'
#' @seealso
#' \code{\link{plot.mwmm_entropy}} for visualization,
#' \code{\link{entropy_weights}} for static (single time point) entropy weighting,
#' \code{\link{calculate_composite}} for applying weights to create composite indices
#'
#' @export
modified_entropy <- function(data, time, alpha = 0.95) {

  # Input validation
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }

  if (length(time) != nrow(data)) {
    stop("Length of 'time' must equal number of rows in 'data'")
  }

  if (!is.numeric(alpha) || length(alpha) != 1) {
    stop("'alpha' must be a single numeric value")
  }

  if (alpha <= 0) {
    stop("'alpha' must be positive")
  }

  # Convert to data frame if matrix

  data <- as.data.frame(data)

  # Ensure all columns are numeric
  non_numeric <- !sapply(data, is.numeric)
  if (any(non_numeric)) {
    warning("Non-numeric columns removed: ", paste(names(data)[non_numeric], collapse = ", "))
    data <- data[, !non_numeric, drop = FALSE]
  }

  if (ncol(data) < 2) {
    stop("'data' must contain at least 2 indicator columns")
  }

  # Get unique time points
  timef <- levels(as.factor(time))
  n_time <- length(timef)
  n_indicators <- ncol(data)

  if (n_time < 2) {
    warning("Only one time period detected. Consider using entropy_weights() for static weighting.")
  }

  # Get number of units per time point (assuming balanced panel)
  n_units <- nrow(data[time == timef[1], ])

  # Initialize matrix to store entropy weights
  entropy_weights <- matrix(0, nrow = n_indicators, ncol = n_time)

  # Create decay weights vector (most recent time gets highest weight)
  # Note: alpha^0 = 1 for most recent, alpha^1, alpha^2, etc. for older periods
  decay_weights <- alpha^((n_time:1) - 1)

  # Normalize decay weights to sum to n_time (preserves scale)
  decay_weights <- decay_weights / sum(decay_weights) * n_time

  # Loop over each time point
  for (t in 1:n_time) {
    # Subset data for current time point
    data_t <- data[time == timef[t], , drop = FALSE]
    n_units_t <- nrow(data_t)

    # Normalize data within current time frame (convert to proportions)
    p_ij <- apply(data_t, 2, function(x) {
      x <- as.numeric(x)
      x_positive <- x - min(x, na.rm = TRUE) + 1e-10  
      x_positive / sum(x_positive, na.rm = TRUE)
    })

    # Apply decay weight to normalized proportions
    decay_weight <- decay_weights[t]
    p_ij_weighted <- p_ij * decay_weight

    # Calculate entropy component: p * log(p)
    e_ij <- apply(p_ij_weighted, 2, function(x) {
      x <- as.numeric(x)
      result <- numeric(length(x))
      for (i in seq_along(x)) {
        if (is.na(x[i]) || x[i] <= 0) {
          result[i] <- 0
        } else {
          result[i] <- x[i] * log(x[i])
        }
      }
      result
    })

    # Calculate entropy for each indicator
    # k is the normalization constant (ensures entropy is between 0 and 1)
    k <- 1 / log(n_units_t)

    # d is the "degree of diversification" (1 - normalized entropy)
    d <- -k * colSums(e_ij, na.rm = TRUE)
    d <- 1 - d

    # Handle edge cases
    d[d < 0] <- 0
    d[!is.finite(d)] <- 0

    # Convert to weights (normalize to sum to 1)
    w <- if (sum(d) > 0) d / sum(d) else rep(1/n_indicators, n_indicators)

    # Store weights
    entropy_weights[, t] <- w
  }

  # Set row and column names
  colnames(entropy_weights) <- timef
  rownames(entropy_weights) <- names(data)

  # Create long format for easy plotting
  weights_long <- data.frame(
    indicator = rep(rownames(entropy_weights), n_time),
    time = rep(as.numeric(colnames(entropy_weights)), each = n_indicators),
    weight = as.vector(entropy_weights)
  )

  # Calculate summary statistics
  summary_stats <- data.frame(
    indicator = rownames(entropy_weights),
    mean_weight = rowMeans(entropy_weights),
    sd_weight = apply(entropy_weights, 1, stats::sd),
    min_weight = apply(entropy_weights, 1, min),
    max_weight = apply(entropy_weights, 1, max),
    trend = apply(entropy_weights, 1, function(x) {
      if (length(x) > 1) {
        stats::coef(stats::lm(x ~ seq_along(x)))[2]
      } else {
        NA_real_
      }
    }),
    row.names = NULL
  )

  # Add interpretation
  summary_stats$interpretation <- ifelse(
    summary_stats$trend > 0.001, "Increasing importance",
    ifelse(summary_stats$trend < -0.001, "Decreasing importance (convergence)",
           "Stable importance")
  )

  # Create result object
  result <- list(
    weights = entropy_weights,
    weights_long = weights_long,
    summary = summary_stats,
    alpha = alpha,
    n_time = n_time,
    n_indicators = n_indicators,
    n_units = n_units,
    call = match.call()
  )

  class(result) <- "mwmm_entropy"
  return(result)
}


#' @title Print Method for mwmm_entropy Objects
#' @description Prints a summary of the modified entropy weighting results.
#' @param x An object of class "mwmm_entropy"
#' @param ... Additional arguments (ignored)
#' @export
print.mwmm_entropy <- function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("  Modified Entropy Weights (mwmm)\n")
  cat("========================================\n")
  cat("\n")
  cat("Decay parameter (alpha):", x$alpha, "\n")
  cat("Time periods:", x$n_time, "\n")
  cat("Indicators:", x$n_indicators, "\n")
  cat("Units per period:", x$n_units, "\n")
  cat("\n")
  cat("Summary of indicator weights:\n")
  cat("-----------------------------\n")
  print(x$summary[, c("indicator", "mean_weight", "trend", "interpretation")],
        row.names = FALSE, digits = 4)
  cat("\n")
  cat("Use plot() to visualize weight evolution over time.\n")
  cat("Access full weights matrix with $weights\n")
  invisible(x)
}


#' @title Plot Method for mwmm_entropy Objects
#' @description Creates a visualization of how entropy weights evolve over time.
#' @param x An object of class "mwmm_entropy"
#' @param highlight Character vector of indicator names to highlight (optional)
#' @param show_labels Logical, whether to show endpoint labels (default TRUE)
#' @param ... Additional arguments passed to ggplot
#' @return A ggplot object
#' @export
plot.mwmm_entropy <- function(x, highlight = NULL, show_labels = TRUE, ...) {

  # Get data
  plot_data <- x$weights_long

  # Base plot
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = .data$time, y = .data$weight,
                                    group = .data$indicator, color = .data$indicator)) +
    ggplot2::geom_line(linewidth = 1.2, alpha = 0.8) +
    ggplot2::geom_point(data = plot_data[plot_data$time %in% range(plot_data$time), ],
                        size = 3, shape = 21, fill = "white", stroke = 1.2) +
    viridis::scale_color_viridis_d(option = "D") +
    ggplot2::labs(
      title = "Evolution of Indicator Importance Over Time",
      subtitle = paste0("Modified entropy weights (alpha = ", round(x$alpha, 4), ")"),
      x = "Time",
      y = "Entropy Weight (Importance)",
      color = "Indicator",
      caption = "Higher weight = more discriminating power (matters more)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add labels at endpoints if requested
  if (show_labels && requireNamespace("ggrepel", quietly = TRUE)) {
    end_data <- plot_data[plot_data$time == max(plot_data$time), ]
    p <- p +
      ggrepel::geom_text_repel(
        data = end_data,
        ggplot2::aes(label = .data$indicator),
        size = 3,
        nudge_x = (max(plot_data$time) - min(plot_data$time)) * 0.05,
        direction = "y",
        hjust = 0,
        segment.size = 0.2,
        segment.alpha = 0.5
      ) +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}


#' @title Summary Method for mwmm_entropy Objects
#' @description Returns the summary statistics data frame.
#' @param object An object of class "mwmm_entropy"
#' @param ... Additional arguments (ignored)
#' @return Data frame with summary statistics
#' @export
summary.mwmm_entropy <- function(object, ...) {
  cat("\nModified Entropy Weights Summary\n")
  cat("================================\n\n")
  cat("Indicators ranked by mean weight (most important first):\n\n")
  sorted_summary <- object$summary[order(-object$summary$mean_weight), ]
  print(sorted_summary, row.names = FALSE, digits = 4)
  invisible(sorted_summary)
}
