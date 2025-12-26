#' @title Fit Segmented (Piecewise) Regression
#'
#' @description
#' Fits a segmented linear regression model to identify structural breaks
#' or threshold effects in the relationship between variables.
#'
#' @param data Data frame
#' @param formula Model formula (e.g., y ~ x)
#' @param seg_var Variable for segmentation (breakpoints)
#' @param n_breaks Expected number of breakpoints. If NULL, uses automatic selection.
#' @param initial_psi Initial breakpoint guesses. If NULL, uses quantiles.
#' @param selection Method for automatic breakpoint selection: "bic" or "aic"
#'
#' @return A segmented model object with additional mwmm attributes
#'
#' @examples
#' \dontrun{
#' # Fit segmented regression for SPI vs CO2 relationship
#' model <- fit_segmented(
#'   data = country_data,
#'   formula = score_spi ~ co2_percap,
#'   seg_var = "co2_percap",
#'   n_breaks = 2
#' )
#'
#' # View breakpoints
#' model$psi
#'
#' # Get segment parameters
#' segment_params(model)
#' }
#'
#' @export
fit_segmented <- function(data, formula, seg_var, n_breaks = NULL,
                          initial_psi = NULL, selection = "bic") {

  if (!requireNamespace("segmented", quietly = TRUE)) {
    stop("Package 'segmented' is required. Install with: install.packages('segmented')")
  }

  # Fit initial linear model
  lm_model <- stats::lm(formula, data = data)

  # Automatic breakpoint selection if n_breaks not specified
  if (is.null(n_breaks)) {
    sel_result <- segmented::selgmented(
      lm_model,
      type = selection,
      plot.ic = FALSE,
      Kmax = 5
    )
    n_breaks <- sel_result$selection.psi$npsi
  }

  # Set initial breakpoint guesses
  if (is.null(initial_psi) && n_breaks > 0) {
    probs <- seq(0.2, 0.8, length.out = n_breaks)
    initial_psi <- stats::quantile(data[[seg_var]], probs = probs, na.rm = TRUE)
  }

  # Fit segmented model
  if (n_breaks == 0) {
    message("No breakpoints detected. Returning linear model.")
    return(lm_model)
  }

  seg_formula <- stats::as.formula(paste("~", seg_var))

  seg_model <- segmented::segmented(
    lm_model,
    seg.Z = seg_formula,
    psi = list(setNames(as.numeric(initial_psi), seg_var)),
    control = segmented::seg.control(
      n.boot = 500,
      it.max = 100,
      conv.psi = TRUE
    )
  )

  return(seg_model)
}


#' @title Extract Segment Parameters
#'
#' @description
#' Extracts the intercept and slope for each segment of a segmented regression,
#' along with segment boundaries.
#'
#' @param model Segmented model object from \code{\link{fit_segmented}}
#'
#' @return Data frame with columns:
#' \describe{
#'   \item{segment}{Segment number}
#'   \item{intercept}{Intercept for this segment}
#'   \item{slope}{Slope for this segment}
#'   \item{start}{X value where segment begins}
#'   \item{end}{X value where segment ends}
#' }
#'
#' @examples
#' \dontrun{
#' model <- fit_segmented(data, y ~ x, "x", n_breaks = 2)
#' params <- segment_params(model)
#' print(params)
#' }
#'
#' @export
segment_params <- function(model) {

  if (!inherits(model, "segmented")) {
    stop("Model must be a segmented regression object")
  }

  coefs <- stats::coef(model)
  breakpoints <- model$psi[, "Est."]
  n_breaks <- length(breakpoints)

  # First segment
  b0 <- coefs["(Intercept)"]
  b1 <- coefs[2]  # First slope coefficient

  params <- data.frame(
    segment = 1,
    intercept = as.numeric(b0),
    slope = as.numeric(b1),
    start = -Inf,
    end = breakpoints[1]
  )

  # Additional segments
  current_intercept <- as.numeric(b0)
  current_slope <- as.numeric(b1)

  for (i in seq_len(n_breaks)) {
    # Get slope change coefficient
    slope_change_name <- names(coefs)[grep(paste0("U", i), names(coefs))]
    if (length(slope_change_name) > 0) {
      slope_change <- as.numeric(coefs[slope_change_name])
    } else {
      slope_change <- 0
    }

    new_slope <- current_slope + slope_change

    # Calculate new intercept to maintain continuity
    bp <- breakpoints[i]
    new_intercept <- current_intercept + current_slope * bp - new_slope * bp

    end_val <- if (i < n_breaks) breakpoints[i + 1] else Inf

    params <- rbind(params, data.frame(
      segment = i + 1,
      intercept = new_intercept,
      slope = new_slope,
      start = bp,
      end = end_val
    ))

    current_intercept <- new_intercept
    current_slope <- new_slope
  }

  rownames(params) <- NULL
  return(params)
}


#' @title Plot Segmented Regression
#'
#' @description
#' Creates a visualization of a segmented regression model.
#'
#' @param model Segmented model object
#' @param data Original data used to fit the model
#' @param x_var Name of x variable
#' @param y_var Name of y variable
#' @param show_breaks Logical, show vertical lines at breakpoints (default TRUE)
#'
#' @return A ggplot object
#'
#' @export
plot_segmented <- function(model, data, x_var, y_var, show_breaks = TRUE) {

  # Get predictions
  data$pred <- stats::predict(model, newdata = data)

  # Base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(alpha = 0.5, size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = .data$pred), color = "red", linewidth = 1.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Segmented Regression",
      x = x_var,
      y = y_var
    )

  # Add breakpoint lines
  if (show_breaks && inherits(model, "segmented")) {
    breakpoints <- model$psi[, "Est."]
    for (bp in breakpoints) {
      p <- p + ggplot2::geom_vline(
        xintercept = bp,
        linetype = "dashed",
        color = "blue",
        alpha = 0.7
      )
    }
  }

  return(p)
}
