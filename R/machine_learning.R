#' @title Random Forest Variable Importance Analysis
#'
#' @description
#' Fits a Random Forest model and extracts variable importance scores,
#' scaled to 0-100 for easy interpretation. Useful for identifying which
#' predictors "matter most" in explaining an outcome.
#'
#' @param data Data frame
#' @param response Name of response/outcome variable
#' @param predictors Character vector of predictor variable names.
#'   If NULL, uses all columns except response.
#' @param ntree Number of trees to grow (default 500)
#' @param importance_type Type of importance: "permutation" (default) or "impurity"
#' @param scale_importance Logical, scale importance to 0-100 (default TRUE)
#' @param ... Additional arguments passed to randomForest
#'
#' @return A list of class "mwmm_varimp" containing:
#' \describe{
#'   \item{model}{The fitted randomForest object}
#'   \item{importance}{Data frame of variable importance scores}
#'   \item{top_predictors}{Vector of top predictor names}
#' }
#'
#' @examples
#' \dontrun{
#' # Analyze what predicts climate awareness
#' result <- rf_importance(
#'   data = survey_data,
#'   response = "climate_awareness",
#'   predictors = c("education", "age", "income", "urban")
#' )
#'
#' # View importance ranking
#' print(result$importance)
#'
#' # Plot
#' plot(result)
#' }
#'
#' @export
rf_importance <- function(data, response, predictors = NULL, ntree = 500,
                          importance_type = "permutation", scale_importance = TRUE, ...) {

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required. Install with: install.packages('randomForest')")
  }

  # Set predictors
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), response)
  }

  # Check columns exist
  missing <- setdiff(c(response, predictors), names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "))
  }

  # Remove rows with NA in response or predictors
  complete_data <- data[complete.cases(data[, c(response, predictors)]), ]

  if (nrow(complete_data) < 10) {
    stop("Insufficient complete cases for Random Forest")
  }

  # Build formula
  formula <- stats::as.formula(paste(response, "~", paste(predictors, collapse = " + ")))

  # Fit model
  rf_model <- randomForest::randomForest(
    formula,
    data = complete_data,
    ntree = ntree,
    importance = TRUE,
    ...
  )

  # Extract importance
  varimp_raw <- randomForest::importance(rf_model)

  # Choose importance measure
  if (importance_type == "permutation") {
    imp_col <- if (is.factor(complete_data[[response]])) "MeanDecreaseAccuracy" else "%IncMSE"
  } else {
    imp_col <- "MeanDecreaseGini"
    if (!"MeanDecreaseGini" %in% colnames(varimp_raw)) {
      imp_col <- "IncNodePurity"
    }
  }

  importance_vals <- varimp_raw[, imp_col]

  # Create importance data frame
  varimp_df <- data.frame(
    variable = rownames(varimp_raw),
    raw_importance = importance_vals,
    row.names = NULL
  )

  # Scale to 0-100 if requested
  if (scale_importance) {
    varimp_df$importance <- rescale_0_100(varimp_df$raw_importance)
  } else {
    varimp_df$importance <- varimp_df$raw_importance
  }

  # Sort by importance
  varimp_df <- varimp_df[order(-varimp_df$importance), ]
  varimp_df$rank <- seq_len(nrow(varimp_df))

  # Create result
  result <- list(
    model = rf_model,
    importance = varimp_df,
    top_predictors = head(varimp_df$variable, 10),
    response = response,
    n_predictors = length(predictors),
    n_obs = nrow(complete_data),
    call = match.call()
  )

  class(result) <- "mwmm_varimp"
  return(result)
}


#' @title Print Method for Variable Importance
#' @export
print.mwmm_varimp <- function(x, n = 10, ...) {
  cat("\n")
  cat("Random Forest Variable Importance\n")
  cat("==================================\n")
  cat("Response:", x$response, "\n")
  cat("Predictors:", x$n_predictors, "\n")
  cat("Observations:", x$n_obs, "\n")
  cat("\n")
  cat("Top", min(n, nrow(x$importance)), "predictors:\n")
  print(head(x$importance[, c("rank", "variable", "importance")], n), row.names = FALSE)
  invisible(x)
}


#' @title Plot Variable Importance
#' @export
plot.mwmm_varimp <- function(x, n = 15, ...) {
  plot_data <- head(x$importance, n)
  plot_data$variable <- factor(plot_data$variable, levels = rev(plot_data$variable))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$importance, y = .data$variable)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$importance, 1)),
                       hjust = -0.1, size = 3) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = paste("Variable Importance for", x$response),
      x = "Importance (0-100 scale)",
      y = NULL
    ) +
    ggplot2::theme_minimal()
}


#' @title Fit Conditional Inference Tree
#'
#' @description
#' Fits a conditional inference tree (ctree) providing unbiased variable selection
#' and interpretable decision rules.
#'
#' @param data Data frame
#' @param response Name of response variable
#' @param predictors Character vector of predictor names
#' @param max_depth Maximum tree depth (default 3)
#' @param min_split Minimum observations to attempt split (default 20)
#' @param ... Additional arguments to ctree_control
#'
#' @return A party object (conditional inference tree)
#'
#' @examples
#' \dontrun{
#' tree <- fit_ctree(
#'   data = survey_data,
#'   response = "climate_action",
#'   predictors = c("awareness", "worry", "beliefs"),
#'   max_depth = 3
#' )
#'
#' # Plot the tree
#' plot(tree)
#' }
#'
#' @export
fit_ctree <- function(data, response, predictors = NULL, max_depth = 3,
                      min_split = 20, ...) {

  if (!requireNamespace("partykit", quietly = TRUE)) {
    stop("Package 'partykit' is required. Install with: install.packages('partykit')")
  }

  if (is.null(predictors)) {
    predictors <- setdiff(names(data), response)
  }

  formula <- stats::as.formula(paste(response, "~", paste(predictors, collapse = " + ")))

  partykit::ctree(
    formula,
    data = data,
    control = partykit::ctree_control(
      maxdepth = max_depth,
      minsplit = min_split,
      ...
    )
  )
}
