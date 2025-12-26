#' @title Package Information
#'
#' @description
#' Displays information about the mwmm package including available functions
#' and the core methodology.
#'
#' @return Invisible NULL, prints to console
#'
#' @examples
#' mwmm_info()
#'
#' @export
mwmm_info <- function() {
  cat("\n")
  cat("================================================================\n")
  cat("  mwmm: Measuring What Matters Most\n")
  cat("  Temporal Entropy Weighting for Sustainable Development\n")
  cat("================================================================\n")
  cat("\n")
  cat("Author: Mohamed Htitich, PhD\n")
  cat("GitHub: https://github.com/MohamedHtitich1/mwmm\n")
  cat("\n")
  cat("CORE FUNCTION:\n")
  cat("  modified_entropy()    - Temporal entropy weights (what matters most)\n")
  cat("\n")
  cat("WEIGHTING & INDICES:\n")
  cat("  entropy_weights()     - Static entropy weights (single time point)\n")
  cat("  calculate_composite() - Weighted composite index\n")
  cat("  calculate_cesp()      - Carbon Efficiency of Social Progress\n")
  cat("  calculate_jts()       - Just Transition Score\n")
  cat("  calculate_mpi()       - Mazziotta-Pareto Index\n")
  cat("\n")
  cat("CONVERGENCE & REGRESSION:\n")
  cat("  phillips_sul()        - Club convergence analysis\n")
  cat("  fit_segmented()       - Segmented/piecewise regression\n")
  cat("\n")
  cat("MACHINE LEARNING:\n")
  cat("  rf_importance()       - Random Forest variable importance\n")
  cat("  fit_ctree()           - Conditional inference trees\n")
  cat("\n")
  cat("VISUALIZATION:\n")
  cat("  plot.mwmm_entropy()   - Plot entropy weight evolution\n")
  cat("  plot_entropy_faceted() - Faceted entropy plots by dimension\n")
  cat("  world_choropleth()    - World map visualization\n")
  cat("  trajectory_plot()     - Time series trajectories\n")
  cat("  scatter_labeled()     - Labeled scatter plots\n")
  cat("  theme_mwmm()          - Publication-ready theme\n")
  cat("\n")
  cat("UTILITIES:\n")
  cat("  rescale_0_100()       - Min-max normalization\n")
  cat("  rescale_spi()         - SPI-style normalization\n")
  cat("  geometric_mean()      - Geometric mean\n")
  cat("  gam_impute()          - GAM-based imputation\n")
  cat("\n")
  cat("Quick start:\n")
  cat("  result <- modified_entropy(data, time = years, alpha = 0.95)\n")
  cat("  plot(result)\n")
  cat("  summary(result)\n")
  cat("\n")
  cat("================================================================\n")
  invisible(NULL)
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "mwmm v1.0.0 - Measuring What Matters Most\n",
    "Temporal entropy weighting for sustainable development\n",
    "\n",
    "Core function: modified_entropy(data, time, alpha)\n",
    "Type mwmm_info() for all available functions\n"
  )
}
