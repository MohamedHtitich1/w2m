#' @title Phillips-Sul Club Convergence Analysis
#'
#' @description
#' Performs Phillips-Sul club convergence analysis to identify groups of
#' countries or units that are converging toward the same steady state.
#'
#' @details
#' The Phillips-Sul methodology:
#' 1. Computes relative transition paths (H) for each unit
#' 2. Tests for global (sigma) convergence using the log-t test
#' 3. If global convergence is rejected, identifies convergence clubs
#'
#' A significant positive t-statistic (> -1.65) suggests convergence.
#'
#' @param data Wide-format data with units as rows and time periods as columns.
#'   The first column(s) should be unit identifiers, remaining columns are time periods.
#' @param unit_col Column index or name for unit identifiers (default 1)
#' @param data_cols Column indices for time period data
#' @param cstar Critical value for club formation (default 0)
#' @param time_trim Fraction of initial time periods to trim (default 1/3)
#' @param HACmethod HAC estimation method (default "FQSB")
#'
#' @return A list of class "mwmm_convergence" containing:
#' \describe{
#'   \item{H}{Relative transition paths}
#'   \item{global_test}{Results of the global convergence test}
#'   \item{clubs}{Club assignments (if convergence rejected)}
#'   \item{converged}{Logical, whether global convergence was found}
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare wide-format data
#' wide_data <- panel_data %>%
#'   select(country, year, score) %>%
#'   pivot_wider(names_from = year, values_from = score)
#'
#' # Run convergence analysis
#' result <- phillips_sul(wide_data, unit_col = 1, data_cols = 2:32)
#'
#' # Check if global convergence
#' if (result$converged) {
#'   print("All units are converging!")
#' } else {
#'   print(paste("Found", length(result$clubs), "convergence clubs"))
#' }
#' }
#'
#' @references
#' Phillips, P. C., & Sul, D. (2007). Transition modeling and econometric
#' convergence tests. \emph{Econometrica}, 75(6), 1771-1855.
#'
#' @export
phillips_sul <- function(data, unit_col = 1, data_cols,
                         cstar = 0, time_trim = 1/3, HACmethod = "FQSB") {

  if (!requireNamespace("ConvergenceClubs", quietly = TRUE)) {
    stop("Package 'ConvergenceClubs' is required. Install with: install.packages('ConvergenceClubs')")
  }

  # Ensure data is a data frame
  data <- as.data.frame(data)

  # Extract numeric data
  numeric_data <- data[, data_cols]

  # Compute relative transition paths
  H <- ConvergenceClubs::computeH(numeric_data, quantity = "both")

  # Test for global convergence
  global_test <- ConvergenceClubs::estimateMod(
    H$H,
    time_trim = time_trim,
    HACmethod = HACmethod
  )

  # Determine if globally converged (t-stat > -1.65)
  converged <- global_test$tstat > -1.65

  # Find clubs if not globally converged
  clubs <- NULL
  if (!converged) {
    clubs <- ConvergenceClubs::findClubs(
      data,
      dataCols = data_cols,
      unit_names = unit_col,
      refCol = max(data_cols),
      cstar = cstar,
      HACmethod = HACmethod
    )
  }

  result <- list(
    H = H,
    global_test = global_test,
    clubs = clubs,
    converged = converged,
    transition_paths = H$h,
    unit_names = data[[unit_col]],
    call = match.call()
  )

  class(result) <- "mwmm_convergence"
  return(result)
}


#' @title Extract Club Membership from Convergence Results
#'
#' @description
#' Converts convergence club results into a tidy data frame with
#' unit-club mappings.
#'
#' @param x Object of class "mwmm_convergence" or output from ConvergenceClubs::findClubs
#'
#' @return Data frame with 'unit' and 'club' columns
#'
#' @examples
#' \dontrun{
#' result <- phillips_sul(wide_data)
#' membership <- extract_clubs(result)
#' print(membership)
#' }
#'
#' @export
extract_clubs <- function(x) {

  # Handle mwmm_convergence objects
  if (inherits(x, "mwmm_convergence")) {
    if (x$converged) {
      return(data.frame(
        unit = x$unit_names,
        club = "Global Convergence",
        stringsAsFactors = FALSE
      ))
    }
    clubs <- x$clubs
  } else {
    clubs <- x
  }

  # Extract clubs from list structure
  club_list <- list()
  i <- 1
  while (!is.null(clubs[[paste0("club", i)]])) {
    club_list[[i]] <- data.frame(
      unit = clubs[[paste0("club", i)]][["unit_names"]],
      club = paste("Club", i),
      stringsAsFactors = FALSE
    )
    i <- i + 1
  }

  # Check for divergent units
  if (!is.null(clubs$divergent)) {
    club_list[[length(club_list) + 1]] <- data.frame(
      unit = clubs$divergent$unit_names,
      club = "Divergent",
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(club_list)
}


#' @title Print Method for Convergence Results
#' @export
print.mwmm_convergence <- function(x, ...) {
  cat("\n")
  cat("======================================\n")
  cat("  Phillips-Sul Convergence Analysis\n")
  cat("======================================\n")
  cat("\n")
  cat("Global Convergence Test:\n")
  cat("  t-statistic:", round(x$global_test$tstat, 4), "\n")
  cat("  Critical value: -1.65\n")
  cat("  Result:", ifelse(x$converged, "CONVERGED", "NOT CONVERGED"), "\n")
  cat("\n")

  if (!x$converged && !is.null(x$clubs)) {
    membership <- extract_clubs(x)
    club_summary <- table(membership$club)
    cat("Convergence Clubs:\n")
    for (club in names(club_summary)) {
      cat("  ", club, ":", club_summary[club], "units\n")
    }
  }

  invisible(x)
}
