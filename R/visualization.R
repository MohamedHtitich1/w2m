#' @title Publication-Ready ggplot Theme
#'
#' @description
#' A clean, minimal theme suitable for academic publications and reports.
#'
#' @param base_size Base font size (default 11)
#' @param base_family Base font family (default "")
#'
#' @return A ggplot theme object
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_mwmm()
#'
#' @export
theme_mwmm <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(color = "gray40"),
      plot.caption = ggplot2::element_text(color = "gray50", size = base_size - 2),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(size = base_size - 1),
      axis.ticks = ggplot2::element_line(color = "gray70"),
      panel.border = ggplot2::element_rect(color = "gray80", fill = NA),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.key = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      strip.background = ggplot2::element_blank()
    )
}


#' @title World Choropleth Map
#'
#' @description
#' Creates a world map with countries colored by a value variable,
#' using Mollweide projection for accurate area representation.
#'
#' @param data Data frame with country codes and values
#' @param value_col Name of the value column to map
#' @param code_col Name of ISO3 country code column
#' @param title Map title
#' @param legend_title Legend title (default: value_col)
#' @param palette Viridis palette: "viridis", "magma", "plasma", "inferno", "cividis"
#' @param direction Direction of color scale: 1 or -1
#' @param na_color Color for missing values
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' world_choropleth(
#'   data = cesp_2020,
#'   value_col = "score_cesp",
#'   code_col = "iso3",
#'   title = "Carbon Efficiency of Social Progress (2020)"
#' )
#' }
#'
#' @export
world_choropleth <- function(data, value_col, code_col = "iso3",
                             title = "", legend_title = NULL,
                             palette = "viridis", direction = 1,
                             na_color = "grey90") {
  
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop("Package 'maps' is required. Install with: install.packages('maps')")
  }
  
  if (is.null(legend_title)) legend_title <- value_col
  
  world <- ggplot2::map_data("world")
  world$iso3 <- maps::iso.alpha(world$region, n = 3)
  
  world_data <- dplyr::left_join(
    world, data,
    by = stats::setNames(code_col, "iso3")
  )
  
  world_data <- world_data[world_data$region != "Antarctica", ]
  
  ggplot2::ggplot(world_data,
                  ggplot2::aes(x = .data$long, y = .data$lat,
                               group = .data$group,
                               fill = .data[[value_col]])) +
    ggplot2::geom_polygon(color = "white", linewidth = 0.1) +
    ggplot2::coord_map("mollweide") +
    viridis::scale_fill_viridis(
      option = palette,
      direction = direction,
      na.value = na_color,
      guide = ggplot2::guide_colorbar(
        title = legend_title,
        title.position = "top",
        barwidth = 15,
        barheight = 0.5
      )
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14)
    )
}


#' @title Trajectory Plot for Time Series
#'
#' @description
#' Creates a line plot showing trajectories over time for multiple groups.
#'
#' @param data Data frame with time series data
#' @param x Time/x variable name
#' @param y Value/y variable name
#' @param group Grouping variable name
#' @param color Color variable (default: same as group)
#' @param label Label variable for endpoints (optional)
#' @param highlight_points Years to highlight with points (optional)
#' @param show_legend Logical, show legend (default FALSE)
#'
#' @return A ggplot object
#'
#' @export
trajectory_plot <- function(data, x, y, group, color = NULL,
                            label = NULL, highlight_points = NULL,
                            show_legend = FALSE) {
  
  if (is.null(color)) color <- group
  
  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x = .data[[x]], y = .data[[y]],
                                    group = .data[[group]],
                                    color = .data[[color]])) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.7) +
    viridis::scale_color_viridis_d() +
    theme_mwmm()
  
  if (!is.null(highlight_points)) {
    highlight_data <- data[data[[x]] %in% highlight_points, ]
    p <- p + ggplot2::geom_point(data = highlight_data, size = 2,
                                 shape = 21, fill = "white", stroke = 1)
  }
  
  if (!is.null(label) && requireNamespace("ggrepel", quietly = TRUE)) {
    end_data <- data[data[[x]] == max(data[[x]]), ]
    p <- p + ggrepel::geom_text_repel(
      data = end_data,
      ggplot2::aes(label = .data[[label]]),
      size = 2.5,
      nudge_x = (max(data[[x]]) - min(data[[x]])) * 0.02,
      direction = "y", hjust = 0, segment.size = 0.2
    )
  }
  
  if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")
  
  return(p)
}


#' @title Scatter Plot with Smart Labels
#'
#' @description
#' Creates a scatter plot with non-overlapping labels using ggrepel.
#'
#' @param data Data frame
#' @param x X variable name
#' @param y Y variable name
#' @param label Label variable name
#' @param color Color variable (optional)
#' @param size Point size variable (optional)
#' @param threshold_x X threshold line (optional)
#' @param threshold_y Y threshold line (optional)
#' @param max_labels Maximum labels to show (default 30)
#'
#' @return A ggplot object
#'
#' @export
scatter_labeled <- function(data, x, y, label, color = NULL, size = NULL,
                            threshold_x = NULL, threshold_y = NULL,
                            max_labels = 30) {
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  
  if (!is.null(color)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color]]),
                                 size = 3, alpha = 0.7)
  } else {
    p <- p + ggplot2::geom_point(size = 3, color = "steelblue", alpha = 0.7)
  }
  
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    if (nrow(data) > max_labels) {
      label_data <- rbind(
        data[order(-data[[y]]), ][1:min(max_labels/2, nrow(data)), ],
        data[order(data[[y]]), ][1:min(max_labels/2, nrow(data)), ]
      )
      label_data <- unique(label_data)
    } else {
      label_data <- data
    }
    
    p <- p + ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(label = .data[[label]]),
      size = 2.5, max.overlaps = 15, box.padding = 0.3
    )
  }
  
  if (!is.null(threshold_x)) {
    p <- p + ggplot2::geom_vline(xintercept = threshold_x,
                                 linetype = "dashed", color = "red", alpha = 0.7)
  }
  if (!is.null(threshold_y)) {
    p <- p + ggplot2::geom_hline(yintercept = threshold_y,
                                 linetype = "dashed", color = "red", alpha = 0.7)
  }
  
  p + theme_mwmm() + viridis::scale_color_viridis_d()
}


#' @title Plot Entropy Weights Evolution (Faceted by Dimension)
#'
#' @description
#' Creates a faceted plot showing how entropy weights evolve over time,
#' grouped by dimension/category. This is the signature visualization
#' of the mwmm package, directly replicating the plots from the
#' "Evolving Importance" paper.
#'
#' @param entropy_result Object from modified_entropy()
#' @param dimensions Named list mapping indicators to dimension names
#' @param ncol Number of facet columns (default 2)
#' @param show_endpoints Show labeled points at start/end years (default TRUE)
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' result <- modified_entropy(data, time = years)
#' 
#' dimensions <- list(
#'   "State Capacity" = c("fiscal_cap", "coord_cap", "deliv_cap"),
#'   "Democratic Accountability" = c("instit_acc", "elect_acc", "soc_acc")
#' )
#' 
#' plot_entropy_faceted(result, dimensions)
#' }
#'
#' @export
plot_entropy_faceted <- function(entropy_result, dimensions, ncol = 2,
                                 show_endpoints = TRUE) {
  
  if (!inherits(entropy_result, "mwmm_entropy")) {
    stop("Input must be an mwmm_entropy object from modified_entropy()")
  }
  
  # Create dimension mapping
  dim_map <- data.frame(
    indicator = unlist(dimensions),
    dimension = rep(names(dimensions), lengths(dimensions)),
    stringsAsFactors = FALSE
  )
  
  # Merge with weights
  plot_data <- entropy_result$weights_long %>%
    dplyr::left_join(dim_map, by = "indicator") %>%
    dplyr::filter(!is.na(.data$dimension))
  
  # Get year range
  years <- range(plot_data$time)
  
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = .data$time, y = .data$weight,
                                    group = .data$indicator,
                                    color = .data$dimension)) +
    ggplot2::geom_line(linewidth = 1.5, alpha = 0.8, show.legend = FALSE)
  
  if (show_endpoints) {
    endpoint_data <- plot_data[plot_data$time %in% years, ]
    p <- p + ggplot2::geom_point(
      data = endpoint_data,
      ggplot2::aes(fill = .data$dimension),
      color = "black", size = 3, shape = 21
    )
    
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      end_labels <- plot_data[plot_data$time == max(plot_data$time), ]
      p <- p + ggrepel::geom_text_repel(
        data = end_labels,
        ggplot2::aes(label = .data$indicator),
        size = 2, color = "black",
        bg.color = "white", bg.r = 0.05,
        nudge_x = 2, direction = "y", hjust = 0,
        segment.size = 0.2, segment.curvature = -0.5,
        xlim = c(NA, years[2] + (years[2] - years[1]) * 0.3)
      )
    }
  }
  
  p <- p +
    ggplot2::facet_wrap(~ .data$dimension, scales = "free_y", ncol = ncol) +
    viridis::scale_color_viridis_d() +
    viridis::scale_fill_viridis_d() +
    ggplot2::scale_x_continuous(
      limits = c(years[1], years[2] + (years[2] - years[1]) * 0.3),
      breaks = seq(years[1], years[2], by = 5)
    ) +
    ggplot2::labs(
      x = "", y = "Diversity of Information (Entropy Weight)",
      fill = "", caption = "Higher weight = greater discriminating power"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = 6),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 9, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
  
  return(p)
}
