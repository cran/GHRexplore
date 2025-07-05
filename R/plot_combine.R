#' @title Combine plots
#' 
#' @description Combines plots, each representing one variable, into a single plot.
#' @details This function takes any input from the `cowplot::plot_grid` function to 
#' customize the organization of the plots.
#' 
#' @param plot_list A list of plots to be combined.
#' @param combine_legend Logical. If TRUE, assumes the legend of all plots is 
#' the same as the legend of the first plot in `plot_list` and final plot
#' shows only one instance of the common legend. Default is FALSE.
#' @param combine_xaxis Logical. If TRUE, removes x axis labels from all but the 
#' last plot. Default is FALSE.
#' @param ncol (from cowplot) Number of colums in the plot grid. Default is 1.
#' @param align (from cowplot) Specifies how plots should be aligned
#' Options are "none", "hv" (align in both directions), "h", and "v" (default). 
#' @param ... Additional arguments passed to `cowplot::plot_grid`.
#' @param ncol_l When combine_legend = TRUE,
#' number of colums in which to align plots and the common legend. Default is 2.
#' @param nrow_l When combine_legend = TRUE,
#' number of rows in which to align plots and the common legend. Default is NULL.
#' @param rel_widths_l When combine_legend = TRUE,
#' vector of widths in which to align plots and the common legend. Default is c(3, 1).
#' @param rel_heights_l When combine_legend = TRUE,
#' vector of heights in which to align plots and the common legend. Default is c(1, 1).
#' @param ncol_legend When combine_legend = TRUE,
#' number of columns the legend should be distributed in. Default is one column.
#' @return A single (cow)plot including the provided multiple plots.
#' @export
#' @seealso \code{\link{plot_compare}}, \code{\link{plot_multiple}}
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Multiple time series plot
#' plots <- plot_multiple(
#'   plot_function = plot_timeseries,
#'   data = dengue_MS,
#'   var = c("tmax", "tmin", "pdsi"),
#'   type = c("cov", "cov", "cov"),
#'   aggregate_space = "meso_code",
#'   pop = "population",
#'   var_label = c("Max Temp", "Min Temp", "PDSI"),
#'   time = "date",
#'   area = "micro_code")
#' 
#' # Combine them with a shared legend
#' plot_combine(plot_list = plots, 
#'              ncol = 1,
#'              align = "v",
#'              combine_legend = TRUE,
#'              combine_xaxis = TRUE,
#'              rel_widths_l = c(7,1))

plot_combine<- function(plot_list, 
                        combine_legend = FALSE, 
                        combine_xaxis = FALSE,
                        ncol = 1,
                        align = "v",
                        ..., 
                        ncol_l = 2,
                        nrow_l = NULL, 
                        rel_widths_l = c(3, 1), 
                        rel_heights_l = c(1,1), 
                        ncol_legend = 1){
  
  # Check if plot_list is a list of ggplot objects
  if (!is.list(plot_list)) {
    stop("plot_list must be a list of ggplot objects")
  }
  if (any(sapply(plot_list, function(x) !inherits(x, "gg")))) {
    stop("All elements in plot_list must be ggplot objects")
  }
  
  # Check if combine_legend and combine_xaxis are logical values
  if (!is.logical(combine_legend)) {
    stop("combine_legend must be a logical value (TRUE or FALSE)")
  }
  if (!is.logical(combine_xaxis)) {
    stop("combine_xaxis must be a logical value (TRUE or FALSE)")
  }
  
  # Check if ncol and align are valid
  if (!is.numeric(ncol) || ncol <= 0) {
    stop("ncol must be a positive integer")
  }
  if (!align %in% c("none", "hv", "h", "v")) {
    stop("align must be one of 'none', 'hv', 'h', or 'v'")
  }
  
  # Validate parameters related to legend (suffix "_l")
  if (combine_legend) {
    if (!is.numeric(ncol_l) || ncol_l <= 0) {
      stop("ncol_l must be a positive integer")
    }
    if (!is.null(nrow_l) && (!is.numeric(nrow_l) || nrow_l <= 0)) {
      stop("nrow_l must be a positive integer or NULL")
    }
    if (!is.numeric(rel_widths_l) || length(rel_widths_l) != 2 || any(rel_widths_l <= 0)) {
      stop("rel_widths_l must be a numeric vector of length 2 with positive values")
    }
    if (!is.null(rel_heights_l) && (!is.numeric(rel_heights_l) || length(rel_heights_l) != 2 || any(rel_heights_l <= 0))) {
      stop("rel_heights_l must be a numeric vector of length 2 with positive values")
    }
  }
  
  
  # Remove x-axis from all but the last plot ----
  
  # Check if combine_xaxis is TRUE and ncol is not 1
  if (combine_xaxis == TRUE && ncol != 1) {
    stop("only use combine_xaxis = TRUE when plots aligned in 1 column")
  }
  
  # Remove x-axis from all but the last plot (if plots are aligned in one column)
  if (combine_xaxis == TRUE && ncol == 1) {
    plot_list <- lapply(seq_along(plot_list), function(i) {
      if (i != length(plot_list)) {
        plot_list[[i]] + theme(axis.title.x = element_blank(),
                               axis.text.x = element_blank(), 
                               axis.ticks.x = element_blank())
      } else {
        plot_list[[i]]  # Keep x-axis for the last plot
      }
    })
  }
  
  
  # Individual legends for each plot ----
  if (combine_legend == FALSE){
  out <- cowplot::plot_grid( plotlist =plot_list, 
                      ncol = ncol, 
                      align = align, 
                      ...)
  }
  
  
  # Common legend for each plot ----
  if (combine_legend == TRUE){
    # Extract the shared legend from one plot
    plot_first <-  plot_list[[1]] +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = ncol_legend))
    
    legend <- cowplot::get_legend(plot_first)
    
    # Remove legend from each plot in plot_list
    plots_no_legend <- lapply(plot_list, function(p) 
      p + theme(legend.position = "none"))
    
    # Align p1 and p2 vertically
    plots_combined <- cowplot::plot_grid( plotlist = plots_no_legend, 
                                            ncol = ncol, 
                                            align = align)
    
    out <- cowplot::plot_grid(plots_combined,
                       legend, 
                       ncol = ncol_l,
                       nrow = nrow_l,
                       rel_widths = rel_widths_l, 
                       rel_heights = rel_heights_l
                       )
  }
  
  return(out)
}


  
 