#' Compare plots
#' 
#' @description Combines multiple plots of several variables in a single graph.
#' @details This function takes any input arguments from `plot_combine()`
#' and `plot_multiple()` to customize the plots and their organization
#' in a grid.
#' 
#' @param plot_function Indicates which of the plot types to use.
#' Options are: 'plot_timeseries', 'plot_heatmap', 'plot_seasonality', 'plot_map'.
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or disease case observations for one or multiple locations.
#' @param var Character vector with the name of the variables to be plotted. 
#' @param type Character vector with the same length of `var` that specifies the 
#' types of variable for each element in `var`. Possible values include 
#' 'cov' (covariate, default), 'counts' (case counts), 
#' and 'inc' (case incidence). If `type='inc'`, `pop` is required.
#' @param ... Additional arguments for `plot_multiple`, `plot_combine`, and the 
#' selected `plot_function`.
#' @return A single (cow)plot containing plots of several variables.
#' @export
#' @seealso \code{\link{plot_multiple}}, \code{\link{plot_combine}}
#'
#' @examples
#' # Load data
#' library("sf")
#' data("dengue_MS")
#' data("map_MS")
#' 
# Comparing time series plots
#' plot_compare(
#'   plot_function = plot_timeseries,
#'   data = dengue_MS,
#'   var = c("dengue_cases",  "pdsi"),
#'   type = c("inc", "cov"),
#'   pop = "population",
#'   time = "date",
#'   area = "micro_code",
#'   var_label = c("Dengue inc", "PDSI"), 
#'   combine_legend = TRUE, 
#'   ncol_legend = 1,
#'   ncol = 1, 
#'   align = "h")
#' 
#' # Comparing seasonality plots
#' plot_compare(
#'   plot_function = plot_seasonality,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "pdsi"),
#'   type = c("counts", "inc", "cov"),
#'   pop = "population",
#'   time = "date",
#'   area = "micro_code",
#'   aggregate_space = "region_code",
#'   pt = 100,
#'   var_label = c("Dengue Cases", "Dengue inc", "Min Temp"), 
#'   ncol_legend = 1,
#'   combine_legend = TRUE)
#' 
#' # Comparing heatmaps plots
#' plot_compare(
#'   plot_function = plot_heatmap,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "pdsi"),
#'   type = c("inc", "cov"),   
#'   pop = "population",
#'   time = "date",
#'   area = "micro_code",
#'   var_label = c("Dengue Cases", "Min Temp"), 
#'   palette = c("Reds", "Blues"),
#'   ncol_legend = 1,
#'   combine_xaxis = TRUE)
#' 
#' # Comparing map plots
#' plot_compare(
#'   plot_function = plot_map,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "tmax"),
#'   type = c("inc", "cov"),  
#'   pop = "population",
#'   time = "date",
#'   area = "micro_code",
#'   var_label= c("Dengue Incidence", "Max Temperature"), 
#'   palette = c("Reds", "Blues"),
#'   map = map_MS,        
#'   map_area = "code", 
#'   by_year = FALSE,
#'   ncol_legend = 1,
#'   combine_xaxis =TRUE)

plot_compare <- function(plot_function, 
                         data, 
                         var,
                         type,
                         ...) {
  
  # Capture all arguments as a named list
  all_args <- list(...)
  
  # Ensure names exist for all arguments
  arg_names <- names(all_args)
  if (is.null(arg_names) || any(arg_names == "")) {
    stop("All additional arguments must be named.")
  }
  
  # Define allowed plot functions
  valid_plot_functions <- c(
    "plot_timeseries", "plot_heatmap", "plot_seasonality", 
    "plot_bivariate", "plot_map"
  )
  
  # Ensure plot_function is either a function or a valid function name
  if (!(is.function(plot_function) || as.character(substitute(plot_function)) %in% valid_plot_functions)) {
    stop(paste("Invalid plot function:", as.character(substitute(plot_function)), 
               "\nAllowed options:", paste(valid_plot_functions, collapse = ", ")))
  }
  
  # Ensure data is a data.frame
  if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame.")
  }
  
  # Ensure var is properly formatted
  if (identical(plot_function, plot_bivariate)) {
    # If using bivariate plot, var must be a list of character vectors
    if (!is.list(var) || !all(sapply(var, is.character))) {
      stop("'var' must be a list of character vectors for plot_bivariate.")
    }
    # Ensure all variables exist in data
    all_vars <- unique(unlist(var))
  } else {
    # For other plots, var must be a character vector
    if (!is.character(var)) {
      stop("'var' must be a character vector.")
    }
    all_vars <- var
  }
  
  # Ensure variables exist in data
  if (!all(all_vars %in% names(data))) {
    missing_vars <- all_vars[!all_vars %in% names(data)]
    stop(paste("The following variables are missing from 'data':", paste(missing_vars, collapse = ", ")))
  }
  
  # Define arguments specific to plot_combine
  plot_combine_args <- c(
    "combine_legend", "combine_xaxis", "ncol_l", "nrow_l", 
    "rel_widths_l", "rel_heights_l", "ncol_legend",
    
    # cowplot::plot_grid arguments
    "ncol", "nrow", "align", "rel_widths", "rel_heights", 
    "labels", "label_size", "label_fontface", "label_colour", 
    "label_position", "label_padding", "hjust", "vjust", 
    "axis", "bg"
  )
  
  # Separate args for plot_multiple and plot_combine
  plot_combine_args_list  <- all_args[names(all_args) %in% plot_combine_args]
  plot_multiple_args_list <- all_args[!names(all_args) %in% plot_combine_args]
  
  # Extract combine_legend safely (set default FALSE if missing)
  combine_legend <- plot_combine_args_list$combine_legend %||% FALSE
  
  # Prevent combining legends when using plot_heatmap or plot_map 
  if (combine_legend == TRUE && (identical(plot_function, plot_heatmap) || identical(plot_function, plot_map))) {
    stop("Legends of heatmap or map plots cannot be combined.")
  }
  
  # Extract combine_legend safely (set default FALSE if missing)
  palette <- plot_multiple_args_list$palette %||% FALSE
  
  # Prevent combining legends when using plot_heatmap
  if (combine_legend == TRUE && length(palette) > 1) {
    stop("Legends of plots with different palettes cannot be combined.")
  }
  
  # Generate plots using plot_multiple
  plots <- do.call(plot_multiple,
                   c(list(plot_function = plot_function, data = data,
                          var = var, type = type),
                     plot_multiple_args_list))
  
  # Combine plots using plot_combine
  out <- do.call(plot_combine, 
                 c(list(plot_list = plots),
                   plot_combine_args_list))
  
  return(out)
}
