#' Multiple plot
#'
#' @description Produces a list of multiple plots of the same type, each 
#' representing one variable.
#' 
#' @details Variable names, types, labels and palette can be customized for 
#' each plot, the rest of parameters will be the same for all variables
#' (options depend on the chosen plot type).
#' 
#' @param plot_function Indicates which of the plot types to use.
#' Options are: 'plot_timeseries', 'plot_heatmap', 'plot_seasonality',
#' and 'plot_map'.
#' @param ... Additional arguments to pass to the plotting function.
#' 
#' @return A list of the different generated plots.
#' @export
#' @seealso \code{\link{plot_compare}}, \code{\link{plot_combine}}
#'
#' @examples
#' # Load data
#' library("sf")
#' data("dengue_MS")
#' data("map_MS")
#' 
# Multiple time series plots
#' plots <- plot_multiple(
#'   plot_function = plot_timeseries,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax"),
#'   type = c("counts", "inc", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp"),
#'   palette = c("blue", "red", "darkgreen"),
#'   time = "date",
#'   area = "micro_code",
#'   facet = TRUE)
#' 
#' # Acess individual plots
#' print(plots[[1]])  
#' 
#' # Multiple heatmap plots
#' plots <- plot_multiple(
#'   plot_function = plot_heatmap,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax"),
#'   type = c("counts", "inc", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp"),
#'   palette = c("Blues", "Reds", "BrBG"),
#'   time = "date",
#'   area = "micro_code")
#' 
#' # Multiple seasonality plots
#' plots <- plot_multiple(
#'   plot_function = plot_seasonality,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax"),
#'   type = c("counts", "inc", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp"),
#'   palette =  c("Blues", "Reds", "BrBG"),
#'   time = "date",
#'   area = "micro_code")
#' 
#' # Multiple map plots
#' plots <- plot_multiple(
#'   plot_function = plot_map, 
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax"),
#'   type = c("counts", "inc", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp"),
#'   palette = c("Reds", "Blues", "Viridis"),
#'   map = map_MS,         
#'   map_area = "code",    
#'   time = "date",
#'   area = "micro_code")

plot_multiple <- function(plot_function, ...) {
  
  # Define allowed plot functions
  valid_plot_functions <- c(
    "plot_timeseries", "plot_heatmap", "plot_seasonality","plot_map"
  )
  
  # Ensure plot_function is either a function or a valid function name
  if (!(is.function(plot_function) || as.character(substitute(plot_function)) %in% valid_plot_functions)) {
    stop(paste("Invalid plot function:", as.character(substitute(plot_function)), 
               "\nAllowed options:", paste(valid_plot_functions, collapse = ", ")))
  }
  
  
  # Determine which multiple plot function to apply
  if (identical(plot_function, plot_map)) {
    return(.plot_multiple_map(plot_function = plot_function, ...))
  } else if (identical(plot_function, plot_timeseries) || 
             identical(plot_function, plot_heatmap) ||
             identical(plot_function, plot_seasonality)) {
    return(.plot_multiple_common(plot_function = plot_function, ...))
  } else {
    stop("Unsupported plot function. Currently supported: 
         'plot_timeseries', 'plot_heatmap', 'plot_seasonality','plot_map'.")
  }
}



