#' Bivariate plot
#' 
#' @description Plots a bivariate graph to visually assess associations.
#' It will be a scatterplot if both variables are numeric and 
#' grouped boxplots if one of them is categorical.
#' 
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or disease case observations for one or multiple locations.
#' @param var Character vector of covariate names with length 2 (x, y). One of 
#' them can be a factor.
#' @param area Character, the name of the variable that identifies the different 
#' areal units or any other grouping of interest. If specified, results are 
#' grouped by this variable. Defaults to NULL (no grouping).
#' @param facet If TRUE, plot each grouping in a different facet.
#' @param free_x_scale If TRUE and facet=TRUE, the x-axis scale is free in 
#' each facet.
#' @param free_y_scale If TRUE and facet=TRUE, the y-axis scale is free in 
#' each facet.
#' @param title Optional title of the plot. 
#' @param var_label A 2 character vector with a custom name for the variables.
#' @param legend A character vector with a custom name for the legend.
#' @param palette GHR, RColorBrewer or colorspace palette (e.g. "Purp"). 
#' Single R colors in `colors()` or hex codes can be used when there is no grouping 
#' or facets are used. Use "-" before the palette name (e.g., "-Reds") to reverse it. 
#' Defaults to a dark green when `area` is NULL or when `facet` is TRUE, otherwise 
#' defaults to the "IDE2" palette. 
#' 
#' @return A ggplot2 scatterplot or boxplot graph.
#' @export
#' 
#' 
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Scatter (two numeric variables) - No grouping
#' plot_bivariate(dengue_MS, 
#'                var = c("pop_density", "tmin"), 
#'                palette = "#d04a2d")
#' 
#' # Scatter (two numeric variables) - Grouping in the same graph
#' plot_bivariate(dengue_MS, 
#'                var = c("pop_density", "tmin"),
#'                var_label = c("Pop. density", "Min temp."),
#'                area = "micro_code")
#' 
#' # Scatter  (two numeric variables) - Grouping in facets
#' plot_bivariate(dengue_MS,
#'                var = c("pop_density", "tmin"),
#'                var_label = c("Pop. density", "Min temp."),
#'                area = "micro_code", facet = TRUE, 
#'                free_x_scale = TRUE)
#' 
#' # Boxplots (one numeric, one categorical) - No grouping
#' plot_bivariate(dengue_MS, 
#'                var = c("pop_density", "biome_name"), 
#'                var_label = c("Pop. density", "Min temp."),
#'                palette = "royalblue")
#' 
#' # Boxplots (one numeric, one categorical) - Grouping
#' plot_bivariate(dengue_MS, 
#'                var = c("biome_name", "tmin"), 
#'                area = "meso_code",
#'                palette = "Accent")

plot_bivariate <- function(data,
                           var,
                           area = NULL,
                           facet = FALSE,
                           free_x_scale = FALSE,
                           free_y_scale = FALSE,
                           title = NULL,
                           var_label = NULL, 
                           legend = NULL, 
                           palette = NULL) {
  
  # Check minimum required arguments are missing
  args <- match.call()
  required_args <- c("data", "var")
  missing_args <- setdiff(required_args, names(args))
  
  if (length(missing_args) > 0) {
    stop(paste(
      "Missing required argument(s):",
      paste(missing_args, collapse = ", ")
    ))
  }  
  
  # Check data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  
  # Check if 'var' is a vector of exactly 2 elements
  if (is.null(var) || length(var) != 2  ) {
    stop("'var' is required and must be a vector with exactly 2 elements.")
  }
  
  # Check var exists in data
  if (!all(var %in% names(data))) {
    stop("One or more elements in 'var' are not found in 'data'.")
  }
  
  # Check if 'var_label' is a vector of exactly 2 elements
  if (!is.null(var_label) && length(var_label) != 2) {
    stop("'var_label' must be a vector with exactly 2 elements corresponding to the variables in the 'var' vector.")
  }
  
  # Check that 'area' is valid if specified
  if (!is.null(area) && is.null(data[[area]])) {
    stop("No column of the data matches the 'area' argument")
  }
  
  # Check for missing values in 'var' variables
  for (v in var) {
    .check_na(v, data)
  }
  
  # Define variables. Only one can be categorical
  var1 <- var[1]
  var2 <- var[2]
  if(all(c(class(data[[var1]]), class(data[[var2]])) %in% c("character", "factor"))){
    stop("Only one categorical variable is allowed.")
  }
  
  # Default palette
  if(is.null(palette)){
    palette <- ifelse(is.null(area)|isTRUE(facet), "#168c81", "IDE2")
  }
  
  ## 1.A: no area specified ----
  if (is.null(area)) {
    
    data <- data  |> 
      dplyr::select(
        x = {{ var1 }},
        y = {{ var2 }}
      ) 
  } else {
    
    ## 1.B: specified area ----
    data <- data |>
      dplyr::select(
        x = {{ var1 }},
        y = {{ var2 }},
        area = {{ area }}
      ) |>
      dplyr::mutate(
        area = as.character(area)
      )
    
    narea <- length(unique(data$area))
    my_palette <- GHR_palette(palette, narea)(narea)
  }
  
  # Plotting ----
  
  # Single color
  if(is.null(area)|isTRUE(facet)){
    
    my_palette <- GHR_palette(palette, 3)(3)
    single_color <- my_palette[3]  
    
    # Message for a single time series and color ramp
    if(length(unique(my_palette))>1){
      message(paste0("A color ramp was selected for a single grouping. ", 
                     "Please select a single color to have more control."))
    }
  }
  
  # Custom variable names
  if (!is.null(var_label)) {
    var1 <- var_label[1]
    var2 <- var_label[2]
  }
  
  # Create legend label
  if (is.null(legend)) {
    legend <- "Area"
  }
  
  out <- ggplot2::ggplot(data) +
    ggplot2::theme_bw() +
    ggplot2::xlab(var1) +
    ggplot2::ylab(var2)
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Boxplot
  if(any(c(class(data[["x"]]), class(data[["y"]])) %in% c("character", "factor"))){
    
    if(is.null(area)|isTRUE(facet)){
      out <- out +
        ggplot2::geom_boxplot(ggplot2::aes(x = .data$x, y = .data$y, colour = area),
                              col = single_color)
    }else{
      out <- out +
        ggplot2::geom_boxplot(ggplot2::aes(x = .data$x, y = .data$y, colour = area)) +
        ggplot2::scale_color_manual(values = my_palette, name = legend) 
    }
    
    
  }else{ # Numerical
    
    if(is.null(area)|isTRUE(facet)){
      out <- out +
        ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, colour = area),
                            col = single_color, size = 1.35)
    }else{
      out <- out +
        ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, colour = area),
                            size = 1.35) +
        ggplot2::scale_color_manual(values = my_palette, name = legend) 
    }
    
  }
  
  # Adjust facet scales if facet == TRUE
  if (facet == TRUE && !is.null(area)) {
    
    # Determine scales
    facet_scales <- "fixed"
    if (free_x_scale && free_y_scale) {
      facet_scales <- "free"
    } else if (free_x_scale) {
      facet_scales <- "free_x"
    } else if (free_y_scale) {
      facet_scales <- "free_y"
    }
    
    out <- out +
      ggplot2::facet_wrap(~ area, scales = facet_scales) +
      ggplot2::guides(color = "none") 
  }
  
  return(out)
}

  
  