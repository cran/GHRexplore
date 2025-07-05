#' @title .plot_multiple_map
#' @description outputs multiple choropleth map plots, each representing one variable 
#' @details variable names and palette can be customized for each plot, otherwise
#' parameters will be the same for each variable plotted 
#' @param plot_function indicates which of the ghr plot types to use
#' Options are: 'plot_map'
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or disease case observations for one or multiple locations.
#' @param var character vector the name of the variables identifying the 
#' variables to be plotted. 
#' @param type character that specifies the type of variable in `var`. 
#' Possible values include 'cov' (covariate, default), 'counts' (case counts), 
#' and 'inc' (case incidence). If `type='inc'`, `pop` is required.
#' @param var_label character vector with custom names. This vector must be the 
#' same length as the number of covariates and cases specified  in 'var' arguments.
#' @param pop only needed if "cases" is passed as argument. the name of the
#' variable that identifies the population.
#' Necessary if type = "incidence".
#' @param pt the scale of the person-time (default 100,000) for incidence rates. 
#' only needed if type = "incidence".
#' @param palette when character vector with length of the number of variables 
#' to be plotted, custom palettes for each plot; when single character, same 
#' palette is applied to all plots. 
#' @param ... Additional arguments passed to `plot_function`.
#' @return a list of plots, each list element named after the variable plotted
#' @noRd
.plot_multiple_map <- function(plot_function, 
                               data, 
                               var, 
                               type,
                               var_label = NULL, 
                               pop = NULL,
                               pt = 100000,
                               palette = "IDE1",  
                               ...) {
  
  # Check if var is provided and is a non-empty vector
  if (is.null(var) || length(var) == 0) {
    stop("'var' must be a non-empty vector of variable names.")
  }
  
  # Ensure var contains only valid column names
  if (!all(var %in% names(data))) {
    stop("One or more elements in 'var' are not found in 'data'.")
  }
  
  # Ensure type contains only valid strings
  if (!all(type %in% c("cov", "counts", "inc"))) {
    stop("One or more elements in 'var' are not found in 'data'.")
  }
  
  # Check that type and var_label length matches var's
  expected_length <- length(var) 
  if (length(type) != expected_length) {
    stop("'type' must have the same length as 'var'")
  }
  if (!is.null(var_label) && length(var_label) != expected_length) {
    stop("'var_label' must have the same length as 'var'")
  }
  
  # Check if palette is either length 1 or the expected length
  if (!is.null(palette)) {
    if (!is.character(palette)) {
      stop("'palette' must be a character vector.")
    }
    if (!(length(palette) == 1 || length(palette) == expected_length)) {
      stop(paste("'palette' must be either length 1 (same for all plots) or match the number of variables.",
                 "Expected length:", expected_length, "but got", length(palette)))
    }
  }
  
  # Initialize an empty list to store plots
  all_plots <- list()
  
  # Loop over each covariate and generate a plot
  for (i in seq_along(var)) {
    v <- var[i]
    t <- type[i]
    
    # var_label 
    var_label_new <- if (!is.null(var_label)) var_label[i] else v
    
    # Adjust index for palettes (use same if single, else per variable)
    palette_index <- if (length(palette) == 1) 1 else i 
    var_palette <-  palette[[palette_index]] 
    
    var_plot <- plot_function(
      data = data,
      var = v,
      type = t,
      pop = pop,
      pt = pt,
      var_label = var_label_new,
      palette = var_palette,  # Pass specific palette
      ...
    )
    
    # Store the plot with the variable name as the key
    all_plots[[paste0("var", i)]] <- var_plot  
  }
  
  return(all_plots)
}


