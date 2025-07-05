#' Correlation plot
#' 
#' @description Plots a correlation matrix of a series of variables.
#' 
#' @param data Data frame containing equally spaced (daily, weekly, monthly) 
#' covariate or disease case observations for one or multiple locations.
#' @param var Character vector containing variables in `data` to include in the
#' correlation matrix.
#' @param var_label Optional character vector of the same length as `var` 
#' containing custom names for the variables.
#' @param method Correlation computation method. Options include "pearson" 
#' (default), "spearman" or "kendall".
#' @param plot_type Character vector of length 2 indicating the type of plot to
#' use in the lower triangular and diagonal (1st element) and the upper triangular
#' (2nd element). Options include "circle", "number" and "raster".
#' @param scale Circle and number size multiplier, e.g. 1.1 increases
#' the size a 10% while 0.9 decreases it a 10%.
#' @param title Optional title of the plot. 
#' @param palette GHR, RColorBrewer or colorspace palette. Use "-" before the palette 
#' name (e.g., "-Reds") to reverse it.
#' @param print Logical. If TRUE, print the correlation matrix.
#' 
#' @return A plot of the correlation matrix.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Pearson correlation plot
#' plot_correlation(dengue_MS, 
#'                  method = "pearson",
#'                  var = c("dengue_cases","pop_density", 
#'                          "tmax", "tmin", "pdsi", "urban",
#'                          "water_network", "water_shortage"),  
#'                  var_label = c("dengue cases","pop. density", 
#'                                "max temp", "min temp", "drought index", "urbanization",
#'                                "water network", "water shortage"),
#'                  title = "Correlation matrix") 
#' 
#' # Print spearman correlation plot of type 'raster' and 'number' 
#' # with another palette 
#' plot_correlation(dengue_MS,
#'                  method = "spearman",
#'                  var = c("dengue_cases","pop_density", 
#'                          "tmax", "tmin", "pdsi", "urban",
#'                          "water_network", "water_shortage"),  
#'                  var_label = c("dengue cases","pop. density", 
#'                                "max temp", "min temp", "drought index", "urbanization",
#'                                "water network", "water shortage"),
#'                  plot_type = c("raster", "number"),
#'                  palette = "-Blue-Red 3") 
#' 

plot_correlation <- function(data,
                             var,
                             var_label = NULL, 
                             method = "pearson",
                             plot_type = c("circle", "number"), 
                             scale = 1,
                             title = NULL,
                             palette = "IDE1", 
                             print = FALSE) {
  
  args <- match.call() # Capture all arguments passed to the function
  
  # Check if required arguments are missing
  required_args <- c("data", "var")
  missing_args <- setdiff(required_args, names(args)) # Identify missing arguments
  
  if (length(missing_args) > 0) {
    stop(paste("Missing required argument(s):", paste(missing_args,
                                                      collapse = ", "
    )))
  }
  
  # Check dataset type
  if (!is.data.frame(data)) {
    stop("'data' should be a 'data.frame'")
  }
  
  # Ensure specified variables are numeric
  if (!all(sapply(data[, var], is.numeric))) {
    stop("'variables should be numeric")
  }
  
  # Use 'var' as labels if 'var_label' is not provided
  if (is.null(var_label) ) {
    var_label <- var 
  }
  
  # Check plot type
  if(length(plot_type)!=2){
    stop("plot_type must be a character vector of length 2")
  }else{
    if(any(!plot_type %in% c("raster", "number", "circle"))){
      stop("Accepted plot_type include: 'raster', 'number', 'circle'")
    }
  }

  # Compute correlation matrix
  correlation_matrix <- as.data.frame(stats::cor(data[, var],
                                                 method = method,
                                                 use = "complete.obs"))
  
  # Create a data frame 'cx' from correlation matrix and manipulate it further
  cx <- correlation_matrix
  
  # Capitalize the first letter of the correlation_matrix rownames
  rownames(correlation_matrix) <- sapply(rownames(correlation_matrix), function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  })
  
  colnames(correlation_matrix) <- sapply(colnames(correlation_matrix), function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  })
  
  # Add a column witht the rownames in data frame 'cx'
  cx$param = row.names(cx)
  
  # Create a data frame with only the covariates and their names 
  facs = unique(cx$param)
  
  facs = data.frame(param=facs,
                    pname = var_label)
  
  # Create a vector to order the variables by the 'var_label' parameter
  fac_ord = var_label
  
  # Convert to df
  cx <- cx |>
    dplyr::left_join(facs, by = "param") |>  # Add user-specified 'var_label' to the correlation data frame
    dplyr::select(-param) |>  # Remove param column 
    # Turn data into long format 
    tidyr::pivot_longer(-pname, names_to = "variable", values_to = "value") |>  
    dplyr::left_join(facs, by = c("variable" = "param")) |>  # Add user-specified 'names' again
    dplyr::rename(x = pname.x, y = pname.y) |>  # Rename repeat columns from join 
    dplyr::mutate(
      x = factor(.data$x, levels = fac_ord, ordered = TRUE),  # Order factors
      y = factor(.data$y, levels = rev(fac_ord), ordered = TRUE)  # Reverse order for y
    ) 
  
  # identify position
  cx$position <- mapply(function(x, y){
    if(which(levels(cx$x) == x) > which(levels(cx$x) == y)){
      "upper"
    }else{
      "lower"
    }
  }, cx$x, cx$y)
  cx_upper <- cx[cx$position=="upper",]
  cx_upper$position <- NULL
  cx_lower <- cx[cx$position=="lower",]
  cx_lower$position <- NULL
  
  # Select color palette
  my_palette <- GHR_palette(palette)(n = 50)
  
  # Create the plot
  out <- ggplot() +
    theme_bw() +
    coord_equal()
  
  # Add title if not NULL
  if(!is.null(title)){
    out <- out + ggplot2::ggtitle(title)
  }
  
  # Lower part
  if(plot_type[1]=="raster"){
    out <- out +
      geom_raster(data = cx_lower, 
                  aes(.data$x, .data$y, fill = .data$value), alpha = 0.95)
    
  }else if(plot_type[1]=="circle"){
    out <- out +
      geom_point(data = cx_lower, 
                 aes(.data$x, .data$y, color = .data$value, size = .data$value),
                 alpha = 0.95) +
      scale_size_continuous(limits = c(0, 1), range = c(scale*1, scale*10)) +
      guides(size = "none")
    
  }else if(plot_type[1]=="number"){
    out <- out +
      geom_label(data = cx_lower, 
                 aes(.data$x, .data$y, label = round(.data$value,2),
                     color = .data$value), 
                 size = scale*3, fill = "white", label.size = 0) 
  }
  
  # Upper part
  if(plot_type[2]=="raster"){
    out <- out +
      geom_raster(data = cx_upper, 
                  aes(.data$x, .data$y, fill = .data$value), alpha = 0.95)
    
  }else if(plot_type[2]=="circle"){
    out <- out +
      geom_point(data = cx_upper, 
                 aes(.data$x, .data$y, color = .data$value, size = .data$value), 
                 alpha = 0.95) +
      scale_size_continuous(limits = c(0, 1), range = c(scale*1, scale*10)) +
      guides(size = "none")
    
  }else if(plot_type[2]=="number"){
    out <- out +
      geom_label(data = cx_upper, 
                 aes(.data$x, .data$y, label = round(.data$value,2),
                     color = .data$value), 
                 size = scale*3, fill = "white", label.size = 0) 
  }
  
  # Common part
  out <- out  +
    scale_x_discrete(position="top") +
    ggplot2::scale_fill_gradientn(
      colors = my_palette, limits = c(-1, 1),
      name = paste0(
        toupper(substring(method, 1, 1)),
        substring(method, 2))) +
    ggplot2::scale_color_gradientn(
      colors = my_palette, limits = c(-1, 1),
      name = paste0(
        toupper(substring(method, 1, 1)),
        substring(method, 2))) +
    theme(plot.title = ggplot2::element_text(hjust = 0.5),
          axis.title = element_blank(),
          axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 0, size = rel(1.1)),
          axis.text.y = element_text(angle=0, size = rel(1.1))) +
    ggplot2::ggtitle(title)
  
  # IF only the correlation matrix is needed ----
  if(isTRUE(print)){
    print(correlation_matrix)
  }
  
  # Return the plot
  return(out)
  
}
