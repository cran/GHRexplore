#' Generate GHR color palettes
#'
#' @description Generates color palettes including custom, ColorBrewer and 
#' colorspace palettes. 
#' 
#' @param palette Name of the GHR, RcolorBrewer or colorspace palette. Use "-" 
#' before the palette name (e.g., "-Reds") to reverse it. A vector of custom
#' colors is also possible.
#' @param ncols Number of colors to sample.
#'
#' @return
#' GHR_palette() returns the function that generates the color palette and the
#' attribute 'na_color'.\cr
#' GHR_palettes() returns a plot with the custom GHR palettes.
#' 
#' @details See all available options by running `GHR_palettes()`,
#' `RColorBrewer::display.brewer.all()` and `colorspace::hcl_palettes(plot=TRUE)`.
#' 
#' @examples GHR_palette("IDE1", 5)(5)
#' 
#' @export

GHR_palette <- function(palette, ncols=30) {
  
  # Palette is a single string
  if(length(palette) == 1){
    
    # Check palette rev
    if(substr(palette, 1, 1) == "-"){
      revert <- TRUE
      palette <- substr(palette, 2, nchar(palette))
    }else{
      revert <- FALSE
    }
    
    # Project palettes (continuous)
    if (palette == "IDE1") {
      colorbar <- .interp_palette(c("#0b4232", "#15634d", "#168c81","#92e8d0", 
                                    "grey95",
                                    "#d4cbf5", "#b98afb", "#453975", "#291f52"),
                                  revert)
      attr(colorbar, 'na_color') <- 'grey50'
    }else if (palette == "Purp") {
      colorbar <- .interp_palette(c("grey95", "#d4cbf5", "#b98afb", 
                                    "#534687", "#291f52"), 
                                  revert)
      attr(colorbar, 'na_color') <- 'grey50'
    }else if (palette == "Green"){
      colorbar <- .interp_palette(c( "grey95", "#92e8d0", "#168c81", 
                                     "#15634d", "#0b4232"),
                                  revert)
      attr(colorbar, 'na_color') <- 'grey50'
    } else if (palette == "BlYlRd") {
      colorbar <- .interp_palette(c("#328ca1", "#F6EDBD", "#e3949e"), revert)
      attr(colorbar, 'na_color') <- 'grey50'
      
      # Project palettes (qualitative)
    }else if (palette=="IDE2") {
      colorbar <- c("#168c81", "#b98afb", 
                    "#92e8d0", "#302461", 
                    "#d4cbf5", "#0e5742",
                    "#6ac0fc", "#4242bd",
                    "#f2ebbb", "#edb76b")
      colorbar <- colorbar[1:min(ncols, length(colorbar))]
      colorbar <- .interp_palette(colorbar, revert)
      attr(colorbar, 'na_color') <- 'grey50'
      
    } else if (palette=="Colorblind") {
      colorbar<- c("#E69F00", "#56B4E9","#009E73",
                   "#F0E442","#0072B2", "#D55E00",
                   "#CC79A7", "#999999", "#000000")
      colorbar <- colorbar[1:min(ncols, length(colorbar))]
      colorbar <- .interp_palette(colorbar, revert)
      attr(colorbar, 'na_color') <- 'grey50'
      
      # RColorBrewer and colorspace palettes
    } else {
      
      # RColorBrewer: Get the maximum number of colors for each palette
      # (hence suppress warnings)
      colorbar <- tryCatch({
        suppressWarnings(
          .interp_palette(RColorBrewer::brewer.pal(ncols, name=palette), revert))},
        error = function(e) {NULL}
      )
      
      # Colorspace: Different functions for different palette types. Returns an 
      # error if it doesn't exist. Partial string matching of palette so need to
      # check in advance
      valid <- row.names(colorspace::hcl_palettes(plot = FALSE))
      if (palette %in% valid) {
        # Qualitative
        if(is.null(colorbar)){
          tryCatch({colorbar <- 
            .interp_palette(colorspace::qualitative_hcl(ncols, palette=palette), revert)}, 
            error = function(e) {NULL})      
        }
        # Sequential
        if(is.null(colorbar)){
          tryCatch({colorbar <- 
            .interp_palette(colorspace::sequential_hcl(ncols, palette=palette), revert)}, 
            error = function(e) {NULL})      
        }
        # Diverging
        if(is.null(colorbar)){
          tryCatch({colorbar <- 
            .interp_palette(colorspace::diverging_hcl(ncols, palette=palette), revert)}, 
            error = function(e) {NULL})      
        }
      }
      
      # R and hex colors
      if(is.null(colorbar) & all(class(try(col2rgb(palette), silent = TRUE))!="try-error")){
        colorbar <- .interp_palette(palette, revert)
      }
    }
  }
  
  # Palette is vector of colors
  if(length(palette) > 1){
    
    # R and hex colors
    if(all(class(try(col2rgb(palette), silent = TRUE))!="try-error")){
      
      colorbar <- .interp_palette(palette, rev = FALSE)
    }
  }
  
  # Invalid palette message
  if(is.null(colorbar)){
    stop("The selected palette is invalid. Please select a GHR, RColorBrewer,
           colorspace palette or a vector of valid colors.")
  }
  
  return(colorbar)
}

#' Show GHR color palettes
#' 
#' @description Creates a visualization of all custom GHR palettes.
#'
#' @rdname GHR_palette
#' @export
#'
#' @examples GHR_palettes()
GHR_palettes <- function(){
  
  # Helper to draw ramps
  draw_ramp <- function(palette){
    
    colors <-  GHR_palette(palette, 8)(8)
    df <- data.frame(color = colors, labels = seq_along(colors)) 
    
    ggplot2::ggplot(data = df) +
      ggplot2::geom_rect(ggplot2::aes(xmin = labels - 0.5, xmax = labels + 0.5,
                                      ymin = 0.5, ymax = 1.5, fill = I(colors)), 
                         color = "white") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::ggtitle(palette) +
      ggplot2::theme_void() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  # GHR palettes
  ghr_palettes <- cowplot::plot_grid(
    draw_ramp("IDE1"), draw_ramp("Purp"), draw_ramp("Green"),
    draw_ramp("BlYlRd"), draw_ramp("IDE2"), draw_ramp("Colorblind"),
    ncol = 3
  )
  
  print(ghr_palettes)
}


#' Interpolate and possibly reverse palette
#'
#' @param pal vector of hex codes that make up the palette 
#' @param rev boolean indicating whether the palette should be inverted
#'
#' @returns Interpolated palette
#' @noRd
.interp_palette <- function(pal, rev){
  
  if(isTRUE(rev)){
    pal <- rev(pal)
  }
  
  grDevices::colorRampPalette(pal)
}