## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

## ----setup, results='hide', message=FALSE-------------------------------------
# Load GHRexplore
library(GHRexplore)

# Other necessary libraries for the vignette
library(dplyr)
library(sf)

## ----load data----------------------------------------------------------------
data("dengue_MS")
glimpse(dengue_MS)

## ----load boundaries----------------------------------------------------------
data("map_MS")
glimpse(map_MS)

## ----IDE palettes, fig.width=6------------------------------------------------
GHR_palettes()

## ----ts1, fig.width=7---------------------------------------------------------
plot_timeseries(dengue_MS, var = "tmin", type = "cov", 
                var_label = "Minimum temp.",
                time = "date", area = "micro_code")

## ----ts2, fig.width=7, fig.height=5-------------------------------------------
plot_timeseries(dengue_MS, var = "tmin", type = "cov", var_label = "Minimum temp.",
                time = "date", area = "micro_code", facet = TRUE)

## ----ts3, fig.width=7---------------------------------------------------------
plot_timeseries(dengue_MS, var = "tmin", type = "cov", var_label = "Minimum temp.",
                time = "date", area = "micro_code", highlight = "50001",
                title = "Micro code 50001")

## ----ts4, fig.width=7---------------------------------------------------------
plot_timeseries(dengue_MS, var = "tmin", type = "cov", var_label = "Minimum temp.",
                time = "date", area = "micro_code", aggregate_space = "meso_code")

## ----ts5, fig.width=7---------------------------------------------------------
plot_timeseries(dengue_MS, var = "dengue_cases", type = "counts",
                time = "date", area = "micro_code", aggregate_space = "meso_code",
                transform = "log10p1")

## ----ts6, fig.width=7---------------------------------------------------------
plot_timeseries(dengue_MS, var = "dengue_cases", type = "inc", pop = "population",
                time = "date", area = "micro_code", aggregate_space = "meso_code",
                pt = 1000, transform = "log10p1")

## ----heatmap1, fig.width=6, fig.height=5--------------------------------------
plot_heatmap(dengue_MS, var = "pdsi", type = "cov", var_label = "PDSI",
             time = "date", area = "micro_code",
             aggregate_space = "meso_code", palette = "-Vik", centering = 0) 

## ----heatmap2, fig.width=6, fig.height=5--------------------------------------
plot_heatmap(dengue_MS, var = "dengue_cases", type = "inc", pop = "population",
             time = "date", area = "micro_code", aggregate_space = "meso_code",
             title= "Monthly Incidence", transform = "log10p1") 

## ----seasonality, fig.width=7, fig.height=4-----------------------------------
plot_seasonality(dengue_MS, var = "tmin", var_label = "Minimum temperature",
                 type = "cov", time = "date", area = "micro_code",   
                 aggregate_space = "meso_code") 

## ----map1, fig.width=5--------------------------------------------------------
plot_map(data = dengue_MS, var = "urban",  time = "date", type = "cov", area = "micro_code",  
         map = map_MS, map_area = "code", by_year = FALSE, var_label= "Urbanicity",
         palette = "-Heat")

## ----map2, fig.width=5, fig.height=4------------------------------------------
plot_map(dengue_MS, var = "dengue_cases", type = "inc", pop = "population", 
         pt = 1000, time = "date", area = "micro_code",  
         map = map_MS, map_area = "code", by_year = TRUE, 
         bins = 5, bins_method = "quantile", palette = "-Rocket")  

## ----map3, fig.width=4, fig.height=3------------------------------------------
plot_map(data = dengue_MS, var = "biome_name", type = "cov",
         time = "date", area = "micro_code", by_year = FALSE,
         map = map_MS, map_area = "code", var_label= "Biome")

## ----biv1, fig.width=5--------------------------------------------------------
plot_bivariate(dengue_MS, 
               var = c("tmax", "pdsi"), 
               var_label = c("Max. temp", "PDSI"),
               area = "meso_code")

## ----biv2, fig.width=7, fig.height=5------------------------------------------
plot_bivariate(dengue_MS, 
               var = c("tmax", "pdsi"), 
               var_label = c("Max. temp", "PDSI"),
               area = "meso_code", 
               facet = TRUE, free_x_scale = TRUE, free_y_scale = TRUE)

## ----biv3, fig.width=6--------------------------------------------------------
plot_bivariate(dengue_MS, 
               var = c("biome_name", "tmax"), 
               var_label = c("Biome", "Max. temp"),
               area = "meso_code")

## ----corr1, fig.width=5, fig.height=4-----------------------------------------
plot_correlation(dengue_MS, 
                 var = c("dengue_cases","pop_density", "tmax", "tmin",
                         "pdsi", "urban", "water_network", "water_shortage")) 

## ----corr2, fig.width=5, fig.height=4-----------------------------------------
plot_correlation(dengue_MS, var = c("dengue_cases","pop_density", "tmax", "tmin",
                                    "pdsi", "urban", "water_network", "water_shortage"),  
                 method = "spearman", plot_type = c("number", "raster"),
                 palette = "RdBu") 

## ----compare, fig.width=7, fig.height=5---------------------------------------
plot_compare(plot_function = plot_timeseries,
             data = dengue_MS, 
             var = c("pdsi", "dengue_cases"),
             type = c("cov", "inc"), 
             var_lab = c("PDSI", "Dengue Incidence"),
             pop = "population",
             time = "date", 
             area = "micro_code", 
             aggregate_space = "meso_code",
             ncol=1, 
             combine_legend=TRUE)

## ----compare2, fig.width=7, fig.height=9--------------------------------------
plot_compare(plot_function = plot_heatmap,
             data = dengue_MS, 
             var = c("pdsi", "dengue_cases"),
             type = c("cov", "inc"), 
             var_lab = c("PDSI", "Incidence"),
             palette = c("Purp", "Reds"), 
             pop = "population",
             time = "date", 
             area = "micro_code", 
             aggregate_space = "meso_code",
             ncol=1)

