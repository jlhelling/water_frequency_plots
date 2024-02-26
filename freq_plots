library(terra)
library(sf)
library(tidyverse)
library(tidyterra)
library(ggpubr)

# set file path
folder_path <- "raster_PATH"
aoi_path <- "AOI_PATH"

# specify aoi to specific reach
# aoi <- st_read(aoi_path) |> filter(id_reach == "Eygue_7")

# List all TIFF files in the directory
tif_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)

# Load each TIFF file and store the raster objects in a list
rasters <- lapply(tif_files, rast)

# creates a frequencyplots based on a defined threshold
create_frequency_plot <- function(raster_list, threshold){
  
  # create classification-matrix: set values over threshold to 1, rest as 0
  m <- c(threshold, 1, 1,
         -1, threshold, 0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  # function to reclassify raster value based on ndwi
  reclassify_water <- function(raster_i){
    # crop to AOI
    raster_i <- raster_i |> crop(aoi |> st_transform(crs(rasters[[1]])), mask=TRUE)
    
    # reclassify
    reclassified <- classify(raster_i, rclmat, include.lowest=TRUE)
    
    return(reclassified)
  }
  
  rasters_reclassified <- lapply(rasters, reclassify_water)
  
  # SUm-up all rastervalues to create frequency map
  # Make sure rasters_reclassified is not empty
  if(length(rasters_reclassified) > 0) {
    # Use the first raster as the starting point
    sum_rast <- rasters_reclassified[[1]]
    
    # If there are more rasters, add them to the sum
    if(length(rasters_reclassified) > 1) {
      for(i in 2:length(rasters_reclassified)) {
        sum_rast <- sum_rast + rasters_reclassified[[i]]
      }
    }
  } else {
    stop("The list of rasters is empty.")
  }
  
  # set plot title
  title <- paste("Water threshold:", threshold)
  
  # Set maximum value of the color scale
  limit_max <- length(rasters_reclassified)
  
  # create plot
  plot <- ggplot() +
    geom_spatraster(data = sum_rast) +
    theme_void() +
    scale_fill_whitebox_c(
      palette = "deep",
      limits = c(0, limit_max),  
      n.breaks = 5, 
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      fill = "",
      title = title
    )+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  return(plot)
}

plot_0_1 <- create_frequency_plot(rasters, 0.1)
plot_0 <- create_frequency_plot(rasters, 0)
plot_01 <- create_frequency_plot(rasters, -0.1)
plot_02 <- create_frequency_plot(rasters, -0.2)
plot_03 <- create_frequency_plot(rasters, -0.3)
plot_04 <- create_frequency_plot(rasters, -0.4)

plots_together <- ggarrange(plot_0_1, plot_0, plot_01, plot_02, plot_03, plot_04, ncol = 3, nrow = 2)

ggsave(filename = paste(folder_path, "XXX_thresholds.png", sep=""), plot = plots_together, scale = 1, width = 15, height = 15, units = "cm")
