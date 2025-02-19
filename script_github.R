
## Load packages
library(googledrive)
library(terra)
drive_auth()

##  setup and define directories
output_folder <- "./PredictionRasters/SCANFI_processed"
download_folder <- "./CovariateRasters/SCANFI"

# if directory doesnt exist, create
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
if (!dir.exists(download_folder)) dir.create(download_folder, recursive = TRUE)

##  define CRS and template raster, this is from BAM github scripts, could be linked with studyarea from spades?
EPSG.5072 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
rast1k <- rast(nrows=4527, ncols=7300, xmin=-4100000, xmax=3200000, ymin=1673000, ymax=6200000, crs = EPSG.5072)

##  Load Web Data Access URLs from csv located on the google drive
## csv url and path to store the csv
csv_file_id <- "1cz0UbXpfD14CU5RrOVYckMKbuNddFGdH_tUJBwScXnQ"
local_csv_path <- "./webDataAccess.csv"

# Download CSV file from Google Drive and store it in local drive for further use
drive_download(as_id(csv_file_id), path = local_csv_path, overwrite = TRUE)

## Read the csv to extract urls, this csv could also help to extract urls for outher sources, with the Dataset ID
webData <- read.csv(local_csv_path, fileEncoding="UTF-8-BOM")
SCANFI_lnd <- subset(webData, Dataset == "Biomass_SCANFI")
SCANFI_parentid <- SCANFI_lnd$url

##  Define a list of years and variable types to process
years <- c("SCANFI_2010", "SCANFI_2015")  # can add more as required
variable_types <- c("Biomass", "height", "sps", "closure")  # extendable list

## Function to Download Data from Google Drive
download_scanfi_files <- function(years, variable_types, parent_id, download_folder) {
  for (year in years) {
    dr_subf <- drive_ls(as_id(parent_id), pattern = year)
    for (var_type in variable_types) {
      dr_var <- drive_ls(as_id(dr_subf), pattern = var_type)
      for (i in 1:nrow(dr_var)) {
        if (endsWith(dr_var$name[i], "tif")) {
          download_path <- file.path(download_folder, dr_var$name[i])
          drive_download(as_id(dr_var$id[i]), path = download_path, overwrite = TRUE)
        }
      }
    }
  }
}

## Run Data Download
download_scanfi_files(years, variable_types, SCANFI_parentid, download_folder)




#### better function integrating year_val at the beginning
### Function to process the rasters with specific requirements accoring to the elements of the variables list (variable_types)


process_scanfi_rasters <- function(input_folder, output_folder, variable_type) {
  file_list <- list.files(path = input_folder, pattern = paste0(variable_type, "*"), full.names = TRUE)
  
  for (file in file_list) {
    outfile <- basename(file)
    raster_data <- rast(file)
    
    # Extract year from filename
    year_val <- as.numeric(gsub(".*_(\\d{4})_v\\d+\\.tif$", "\\1", outfile))
    
    # print if year is NA
    if (is.na(year_val)) {
      warning(paste("Unable to extract year from filename:", outfile))
      next
    }
    
    # Reproject to match the 1km resolution of the template raster, this could be study_area?
    raster_pj <- terra::project(raster_data, rast1k)
    
    ## Variable specific processing, this could be defined during the setup project? or by default it will run for all ##
    if (variable_type == "height") {
      # calculate CV
      cv <- function(x) {
        y <- na.omit(sample(x, size = 10, replace = FALSE))
        sd(y) / mean(y)
      }
      raster_cv <- aggregate(raster_pj, fact = 33, fun = cv)
      raster_cv_pj <- terra::project(raster_cv, rast1k, res = 1000, method = "bilinear", align = TRUE)
      raster_5k_cv <- terra::focal(raster_cv_pj, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
    }
    
    if (variable_type == "sps") {
      # Load the corresponding Biomass raster for that year
      biomass_raster_path <- file.path(output_folder, paste0("SCANFIbiomass1km_", year_val, ".tif"))
      #browser()
      if (file.exists(biomass_raster_path)) {
        biomass_raster <- rast(biomass_raster_path)
        # Convert species-level percentage to biomass by multiplying with total biomass
        raster_pj <- (raster_pj / 100) * biomass_raster
      } else {
        warning(paste("Biomass raster not found for year:", year_val, "Skipping:", outfile))
        next
      }
    }
    
    if (variable_type == "closure") {
      message(paste("Processing Crown Closure for year", year_val,"- Applying focal mean smoothing."))
    }
    
    # Compute smoothed version using focal mean (default treatment for all and this is done on 1km resolution)
    raster_5k <- terra::focal(raster_pj, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
    
    # Crop the rasters
    raster_1k_cp <- crop(raster_pj, ext(rast1k))
    raster_5k_cp <- crop(raster_5k, ext(rast1k))
    
    # Write processed rasters to output
    if (variable_type == "height") {
      writeRaster(raster_cv_pj, file.path(output_folder, paste0("SCANFI_heightcv_1km_", year_val, ".tif")), overwrite = TRUE)
      writeRaster(raster_5k_cv, file.path(output_folder, paste0("SCANFI_heightcv_5x5_", year_val, ".tif")), overwrite = TRUE)
    } else {
      writeRaster(raster_1k_cp, file.path(output_folder, paste0(sub(paste0(variable_type,"_"), "", sub("S_.*", "", outfile)), variable_type, "_1km_", year_val, ".tif")), overwrite = TRUE)
      writeRaster(raster_5k_cp, file.path(output_folder, paste0(sub(paste0(variable_type,"_"), "", sub("S_.*", "", outfile)), variable_type, "_5x5_", year_val, ".tif")), overwrite = TRUE)
    }
  }
}

# run the function for all variables
for (var_type in variable_types) {
  process_scanfi_rasters(download_folder, output_folder, var_type)
}



## process variables individually
process_scanfi_rasters(download_folder, output_folder, "height")




