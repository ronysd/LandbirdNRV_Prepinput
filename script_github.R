

### SCANFI data downloadm SCANFI is seperated as the naming convention for SCANFI data is bit different, need to marge it with the other variables

##########################   Download and process SCANFI variables ########################



## Load packages
library(googledrive)
library(terra)
drive_auth()

##  setup and define directories
output_folder <- "./PredictionRasters/SCANFI_processed"
download_folder <- "./CovariateRasters/SCANFI"

# create directory
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
if (!dir.exists(download_folder)) dir.create(download_folder, recursive = TRUE)

##  define CRS and template raster, this is from BAM github scripts, could be linked with studyarea from spades?
EPSG.5072 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
rast1k <- rast(nrows=4527, ncols=7300, xmin=-4100000, xmax=3200000, ymin=1673000, ymax=6200000, crs = EPSG.5072)

##  Load Web Data Access URLs from csv located on the google drive
## csv url and path to store the csv
csv_file_id <- "1cz0UbXpfD14CU5RrOVYckMKbuNddFGdH_tUJBwScXnQ"
local_csv_path <- "./webDataAccess.csv"

# Download CSV file from Google Drive and store it in local drive for further use (the csv contains url for 
# Landfire biomass, height, CONUS, Landcover_hermosilla, peatland, roads etc)
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
### Function to process the rasters with specific requirements according to the elements of the variables list (variable_types)


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
    
    # Biomass processing - If requested, process Biomass only
    if (variable_type == "Biomass") {
      message(paste("Processing Biomass for year", year_val))
      
      # Reproject to 1km resolution
      raster_pj <- terra::project(raster_data, rast1k)
      
      # Compute 5km mean at 1km resolution
      raster_5k <- terra::focal(raster_pj, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      
      # Crop both rasters
      raster_1k_cp <- crop(raster_pj, ext(rast1k))
      raster_5k_cp <- crop(raster_5k, ext(rast1k))
      
      # Save raster outputs
      writeRaster(raster_1k_cp, file.path(output_folder, paste0("SCANFIbiomass1km_", year_val, ".tif")), overwrite = TRUE)
      writeRaster(raster_5k_cp, file.path(output_folder, paste0("SCANFIbiomass5x5_", year_val, ".tif")), overwrite = TRUE)
      
      next
    }
    
    # If species level percentage (sps) is requested only, check if Biomass exists, as biomass is needed to calculate species level Biomass
    if (variable_type == "sps") {
      biomass_raster_path <- file.path(output_folder, paste0("SCANFIbiomass1km_", year_val, ".tif"))
      
      # if biomass doesnt exist, process biomass first
      if (!file.exists(biomass_raster_path)) {
        message(paste("Biomass raster missing for year", year_val, "- Running Biomass processing first."))
        process_scanfi_rasters(input_folder, output_folder, "Biomass")
      }
      
      # now load the rasters
      if (file.exists(biomass_raster_path)) {
        biomass_raster <- rast(biomass_raster_path)
        raster_pj <- (raster_pj / 100) * biomass_raster  # Convert species percentage to biomass
      } else {
        warning(paste("Biomass raster still not found for year:", year_val, "Skipping:", outfile))
        next
      }
    }
    
    
    ## height processing
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
    
    # if (variable_type == "sps") {
    #   # Load the corresponding Biomass raster for that year
    #   biomass_raster_path <- file.path(output_folder, paste0("SCANFIbiomass1km_", year_val, ".tif"))
    #   #browser()
    #   if (file.exists(biomass_raster_path)) {
    #     biomass_raster <- rast(biomass_raster_path)
    #     # Convert species-level percentage to biomass by multiplying with total biomass
    #     raster_pj <- (raster_pj / 100) * biomass_raster
    #   } else {
    #     warning(paste("Biomass raster not found for year:", year_val, "Skipping:", outfile))
    #     next
    #   }
    # }
    
    ## crown closure processing
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



## process variables individually, to check
process_scanfi_rasters(download_folder, output_folder, "Biomass")





####### Working code for downloading and processing other variables except SCANFI


## Load Libraries
library(dplyr)
library(googledrive)
library(terra)

## Load Web Data CSV
csv_path <- "./webDataAccess.csv"
webData <- read.csv(csv_path, fileEncoding = "UTF-8-BOM")

##  define CRS and template raster, this is from BAM github scripts, could be linked with studyarea from spades?
EPSG.5072 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
rast1k <- rast(nrows=4527, ncols=7300, xmin=-4100000, xmax=3200000, ymin=1673000, ymax=6200000, crs = EPSG.5072)


## This is done here manually, but the list can be extended or replaced with new ID/urls and uploaded as csv with all required informations. currently the code works for google drive IDs and web urls for shapefiles 
new_entries <- data.frame(
  Dataset = c("Human_Footprint", "Greenup", "Wetland","Dormancy", "ClimateNormal","Topography"),
  year = c("N/A", "c(2000,2004,2009,2014,2019)", "N/A","c(2000,2004,2009,2014,2019)", "1991_2020","N/A"),  
  url = c("1zxCWCVbxreX6E6JpRcxVKvWD8WtnJkqG",  # HF
          "1-4D_UM4VQT-7_93e8_FVVTCZZQ1hJdI0",  # Greenup
          "1k4vjkrdBo4kCVlNGut9mw4vjax49qJqT",  # Wetland
          "1-AHhUtLL9dr0rD0IjV3KtmB7UDWAFdf8", # Dormancy
          "1mG6g8sJVsHScGKxbW9X1xz6ExvsNv1oj",  # ClimateNormal
          "1-CJ9bAMNhSqMk04CksdDin2-w5JFqcs_"), # Topography
  source_type = rep("googledrive id", 6)  # All are from Google Drive
)

# add new entries to existing dataset
webData <- bind_rows(webData, new_entries)



## Function to Define Download Paths dynamically
define_download_paths <- function(base_path, datasets) {
  paths <- setNames(file.path(base_path, datasets), datasets)
  lapply(paths, function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE))
  return(paths)
}

## Set Base Directory & Define Dataset Paths Dynamically
base_download_path <- "./CovariateRasters"
datasets <- c("Topography", "Human_Footprint", "Greenup", 
              "Dormancy", "Wetland", "ClimateNormal", "Canada road") #"SCANFI", 
download_paths <- define_download_paths(base_download_path, datasets)




############# Function to Download Rasters from Google Drive & Web ############# 
download_rasters <- function(dataset_name, download_folder, csv_path) {
  if (!dir.exists(download_folder)) dir.create(download_folder, recursive = TRUE)
  
  # Load webData CSV and extract dataset info
  dataset_info <- subset(webData, Dataset == dataset_name)
  
  if (nrow(dataset_info) == 0) {
    warning("Dataset not found in CSV: ", dataset_name)
    return(NULL)
  }
  
  # Extract parent ID or URL
  parent_id <- as.character(dataset_info$url[1])
  source_type <- as.character(dataset_info$source_type[1])
  
  if (is.na(parent_id) || parent_id == "") {
    warning("No Google Drive ID or URL found for dataset: ", dataset_name)
    return(NULL)
  }
  
  # Google drive download
  if (grepl("googledrive", source_type, ignore.case = TRUE)) {
    file_list <- drive_ls(as_id(parent_id))
    filenames <- file_list$name
    
    for (i in seq_along(file_list$id)) {
      download_path <- file.path(download_folder, filenames[i])
      
      if (!file.exists(download_path)) {
        message("Downloading: ", filenames[i])
        drive_download(as_id(file_list$id[i]), path = download_path, overwrite = TRUE)
      } else {
        message("Skipping (already exists): ", filenames[i])
      }
    }
    
    # Canada road dataset (download + Unzip)
  } else if (grepl("weblink", source_type, ignore.case = TRUE)) {
    options(timeout = 600)  # Increase timeout to 10 minutes
    
    yearlist <- sort(unique(dataset_info$year)) # Extract years
    
    for (yr in yearlist) {
      url <- dataset_info$url[dataset_info$year == yr]
      file_name <- basename(url)
      download_path <- file.path(download_folder, file_name)
      
      if (!file.exists(download_path)) {
        message("Downloading: ", file_name)
        download.file(url, download_path, method = "auto", mode = "wb")
        
        # Unzip file
        unzip_folder <- file.path(download_folder, as.character(yr))
        if (!dir.exists(unzip_folder)) dir.create(unzip_folder, showWarnings = FALSE)
        unzip(download_path, exdir = unzip_folder)
        file.remove(download_path)  # Remove ZIP after extraction, the problem with this is when the function reruns, 
                                    # it downloads the zip file again, even though the shapefile folders are there because of Unzip
      } else {
        message("Skipping (already exists): ", file_name)
      }
    }
  }
}
## Run download for all datasets in list
for (dataset_name in datasets) {
  download_rasters(dataset_name, download_paths[[dataset_name]], csv_path)
}



################### Process_raster function for all variable (Except SCANFI, trying to incorporate SCANFI with this)


library(terra)
library(sf)
library(dplyr)

# Function to define processed folder paths dynamically
define_processed_paths <- function(base_output, datasets) {
  paths <- setNames(file.path(base_output, paste0(datasets, "_processed")), datasets)
  lapply(paths, function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE))
  return(paths)
}


## latest process_rasters function V2 
library(terra)
library(sf)

process_rasters <- function(input_folder, output_folder, dataset_to_process = NULL) {
  dataset_types <- c("SCANFI", "Topography", "Human_Footprint", 
                     "Greenup", "Dormancy", "Wetland", 
                     "ClimateNormal", "Canada road")
  
  if (!is.null(dataset_to_process)) {
    dataset_types <- intersect(dataset_types, dataset_to_process)
  }
  
  for (dataset in dataset_types) {
    input_path <- file.path(input_folder, dataset)
    output_path <- file.path(output_folder, paste0(dataset, "_processed"))
    
    if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
    
    # Get files for dataset
    file_list <- list.files(input_path, full.names = TRUE, recursive = TRUE)
    
    for (file in file_list) {
      outfile <- basename(file)
      
      # process canada road (Shapefile)
      if (dataset == "Canada road" && grepl("\\.shp$", outfile)) {
        message(paste("Processing Canada road dataset:", outfile))
        
        ## Get the year name from the directory, as the file name doesn't contain year
        year_val <- basename(dirname(file))  
        
        roads <- st_read(file)  # Read shapefile
        roads <- st_transform(roads, crs = st_crs(rast1k))  # Reproject
        roads_vec <- vect(roads)  # Convert to terra format
        
        # Rasterize using to compute road length density
        road_1k <- rasterizeGeom(roads_vec, rast1k, fun="length", unit="km")
        
        # Apply 5km focal mean smoothing
        road_5k <- focal(road_1k, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
        
        # Save outputs
        writeRaster(road_1k, file.path(output_path, paste0("canroad_1km_", year_val, ".tif")), overwrite = TRUE)
        writeRaster(road_5k, file.path(output_path, paste0("canroad_5x5_", year_val, ".tif")), overwrite = TRUE)
        next
      }
      #  Process Dormancy and greenup 
      if (dataset == "Dormancy" || dataset == "Greenup") {
        # Extract year from filename
        year_val <- as.numeric(gsub(".*_(\\d{4})\\.tif$", "\\1", outfile))
        if (is.na(year_val)) {
          warning(paste("Unable to extract year for", dataset, "from:", outfile))
          next
        }
        #browser()
        # Load raster
        raster_data <- rast(file)
        
        # Reproject to 1km resolution
        raster_pj <- terra::project(raster_data, rast1k)
        
        # Compute focal mean for 5km smoothing
        raster_5k <- terra::focal(raster_pj, w = matrix(1,5,5), fun = mean, na.rm = TRUE)
        
        #Custom naming
        if (dataset == "Dormancy") {
          prefix <- "StandardDormancy"
        } else if (dataset == "Greenup") {
          prefix <- "StandardGreenup"
        }
        
        # save output
        writeRaster(raster_pj, file.path(output_path, paste0(prefix, "_1km_", year_val, ".tif")), overwrite = TRUE)
        writeRaster(raster_5k, file.path(output_path, paste0(prefix, "_5x5_", year_val, ".tif")), overwrite = TRUE)
        next
      }
      
      # Process ClimateNormal
      if (dataset == "ClimateNormal") {
        # Extract filename, not year as climateNormal is not for a specific year
        base_name <- gsub("Normal_1991_2020_", "", outfile)  
        base_name <- sub("\\.tif$", "", base_name)
        #browser()
        # Load raster
        raster_data <- rast(file)
        
        # Reproject to 1km resolution
        raster_pj <- terra::project(raster_data, rast1k)
        
        # Compute focal mean for 5k smoothing
        raster_5k <- terra::focal(raster_pj, w = matrix(1,5,5), fun = mean, na.rm = TRUE)
        
        # save output
        writeRaster(raster_pj, file.path(output_path, paste0(base_name, "_1km.tif")), overwrite = TRUE)
        writeRaster(raster_5k, file.path(output_path, paste0(base_name, "_5x5.tif")), overwrite = TRUE)
        next  # Skip to next file
      }
      
      #  General raster processing, for those raster that doesn't fall under the specific categories mentioned above, 
      #  now trying to incorporate SCANFI with this also
      if (grepl("\\.tif$", outfile)) {
        raster_data <- rast(file)
        
        # Extract year if applicable
        year_val <- as.numeric(gsub(".*_(\\d{4})_.*\\.tif$", "\\1", outfile))
        if (is.na(year_val)) year_val <- "unknown"
        
        # Reproject to 1km resolution
        raster_pj <- terra::project(raster_data, rast1k)
        
        ## Process SCANFI, still need to work on this
        if (dataset == "SCANFI") {
          if (grepl("height", outfile)) {
            # Compute CV for height
            cv <- function(x) {
              y <- na.omit(sample(x, size = 10, replace = FALSE))
              sd(y) / mean(y)
            }
            raster_cv <- aggregate(raster_pj, fact = 33, fun = cv)
            raster_cv_pj <- terra::project(raster_cv, rast1k, res = 1000, method = "bilinear", align = TRUE)
            raster_5k_cv <- terra::focal(raster_cv_pj, w = matrix(1,5,5), fun = mean, na.rm = TRUE)
            
            writeRaster(raster_cv_pj, file.path(output_path, paste0("SCANFI_heightCV_1km_", year_val, ".tif")), overwrite = TRUE)
            writeRaster(raster_5k_cv, file.path(output_path, paste0("SCANFI_heightCV_5x5_", year_val, ".tif")), overwrite = TRUE)
            next
          }
          
          if (grepl("sps", outfile)) {
            biomass_raster_path <- file.path(output_folder, "SCANFI_processed", paste0("SCANFIbiomass1km_", year_val, ".tif"))
            if (file.exists(biomass_raster_path)) {
              biomass_raster <- rast(biomass_raster_path)
              raster_pj <- (raster_pj / 100) * biomass_raster
            } else {
              warning(paste("Biomass raster not found for year:", year_val, "Skipping:", outfile))
              next
            }
          }
          
          if (grepl("closure", outfile)) {
            message(paste("Processing Crown Closure for year", year_val, "- Applying focal mean smoothing."))
          }
        }
        
        ## Processing applicable to all rasters
        # Compute focal mean
        raster_5k <- terra::focal(raster_pj, w = matrix(1,5,5), fun = mean, na.rm = TRUE)
        
        # Crop to extent
        raster_1k_cp <- crop(raster_pj, ext(rast1k))
        raster_5k_cp <- crop(raster_5k, ext(rast1k))
        
        # Write processed files
        writeRaster(raster_1k_cp, file.path(output_path, paste0(sub(".tif", "_1km.tif", outfile))), overwrite = TRUE)
        writeRaster(raster_5k_cp, file.path(output_path, paste0(sub(".tif", "_5x5.tif", outfile))), overwrite = TRUE)
      }
    }
  }
}



# Run the function
base_folder <- "./CovariateRasters"
output_base_folder <- "./PredictionRasters"
process_rasters(base_folder, output_base_folder, dataset_to_process = "Topography")


## for all variables
process_rasters(base_folder, output_base_folder)
