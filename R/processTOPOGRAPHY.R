processTopography <- function(url, studyAreaRas, processed = NULL) {
  library(googledrive)
  library(terra)
  library(reproducible)
  
  dPath <- "~/tmp/Topography/"
  dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
  
  message("Loading Topography raster data from: ", url)
  
  # format file names
  cleanTopoNames <- function(names_vector) {
    names_vector <- gsub("\\.tif$", "", names_vector)
    names_vector <- gsub("_canada", "", names_vector, ignore.case = TRUE)
    names_vector <- gsub("^_", "", names_vector)
    return(names_vector)
  }
  
  if (processed) {
    topo_rasters <- prepInputs(
      url = url,
      fun = quote({
        tfp <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
        b <- terra::rast(tfp)
        names(b) <- gsub("\\.tif$", "", basename(tfp))
        b
      }),
      destinationPath = dPath,
      to = studyAreaRas
    ) |> Cache()
    
    return(topo_rasters)
    
  } else { ## if unprocessed
    topo_raw <- prepInputs(
      url = url,
      fun = quote({
        tfp <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
        b <- terra::rast(tfp)
        names(b) <- cleanTopoNames(basename(tfp))
        b
      }),
      destinationPath = dPath,
      to = studyAreaRas
    ) |> Cache()
    
    # Rename layers to add _1km
    names(topo_raw) <- paste0(names(topo_raw), "_1km")
    
    return(topo_raw)
  }
}
