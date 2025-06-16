processROAD <- function(csv_file_id, studyAreaRas) {
  dPath <- "~/tmp/road_data/"
  
  # Download, read and filter CSV 
  local_csv_path <- tempfile(fileext = ".csv")
  drive_download(as_id(csv_file_id), path = local_csv_path, overwrite = TRUE)
  
  roadData <- read.csv(local_csv_path, fileEncoding = "UTF-8-BOM")
  roadData <- subset(roadData, Dataset == "Canada road") ## filtering can be done based on what we need, i.e. US road
  roadData$year <- as.numeric(roadData$year)
  
  roadRasterList <- list()
  
  # Loop through all available years, extract the URL and apply prepinput
  for (i in seq_len(nrow(roadData))) {
    year <- roadData$year[i]
    road_url <- roadData$url[i]
    
    roadVect <- prepInputs(
      url = road_url,
      fun = "terra::vect",
      destinationPath = dPath,  # what should be the temp path?
      projectTo = studyAreaRas
    ) |> Cache()
    
    roadRaster_1km <- rasterizeGeom(roadVect, studyAreaRas, fun = "length", unit = "km") |> Cache()
    roadRaster_5km <- terra::focal(roadRaster_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE) |> Cache()
    names(roadRaster_1km) <- paste0("canroad_1km_",year)
    names(roadRaster_5km) <- paste0("canroad_5x5_",year)
    roadRasterList[[as.character(year)]] <- list(
      canroad_1km = roadRaster_1km,
      canroad_5x5 = roadRaster_5km
    )
  }
  
  return(roadRasterList)
}
