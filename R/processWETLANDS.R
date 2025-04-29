processWETLANDS <- function(wetlandsURL, studyAreaRas, processed = TRUE) {
  dPath <- "~/tmp/wetlands/"
  dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
  
  library(googledrive)
  library(terra)
  library(reproducible)
  
  main_folder_id <- sub(".*/folders/([^/]+)$", "\\1", wetlandsURL)
  drive_folders <- drive_ls(as_id(main_folder_id), type = "folder")
  
  if (processed) {
    message("Detected processed Wetlands: 1km and 5x5 folders.")
    result_list <- list()
    for (res in c("1km", "5x5")) {
      folder_id <- drive_folders$id[drive_folders$name == res]
      rasters <- prepInputs(
        url = paste0("https://drive.google.com/drive/folders/", folder_id),
        destinationPath = file.path(dPath, res),
        fun = quote({
          tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
          b <- terra::rast(tf)
          names(b) <- gsub("\\.tif$", "", basename(tf))
          b
        }),
        to = studyAreaRas
      ) |> Cache()
      result_list[[res]] <- rasters
    }
    return(result_list)
  } else {
    message("Detected URL for unprocessed wetland rasters")
    
    # Flat folder download
    rasters <- prepInputs(
      url = wetlandsURL,
      destinationPath = dPath,
      fun = quote({
        tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
        b <- terra::rast(tf)
        names(b) <- gsub("\\.tif$", "", basename(tf))
        b
      }),
      to = studyAreaRas
    ) |> Cache()
    
    # Split into 1km and 5x5 (focal mean)
    rasters_1km <- rasters
    rasters_5x5 <- terra::focal(rasters_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
    
    return(list("1km" = rasters_1km, "5x5" = rasters_5x5))
  }
}
