processGreenupDormancy <- function(url, studyAreaRas, varPrefix = NULL) {
  dPath <- "~/tmp/"
  dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
  
  message("Loading raster data from: ", url)
  
  ras <- prepInputs(
    url = url,
    fun = quote({
      tfp <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
      b <- terra::rast(tfp)
      
      if (!is.null(varPrefix)) {
        original_names <- basename(tfp)
        
        # Extract 4-digit years from filenames
        years <- stringr::str_extract(original_names, "\\d{4}")
        new_names <- ifelse(!is.na(years),
                            paste0(varPrefix, "_1km_", years),
                            paste0(varPrefix, "_1km"))
        
        names(b) <- new_names
      }
      
      b
    }),
    destinationPath = dPath,
    to = studyAreaRas
  ) |> Cache()
  
  return(ras)
}
