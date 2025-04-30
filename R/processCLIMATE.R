processCLIMATE <- function(url, studyAreaRas) {
  library(terra)
  library(reproducible)
  
  dPath <- "~/tmp/"
  dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
  
  message("Loading climate data from: ", url)
  
  ras <- prepInputs(
    url = url,
    fun = quote({
      tfp <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
      b <- terra::rast(tfp)
      names_b <- gsub("\\.tif$", "", basename(tfp)) 
      
      # renaming climate Normal_1991_2020_ files
      names_b <- ifelse(
        grepl("^Normal_1991_2020_", names_b),
        {
          cleaned_name <- gsub("^Normal_1991_2020_", "", names_b)
          cleaned_name <- gsub("(Tave|PPT|DD)_([a-z0-9]+)", "\\1\\2", cleaned_name)  # Remove underscore in patterns like DD_0
          ifelse(grepl("_1km$", cleaned_name), cleaned_name, paste0(cleaned_name, "_1km"))  # Add _1km if missing
        },
        # For other files, append _1km only if not already present
        ifelse(grepl("_1km$", names_b), names_b, paste0(names_b, "_1km"))
      )
      
      names(b) <- names_b
      b
    }),
    destinationPath = dPath,
    to = studyAreaRas
  ) |> Cache()
  
  return(ras)
}
