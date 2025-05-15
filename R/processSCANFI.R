processSCANFI <- function(SCANFIurl, studyAreaRas, processed = TRUE) {
  library(googledrive)
  library(terra)
  library(reproducible)
  
  dPath <- "~/tmp/SCANFI/"
  dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
  
  # Standardize SCANFI layer names
  standardizeSCANFIlayerNames <- function(names_vector) {
    names_vector <- gsub("\\.tif$", "", names_vector)
    names_vector <- gsub("SCANFI_att_", "SCANFI", names_vector)
    names_vector <- gsub("SCANFI_sps_", "SCANFI", names_vector)
    names_vector <- gsub("_1km", "", names_vector)
    names_vector <- gsub("_5x5", "", names_vector)
    names_vector <- gsub("_S_", "_", names_vector)
    names_vector <- gsub("_v[0-9]+$", "", names_vector)
    return(names_vector)
  }
  
  
  drive_folders <- drive_ls(as_id(SCANFIurl), type = "folder")
  
  #  Processed SCANFI
  if (processed) {
    result_list <- list()
    for (res in c("1km", "5x5")) {
      folder_id <- drive_folders$id[drive_folders$name == res]
      tfp <- prepInputs(
        url = paste0("https://drive.google.com/drive/folders/", folder_id),
        destinationPath = file.path(dPath, res),
        fun = quote({
          tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
          b <- rast(tf)
          names(b) <- standardizeSCANFIlayerNames(basename(tf)) #, resolution = res
          names(b) <- sub("^(SCANFI[^_]+)(_?)([0-9]{4})$", paste0("\\1_", res, "_\\3"), names(b))
          
          b
        }),
        to = studyAreaRas
      )|> Cache()
      result_list[[res]] <- tfp
    }
    return(result_list)
  }
  
  # Unprocessed SCANFI
  variable_types <- c("biomass", "height", "sps", "closure")
  cv <- function(x) {
    y <- na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
    sd(y) / mean(y)
  }
  
  subfolders <- drive_folders$name
  result_list <- list("1km" = list(), "5x5" = list())
  
  for (subfolder in subfolders) {
    year <- gsub("SCANFI_", "", subfolder)
    folder_id <- drive_folders$id[drive_folders$name == subfolder]
    
    message("Processing year: ", year)
    files <- prepInputs(
      url = paste0("https://drive.google.com/drive/folders/", folder_id),
      destinationPath = file.path(dPath, subfolder),
      fun = quote({
        tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
        rast(tf)
      }),
      to = studyAreaRas
    )|> Cache()
    
    for (i in 1:nlyr(files)) {
      name_i <- names(files)[i]
      ras_i <- files[[i]]
      
      var_type <- dplyr::case_when(
        grepl("biomass", name_i, ignore.case = TRUE) ~ "biomass",
        grepl("height", name_i, ignore.case = TRUE) ~ "height",
        grepl("sps", name_i, ignore.case = TRUE) ~ "sps",
        grepl("closure", name_i, ignore.case = TRUE) ~ "closure",
        TRUE ~ NA_character_
      )
      if (is.na(var_type)) next
      
      ras_1km <- project(ras_i, studyAreaRas)
      ras_5x5 <- focal(ras_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      
      if (var_type == "height") {
        ras_cv <- aggregate(ras_1km, fact = 33, fun = cv)
        ras_cv_1km <- project(ras_cv, studyAreaRas)
        ras_cv_5x5 <- focal(ras_cv_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
        
        result_list[["1km"]][[paste0("SCANFIheightcv_1km_", year)]] <- ras_cv_1km
        result_list[["5x5"]][[paste0("SCANFIheightcv_5x5_", year)]] <- ras_cv_5x5
      }
      
      if (var_type == "sps" && paste0("SCANFIBiomass_1km_", year) %in% names(result_list[["1km"]])) {
        biomass_ras <- result_list[["1km"]][[paste0("SCANFIBiomass_1km_", year)]]
        ras_1km <- (ras_1km / 100) * biomass_ras
        ras_5x5 <- focal(ras_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      }
      #browser()
      base_name <- standardizeSCANFIlayerNames(name_i)
      base_name_1km <- sub("^(SCANFI[^_]+)(_?)([0-9]{4})$", paste0("\\1_", "1km", "_\\3"), base_name)
      base_name_5x5 <- sub("^(SCANFI[^_]+)(_?)([0-9]{4})$", paste0("\\1_", "5x5", "_\\3"), base_name)
      result_list[["1km"]][[base_name_1km]] <- ras_1km
      result_list[["5x5"]][[base_name_5x5]] <- ras_5x5
      
    }
  }
  
  return(list("1km" = rast(result_list[["1km"]]), "5x5" = rast(result_list[["5x5"]])))
}
