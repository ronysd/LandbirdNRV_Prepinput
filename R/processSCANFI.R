processSCANFI <- function(SCANFIurls, studyAreaRas) {
  dPath <- "~/tmp/SCANFI/"
  main_folder_id <- sub(".*/folders/([^/]+)$", "\\1", SCANFIurls)
  drive_folders <- drive_ls(as_id(main_folder_id), type = "folder")
  biomassRaster_list <- list()
  for (i in seq_len(nrow(drive_folders))) {
    subfolder_id <- drive_folders$id[i]
    subfolder_name <- drive_folders$name[i]
    subfolder_url <- paste0("https://drive.google.com/drive/folders/", subfolder_id)
    biomassRaster_list[[subfolder_name]] <- prepInputs(
      url = subfolder_url,
      fun = quote({
        tfp <- sort(targetFilePath)
        tfp <- tfp[grepl("\\.tif$", tfp)]
        biomassRasters <- terra::rast(tfp)
        names(biomassRasters) <- basename(tfp)
        biomassRasters
      }),
      destinationPath = dPath, to = studyAreaRas
    ) |> Cache()
  }
  return(biomassRaster_list)
}