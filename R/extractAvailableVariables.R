
extractAvailableVariables <- function(outSim) {
  extract_from_raster <- function(raster_obj, path_prefix) {
    if (inherits(raster_obj, "SpatRaster")) {
      layer_names <- names(raster_obj)
      matches <- stringr::str_match(layer_names, "^(.+?)_?(\\d{4})?$")
      data.frame(
        source = path_prefix,
        full = layer_names,
        base = matches[, 2],
        year = as.integer(matches[, 3]),
        stringsAsFactors = FALSE
      )
    } else NULL
  }
  
  extract_from_list <- function(obj, path = "outSim") {
    result <- list()
    for (nm in names(obj)) {
      new_path <- paste0(path, "$", nm)
      val <- obj[[nm]]
      if (inherits(val, "SpatRaster")) {
        result[[new_path]] <- extract_from_raster(val, new_path)
      } else if (is.list(val)) {
        result[[new_path]] <- extract_from_list(val, new_path)
      }
    }
    do.call(rbind, result)
  }
  
  sources_to_check <- list(match = outSim$match, static = outSim$static)
  final <- do.call(rbind, lapply(names(sources_to_check), function(section) {
    extract_from_list(sources_to_check[[section]], path = paste0("outSim$", section))
  }))
  
  smt_additions <- final |>
    filter(base %in% c("ERAPPTsm_1km", "ERATavesm_1km")) |>
    mutate(base = gsub("sm", "smt", base))
  
  final <- bind_rows(final, smt_additions)
  rownames(final) <- NULL
  return(final)
}
