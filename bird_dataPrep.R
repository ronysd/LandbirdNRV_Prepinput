## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.

defineModule(sim, list(
  name = "bird_dataPrep",
  description = "This module  downloads and prepares variables for NRV calculation in Landbird modeling, integrating greenup, domancy, Canada roads, SCANFI, Wetland and Topography data",
  keywords = c("NRV", "bird modeling", "habitat suitability", "predictors"),
  authors = structure(list(list(given = c("Sourav", ""), family = "Das", role = c("aut", "cre"), email = "souravdron@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(bird_dataPrep = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "bird_dataPrep.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "terra", "reproducible", "googledrive","dplyr"),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA, "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaRas", "SpatRaster", NA, NA, NA, "Study area raster, based on the common projection raster."),
    defineParameter(".seed", "list", list(), NA, NA, "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "studyAreaRas",objectClass =  "SpatRaster", desc = "Study area raster defining spatial extent."),
    expectsInput(objectName ="greenupURL", objectClass = "character", desc ="URL for greenup data.",
                 sourceURL ="https://drive.google.com/drive/folders/1S7tF7vlWuCsKNcm9B8-9sdoX68Ewfq9G"),
    expectsInput(objectName ="dormancyURL", objectClass = "character", desc ="URL for dormancy data.",
                 sourceURL ="https://drive.google.com/drive/folders/1aF2bNq5emngUN4zWahz-awCixqA5qV6x"),
    expectsInput(objectName ="wetlandsURL", objectClass = "character", desc ="URL for wetland data.",
                 sourceURL ="https://drive.google.com/drive/folders/1pcBxwuR8ZM6bslF8oWyZo6sM8yOtbzU8"),
    expectsInput(objectName ="topographyURL", objectClass = "character", desc ="URL for topography data.",
                 sourceURL ='https://drive.google.com/drive/folders/1tOtA6gqNN55xGiGuxTG6YNHLF3r3QvRK'),
    expectsInput(objectName ="roadID", objectClass = "character", "google URL ID for the csv with Canadian road network shapefile URLs for different years.", 
                 sourceURL = "https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/grnf000r10a_e.zip"),
    expectsInput(objectName = "climateNormalURL", objectClass = "character", desc = "Google Drive folder ID for Climate Normal data.",
                 sourceURL = "https://drive.google.com/drive/folders/1FP2hWm9VzIofnycRAGmNDwj-X5VPEdsw"),
    expectsInput(objectName = "climateAnnualURL", objectClass = "character", desc = "Google Drive folder ID for Annual Climate data.",
                 sourceURL = "https://drive.google.com/drive/folders/1GSkhN8-zJijMAZUn_TTGIDtShVN0R6JQ"),
    expectsInput(objectName ="hfURL", objectClass = "character", "URL for Canadian human footprint (disturbance proxy) data.", 
                 sourceURL =  "https://drive.google.com/drive/folders/1PsqXP-FYrdQrZ3uEkyR8BiyQOn3wZCW1"),
    expectsInput(objectName ="SCANFIurls", objectClass = "character", desc = "Google Drive URL for Processed SCANFI (or unprocessed, if theres any, remember to change processed =TRUE/FALSE for the processSCANFI call) biomass raster datasets.", 
                 sourceURL ="https://drive.google.com/drive/folders/1zF0PozF8j7u3K6x8gMblFwAZ41ZhmVPs")
  ),
  outputObjects = bindrows(
    # This needs modification, as currently the output is only matched and static, and later, the stacked layer
    createsOutput(objectName ="greenupProcessed", objectClass = "list", desc= "Processed greenup raster with focal mean applied."),
    createsOutput(objectName ="dormancyProcessed", objectClass = "list", desc= "Processed dormancy raster with focal mean applied."),
    createsOutput(objectName ="wetlandsProcessed_1km", objectClass = "SpatRaster", desc= "Processed wetland raster with focal mean applied."),
    createsOutput(objectName ="topographyProcessed", objectClass = "list", desc= "Processed topography raster with focal mean applied."),
    createsOutput(objectName ="roadProcessed", objectClass = "list", "Rasterized road density with 5km focal mean applied."),
    createsOutput(objectName = "climateNormal", objectClass = "SpatRaster", desc = "Climate normal variables at 1km resolution."),
    createsOutput(objectName = "climateAnnual", objectClass = "SpatRaster", desc = "Annual climate variables at 1km resolution."),
    createsOutput(objectName ="hfProcessed", objectClass = "list", "Rasterized human footprint at 1km resolution and with 5x5 focal mean applied."),
    createsOutput("SCANFIProcessed", objectClass = "list", desc= "Processed SCANFI biomass data at 1km resolution and with 5x5 focal mean applied.")
  )
))

doEvent.bird_dataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ## Initialize and process the data
      sim <- Init(sim)
      
      ## Schedule future events if needed
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "bird_dataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "bird_dataPrep", "save")
    },
    plot = {
      ## Function to handle plots
      plotFun(sim)
    },
    save = {
      ## Function to handle saving outputs
      sim <- Save(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  dPath <- "~/tmp/"
  
  # Greenup Processing - 1km (or to study area res and extent)
  sim$match$greenupProcessed$greenup_1km <- processGreenupDormancy(sim$greenupURL,out$studyAreaRas,
                                                                   varPrefix = "StandardGreenup") ## VarPrefix renames the layer to make them model-ready
  
  # Dormancy Processing - 1km (or to study area res and extent)  
  
  sim$match$dormancyProcessed$dormancy_1km <-processGreenupDormancy(sim$dormancyURL,out$studyAreaRas,
                                                                    varPrefix = "StandardDormancy") ## VarPrefix renames the layer to make them model-ready
  
  ## Road Processing - 1km and 5km
  
  sim$match$roadProcessed <- processROAD(sim$roadID, sim$studyAreaRas)
  
  ## SCANFI Processing - 1km and 5km
  sim$match$SCANFI_processed <- processSCANFI(sim$SCANFIurls, sim$studyAreaRas, processed=TRUE)
  #browser()
  
  #Process  climate annual data
  
  sim$match$climateAnnual <-processCLIMATE(sim$climateAnnualURL, sim$studyAreaRas)
  
  ### process CCNL (artifical light) data, this is from GEE, so using the processed variable
  
  sim$match$CCNL <- processCCNL(sim$CCNLurls, sim$studyAreaRas)
  
  ### process SCANFI and MODIS LCC data, SCANFI could be processed TRUE or FALSE, for MODIS, currently only processed=TRUE
  
  sim$match$MODIS <- processLCC(sim$MODISurls,out$studyAreaRas,processed = TRUE)
  
  sim$match$VLCE <- processLCC(sim$VLCEurls,out$studyAreaRas,processed = TRUE)
  
  sim$match$SCANFILCC <- processLCC(sim$SCANFILCCurls,out$studyAreaRas,processed = FALSE)
  
  ## process climate normal data
  sim$static$climateNormal <-processCLIMATE(sim$climateNormalURL, sim$studyAreaRas)
  ## Human Footprint (HF) Processing - 1km and 5km
  sim$static$hfProcessed$hf_1km <- prepInputs(
    url = sim$hfURL,
    fun = "terra::rast", destinationPath = dPath, to = sim$studyAreaRas
  ) |> Cache()
  names(sim$static$hfProcessed$hf_1km) <- "CanHF_1km"
  sim$static$hfProcessed$hf_5km <- terra::focal(sim$static$hfProcessed$hf_1km, 
                                                w = matrix(1, 5, 5), fun = mean, na.rm = TRUE) |> Cache()
  names(sim$static$hfProcessed$hf_5km) <- "CanHF_5x5"  
  #browser()
  
  
  sim$static$wetlandsProcessed <- processWETLANDS(sim$wetlandsURL, sim$studyAreaRas, processed = TRUE)
  
  ## Topography Processing - 1km and 5km
  
  sim$static$topographyProcessed <- processTopography(
    url = sim$topographyURL,sim$studyAreaRas, processed = TRUE)
  # sim$static$topographyProcessed$topography_1km <- prepInputs(
  #   url = sim$topographyURL,
  #   fun = "terra::rast", destinationPath = dPath, to = sim$studyAreaRas
  # ) |> Cache()
  # 
  # sim$static$topographyProcessed$topography_5km <- terra::focal(sim$static$topographyProcessed$topography_1km, 
  #                                                  w = matrix(1, 5, 5), fun = mean, na.rm = TRUE) |> Cache()
  # 
  
  
  ## Prepare data table for creating the raster stack that will beused to project the model
  
  sim$lag_df <- readExtractionLag(url = sim$lagURL, destinationPath = "~/bird_NRV/inputs/")
  
  # Extract variable metadata
  sim$vars_available <- extractAvailableVariables(sim)
  
  # Build stacks for all years
  sim$stack_list <- buildRasterStackAllYears(
    outSim = sim,
    lag_df = sim$lag_df,
    vars_available = sim$vars_available
  )
  return(invisible(sim))
}

Save <- function(sim) {
  ## Example save function
  return(invisible(sim))
}

plotFun <- function(sim) {
  ## Example plot function
  return(invisible(sim))
}



.inputObjects <- function(sim) {
  dPath <- "~/tmp/"
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("studyAreaRas", sim)) {
    sim$studyAreaRas <- Cache(prepInputs,
                              url = extractURL("studyAreaRas"),
                              destinationPath = dPath,
                              fun = "terra::rast",
                              userTags = c("object:studyAreaRas"))
  }
  
  if (!suppliedElsewhere("greenupURL", sim)) {
    sim$greenupURL <- "https://drive.google.com/drive/folders/1S7tF7vlWuCsKNcm9B8-9sdoX68Ewfq9G"
  }
  
  if (!suppliedElsewhere("dormancyURL", sim)) {
    sim$dormancyURL <- "https://drive.google.com/drive/folders/1aF2bNq5emngUN4zWahz-awCixqA5qV6x"
  }
  
  if (!suppliedElsewhere("wetlandsURL", sim)) {
    sim$wetlandsURL <- "https://drive.google.com/drive/folders/1qneFtxUG00PNIZjy3mnPyy3wwiI2st90"
  }
  
  if (!suppliedElsewhere("topographyURL", sim)) {
    sim$topographyURL <- "https://drive.google.com/drive/folders/1poL9tnzWfvSjFn7uBJkRi1b7crF-HCLJ"
  }
  
  if (!suppliedElsewhere("roadID", sim)) {
    sim$roadID <- "1jIJ4MyBAY8CMgqvh45gkTCGkX-AcpX0N"
  }
  
  if (!suppliedElsewhere("climateNormalURL", sim)) {
    sim$climateNormalURL <- "https://drive.google.com/drive/folders/1FP2hWm9VzIofnycRAGmNDwj-X5VPEdsw"
  }
  if (!suppliedElsewhere("climateAnnualURL", sim)) {
    sim$climateAnnualURL <- "https://drive.google.com/drive/folders/1GSkhN8-zJijMAZUn_TTGIDtShVN0R6JQ"
  }
  
  if (!suppliedElsewhere("hfURL", sim)) {
    sim$hfURL <- "https://drive.google.com/drive/folders/1PsqXP-FYrdQrZ3uEkyR8BiyQOn3wZCW1"
  }
  
  if (!suppliedElsewhere("SCANFIurls", sim)) {
    sim$SCANFIurls <- "https://drive.google.com/drive/folders/1zF0PozF8j7u3K6x8gMblFwAZ41ZhmVPs"
    
  }
  if (!suppliedElsewhere("CCNLurls", sim)) {
    sim$CCNLurls <- "https://drive.google.com/drive/folders/1faup1xdWMomP_4vD3yJhD8Ge2gFGCtyn"
  }
  
  if (!suppliedElsewhere("MODISurls", sim)) {
    sim$MODISurls <- "https://drive.google.com/drive/folders/1GYuIuzIvPrq9Wu8r4oJiJrhwftcTc07k"
  }
  
  if (!suppliedElsewhere("VLCEurls", sim)) {
    sim$VLCEurls <- "https://drive.google.com/drive/folders/1QFoPIDYVO_BwjphWdYy0mpjQGfdlj9qb"
  }
  
  if (!suppliedElsewhere("SCANFILCCurls", sim)) {
    sim$SCANFILCCurls <- "https://drive.google.com/drive/folders/19rEnpGJWnTDq1BCWOu0oJPI0I_O3ugK5"
  }
  
  if (!suppliedElsewhere("sim$lagURL", sim)) {
    sim$lagURL <- "https://drive.google.com/drive/folders/1YthVvq2u3PxUJcmomrAbfr5fJWcpOxIn"
  }
  return(invisible(sim))
}
