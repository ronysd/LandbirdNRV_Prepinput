library(reproducible)

out <- SpaDES.project::setupProject(
  updateRprofile = FALSE,
  name = "prepinput_bird",
  useGit = FALSE,
  #require = c("PredictiveEcology/LandR@development (>= 1.1.1.9001)"),
  paths = list(projectPath = "~/bird_NRV/",
               modulePath = file.path("modules"),
               cachePath = file.path("cache"),
               scratchPath = tempdir(),
               inputPath = file.path("inputs"),
               outputPath = file.path("outputs")
  ),
  modules = "bird_dataPrep" 
  ## Module that can predict/forecast the models (from BRT model objects) with respective variables will be added here
  ## Later other module ecosystems i.e. LandR, LandR.CS, FireSence, scfm will be included here, which could provide the cohort data
  ## for extracting climate sensitive/insensitive biomass growth 
  ,
  params = list(
    .globals = list(.studyAreaName = "BCR60", ## change based on BCR zone for the study area
                    dataYear = 2011,
                    sppEquivCol = "LandR",
                    .plots = c("png")),
    scfmDriver = list(targetN = 3000, #default is 4000 - more adds time + precision
                      .useParallelFireRegimePolys = 4) #requires snow
  ),
  options = list(#spades.allowInitDuringSimInit = TRUE,
    spades.allowSequentialCaching = TRUE,
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = 1,
    gargle_oauth_email = "souravdron@gmail.com",
    gargle_oauth_cache = "~/.secret",
    gargle_oauth_client_type = "web",
    reproducible.useMemoise = TRUE
  ),
  packages = c('RCurl', 'XML', 'snow', 'googledrive','terra', 'httr2', 'sf', 'dplyr',"terra", "PredictiveEcology/reproducible@AI (HEAD)"),
  times = list(start = 2000, end = 2022), # may not need this for dataprep module?
  studyArea = {
    targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                       "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    BCRdata <- prepInputs(url="https://drive.google.com/drive/folders/108j7UNteBM63gyYqEjRbL4jSsF1UwBiB",destinationPath = 'inputs/',fun = "terra::vect")|> Cache()
    BCR60 <-  BCRdata[ BCRdata$subUnit == "60",] #
    BCR60 <- terra::project(BCR60, targetCRS)
    #BCR60 <- terra::vect(BCR60)
  },
  studyAreaRas = {
        rtml<- terra::rast(studyArea, res = c(1000, 1000))
        terra::crs(rtml) <- terra::crs(studyArea)
        rtml[] <- 1
        rtml <- terra::mask(rtml, studyArea)
      }
  
)

outSim <- do.call(SpaDES.core::simInitAndSpades, out)







