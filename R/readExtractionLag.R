readExtractionLag <- function(url, destinationPath) {
  require(reproducible)
  require(readxl)

  lag_df <- prepInputs(
    url = url,
    fun = readxl::read_excel,
    destinationPath = destinationPath,
    targetFile = "NationalModels_V5_VariableList.xlsx",
    sheet = "ExtractionLookup"
  )
  return(lag_df)
}
