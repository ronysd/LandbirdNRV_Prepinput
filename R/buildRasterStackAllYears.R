buildRasterStackAllYears <- function(outSim, lag_df, vars_available, years = seq(1985, 2020, 5)) {

  stack_list <- list()
  covariates <- unique(vars_available$base)
  year.out <- data.frame()

  for (cov in covariates) {
    vars_cov <- vars_available |> filter(base == cov, !is.na(year))

    if (nrow(vars_cov) == 0) {
      year.out <- bind_rows(year.out, data.frame(base = cov, year = NA, predyear = years))
    } else {
      is_lagged <- cov %in% lag_df$Label
      years_cov <- unique(vars_cov$year)
      if (is_lagged) years_cov <- years_cov + 1

      dt <- data.table(year = years_cov, val = years_cov)
      setattr(dt, "sorted", "year")
      setkey(dt, year)
      match_years <- dt[J(years), roll = "nearest"]$val
      if (is_lagged) match_years <- match_years - 1

      year.out <- bind_rows(year.out, data.frame(base = cov, year = match_years, predyear = years))
    }
  }

  matched_rows <- left_join(year.out, vars_available, by = c("base", "year")) |> filter(!is.na(full))

  for (prediction_year in years) {
    message("Building raster stack for year: ", prediction_year)
    vars_year <- matched_rows |> filter(predyear == prediction_year)
    loaded_rasters <- list()

    for (i in seq_len(nrow(vars_year))) {
      row <- vars_year[i, ]
      message(
        "Trying: ", row$base,
        if (!is.na(row$year)) paste0(" (year = ", row$year, ")"),
        " from ", row$source, " using layer: ", row$full
      )
      lyr <- tryCatch(
        {
          r <- outSim
          for (key in strsplit(row$source, "\\$")[[1]][-1]) {
            r <- r[[key]]
          }
          r <- r[[row$full]]
          names(r) <- row$base
          r
        },
        error = function(e) {
          message("Failed to extract ", row$base, " → ", e$message)
          NULL
        }
      )
      if (!is.null(lyr)) loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
    }

    if (length(loaded_rasters) == 0) next

    template_ras <- loaded_rasters[[1]]
    method_ras <- rast(template_ras)
    values(method_ras) <- "PC"
    names(method_ras) <- "method"
    year_ras <- rast(template_ras)
    values(year_ras) <- prediction_year
    names(year_ras) <- "year"

    stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
    stack_list[[as.character(prediction_year)]] <- stack_out

    message("stack built with ", nlyr(stack_out), " layers for ", prediction_year)
  }
  message("Finished building stacks for ", length(stack_list), " years.")
  return(stack_list)
}
