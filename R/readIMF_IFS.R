
#' Read IMF IFS data
#'
#' @author Benjamin Peeters
#'
readIMF_IFS <- function() {
  filePath <- file.path("v1", "IFS_03-07-2025 09-25-37-07_timeSeries.csv")

  dataInit <- utils::read.csv2(filePath, sep = ",") %>%
    filter(.data$Attribute == "Value", .data$Indicator.Code == "FIGB_PA") %>%
    select(-c(2, 3, 4, 5)) %>%
    rename("country" = "Country.Name", "baseYear" = "Base.Year")

  data <- tidyr::pivot_longer(dataInit, cols = matches("^X.*$"), names_to = "year") %>%
    filter(.data$value != "", grepl("^X[0-9]{4}$", .data$year)) %>%
    mutate("value" = as.numeric(.data$value), "year" = as.numeric(gsub("X", "", .data$year)))

  # remove baseYear
  data <- select(data, -c("baseYear"))

  # 3 dim: region, year, others
  x <- as.magpie(data, spatial = 1)

  return(x)
}


convertIMF_IFS <- function(x) {
  toAddLater <- c(
    "Euro Area", "Fiji, Rep_ of", "Kyrgyz Rep_", "Moldova, Rep_ of",
    "Netherlands, The", "Poland, Rep_ of", "Slovak Rep_", "Venezuela, Rep_ Bolivariana de"
  )

  # TODO: to remove 'ant' cleanly

  # xEurozone <- x["Euro Area", , ]
  x <- x[toAddLater, , , invert = TRUE]

  # convert to ISO3 codes
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1),
    mapping = c(
      "Armenia, Rep_ of" = "ARM",
      "Czech Rep_" = "CZE",
      "Ethiopia, The Federal Dem_ Rep_ of" = "ETH",
      "Korea, Rep_ of" = "KOR",
      "Slovenia, Rep_ of" = "SVN"
    ),
    warn = TRUE
  )
  # TODO: fill remaing countries and set `warn` to FALSE
  # TODO: deal with euro area


  # fill missing (NA or 0)
  x <- toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = "ANT")

  # TODO: To complete the list
  # eurozone <- c("DEU", "FRA", "BEL", "ITA", "ESP")
  # x[eurozone,,] <- xEurozone

  return(x)
}
