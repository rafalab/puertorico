#' Get tidy population estimates for Puerto Rico
#' 
#' @description This function returns a tidy dataset of population estimates for Puerto Rico. It pulls data from the US Census Bureau population estimates and the  United Nations 2022 Revision of World Population Prospects (\url{https://population.un.org/wpp/}). 
#' 
#' @param years numeric argument. It refers to the years for which the user wants population estimates.
#' @param product character argument. Refers to the USCB product to call: "acs" for American Community Survey, "pep" for Population Estimates and Projecttions, and "decennial" for Decennial Census. 
#' @param municipio.pep logical argument. Defaults to FALSE. If TRUE, it returns populations estimates (PEP) by municipio for preselected age groups. For ACS and Decennial products, municipio is always TRUE.
#' @param census_key character argument. The census key is provided by the US Census Bureau. To obtain a census key, please visit \url{https://api.census.gov/data/key_signup.html}.
#' @examples 
#' \dontrun{
#' # Example usage of the function with sensitive information
#' get_census_estimates(2011:2015, product = "acs", municipio.pep = FALSE, census_key = yourcensuskey)
#' }
#' @import data.table httr jsonlite stringr
#' @importFrom purrr map_df
#' @importFrom dplyr recode
#' @export
get_census_estimates <- function(years, product, municipio.pep = FALSE, census_key) {
  if (product == "decennial" & !all(years %in% c(2000, 2010, 2020))) {
    return(print("Decennial products are only available for 2000, 2010, 2020. Change years argument."))
  }
  if (product == "pep" & any(years <= 2019) & any(years >= 2020)) {
    return(print("Run two separate calls, one for years before 2020 and another one for 2020 onward."))
  }
  
  if (product == "acs" & any(years < 2009)) {
    return(print("This package calls 5-year ACS products which are available 2009 onwards. Change years argument."))
  }
  
  if (product == "pep") {
    if (any(years >= 2000) & any(years <= 2009)){
      years_pep <- 2000
      temp.1 <- purrr::map_df(years_pep, function(y) {
        tmp <- get_wrangle_estimates(
          product = product,
          year_input = y,
          municipio = municipio.pep,
          census_key = census_key
        )
        return(tmp)
      })
    } else {
      temp.1 <- NULL
    }
    
    if (any(years >= 2010) & any(years <= 2019)) {
      years_pep <- years
      temp.2 <- purrr::map_df(years_pep, function(y) {
        tmp <- get_wrangle_estimates(
          product = product,
          year_input = y,
          municipio = municipio.pep,
          census_key = census_key
        )
        return(tmp)
      })
    } else {
      temp.2 <- NULL
    }
    
    if (any(years >= 2020) & any(years <= 2023)){
      years_pep <- 2020
      temp.3 <- purrr::map_df(years_pep, function(y) {
          tmp <- get_wrangle_estimates(
            product = product,
            year_input = y,
            municipio = municipio.pep,
            census_key = census_key
          )
          return(tmp)
        })
    } else {
      temp.3 <- NULL
    }
    
   temp <- data.table(rbind(temp.1, temp.2, temp.3))
   temp <- temp[year %in% years]
   
  } else {
  temp <- purrr::map_df(years, function(y) {
    tmp <- get_wrangle_estimates(
      product = product,
      year_input = y,
      municipio = NULL,
      census_key = census_key
    )
    return(tmp)
  })
}
  return(temp)
}

