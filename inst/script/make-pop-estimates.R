## Functions used to make pop estimates ##

#### Pulling Puerto Rico Census Data from APIs ####

## product <- acs1 ; acs3, acs5,  
### product details here: https://api.census.gov/data.html
## acs1 only provides estimates for counties where population >= 65,000
## acs5 https://www.census.gov/data/developers/data-sets/acs-5year.html


get_un_data <- function(start_year, end_year){
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  library(purrr)
  
  base_url_UNPD <- "https://population.un.org/dataportalapi/api/v1"
  
  puerto_rico_iso3 <- "PRI"
  puerto_rico_iso2 <- "PR"
  puerto_rico_id <- "630"
  indicator_code <- "47"
  start_year <- start_year
  end_year <- end_year
  
  target <- paste0(base_url_UNPD, "/data/indicators/", indicator_code, 
                   "/locations/", puerto_rico_id, "/start/", start_year, "/end/", end_year)
  
  #call the API 
  response <- fromJSON(target)
  
  #get the table with data available in the first page
  df_UNPD <- response$data
  
  #until there are next pages available
  while (!is.null(response$nextPage)){
    #call the API for the next page
    response <- fromJSON(response$nextPage)
    #add the data of the new page to the data.frame with the data of the precious pages
    df_UNPD <- rbind(df_UNPD, response$data)
  }
  return(df_UNPD)
}



get_census_data <- function(product, subproduct, year, variables, municipio = F, group = F, 
                            table_type = NULL, census_key) {
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  library(purrr)
  
  if (any(year <= 2014) & !municipio){
    year_new <- year[year<=2014]
    if (any(year_new %in% c(2000:2009))){
      x<- GET(paste0("http://api.census.gov/data/2000/pep/int_charage?get=POP,SEX,AGE,DATE_&for=state:72&key=", census_key))
      
      data_census_x <- fromJSON(rawToChar(x$content), flatten=T) %>% 
        as.data.frame() %>%
        row_to_names(1) %>%
        filter(AGE != 999, !(DATE_ %in% c("1","12")), SEX != 0) %>%
        mutate(date = as.numeric(DATE_)) %>%
        mutate(year = case_when(date == 2 ~ 2000, 
                                date == 3 ~ 2001,
                                date == 4 ~ 2002,
                                date == 5 ~ 2003, 
                                date == 6 ~ 2004,
                                date == 7 ~ 2005,
                                date == 8 ~ 2006, 
                                date == 9 ~ 2007, 
                                date == 10 ~ 2008, 
                                date == 11 ~ 2009)) %>%
        select(AGE, SEX, POP, year) %>%
        setNames(c("age", "gender", "estimate", "year")) %>%
        filter(year %in% year_new) %>%
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
        mutate(age = as.numeric(age),
               estimate = as.numeric(estimate))
    } else {data_census_x = NULL}
    
    if (any(year_new %in% c(2010:2014))){
      y <- GET(paste0("https://api.census.gov/data/2014/pep/prcagesex?get=POP,SEX,STNAME,AGE,DATE_&for=state:72&key=", census_key))
      
      data_census_y<- fromJSON(rawToChar(y$content), flatten=T) %>% 
        as.data.frame() %>%
        row_to_names(1) %>%
        filter(AGE != 999, !(DATE_ %in% c("1","2")), SEX != 0) %>%
        mutate(date = as.numeric(DATE_)) %>%
        mutate(year = case_when(date == 3 ~ 2010,
                                date == 4 ~ 2011,
                                date == 5 ~ 2012, 
                                date == 6 ~ 2013, 
                                date == 7 ~ 2014)) %>%
        select(AGE, SEX, POP, year) %>%
        setNames(c("age", "gender", "estimate", "year")) %>%
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
        filter(year %in% year_new) %>%
        mutate(age = as.numeric(age),
               estimate = as.numeric(estimate))
    } else {data_census_y = NULL}
    
    data_census <- list(rbind(data_census_x, data_census_y))
    variables_information = NULL
    
  }
  
  if (any(year > 2014)){
    year = year[year>2014]
    if (year == 2018 & product == "pep" & municipio == FALSE){
      url <- paste0("https://api.census.gov/data/2018/pep/charage?get=AGE,SEX,POP&DATE_CODE=11&for=state:72&key=",
                    census_key)
      data_census <- GET(url)
      data_census <- fromJSON(rawToChar(data_census$content), flatten=T) %>% 
        as.data.frame() %>%
        row_to_names(1)
      variables_information <- NULL
    } else {
      
      url_base <- ifelse(is.null(table_type), 
                         paste0(c("http://api.census.gov/data", year, product, 
                                  subproduct), collapse = "/"), 
                         paste0(c("http://api.census.gov/data", year, product, 
                                  subproduct, table_type), collapse = "/"))
      
      if (group){ 
        url <- ifelse(product == "pep", 
                      paste0(url_base, "?get=", variables, "&for=state:72&key=", census_key),
                      paste0(url_base, "?get=group(", variables,")&for=county:*&in=state:72&key=", 
                             census_key))
      } else {
        
        
        variables <- ifelse(product=="pep" & subproduct == "charagegroups" & year == "2019", 
                            paste0(c( paste(str_replace(variables, "GEONAME", "NAME"))), collapse = ","), 
                            ifelse(product=="pep" ,
                                   paste0(c( paste(variables)), collapse = ","), ### verify if NAME is needed 
                                   paste0(c("NAME", "COUNTY", paste(variables)), collapse=",")))
        url <- ifelse(product == "pep" & municipio, 
                      paste0(url_base, "?get=",variables,"&for=county:*&in=state:72&key=", 
                             census_key), ifelse(product == "pep",
                                                 paste0(url_base, "?get=",variables,"&for=state:72&key=", census_key),
                                                 paste0(url_base, "?get=",variables,"&for=county:*&in=state:72&key=", 
                                                        census_key)))
        
        
        data_census <- GET(url)
      }
      
      if (product=="pep" & subproduct == "charagegroups" & year == "2019") {
        data_census <- fromJSON(rawToChar(data_census$content), flatten=T) %>% 
          as.data.frame() %>%
          row_to_names(1) %>%
          rename(GEONAME = "NAME")
      } else {
        data_census <- fromJSON(rawToChar(data_census$content), flatten=T) %>% 
          as.data.frame() %>%
          row_to_names(1)
      }
      
      variables_url <- paste0(url_base, "/variables")
      variables_information <- GET(variables_url)
      variables_information <- fromJSON(rawToChar(variables_information$content)) %>% 
        row_to_names(1) %>%
        as.data.frame() %>%
        filter(name %in% colnames(data_census))
    }
  }
  
  lista_censo <- list(data_census, variables_information)
  return(lista_censo)
}
