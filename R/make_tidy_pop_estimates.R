#' Make tidy pop estimates
#' 
#' @description This function returns a tidy dataset of population estimates for Puerto Rico. It pulls data from the US Census Bureau population estimates and the  United Nations [2022 Revision of World Population Prospects](https://population.un.org/wpp/). 
#' 
#' @param year numeric argument. It refers to the years for which the user wants population estimates. Earliest year is 1950 and can go beyond the actual year, in which case it provides UN population projections. 
#' @param municipio logical argument. Defaults to FALSE. If TRUE, it returns populations estimates by municipio for preselected age groups. 
#' @param un_data logical argument. Defaults to FALSE. If TRUE, it returns population estimates from the UN only. Otherwise it provides US Census Bureau population estimates when available.
#' @param census_key character argument. The census key is provided by the US Census Bureau. To obtain a census key, please visit [](https://api.census.gov/data/key_signup.html).
#' 
#' @example 
#' 
#' make_tidy_pop_estimates(c(2017:2019), census_key)
#' 
#' @export
make_tidy_pop_estimates <- function(year, municipio = FALSE, un_data = FALSE,
                                      census_key) {
  
  source(file.path(system.file("inst/script", package = "puertorico"), "make-pop-estimates.R"))
  
  
  table_type = NULL
  group = FALSE 
  
  if (municipio) {
    variables = c("AGEGROUP", "SEX", "GEONAME", "POP")
    subproduct = "charagegroups"
  } else {
    variables = c("AGE", "SEX", "POP")
    subproduct = "charage"
  }
  
  if (un_data) {
    start_year <- min(year)
    end_year <- max(year)
    temp_un <- get_un_data(start_year, end_year) |>
      filter(sex != "Both sexes", variant == "Median") |>
      mutate(year = as.numeric(timeLabel)) |>
      mutate(age = as.numeric(ageStart)) |>
      mutate(sex = recode(sex, `Female`="F", `Male`="M")) |>
      select(age, sex, value, year) |>
      setNames(c("age","gender","estimate","year"))
    out <- temp_un
  } else {
    
    if(any(year<2000)){
      years <- year[year<2000]
      start_year <- min(years)
      end_year <- max(years)
      
      temp_un <- get_un_data(start_year, end_year) |>
        filter( sex != "Both sexes", variant == "Median") |>
        mutate(year = as.numeric(timeLabel)) |>
        mutate(age = as.numeric(ageStart)) |>
        mutate(sex = recode(sex, `Female`="F", `Male`="M")) |>
        select(age, sex, value, year) |>
        setNames(c("age","gender","estimate","year"))
    } else {temp_un <- NULL}
    
    if  (any(year <= 2014)) {
      year_api <- year[year<=2014]
      temp_x <- map_df(year_api, function(y) {
        tmp <- get_census_data(
          product = NULL,
          subproduct = NULL,
          year = y,
          variables = NULL,
          municipio = municipio,
          group = group,
          table_type = table_type,
          census_key = census_key
        )
        tmp <- tmp[[1]]
        return(tmp)
      })
    } else {temp_x <- NULL} 
    
    if (any(year < 2020) & any(year > 2014)) {
      year_api <- year[!(year %in% c(2020, 2021)) & year > 2014]
      temp <- map_df(year_api, function(y) {
        tmp <- get_census_data(
          product = "pep",
          subproduct = subproduct,
          year = y,
          variables = variables,
          municipio = municipio,
          group = group,
          table_type = table_type,
          census_key = census_key
        )
        tmp <- tmp[[1]]
        tmp$year <- as.character(y)
        return(tmp)
      })
    }
    
    
    if (all(year >= 2020) & municipio == TRUE){
      popest_muni_url <-
        "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-72.csv"
      
      out <- read.csv(popest_muni_url,
                      fileEncoding = "ISO-8859-1") |>
        select(NAME, YEAR, matches("AGE.+_[FEM|MALE]")) |>
        rename(municipio = "NAME", year = "YEAR") |>
        filter(year != 1) |>
        mutate(year = recode(as.character(year),  `2` = "2020", `3` = "2021")) |>
        mutate(municipio = str_remove(municipio, " Municipio")) |>
        pivot_longer(cols = c(-municipio,-year)) |>
        mutate(name = str_remove(name, "AGE")) |>
        separate(name, c("age", "gender")) |>
        mutate(age = recode(
          age,
          `04` = "0004",
          `59` = "0509",
          `513` = "0513",
          `85PLUS` = "85Inf"
        )) |>
        pivot_wider(names_from = age, values_from = value) |>
        select(
          municipio,
          year,
          gender,
          "0004",
          "0509",
          "1014",
          "1519",
          "2024",
          "2529",
          "3034",
          "3539",
          "4044",
          "4549",
          "5054",
          "5559",
          "6064",
          "6569",
          "7074",
          "7579",
          "8084",
          "85Inf"
        ) |>
        filter(!(gender %in% c("TOT"))) |>
        mutate(gender = recode(gender, MALE = "M", FEM = "F")) |>
        pivot_longer(-c("municipio", "gender", "year"),
                     names_to = "age",
                     values_to = "poblacion") |>
        mutate(start = as.numeric(str_extract(age, "\\d{2}")),
               end = as.numeric(str_remove(age, "\\d{2}"))) |>
        select(municipio, start, end, gender, poblacion, year)
    } else if (all(year>=2020) & municipio ==FALSE){
      
      popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/puerto-rico/asrh/prc-est2021-syasex.xlsx"
      
      out <- read.xlsx(popest_sya_url,
                       fillMergedCells = T, startRow = 5, check.names = T,
                       rows = c(1:93)) |>
        select(X1, matches(".1|.2"), -matches("total")) |>
        rename(age = X1) |>
        rename_with(~gsub(pattern = "\\.2", replacement = "_2021", .x)) |>
        rename_with(~gsub(pattern = "\\.1", replacement = "_2020", .x)) |>
        filter(!(age %in% c("Total")) & !is.na(age)) |>
        mutate(age = str_remove(age, "."),
               age = str_remove(age, "\\+")) |>
        pivot_longer(cols = c(-age), names_to = "gender", values_to = "estimate") |>
        separate(gender, c("gender","year")) |>
        mutate(gender = recode(gender, Male = "M", Female = "F")) |>
        mutate(estimate = as.numeric(estimate),
               age = as.numeric(age))
    } else if (any(year %in% c(2020, 2021)) &
               any(year < 2020) &
               municipio==TRUE) {
      popest_muni_url <-
        "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-72.csv"
      
      popest_muni_tidy <- read.csv(popest_muni_url,
                                   fileEncoding = "ISO-8859-1") |>
        select(NAME, YEAR, matches("AGE.+_[FEM|MALE]")) |>
        rename(municipio = "NAME", year = "YEAR") |>
        filter(year != 1) |>
        mutate(year = recode(as.character(year),  `2` = "2020", `3` = "2021")) |>
        mutate(municipio = str_remove(municipio, " Municipio")) |>
        pivot_longer(cols = c(-municipio,-year)) |>
        mutate(name = str_remove(name, "AGE")) |>
        separate(name, c("age", "gender")) |>
        mutate(age = recode(
          age,
          `04` = "0004",
          `59` = "0509",
          `513` = "0513",
          `85PLUS` = "85Inf"
        )) |>
        pivot_wider(names_from = age, values_from = value) |>
        select(
          municipio,
          year,
          gender,
          "0004",
          "0509",
          "1014",
          "1519",
          "2024",
          "2529",
          "3034",
          "3539",
          "4044",
          "4549",
          "5054",
          "5559",
          "6064",
          "6569",
          "7074",
          "7579",
          "8084",
          "85Inf"
        ) |>
        filter(!(gender %in% c("TOT"))) |>
        mutate(gender = recode(gender, MALE = "M", FEM = "F")) |>
        pivot_longer(-c("municipio", "gender", "year"),
                     names_to = "age",
                     values_to = "poblacion") |>
        mutate(start = as.numeric(str_extract(age, "\\d{2}")),
               end = as.numeric(str_remove(age, "\\d{2}"))) |>
        select(municipio, start, end, gender, poblacion, year)
      
      out <- temp |>
        select(-state, -county) |>
        setNames(c("agegroup", "gender", "municipio", "poblacion", "year")) |>
        mutate(agegroup = as.numeric(agegroup)) |>
        filter(agegroup %in% c(1:18), gender %in% c("1", "2")) |>
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) |>
        mutate(agegroup = case_when(agegroup == 1 ~ "0004", agegroup == 2 ~ "0509", 
                                    agegroup == 3 ~ "1014", agegroup == 4 ~ "1519", 
                                    agegroup == 5 ~ "2024", agegroup == 6 ~ "2529", 
                                    agegroup == 7 ~ "3034", agegroup == 8 ~ "3539", 
                                    agegroup == 9 ~ "4044", agegroup == 10 ~ "4549", 
                                    agegroup == 11 ~ "5054", agegroup == 12 ~ "5559", 
                                    agegroup == 13 ~ "6064", agegroup == 14 ~ "6569", 
                                    agegroup == 15 ~ "7074", agegroup == 16 ~ "7579", 
                                    agegroup == 17 ~ "8084", agegroup == 18 ~ "85Inf")) |>
        mutate(start = as.numeric(str_extract(agegroup, "\\d{2}")),
               end = as.numeric(str_remove(agegroup, "\\d{2}"))) |>
        mutate(municipio = str_remove(municipio, " Municipio, Puerto Rico")) |> 
        select(-agegroup) |> 
        mutate(poblacion = as.numeric(poblacion)) |>
        full_join(popest_muni_tidy)
      
    } else if (any(year %in% c(2020, 2021)) &
               any(year < 2020) &
               municipio==FALSE) {
      
      popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/puerto-rico/asrh/prc-est2021-syasex.xlsx"
      
      popest_sya_tidy <- read.xlsx(popest_sya_url,
                                   fillMergedCells = T, startRow = 5, check.names = T,
                                   rows = c(1:93)) |>
        select(X1, matches(".1|.2"), -matches("total")) |>
        rename(age = X1) |>
        rename_with(~gsub(pattern = "\\.2", replacement = "_2021", .x)) |>
        rename_with(~gsub(pattern = "\\.1", replacement = "_2020", .x)) |>
        filter(!(age %in% c("Total")) & !is.na(age)) |>
        mutate(age = str_remove(age, "."),
               age = str_remove(age, "\\+")) |>
        pivot_longer(cols = c(-age), names_to = "gender", values_to = "estimate") |>
        separate(gender, c("gender","year")) |>
        mutate(gender = recode(gender, Male = "M", Female = "F")) |>
        mutate(estimate = as.numeric(estimate),
               age = as.numeric(age))
      
      out <- temp |> 
        select(AGE, SEX, POP, year) |>
        setNames(c("age", "gender", "estimate", "year")) |>
        filter(gender %in% c("1", "2"), !(age %in% c("999"))) |>
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) |>
        mutate(age = as.numeric(age), 
               estimate = as.numeric(estimate)) |>
        full_join(popest_sya_tidy, by = c("age", "gender", "estimate", "year"))
      
    } else if (!any(year %in% c(2020, 2021)) &
               municipio == TRUE) {
      out <- temp |>
        select(-state, -county) |>
        setNames(c("agegroup", "gender", "municipio", "poblacion", "year")) |>
        mutate(agegroup = as.numeric(agegroup)) |>
        filter(agegroup %in% c(1:18), gender %in% c("1", "2")) |>
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) |>
        mutate(agegroup = case_when(agegroup == 1 ~ "0004", agegroup == 2 ~ "0509", 
                                    agegroup == 3 ~ "1014", agegroup == 4 ~ "1519", 
                                    agegroup == 5 ~ "2024", agegroup == 6 ~ "2529", 
                                    agegroup == 7 ~ "3034", agegroup == 8 ~ "3539", 
                                    agegroup == 9 ~ "4044", agegroup == 10 ~ "4549", 
                                    agegroup == 11 ~ "5054", agegroup == 12 ~ "5559", 
                                    agegroup == 13 ~ "6064", agegroup == 14 ~ "6569", 
                                    agegroup == 15 ~ "7074", agegroup == 16 ~ "7579", 
                                    agegroup == 17 ~ "8084", agegroup == 18 ~ "85Inf")) |>
        mutate(start = as.numeric(str_extract(agegroup, "\\d{2}")),
               end = as.numeric(str_remove(agegroup, "\\d{2}"))) |>
        mutate(municipio = str_remove(municipio, " Municipio, Puerto Rico")) |> 
        select(-agegroup) |> 
        mutate(poblacion = as.numeric(poblacion))
    } else if (!any(year %in% c(2020, 2021)) &
               municipio==FALSE &
               any(year>=2000)){
      out <- temp |>
        select(AGE, SEX, POP, year) |>
        setNames(c("age", "gender", "estimate", "year")) |>
        filter(gender %in% c("1", "2"), !(age %in% c("999"))) |>
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) |>
        mutate(age = as.numeric(age),
               estimate = as.numeric(estimate))
      
    } else {out <- NULL}
    
    out <- rbind(temp_un,temp_x, out)
    
  }
  return(out)
}