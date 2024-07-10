#' Helper Function 1: extract numeric values using regular expression
#'
#' @param text text from which to extract numeric values
#' @return numeric values in the text

extract_numeric_ranges <- function(text) {
numeric_values <- paste(str_match(text, "(\\d+)\\D*(\\d*)")[, 2:3], collapse = "-")
return(numeric_values)
}

#' Helper Function 2: make a data.table of variable names
#'
#' @param list_var list from which to create a data.table
#' @return  data.table

vars_names <- function(list_var) {
  x <- rbindlist(lapply(names(list_var), function(name) {
    # Create a data.table for each list element, including the name
    dt_item <- as.data.table(list_var[[name]])
    dt_item[, name := name]
  }), fill = TRUE)
  x <- x[,c("name", "label")]
  return(x)
}

#' Helper Function 3: wrangle ACS and Decennial Census data
#'
#' @param dat data as downloaded from USCB
#' @param product acs or decennial
#' @param variable_names variable data.table output from var_names function
#' @param year_input years in dat
#' @return  data.table

wrangle_dat <- function(dat, product = NULL, variable_names = NULL, year_input) {
  
  patterns = NAME = municipio = label = variable = estimate = moe = gender= agegroup = NULL # due to NSE notes in R CMD check
  
  if(product == "pep"){
    return(print("Use wrangle_pep function"))
  }
  
  if (product == "acs") {
    cols_end_with <- c("E", "M")
    pattern_var <- "B01001_"
    male_no <- c(3:25)
    female_no <- c(27:49)
  } else if (product == "decennial") {
    
    if (year_input == 2020) {
      male_no <- c(3:25)
      female_no <- c(27:49)
      cols_end_with <- "N"
      pattern_var <- "P12_0"
    } else if (year_input == 2010) {
      male_no <- c(3:25)
      female_no <- c(27:49)
      cols_end_with <- ""
      pattern_var <- "P0120"
    }  else if (year_input == 2000)  {
      male_no <- c(3:25)
      female_no <- c(27:49)
      cols_end_with <- ""
      pattern_var <- "P0120"
    }
  }
  
  ageRange_starts <- c( seq(0, 85, 5))
  ageRange_ends <- c(ageRange_starts[-1]-1, Inf)
  ageRange_levels <- paste(ageRange_starts, ageRange_ends, sep = "-")
  ageRange_levels[length(ageRange_levels)] <- paste0(ageRange_starts[length(ageRange_levels)],"Inf")
  
  
  dat <- as.data.table(dat)
  dat <- stats::setNames(dat, as.character(dat[1,]))
  dat <- dat[-1,]
  dat[, "municipio" := NAME]
  dat[, "NAME" := NULL]
  dat[, "GEO_ID" := NULL]
  dat[, "county" := NULL]
  dat[, "state" := NULL]
  dat[, "municipio" := str_remove(municipio, " Municipio, Puerto Rico")]
  
  if (length(cols_end_with) == 2) {
    cols_to_keep <- c("municipio", 
                      colnames(dat)[endsWith(colnames(dat),cols_end_with[1])],
                      colnames(dat)[endsWith(colnames(dat),cols_end_with[2])])
  } else {
    cols_to_keep <- c("municipio", 
                      colnames(dat)[endsWith(colnames(dat),cols_end_with)])
  }
  
  dat <- dat[, mget(cols_to_keep)]
  dat <- melt(dat,
              id.vars = "municipio", 
              measure.vars = patterns(pattern_var),
              variable.name = "variable",
              value.name = "estimate")
  
  dat <- merge(dat, variable_names, by.x = "variable", by.y = "name", all.x = T)
  
  if (product == "acs") {
    dat[, "label" := ifelse(is.na(label), "MOE", label)]
    dat <- dat[, "variable" := str_remove(variable,paste0(cols_end_with, collapse= "|"))]
    moe_dat <- dat[label == "MOE"]
    moe_dat[, "moe" := estimate]
    moe_dat[, "moe" := ifelse(moe == "-555555555", 0, moe)]
    moe_dat[, "estimate" := NULL]
    moe_dat[, "label" := NULL]
    est_dat <- dat[label != "MOE"]
    dat <- merge(moe_dat, est_dat, by = c("municipio","variable"))
    
  }
  
  dat[, "variable" := str_remove(variable, pattern_var)]
  
  
  if (all(cols_end_with != "") & product != "acs") {
    dat[, "variable" := as.numeric(str_remove(variable, cols_end_with))]
  } else {
    dat <- dat[!is.na(label)]
    dat[, "variable" := as.numeric(variable)]
  }
  dat[, "gender" := NA]
  dat[, "gender" := fcase(variable %in% male_no, "M", 
                        variable %in% female_no, "F",
                        default = NA)]
  dat[, "label" := str_remove(label, "Estimate!!Total:!!\\b(?:Female|Male)\\b:!!")]
  
  dat <- dat[!is.na(gender)]
  dat[, "agegroup" := sapply(label, extract_numeric_ranges)]
  dat[, "agegroup" :=  fifelse(agegroup == "5-", "0-4",
                             fifelse(agegroup == "85-", "85+", agegroup))]
  
  if (product == "acs") {
    dat <- dat[, c("municipio", "agegroup", "gender", "estimate", "moe")]
  } else {
    dat <- dat[, c("municipio", "agegroup", "gender", "estimate")]
  }
  return(dat)
  
}

#' Helper Function 4: wrangle PEP data
#'
#' @param dat data as downloaded from USCB
#' @param municipio.pep TRUE or FALSE
#' @param year_input years in dat
#' @return  data.table

wrangle_pep <- function(dat, municipio.pep, year_input) {
  
estimate = . =DATE_DESC = gender = NAME = municipio = YEAR = CTYNAME = age = ageRange = agegroup = NULL # due to NSE notes in R CMD check
  
  ageRange_starts <- c( seq(0, 85, 5))
  ageRange_ends <- c(ageRange_starts[-1]-1, Inf)
  ageRange_levels <- paste(ageRange_starts, ageRange_ends, sep = "-")
  ageRange_levels[length(ageRange_levels)] <- paste0(ageRange_starts[length(ageRange_levels)],"Inf")

  dat <- as.data.table(dat)
  if (year_input < 2020) {
    dat <- stats::setNames(dat, as.character(dat[1,]))
    dat <- dat[-1,]
    dat[, "year" :=  sub(".*([0-9]{4}).*", "\\1", DATE_DESC)]
    dat <- dat[!grepl("census|base", DATE_DESC, ignore.case = TRUE)]
    
    if (!municipio.pep) {
      dat <- dat[, c("SEX", "AGE", "year", "POP")]
      dat <- stats::setNames(dat, c("gender", "age", "year", "estimate"))
      dat <- dat[gender!=0 & age != 999]
      dat[, "gender" := dplyr::recode(gender, 
                             `1` = "M", 
                             `2` = "F")]
      dat <- dat[, "age" := as.numeric(age)]
    } else {
      old_substrings <- c("NAME", "GEONAME")
      new_name <- "municipio"
      
      dat <- rename_columns(dat, old_substrings, new_name)
      
      dat <- dat[, c("municipio","SEX", "AGEGROUP", "year", "POP")]
      dat[, "municipio" := str_remove(municipio, " Municipio, Puerto Rico")]
      dat <- stats::setNames(dat, c("municipio","gender", "agegroup", "year", "estimate"))
      ageRanges <- stats::setNames(ageRange_levels, 1:18)
      dat[, "ageRange" := ageRanges[agegroup]]
      dat <- dat[gender!=0 & !is.na(ageRange)]
      dat[, "gender" := dplyr::recode(gender, 
                             `1` = "M", 
                             `2` = "F")]
      dat[, "agegroup" := NULL]
    } 
    
  } else if (year_input >= 2020) {
    dat[, "municipio" := str_remove(CTYNAME, " Municipio")] 
    dat[, "year" := YEAR + 2020 - 2]
    dat <- dat[year >= 2020]
    dat <- dat[, c("year", "municipio", "AGE", "TOT_MALE", "TOT_FEMALE")]
    dat <- stats::setNames(dat, c( "year", "municipio", "age", "M", "F"))
    dat <- melt(dat,
                id.vars = c( "municipio", "age", "year"),
                variable.name = "gender", 
                value.name = "estimate")
    if (!municipio.pep) {
      dat <- dat[, keyby = .(age, year, gender), .(estimate = sum(estimate))]
    }
  }
  
  return(dat)
}

#' Helper Function 5: get and wrangle estimates by single year
#'
#' @param year_input numeric argument. It refers to the year for which the user wants population estimates.
#' @param product character argument. Refers to the USCB product to call: "acs" for American Community Survey, "pep" for Population Estimates and Projecttions, and "decennial" for Decennial Census. 
#' @param municipio.pep logical argument. Defaults to FALSE. If TRUE, it returns populations estimates by municipio for preselected age groups. 
#' @param census_key character argument. The census key is provided by the US Census Bureau. To obtain a census key, please visit \url{https://api.census.gov/data/key_signup.html}.
#' @return  data.table


get_wrangle_estimates <- function(year_input, product, municipio.pep, census_key) {
  
  if (product == "acs") {
    state_code <- 72
    api <- paste0("https://api.census.gov/data/", year_input,
                  "/acs/acs5?get=group(B01001)&for=county:*&in=state:", 
                  state_code,"&key=")
    x <- httr::GET(paste0(api, census_key))
    dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T) 
    vars <- jsonlite::fromJSON(paste0("https://api.census.gov/data/", year_input, "/acs/acs5/variables.json"), flatten = T)$variables
    var_names <- vars_names(vars)
    dat <- wrangle_dat(dat, product = "acs", variable_names = var_names)
  } else if (product == "decennial") {
    if (year_input %in%  c(2020)){ 
      api <- "https://api.census.gov/data/2020/dec/dhc?get=group(P12),NAME&for=county:*&in=state:72&key="
      x <- httr::GET(paste0(api, census_key))
      dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T) 
      vars <- jsonlite::fromJSON(paste0("https://api.census.gov/data/", year_input, "/dec/dhc/variables.json"), flatten = T)$variables
      
    } else if (year_input %in% c(2010)) {
      api <- "https://api.census.gov/data/2010/dec/sf1?get=group(P12)&for=county:*&in=state:72&key="
      x <- httr::GET(paste0(api, census_key))
      dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T) 
      vars <- jsonlite::fromJSON(paste0("https://api.census.gov/data/", year_input, "/dec/sf1/variables.json"), flatten = T)$variables
      
    } else if (year_input %in% c(2000)) {
      api <- "https://api.census.gov/data/2000/dec/sf1?get=group(P012),NAME&for=county:*&in=state:72&key="
      x <- httr::GET(paste0(api, census_key))
      dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T) 
      vars <- jsonlite::fromJSON(paste0("https://api.census.gov/data/", year_input, "/dec/sf1/variables.json"), flatten = T)$variables
    }
    var_names <- vars_names(vars)
    dat <- wrangle_dat(dat, product = "decennial", variable_names = var_names, year_input = year_input)
  } else if (product == "pep") {
    if (year_input %in% 2000:2009) {
      if (!municipio.pep) {
        api <- "https://api.census.gov/data/2000/pep/int_charage?get=GEONAME,SEX,AGE,AGE_DESC,POP,DATE_DESC,DATE_,LASTUPDATE&for=state:72&key="
        x <- httr::GET(paste0(api, census_key))
        dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T)
      } else {
        api <- "https://api.census.gov/data/2000/pep/int_charagegroups?get=GEONAME,AGEGROUP,SEX,POP,DATE_DESC&for=county:*&in=state:72&SEX!=0&key="
        x <- httr::GET(paste0(api, census_key))
        dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T)
      }
    } else if (year_input %in% 2010:2019) {
      if (!municipio.pep) {
        api <- "https://api.census.gov/data/2019/pep/charage?get=POP,AGE,SEX,DATE_CODE,DATE_DESC&for=state:72&key="
        x <- httr::GET(paste0(api, census_key))
        dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T)
      } else {
        api <- "https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP,AGEGROUP,SEX,DATE_CODE,DATE_DESC&for=county:*&in=state:72&key="
        x <- httr::GET(paste0(api, census_key))
        dat <- jsonlite::fromJSON(rawToChar(x$content), flatten=T)
      }
    } else if (year_input %in% 2020:2023) {
      year_input <- 2021
      url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/asrh/cc-est2023-syagesex.csv"
      dat <- utils::read.csv(url)  
    }
    dat <- wrangle_pep(dat, municipio.pep, year_input)
  }
  return(dat)
}

#' Helper Function 6: function to rename columns containing specific substrings
#'
#' @param df data.frame/data.table
#' @param old_substrings old column names
#' @param new_name new column names
#' @return  data.table
rename_columns <- function(df, old_substrings, new_name) {
  names(df) <- sapply(names(df), function(col_name) {
    if (any(grepl(paste(old_substrings, collapse="|"), col_name))) {
      return(new_name)
    } else {
      return(col_name)
    }
  })
  df
}
