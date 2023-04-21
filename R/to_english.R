#' To English
#'
#'@description `to_english` takes an character argument and removes diacritic marks, dieresis or tildes from modern latin alphabet letters. 
#'
#'@details This function serves for English language character normalization by removing traditional symbols used in other languages.
#' 
#' @param ... Character argument.
#' 
#' @example 
#' 
#' to_english(c("niño y niña", "método", "matemáticas"))


to_english <- function(x, remove.space = TRUE) {
  x <- x |>
    replace_na("") |>
    str_to_lower() |>
    str_replace_all(c(
      "á" = "a",
      "é" = "e",
      "í" = "i",
      "ó" = "o",
      "ú" = "u",
      "ü" = "u",
      "ñ" = "n"
    ))
  if(remove.space) x <- str_remove_all(x ,"\\s+") 
  else x <- str_trim(x)
}


