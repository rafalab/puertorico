#' To English
#'
#'@description `to_english` takes an character argument and removes diacritic marks, dieresis or tildes from modern latin alphabet letters. 
#'
#'@details This function serves for English language character normalization by removing traditional symbols used in other languages.
#' 
#'@param x character argument.
#'@param remove.space defaults to `TRUE`. Removes spaces between letters. 
#' 
#'@examples
#' to_english(c("niño y niña", "método", "matemáticas"))
#' 
#'@export
to_english <- function(x, remove.space = TRUE) {
  x <- x |>
    tidyr::replace_na("") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(c(
      "á" = "a",
      "é" = "e",
      "í" = "i",
      "ó" = "o",
      "ú" = "u",
      "ü" = "u",
      "ñ" = "n"
    ))
  if(remove.space) x <- stringr::str_remove_all(x ,"\\s+") 
  else x <- stringr::str_trim(x)
  return(x)
}


