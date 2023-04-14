to_english <- function(x){
  replace_na(x, "") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ü", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_remove_all("\\s+")
}
