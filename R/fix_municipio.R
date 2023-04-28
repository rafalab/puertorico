#' Fix municipio names
#'
#'@description `fix_municipio` fixes typical data entry errors found in municipio names by matching each argument to the set correctly spelled municipios using Levenshtein metric.
#'
#'@param x character string or vector of character strings from Puerto Rico municipios
#'@param min.dist numeric.
#'@param min.score numeric. Refers to the minimum acceptable ... for municipio based matches.
#'@param check.barrios logical. Defaults to `TRUE`. If `FALSE` then the function does not verify if the input is a barrio.
#'@param min.score.barrio numeric. Refers to the minimum acceptable ... for barrio based matches. Note that this parameter is used when `check.barrios` is `TRUE`.
#'@param first.letter.match logical. Defaults to `TRUE`. If `FALSE` then the function does not use the first letter match.
#'@param return.dist logical. Defaults to `FALSE`. If `TRUE` returns the Levenshteins metric for each match.
#'@param additional.barrios Defaults to `NULL`. data.frame with the first column of additional barrios and the second column of their corresponding municipios.
#'
#'@examples
#'
#'example_1 <- c("franques", "manati", "Veja Baja" , "Comerip", "San Juan", "Mayaguez", "rio piedras")
#'
#'fix_municipio(example_1)
#'
#'fix_municipio(example_1, check.barrios = FALSE)
#'
#' @export
#' @import data.table
#' @import stringr
#' @import stringdist
#' @import dplyr


fix_municipio <- function(x, min.dist = 2,
                          min.score = 1/3,
                          check.barrios = TRUE,
                          min.score.barrio = 2/9,
                          first.letter.match = TRUE,
                          return.dist = FALSE,
                          additional.barrios = NULL) {


  municipios <- c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "Añasco",
                  "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayamón", "Cabo Rojo",
                  "Caguas", "Camuy", "Canóvanas", "Carolina", "Cataño", "Cayey", "Ceiba",
                  "Ciales", "Cidra", "Coamo", "Comerío", "Corozal", "Culebra", "Dorado",
                  "Fajardo", "Florida", "Guánica", "Guayama", "Guayanilla", "Guaynabo",
                  "Gurabo", "Hatillo", "Hormigueros", "Humacao", "Isabela", "Jayuya",
                  "Juana Díaz", "Juncos", "Lajas", "Lares", "Las Marías", "Las Piedras",
                  "Loíza", "Luquillo", "Manatí", "Maricao", "Maunabo", "Mayagüez", "Moca",
                  "Morovis", "Naguabo", "Naranjito", "Orocovis", "Patillas", "Peñuelas",
                  "Ponce", "Quebradillas", "Rincón", "Río Grande", "Sabana Grande", "Salinas",
                  "San Germán", "San Juan", "San Lorenzo", "San Sebastián", "Santa Isabel",
                  "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja",
                  "Vieques", "Villalba", "Yabucoa", "Yauco")
  x <- as.factor(x)
  query <- levels(x)
  ans <- as.character(rep(NA, length(query)))
  barrio <- as.character(rep(NA, length(query)))
  the_dist <- rep(NA, length(ans))
  the_score <- rep(NA, length(ans))

  if(all(query %in% municipios)){ # if all map, we are done
    ans <- query
    barrio <- as.character(rep(NA, length(query)))
  } else{ ## if not, we will convert to lower case, remvoe accents and non-characters
    map_mun <- data.frame(id = to_english(municipios), municipio = municipios)

    query <- query |>
      to_english() |>
      str_remove_all("[^a-zA-Z]+")

    ## remove common errors
    query[query=="puertorico" | query == "unknown" | query == "pr"] <- ""

    ## we will store answers in ans and if matching barrios, store that in barrio
    ind1 <- query %in% map_mun$id
    ans[ind1] <- map_mun$municipio[match(query[ind1], map_mun$id)]

    ## if not all matched keep going
    if(!all(ind1) & check.barrios){

      load(file.path(system.file("data", package = "puertorico"), "barrios.rda"))

      ## if there is ambiguity we do not assign a municipio

      if (!is.null(additional.barrios)) {
        colnames(additional.barrios) <- c("original_barrio", "municipio")
        additional.barrios$barrio <- to_english(additional.barrios$original_barrio)
        additional.barrios$id <- to_english(additional.barrios$municipio)
        barrios <- rbind(barrios, additional.barrios)
      }

      ## use tmp to match barrios or misspellings in query to barrios in our table
      query_dt <- data.table(barrio = query[!ind1])
      setkey(query_dt, barrio)
      setkey(barrios, barrio)
      tmp <- merge(query_dt, barrios, by = "barrio", all.x = TRUE)

      ans[!ind1] <- tmp$municipio
      barrio[!ind1] <- tmp$original_barrio

      ## keep those not matched to search and see if municipio a subset like sanjuan in sanjuanpr
      ind <- which(is.na(ans))
      if(length(ind)>0){

        for(i in 1:nrow(map_mun)){
          ind2 <- str_which(query[ind], map_mun$id[i])
          if(length(ind2)>0) ans[ind[ind2]] <- map_mun$municipio[i]
        }


        ##for those not matched and find fuzzy match with municipios
        ind <- which(is.na(ans))

        if(length(ind)>0){

          ## distance to municipio
          d_mun <- stringdist::stringdistmatrix(query[ind], map_mun$id, method = "lv")
          ind_mun <- apply(d_mun, 1, which.min)
          min_d_mun <- apply(d_mun, 1, min)
          if(first.letter.match){ ##require first letter matches
            min_d_mun[str_sub(query[ind],1,1) != str_sub(map_mun$id[ind_mun],1,1)] <- Inf
          }
          score_mun <- min_d_mun / nchar(query[ind])

          ##if criteria met, keep it
          keep <- min_d_mun <= min.dist & score_mun <= min.score

          if(length(keep)>0){
            ans[ind][keep] <-map_mun$municipio[ind_mun][keep]
            the_dist[ind][keep] <- min_d_mun[keep]
            the_score[ind][keep] <- score_mun[keep]
          }
          ##for those not matched check for fuzzy match with barrio
          ind <- which(is.na(ans))

          if(length(ind)>0){
            #distance to barrio
            d_bar <- stringdist::stringdistmatrix(query[ind], barrios$barrio, method = "lv")
            ind_bar <- apply(d_bar, 1, which.min)
            min_d_bar <- apply(d_bar, 1, min)
            if(first.letter.match){
              min_d_bar[str_sub(query[ind], 1, 1) != str_sub(barrios$barrio[ind_bar], 1, 1)] <- Inf
            }
            score_bar <- min_d_bar / nchar(query[ind])

            keep <- min_d_bar <= min.dist & score_bar <= min.score.barrio
            if(length(keep)>0){
              ans[ind][keep] <- barrios$municipio[ind_bar][keep]
              barrio[ind][keep] <- barrios$original_barrio[ind_bar][keep]
              the_dist[ind][keep] <- min_d_bar[keep]
              the_score[ind][keep] <- score_bar[keep]
            }

            ind <- which(is.na(ans))

            if(length(ind)>0){
              ##check if barrio included
              for(i in 1:nrow(barrios)){
                ind2 <- str_which(query[ind], barrios$barrio[i])
                if(length(ind2)>0){
                  ans[ind[ind2]] <- barrios$municipio[i]
                  barrio[ind[ind2]] <- barrios$original_barrio[i]
                }
              }
            }
          }
        }
      }
    }
  }
  look_up <- data.frame(original = levels(x),
                        predicted = ans,
                        barrio.match = barrio,
                        dist = the_dist,
                        score = the_score)

  ret <- data.frame(original = as.character(x)) |>
    left_join(look_up, by = "original")

  if(return.dist){
    return(ret)
  } else{
    return(ret$predicted)
  }
}

