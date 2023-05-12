### making municipio.names, municipio.abr, municipio.centers, municipio.regions ###
library(tidyverse)
municipios_names_abb <- readr::read_csv("inst/extdata/Codigos_Censales_y_Abrev_Municipios.csv") |> 
                        na.omit() |> filter(ABR != "PRI")

municipios.abr <- municipios_names_abb$ABR
municipios.names <- municipios_names_abb$Municipio

municipios.centers <- list(x = map_centers$X, y = map_centers$Y)
municipios.health.regions <- municipios_names_abb$region_salud |> as.factor()

municipios <- cbind(municipio.names, municipio.abr, municipio.health.regions, "x"=municipio.centers$x, "y"=municipio.centers$y)

save(municipios,municipio.abr, municipio.names, municipio.centers, municipio.health.regions, 
     file = "data/municipios.rda")
