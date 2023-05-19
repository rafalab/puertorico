library(sf)
library(tidyverse)
map <- st_read("inst/extdata/pri_adm_2019_shp/pri_admbnda_adm1_2019.shp") |>
  st_transform(crs = 4326) |>
  st_crop(xmin = -67.4, xmax = -65.3, ymin = 17.9, ymax = 18.5) ## quitar a Mona

map_centers <- data.frame(municipio = map$ADM1_ES, st_coordinates(st_centroid(map)))

map2 <- as(map, "Spatial")
map2 <- map2@polygons ## para extraer polygons 
names(map2) <- map$ADM1_ES
map <- map_df(seq_along(map2), function(i){
  tmp <- map2[[i]]@Polygons
  tmp <- tmp[order(sapply(tmp,function(x) x@area), decreasing = TRUE)]
  
  # centro, de todo el muni o solo el grande? donde lo ubicamos?
  # cuantas partes tiene loiza
    map_df(seq_along(tmp), function(j){
    tmp2 <-  map2[[i]]@Polygons[[j]]@coords
    data.frame(municipio = names(map2)[i], part = j, X = tmp2[,1], Y = tmp2[,2], area = map2[[i]]@Polygons[[j]]@area) # outline del pedazo j del pueblo i
  })
})

## IMPORTANT: part and municipio work together

## Part and municipio work together, must use both. 
## In ggplot, use interaction term in group parameter. To see why, verify Toa Baja. 
# map |> filter(municipio == "Toa Baja") |> ggplot(aes(x = X, y = Y, group = interaction(municipio, part))) +
#   geom_polygon(fill = "white", color = "black")
# map |> filter(municipio == "Toa Baja", part ==1 ) |> ggplot(aes(x = X, y = Y, group = interaction(municipio, part))) +
#   geom_polygon(fill = "white", color = "black")
# map |> filter(municipio == "Toa Baja", part == c(1, 2)) |> ggplot(aes(x = X, y = Y, group = interaction(municipio, part))) +
#   geom_polygon(fill = "white", color = "black")
# map |> filter(municipio == "Toa Baja", part == c(1, 3)) |> ggplot(aes(x = X, y = Y, group = interaction(municipio, part))) +
#   geom_polygon(fill = "white", color = "black")

## However, we don't need small islands that belong to the municipio,
## thus we filter them by area.

map <- map |> filter(area >= 10^-4) ## eliminate small islands
## adding the part variable will guarantee that the text is centered
map_centers <- map_centers |> arrange(municipio) |> mutate(part = 1) 

save(map, map_centers, file = "data/map.rda")

## tests

# map  |> ggplot(aes(x = X, y = Y, group = interaction(municipio, part))) +
#  geom_polygon(fill = "white", color = "black") + 
#  geom_text(data = map_centers, aes(x = X, y = Y, label=municipio), cex = 2)

 
#
