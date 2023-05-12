# make barrios rda

library(data.table)

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

map_mun <- data.frame(id = to_english(municipios), municipio = municipios)

barrios_path <- file.path(system.file("extdata", package = "puertorico"), "pr-barrios.csv")
barrios <- fread(barrios_path, colClasses = c("character", "character"))

# Remove duplicates
barrios[, n := .N, by = barrio]
barrios <- barrios[n == 1, !c("n")]

# Add commonly used names by hand
common_names <- data.frame(
  barrio = c("Río Piedras", "Hato Rey", "Barrio Obrero", "Bo Obrero", "Condado", "Miramar",
             "Caparra Heights","College Park","Villa Palmeras",
             "Fort Buchanan", "Caparra",
             "Mercedita",
             "Saint Just", "Trujillo", "Tru Alto",
             "Levittown",
             "Bajadero",
             "Ramey", "Aquadilla",
             "Palmer",
             "Isla Verde", "Urb Country Club",
             "La Plata",
             "Villa Rica", "Rio Hondo",
             "Palmas del Mar",
             "Cieba", "Roosvelt Roads",
             "Campanilla",
             "Caomo",
             "Yuaco",
             "Lioza",
             "Tao Baja",
             "Tao Alta",
             "A Buenas"),
  municipio = c("sanjuan","sanjuan","sanjuan","sanjuan","sanjuan", "sanjuan",
                "sanjuan","sanjuan","sanjuan",
                "guaynabo","guaynabo",
                "ponce",
                "trujilloalto", "trujilloalto","trujilloalto",
                "toabaja",
                "arecibo",
                "aguadilla", "aguadilla",
                "riogrande",
                "carolina", "carolina",
                "aibonito",
                "bayamon", "bayamon",
                "humacao",
                "ceiba", "ceiba",
                "toabaja",
                "coamo",
                "yauco",
                "loiza",
                "toabaja",
                "toalta",
                "aguasbuenas"))

barrios <- rbindlist(list(barrios, common_names), fill = TRUE)

# Convert to English
barrios[, `:=`(original_barrio = barrio,
                  barrio = to_english(barrio),
                  municipio = to_english(municipio))]

# Rename columns
setnames(barrios, "municipio", "id")

# Join with map_mun
barrios <- merge(barrios, map_mun, by = "id", all.x = TRUE)
barrios$barrio[barrios$barrio == "barriopueblo(isabelii)"] <- "isabellii"


save(barrios, file = "data/barrios.rda")

