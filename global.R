ptgal <- terra::vect("mapas/utm10+ilhas.gpkg")

atlas <- read.csv("dados/atlas_mamiferos_portugal_2019.csv")

# add external data:
datapaper <- read.csv("dados/datapaper_utm10.csv")
redbook <- read.csv("dados/redbook_utm10.csv")

# match columns among data sets:
names(atlas)
datapaper <- data.frame(datapaper[ , "utm10", drop = FALSE],
                        centr_x = NA_real_,
                        centr_y = NA_real_,
                        datapaper[ , "especie", drop = FALSE],
                        ordem = NA_character_,
                        recente = NA_integer_,
                        confirmado = NA_integer_)
redbook <- data.frame(redbook[ , "utm10", drop = FALSE],
                      centr_x = NA_real_,
                      centr_y = NA_real_,
                      redbook[ , "especie", drop = FALSE],
                      ordem = NA_character_,
                      recente = NA_integer_,
                      confirmado = NA_integer_)

# add 'source' column:
atlas$source <- "atlas"
datapaper$source <- "datapaper"
redbook$source <- "redbook"

# join data sets:
dados <- rbind(atlas, datapaper, redbook)
