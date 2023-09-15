atlas <- read.csv("dados/atlas_mamiferos_portugal_2019.csv")

ptgal <- vect("mapas/utm10+ilhas.gpkg")

datapaper <- read.csv("dados/datapaper_utm10.csv")

redbook <- read.csv("dados/redbook_utm10.csv")

atlas$source <- "atlas"
datapaper$source <- "datapaper"
redbook$source <- "redbook"
