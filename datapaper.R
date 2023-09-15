library(terra)


# UTM GRID MAP ####

u10 <- vect("mapas/utm10+ilhas.gpkg")
plot(u10)


# ATLAS OCCURRENCE DATA ####

atlas <- read.csv("dados/atlas_mamiferos_portugal_2019.csv")
especies_atlas <- sort(unique(atlas$especie))


# DATAPAPER OCCURRENCE DATA ####

# download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3654&file=ecy3654-sup-0001-Data_S1.zip", destfile = "dados/datapaper.zip")  # command didn't work

# manually downloaded file from https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.3654

unzip("dados_externos/Grilo et al 2022 datapaper supp.zip", exdir = "dados_externos")
# dp <- read.csv("dados/DataPaper/MAMMALS_PORTUGAL_DATA_SET 2021-11.csv")
dp <- read.csv("dados_externos/ecy3654-sup-0001-DataS1/MAMMALS_PORTUGAL_DATA_SET 2021-11.csv")
head(dp)
unlink("dados_externos/ecy3654-sup-0001-DataS1", , recursive = TRUE)


# MATCH SPECIES NAMES ####

especies_datapaper <- sort(unique(dp$Species))
setdiff(especies_datapaper, especies_atlas)  # muitas (inclui morcegos, marinhos e urso)
setdiff(especies_atlas, especies_datapaper)  # "Microtus agrestis" "Ovis aries"

# make datapaper species match names in Atlas:
especies_datapaper[grep("Microtus", especies_datapaper)]
especies_datapaper[grep("Ovis", especies_datapaper)]
dp$Species[dp$Species == "Microtus rozianus"] <- "Microtus agrestis"  # recent taxonomic reassign
dp$Species[dp$Species == "Ovis ammon musimos"] <- "Ovis aries"


# SUBSET FOR SPECIES IN ATLAS ####

nrow(dp)  # 107852
dp <- subset(dp, Species %in% especies_atlas)
nrow(dp)  # 79187


# EXTRACT UTM CELL FOR DATAPAPER RECORDS ####

names(dp)
names(u10)
dp <- vect(dp, geom = c("Longitude", "Latitude"), crs = "epsg:4326")
dp_utm10 <- extract(u10[ , "utm10"], dp)
nrow(dp_utm10)
nrow(dp)  # same

head(dp_utm10)
dp_utm10 <- data.frame(dp_utm10[ , "utm10", drop = FALSE], dp[ , "Species", drop = FALSE])
head(dp_utm10)

nrow(dp_utm10)  # 79187
dp_utm10 <- unique(dp_utm10)
nrow(dp_utm10)  # 5216

head(dp_utm10)
names(dp_utm10)[2] <- "especie"  # to match atlas


# remove UTMs that were already in Atlas for each species?

# add M. rozianus (reassigned from M. agrestis)?


# EXPORT TABLE

write.csv(dp_utm10, "dados/datapaper_utm10.csv", row.names = FALSE)
