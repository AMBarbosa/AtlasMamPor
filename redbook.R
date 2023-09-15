library(terra)
library(data.table)


# UTM GRID MAP ####

u10 <- terra::vect("mapas/utm10+ilhas.gpkg")
plot(u10, lwd = 0.2)


# ATLAS OCCURRENCE DATA ####

atlas <- read.csv("dados/atlas_mamiferos_portugal_2019.csv")
especies_atlas <- sort(unique(atlas$especie))


# RED BOOK OCCURRENCE DATA ####

# import ICNF mammal presence data from GBIF:

# unzip("dados_externos/GBIF_Mammalia_pres_ICNF_simple.zip", exdir = "dados_externos")
# REFERENCE: GBIF.org (15 September 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.fve5xx
# download criteria:
# Scientific name:
# Mammalia
# Occurrence status:
# present
# Publisher:
# ICNF - Instituto da Conservação da Natureza e das Florestas
# Dataset:
# Red Book of Mammals in Portugal (part I) Red Book of Mammals in Portugal (part II) Red Book of Mammals in Portugal (part III)

# rb <- read.csv("dados_externos/0017776-230828120925497.csv", sep = "\t")
# nrow(rb)  # 474173 in 'simple' data download
# ncol(rb)  # 50 in 'simple' data download
#
# unlink("dados_externos/0017776-230828120925497.csv", recursive = TRUE)

# but this dataset misses the column 'identificationVerificationStatus', which allows excluding unverified records that ae not shown in the Red Book


# import Darwin core instead:

unzip("dados_externos/GBIF_Mammalia_pres_ICNF_DarwinCore.zip", exdir = "dados_externos/GBIF_DarwinCore")
# rb <- read.table("dados_externos/GBIF_DarwinCore/occurrence.txt", sep = "\t", header = TRUE)  # this (as well as read.csv) imported with errors
rb <- data.table::fread("dados_externos/GBIF_DarwinCore/occurrence.txt")
# Warning message:
#   In data.table::fread("dados_externos/GBIF_DarwinCore/occurrence.txt") :
#   Found and resolved improper quoting out-of-sample. First healed line 105028: <<4071738302				CC_BY_4_0	2022-02-05T00:00:00Z	ICNF - Instituto da Conservação da Natureza e das Florestas			occurrence	53f347a7-99ce-4528-a085-5c76eb1301a0			ICNF	red_book_of_mammals_portugal	Red Book of Mammals Portugal	FCiencias.ID	MACHINE_OBSERVATION			"{projectName":"LVMP","wingInMilimetres":"","weightInGrams":"","dnaSample":""}	00068aff-7441-426a-abd6-4964d0675c0e	P3_097881		Tiago Marques													PRESENT				Consórcio Consultoria Científica Chiroptera (2021). Inventariação acústica>>. If the fields are not quoted (e.g. field separator does not appear within any field), try quote="" to avoid this warning.
# this seems to have fixed the errors; nrow now the same as 'simple' data donwload
gc()

rb <- as.data.frame(rb)

nrow(rb)  # 474173
ncol(rb)  # 212 (many more cols) in 'Darwin core' data download
head(rb)
unlink("dados_externos/GBIF_DarwinCore", recursive = TRUE)

names(rb)
unique(rb$institutionCode)  # "ICNF"
unique(rb$collectionCode)  # "red_book_of_mammals_portugal"      "red_book_of_mammals_in_portugal_2"
unique(rb$occurrenceStatus)  # "PRESENT"
unique(rb$basisOfRecord)
# [1] "LIVING_SPECIMEN"  "MATERIAL_SAMPLE"  "HUMAN_OBSERVATION"
# [4] "MACHINE_OBSERVATION"  "PRESERVED_SPECIMEN"  "OCCURRENCE"
unique(rb$typeStatus)  # NA
unique(rb$identificationVerificationStatus)
# [1] "verified by specialist"  "verified"  "unverifiable"
# [4] "verification required"
sum(is.na(rb$identificationVerificationStatus))  # 0


rb_unverified <- rb
nrow(rb_unverified)  # 474173

rb <- subset(rb_unverified, rb_unverified$identificationVerificationStatus %in% c("verified by specialist", "verified"))
nrow(rb)  # 193108
rb <- subset(rb, rb$identificationVerificationStatus %in% c("verified by specialist"))
nrow(rb)  # 112427


# exclude data with excessive spatial error for 10x10 km2 cells:

hist(rb$coordinateUncertaintyInMeters)
range(rb$coordinateUncertaintyInMeters, na.rm = TRUE)  # 0.01 7025.00
table(rb$coordinateUncertaintyInMeters)
rb <- subset(rb, is.na(rb$coordinateUncertaintyInMeters) | rb$coordinateUncertaintyInMeters < (10000 * sqrt(2)) / 2)
nrow(rb) # 112427


# map records:

unique(rb$year)
sum(is.na(rb$year))  # 0

species <- "Chionomys nivalis"
species <- "Galemys pyrenaicus"
species <- "Lynx pardinus"
species <- "Canis lupus"
species <- "Microtus cabrerae"
species <- "Microtus rozianus"
species <- "Microtus lusitanicus"

# by collectionCode:
terra::plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2), axes = FALSE)
points(rb[rb$species == species & rb$collectionCode == "red_book_of_mammals_in_portugal_2", c("decimalLongitude", "decimalLatitude")], pch = 15, cex = 0.7, col = adjustcolor("darkgrey", 0.5))
points(rb[rb$species == species & rb$collectionCode == "red_book_of_mammals_portugal", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.3, col = "darkred")
title(species, font.main = 3, cex.main = 0.8)
legend("bottom", legend = c("red_book_of_mammals_portugal", "red_book_of_mammals_in_portugal_2"), pch = c(20, 15), col = c("darkred", adjustcolor("darkgrey", 0.5)), cex = 0.7, title = "GBIF collectionCode:", bg = "white", box.lwd = 0, xpd = NA)

# by year:
terra::plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2), axes = FALSE)
points(rb[rb$species == species & rb$year >= 1990 & rb$year <= 2004, c("decimalLongitude", "decimalLatitude")], pch = 15, cex = 0.7, col = adjustcolor("darkgrey", 0.5))
points(rb[rb$species == species & rb$year >= 2005 & rb$year <= 2021, c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.3, col = "darkred")
title(species, font.main = 3, cex.main = 0.8)
legend("bottom", legend = c("2005-2021", "1990-2004"), pch = c(20, 15), col = c("darkred", adjustcolor("darkgrey", 0.5)), cex = 0.7, title = "GBIF year:", bg = "white", box.lwd = 0, xpd = NA)


# both datasets look OK (ICNF GBIF upload errors reverted)
# for the species above, it looks like 'red_book_of_mammals_portugal' might contain the recent records, 'red_book_of_mammals_in_portugal_2' older records

# however, the "year" column does not seem to reflect that:
hist(rb$year)
hist(rb[rb$collectionCode == "red_book_of_mammals_portugal", ]$year)
hist(rb[rb$collectionCode == "red_book_of_mammals_in_portugal_2", ]$year)
range(rb[rb$collectionCode == "red_book_of_mammals_portugal", ]$year, na.rm = TRUE)  # 1873 2021
range(rb[rb$collectionCode == "red_book_of_mammals_in_portugal_2", ]$year, na.rm = TRUE)  # 1723 2022
# maybe they put in publication rather than observation year?


# MATCH SPECIES NAMES ####

especies_redbook <- sort(unique(rb$species))
setdiff(especies_redbook, especies_atlas)  # muitas (inclui morcegos, marinhos e urso)
setdiff(especies_atlas, especies_redbook)  # "Neovison vison" - not in red book because exotic - ok


# SUBSET FOR SPECIES IN ATLAS ####

nrow(rb)  # 471875
rb <- subset(rb, rb$species %in% especies_atlas)
nrow(rb)  # 175956
setdiff(rb$species, especies_atlas)  # character(0) - ok


# EXTRACT UTM CELL FOR RED BOOK RECORDS ####

names(rb)
names(u10)
rb_u10 <- terra::extract(u10[ , "utm10"], rb[ , c("decimalLongitude", "decimalLatitude")])
head(rb_u10)
tail(rb_u10)  # last id.y smaller than row names

nrow(rb)  # 175956
nrow(rb_u10)  # 175970 - more UTM-point rows than points?

# remove points without UTM cell:
sum(is.na(rb_u10$utm10))  # 1489
rb_u10 <- rb_u10[!is.na(rb_u10$utm10), ]
nrow(rb_u10)  # 174481

head(rb_u10)
tail(rb_u10)

nrow(rb)  # 175956
length(unique(rb_u10$id.y))  # 174467 (after removing NA u10)
nrow(unique(rb_u10))  # 174481 - so some points have more than one UTM code?!
dups <- rb_u10[duplicated(rb_u10$id.y, fromLast = FALSE) | duplicated(rb_u10$id.y, fromLast = TRUE), ]  # https://stackoverflow.com/questions/7854433/finding-all-duplicate-rows-including-elements-with-smaller-subscripts
dups

nrow(rb) == nrow(rb_u10) - sum(duplicated(rb_u10$id.y))  # FALSE


plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2))
points(rb[dups$id.y, c("decimalLongitude", "decimalLatitude")])  # points seem to fall exactly on the border between two UTM cells

head(rb_u10)
tail(rb_u10)
names(rb)
names(rb_u10)

# cant use 'data.frame' because row number doesn't match, so use 'merge':
rb$id.y <- 1:nrow(rb)
rb_u10 <- merge(rb_u10[ , c("id.y", "utm10")], rb[ , c("id.y", "species")], by = "id.y", all.x = TRUE)
head(rb_u10)

nrow(rb_u10)  # 174481
nrow(unique(rb_u10))  # 174481
nrow(unique(rb_u10[ , -1]))  # 13764 without id.y

rb_u10 <- unique(rb_u10[ , c("utm10", "species")])
nrow(rb_u10)  # 13764
head(rb_u10)
names(rb_u10)[2] <- "especie"  # to match atlas


# EXPORT TABLE

write.csv(rb_u10, "dados/redbook_utm10.csv", row.names = FALSE)
