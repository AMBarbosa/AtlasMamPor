library(terra)
library(geodata)


# UTM GRID MAP ####

u10 <- terra::vect("mapas/utm10+ilhas.gpkg")
plot(u10, lwd = 0.2)


# ATLAS OCCURRENCE DATA ####

atlas <- read.csv("dados/atlas_mamiferos_portugal_2019.csv")
especies_atlas <- sort(unique(atlas$especie))


# RED BOOK OCCURRENCE DATA ####

# import ICNF mammal presence data from GBIF:

icnf <- read.csv("dados_externos/GBIF_Mammalia_pres_ICNF.csv", sep = "\t")  # downloaded 14 Sep 2023
nrow(icnf)  # 511482
head(icnf)
names(icnf)
icnf <- icnf[ , c("species", "family", "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters", "year", "basisOfRecord", "institutionCode", "collectionCode")]
head(icnf)
unique(icnf$institutionCode)  # "ICNF"


# subset to Red Book data:

unique(icnf$collectionCode)  # several
rb <- icnf[grep("red_book_of_mammals", icnf$collectionCode), ]
unique(rb$collectionCode)
# "red_book_of_mammals_portugal" "red_book_of_mammals_in_portugal_2"
nrow(rb) # 474173


# exclude data with excessive spatial error for 10x10 km2 cells:

hist(rb$coordinateUncertaintyInMeters)
range(rb$coordinateUncertaintyInMeters, na.rm = TRUE)  # 1e-02 1e+06
table(rb$coordinateUncertaintyInMeters)
rb <- subset(rb, is.na(rb$coordinateUncertaintyInMeters) | rb$coordinateUncertaintyInMeters < (10000 * sqrt(2)) / 2)
nrow(rb) # 471875

# map records:
terra::plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2))
points(rb[rb$species == "Galemys pyrenaicus" & rb$collectionCode == "red_book_of_mammals_portugal", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = "blue")
points(rb[rb$species == "Galemys pyrenaicus" & rb$collectionCode == "red_book_of_mammals_in_portugal_2", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = adjustcolor("red", 0.5))

terra::plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2))
points(rb[rb$species == "Chionomys nivalis" & rb$collectionCode == "red_book_of_mammals_portugal", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = "blue")
points(rb[rb$species == "Chionomys nivalis" & rb$collectionCode == "red_book_of_mammals_in_portugal_2", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = adjustcolor("red", 0.5))

terra::plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2))
points(rb[rb$species == "Lynx pardinus" & rb$collectionCode == "red_book_of_mammals_portugal", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = "blue")
points(rb[rb$species == "Lynx pardinus" & rb$collectionCode == "red_book_of_mammals_in_portugal_2", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.3, col = adjustcolor("red", 0.5))

terra::plot(u10, lwd = 0.1, ext = c(-10, -6, 37, 42.2))
points(rb[rb$species == "Canis lupus" & rb$collectionCode == "red_book_of_mammals_portugal", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = "blue")
points(rb[rb$species == "Canis lupus" & rb$collectionCode == "red_book_of_mammals_in_portugal_2", c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.3, col = adjustcolor("red", 0.5))

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


# BUILD FINAL TABLE

head(atlas)
final <- data.frame(rb_u10[ , "utm10", drop = FALSE],
                    centr_x = NA_real_,
                    centr_y = NA_real_,
                    rb_u10[ , "especie", drop = FALSE],
                    ordem = NA_character_,
                    recente = NA_integer_,
                    confirmado = NA_integer_)
head(final)
tail(final)

write.csv(final, "dados/redbook_utm10.csv", row.names = FALSE)
