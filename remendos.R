# subset to Red Book data and relevant columns:

sort(unique(icnf$collectionCode))
# [1] "atlas_morcegos"                    "especies_diretiva_habitats_07_12"
# [3] "especies_diretiva_habitats_13_18"  "psrn2000_fauna_linhas"
# [5] "psrn2000_fauna_poligonos"          "psrn2000_fauna_pontos"
# [7] "red_book_of_mammals_in_portugal_2" "red_book_of_mammals_portugal"
# [9] "Wolf_packs"
rb <- icnf[grep("red_book_of_mammals", icnf$collectionCode), c("species", "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters", "year", "basisOfRecord", "institutionCode", "collectionCode")]
unique(rb$collectionCode)
# "red_book_of_mammals_portugal" "red_book_of_mammals_in_portugal_2"
nrow(rb) # 474173
