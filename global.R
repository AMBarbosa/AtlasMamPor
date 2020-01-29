mam_pt <- read.csv("dados/atlas_mamiferos_portugal_2019.csv")
ptgal <- readOGR(dsn = "mapas", layer = "utm10+ilhas", verbose = FALSE)

