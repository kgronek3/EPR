# Ekonometria przestrzennia w R - zajęcia 1
getwd()
setwd("/home/oic/IiE_MGR/EkonometriaPrzestrzennaR")
getwd()

pl <- readOGR("./data", "Panstwo") # 1 jedn.
pl <- spTransform(pl, CRS("+proj=longlat +datum=NAD83"))
plot(pl)

woj <- readOGR("./data", "wojewodztwa") # 16 jedn.
woj <- spTransform(woj, CRS("+proj=longlat +datum=NAD83"))
plot(woj)

pow <- readOGR("./data", "powiaty") # 380 jedn.
pow <- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
plot(pow)

# Bez transformacji
woj_coord <- readOGR("./data", "wojewodztwa") # 16 jedn.

plot(woj_coord)
degAxis(1)
degAxis(2)

# Po transformacji
woj_coord <- readOGR("./data", "wojewodztwa") # 16 jedn.
woj_coord <- spTransform(woj_coord, CRS("+proj=longlat +datum=NAD83"))

plot(woj_coord)
degAxis(1)
degAxis(2)

class(woj)

# wczytywanie danych
dane <- read.csv("data/data_nts4_2019_pl.csv",
                 sep = ";", dec = ",", header = TRUE)

pow_df <- as.data.frame(pow)

summary(dane)
names(dane)

# współrzędne środków powiatów
crds <- coordinates(pow)
head(crds)

# mapa administracyjna
plot(pl, lwd = 3)
plot(pow, add = TRUE)
plot(woj, add = TRUE, lwd = 2)
points(crds, pch = 21, bg = "red", cex = 0.8)

# podpisywanie regionów
plot(pow)
pointLabel(crds, as.character(pow_df$NAME_2), cex = 0.6)

plot(pow)
text(crds, as.character(pow.df$NAME_2), cex = 0.6)

# interaktywna edycja nazw regionów
plot(pow)
identify(crds, labels = as.character(pow_df$jpt_nazwa_))

par(mar = c(5, 5, 5, 4))
plot(pow)
addnortharrow()
map.scale(x = 13.3, y = 49.9, ration = FALSE, relwidth = 0.2)
addscalebar()

plot(woj)
degAxis(1)
degAxis(2)
compassRose(15, 49.7, rot = 0, cex = 1)
plot(gridlines(woj), add = TRUE)
