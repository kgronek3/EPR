# Ekonometria przestrzennia w R - zajęcia 1

a <- rgb(runif(21, 0, 1), runif(21, 0, 1), runif(21, 0, 1))
a

# generowanie losowych kolorów i rysowanie z image()
a.dim1 <- length(a)
image(1:a.dim1, 1,
      as.matrix(1:a.dim1),
      col = a, xlab = "grDevices::rgb() / random colors ")

# rysowanie nazw kolorów w rastrach
colors() # the first few lines of color names
library(raster)
r <- raster(xmn = 0, xmx = 22, ymn = 0, ymx = 30, nrows = 30, ncols = 22)
r[] <- 1:660
plot(r, col = colors()) #rasters (cells) with colours

# zmiana angielskich nazw kolorów na składowe RGB
# działa także dla kolorów wyrażonych szestnastkowo
col2rgb(c("azure", "azure1", "azure2"), alpha = FALSE)
col2rgb(c("#4B424F", "#BFD15C", "#A44845"), alpha = FALSE)

# wyświetla kolory z RColorBrewer
library(RColorBrewer)
display.brewer.all() # wszystkie palety z pakietu

display.brewer.pal(11, "Spectral") # wyświetlenie palety

display.brewer.pal(9, "OrRd") # wyświetlenie palety

display.brewer.pal(4, "Paired") # wyświetlenie palety

brewer.pal(4, "Paired") # wyświetlenie palety

cols <- brewer.pal(n = 5, name = "RdBu") # zapisanie wybranych kolorów

# zobaczmy kolory z wensaderson::
library(wesanderson)
cols1 <- wes_palette("GrandBudapest1", 4, type = "discrete")
cols1

cols2 <- wes_palette("GrandBudapest1", 21, type = "continuous")
cols2

# generowanie 21 dywergencyjnych kolorów z quickPlot::divergentColors()
library(quickPlot)

# przykład
a <- divergentColors("darkred", "darkblue", -10, 10, 0, "white")
a
a.dim1 <- length(a)
image(1:a.dim1, 1, as.matrix(1:a.dim1),
      col = a, xlab = "quickPlot::divergentColors() / darkred-darkblue")

# jeszcze inna paleta
a <- divergentColors("chocolate4", "peachpuff4", -10, 10, 0, "white")
a
a.dim1 <- length(a)
image(1:a.dim1, 1, as.matrix(1:a.dim1),
      col = a, xlab = "quickPlot::divergentColors() / chocolate-peachpuff")

# viridis:: z kolorami dla daltonistów (color-blind)
# kolory podzielone na 5 palet (opt): "magma" ("A"), "inferno" ("B"),
# plasma (C), viridis (D), cividis (E)

library(viridis)
library(scales)
viridis.map

col1 <- viridis(15, option = "D") # domyślne kolory palety viridis
show_col(col1)

col2 <- viridis(15, option = "B") # paleta inferno
show_col(col2)

# pokolorowane heksagony
# install.packages("hexbin")
library(ggplot2)
ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
    geom_hex() + coord_fixed() + scale_fill_viridis() + theme_bw()

# Maps
getwd()
setwd("/home/oic/IiE_MGR/EkonometriaPrzestrzennaR")
getwd()

pl <- readOGR("./data", "Panstwo") # 1 jedn.
pl <- spTransform(pl, CRS("+proj=longlat +datum=NAD83"))
plot(pl)

woj <- readOGR("./data", "wojewodztwa") # 16 jedn.
woj <- spTransform(woj, CRS("+proj=longlat +datum=NAD83"))
plot(woj)

# 6 powiatow ma taka sama nazwe
# gmin ponad 2000

pow <- readOGR("./data", "powiaty") # 380 jedn.
pow <- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
plot(pow)

class(pow)

# CRS - Sferyczne dane 
# MERCS - Planarne dane

# wczytywanie danych
dane <- read.csv("data/data_nts4_2019_pl.csv",
                 sep = ";", dec = ",", header = TRUE)

dim(dane)
summary(dane)
names(dane)

head(dane)

# konwersja danych z sp do df
pow.df <- as.data.frame(pow)

head(pow.df)

# współrzędne środków powiatów
crds <- coordinates(pow)

# mapa administracyjna
plot(pl, lwd = 3)
plot(pow, add = TRUE)
plot(woj, add = TRUE, lwd = 2)
points(crds, pch = 21, bg = "red", cex = 0.8)

# podpisywanie regionów
# zamiast NAME_2 wybrać zmienną np. stolic regionów
plot(pow)

pow.df <- pow.df %>% lapply(., iconv, to = "UTF-8") %>% tibble::as_tibble()

pointLabel(crds, as.character(pow.df$jpt_nazwa_), cex = 0.6)

plot(pow)
text(crds, as.character(pow.df$jpt_nazwa_), cex = 0.6)

# interaktywna edycja nazw regionów
plot(pow)
identify(crds, labels = as.character(pow.df$jpt_nazwa_))

# wycinek mapy – jeden region (sp)

woj.df <- as.data.frame(woj)
summary(woj.df)

pow.df <- as.data.frame(pow)
summary(pow.df)

# mapa regionalna – województwo lubelskie
# wersja A – zapisanie osobno bazy danych z shp
woj.df <- as.data.frame(woj)
lub.woj <- woj[woj.df$jpt_nazwa_ == "lubelskie", ]
plot(lub.woj, main = "Lubelskie NTS2")

# wersja B – skorzystanie ze slotu data w pliku klasy sp
plot(woj[woj@data$jpt_nazwa_ == "lubelskie", ],
     main = "Lubelskie NTS2")

# mapa powiatów woj.lubelskiego
# pow.df <- as.data.frame(pow) # nie ma identyfikatora województw
dane15 <- dane[dane$rok == 2015, ]
lub.pow <- pow[dane15$wojew_nazwa == "Lubelskie", ]
plot(lub.pow, main = "Lubelskie NTS4")
plot(lub.woj, add = TRUE, lwd = 2)

woj.df <- as.data.frame(woj)
woj1 <- woj[woj.df$jpt_nazwa_ == "kujawsko-pomorskie", ]
woj2 <- woj[woj.df$jpt_nazwa_ == "wielkopolskie", ]
plot(woj2)
plot(woj1, add = TRUE)

?spRbind

woj3 <- spRbind(woj1, woj2)
plot(woj3)
woj3.df <- as.data.frame(woj3)
woj3.df
crds.woj3 <- coordinates(woj3)
points(crds.woj3)
text(jitter(crds.woj3), labels = woj3.df$jpt_nazwa_)

# mapa kolorystyczna – testowanie lokalizacji
x <- dane$wojew_nr[dane$rok == 2006]
brks <- (0 : 16) * 2
cols <- c("blue3", "cornflowerblue", "seagreen1", "yellow",
          "chocolate1", "orangered1", "brown3", "coral4",
          "salmon4", "aquamarine3", "darkgreen", "chartreuse3",
          "cyan4", "darkred", "darkviolet", "yellow", "blue")
plot(pow, col = cols[findInterval(x, brks)], border = "grey50")
plot(woj, add = TRUE, lwd = 2)

# etykiety nazw województw
crds.woj <- coordinates(woj)
woj.df <- as.data.frame(woj)
woj.df <- woj.df %>% lapply(., iconv, to = "UTF-8") %>% tibble::as_tibble()

text(crds.woj, label = woj.df$jpt_nazwa_, cex = 0.8, font = 2)

# to samo, ale z cienowaniem
dens <- (2:length(brks)) * 3
par(mar = c(1, 1, 1, 1))
plot(pow,
     density = dens[findInterval(x, brks, all.inside = TRUE)],
     border = "grey80")
plot(woj, add = TRUE, lwd = 1)
par(mar = c(5, 4, 4, 2))

# mapa stopy bezrobocia wg powiatów
library(classInt)
zmienna <- dane$XA21[dane$rok == 2009]
summary(zmienna)

przedzialy <- 8
kolory <- brewer.pal(przedzialy, "BuPu")  # wybór kolorów
klasy <- classIntervals(zmienna, przedzialy, style = "fixed",
fixedBreaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40))
tabela.kolorów <- findColours(klasy, kolory)

plot(pow, col = tabela.kolorów)
plot(woj, lwd = 2, add = TRUE)
legend("bottomleft", legend = names(attr(tabela.kolorów, "table")),
       fill = attr(tabela.kolorów, "palette"), cex = 1, bty = "n")
title(main = "Stopa bezrobocia w powiatach w 2009r.")

savePlot(filename = "rys01", type = "jpg")

# sposób z colorRampPalette() z grDevices::
library(shape)
zmienna <- dane$XA21[dane$rok == 2015]
maxy <- 40
breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)

nclr <- 8
plotclr <- brewer.pal(nclr, "Reds") # z pakietu RColorBrewer
fillRed <- colorRampPalette(plotclr) # z pakietu grDevices
colcode <- fillRed(maxy)[round(zmienna) + 1] #fillRed jest funkcją

plot(pow, col = colcode, lty = 0, border = "gray")
plot(woj, add = TRUE, lwd = 1, border = "gray60")
map.scale(x = 17.5, y = 49.17, ratio = FALSE,
          relwidth = 0.15, metric = TRUE, cex = 0.65)
compassRose(15, 49.7, rot = 0, cex = 1) # z pakietu sp
colorlegend(posy = c(0.05, 0.9), posx = c(0.9, 0.92), col = fillRed(maxy),
            zlim = c(0, maxy), zval = breaks, main.cex = 0.9) # z pakietu shape
title(main = "Stopa bezrobocia w 2015 r.",
      sub = "Na poziomie NTS4, wg danych GUS")

# Kod startowy – wczytywanie danych obszarowych – w klasie sf

# starter - pakiety
library(sf)
library(spdep)
library(ggplot2)

# ustawienie Working Directory

# Uwaga: w tym pliku jest tak:
# mapy, których nazwa obiektu jest pisana małymi literami (pow, woj)
# są wczytane w klasie sp, a te pisane WIELKIMI LITERAMI (POW, WOJ) w sf

# wczytanie map w klasie sf
POW <- st_read("data/powiaty.shp")
WOJ <- st_read("data/wojewodztwa.shp")

# zmiana projekcji
POW <- st_transform(POW, 4326) 	# konwersja 4326=WGS84, 4267=NAD27
WOJ <- st_transform(WOJ, 4326) 	# konwersja do WGS84

POW

### informacyjnie - sprawdzenie co to za projekcja
st_crs(27700)$proj4string
#[1] "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"

st_crs(4326)$proj4string
#[1] "+proj=longlat +datum=WGS84 +no_defs"

# przy okazji, kod dla NAD27 to 4267
st_crs(4267)$proj4string

# konwersja wsteczna z sf do sp
POW.sp <- as_Spatial(POW, cast = TRUE, IDs = "jpt_kod_je")

# wczytywanie danych w csv
dane <- read.csv("data/data_nts4_2019_pl.csv",
                 sep = ";", dec = ",", header = TRUE)
dim(dane)

# Mapa konturowa z centroidami - sf

# mapa konturowa z użyciem komendy plot()
plot(st_geometry(POW))		# rysuje kontur

# mapa konturowa z użyciem komendy ggplot()
ggplot() + geom_sf(data = POW)

# mapa konturowa z centroidami
plot(st_geometry(POW))	# rysuje kontur
plot(st_geometry(st_centroid(POW)), pch = 21, bg = "red", add = TRUE)

# obiekt z koordynatami - centroidami
crds <- st_centroid(POW) # nowa geometria punktowa w klasie sf i data.frame
head(crds)

# Mapa konturowa z warstwą kolorystyczną - sf

# sposób z plot()

# dopisujemy zmienną ze zbioru i rysujemy
POW$dist <- dane$dist[dane$rok == 2017]
plot(POW["dist"], key.pos = 4)

# to nie działa świetnie
plot(POW) 	# rysuje wszystkie zmienne

# sposób z choroLayer() z cartpgraphy::

# w help jest sporo funkcji ustawiających szczegóły
#https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html

library(cartography)

# grafika niebieska
choroLayer(POW, var = "dist")
title("Distance from poviat to regional core city")

# grafika beżowa
plot(st_geometry(POW), col = NA, border = "white", bg = "#aadaff")
choroLayer(x = POW, var = "dist", method = "equal", nclass = 5,
  col = carto.pal(pal1 = "sand.pal", n1 = 5),
  border = "white", lwd = 0.5, legend.pos = "bottomleft",
  legend.title.txt = "Distance to regional core city", add = TRUE)
layoutLayer(title = "Spatial accessibility within NTS2 regions, Poland",
            sources = "Sources: DDL GUS",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE,
            theme = "sand.pal")
north(pos = "topleft") # north arrow

#### TUTAJ SKONCZONE ZAJECIA 02 ############################################################

# sposób z ggplot()

# wykres danych regionalnych – zmienna dist
POW$wojew_nazwa <- dane15$wojew_nazwa

ggplot() +
    geom_sf(data = POW, aes(fill = dist)) +
    scale_y_continuous(breaks = 34:36)

# zapisanie rysunku do obiektu + wywołanie
WOJ$wojew_nr <- 1:16

plot1 <- ggplot() + geom_sf(WOJ, mapping = aes(fill = wojew_nr)) +
    labs(title = "Podział administracyjny NUTS2, Polska")

plot1

POW$XA21_15 <- dane15$XA21

ggplot() +
    geom_sf(POW, mapping = aes(geometry = geometry, fill = XA21_15)) +
    scale_fill_gradient(low = "#d3e7e7",  high = "#300bdd") +
    labs(title = "Unemployment rate in NTS4 poviats, 2015")

# interaktywne mapy z tmap::
# wczytanie pakietu
library(tmap)

# mapa konturowa na szaro
qtm(POW)

# tworzy mapę w przeglądarce www – interaktywną i z podkładem GIS
tmap_mode("view")

tm_shape(POW) + tm_fill("dist", palette = sf.colors(5))

# zapisuje ostatnią aktywną mapę w oknie R
ttm()

tmap_last()

# interaktywne mapy z mapview::

# z użyciem pakietu mapview::
library(mapview)

mapviewOptions(fgb = FALSE) # needed when creating web pages
mapview(POW["dist"], col.regions = sf.colors(10), fgb = FALSE)

# Jeśli nie działa viewer() na linuxie wpisać
# options(browser = "/usr/lib/firefox/firefox")

# kilka map razem

# lokalizacja kilku dużych miast – obiekt klasy df
miasta <- data.frame(city = 0, x = 0, y = 0)# pusty obiekt
miasta[1, ] <- c("Warszawa", 20.7810088, 52.2330269)# lokalizacja Warszawa
miasta[2, ] <- c("Kraków", 19.8647884, 50.0469018)# lokalizacja Kraków
miasta[3, ] <- c("Gdańsk", 18.5499432, 54.3612063)# lokalizacja Gdańsk
miasta[4, ] <- c("Poznań", 16.3603594, 52.3997116)# lokalizacja Poznań

# sprawdzamy, czy dane są numeryczne + konwersja na numeryczne
summary(miasta)

miasta$x <- as.numeric(as.character(miasta$x))
miasta$y <- as.numeric(as.character(miasta$y))

summary(miasta)

# zmiana klas – df sp sf
miasta.sp <- miasta # przygotowanie do zmiany klasy na sp
coordinates(miasta.sp) <- c("x", "y") # zmiana klasy na sp
proj4string(miasta.sp) <- CRS("+proj=longlat +datum=NAD83")
miasta.sf <- st_as_sf(miasta.sp) # konwersja do sf

# geometrie - okręgi o promieniu 100 km od miast
# promień 1km to dist=1000
circle.waw <- st_buffer(miasta.sf[1, ], dist = 100000)
circle.kra <- st_buffer(miasta.sf[2, ], dist = 100000)
circle.gda <- st_buffer(miasta.sf[3, ], dist = 100000)
circle.poz <- st_buffer(miasta.sf[4, ], dist = 100000)

# ucięcie powiatów wg okręgów
pow.circle.waw <- st_intersection(POW, circle.waw)
# nie działa bo różne projekcje

# ujednolicenie projekcji – do NAD27
POW <- st_transform(POW, 4267) 	# konwersja 4326=WGS84, 4267=NAD27
circle.waw <- st_transform(circle.waw, 4267) # konwersja 4326=WGS84, 4267=NAD27
circle.kra <- st_transform(circle.kra, 4267) # konwersja 4326=WGS84, 4267=NAD27
circle.gda <- st_transform(circle.gda, 4267) # konwersja 4326=WGS84, 4267=NAD27
circle.poz <- st_transform(circle.poz, 4267) # konwersja 4326=WGS84, 4267=NAD27

# ucięcie powiatów wg okręgów
POW$dist <- dane$dist[dane$rok == 2017]	# dodanie zmiennej do mapy
POW.circle.waw <- st_intersection(POW, circle.waw) # złożenie warstw
POW.circle.kra <- st_intersection(POW, circle.kra)
POW.circle.gda <- st_intersection(POW, circle.gda)
POW.circle.poz <- st_intersection(POW, circle.poz)

# grafika w oknie R
plot(st_geometry(POW.circle.waw))	# rysuje kontur

ggplot() +
    geom_sf(data = POW.circle.waw, aes(fill = dist)) +
    scale_y_continuous(breaks = 34:36)

# grafika interaktywna w przeglądarce
mapview(POW, zcol = "dist") # map + variable

mapview(POW.circle.waw, zcol = NULL) # just contours of selected regions

# mapy interaktywne wielowarstwowe
# powiaty + okręgi z powiatami i zmienną
mapview(POW.circle.waw, zcol = "dist") + mapview(POW.circle.kra, zcol = "dist") + mapview(POW.circle.gda, zcol = "dist") + mapview(POW.circle.poz, zcol = "dist") + mapview(POW, zcol = NULL)

# to samo + dodatkowo punkty
mapview(miasta.sf) + mapview(POW.circle.waw, zcol = "dist") + mapview(POW.circle.kra, zcol = "dist") + mapview(POW.circle.gda, zcol = "dist") + mapview(POW.circle.poz, zcol = "dist") + mapview(POW, zcol = NULL)

# Kod startowy – wczytywanie danych punktowych – sp i sf

# wczytywanie danych punktowych
firmy <- read.csv("data/point_data.csv", header = TRUE, dec = ".", sep = ",")
summary(firmy)

# ograniczenie liczby kolumn
firmy.sp <- firmy[,c("SEC_PKD7", "empl", "subreg", "poviat", "roa", "coords.x1", "coords.x2")]

# zmiana klasy z data.frame na sp
coordinates(firmy.sp) <- c("coords.x1", "coords.x2") # zmiana klasy na sp

# nadanie projekcji danym punktowym
proj4string(firmy.sp) <- CRS("+proj=longlat +datum=NAD83")
firmy.sp <- spTransform(firmy.sp, CRS("+proj=longlat +datum=NAD83"))

# konwersja z sp do sf
firmy.sf <- st_as_sf(firmy.sp)

# Mapa punktowa – sp – tylko lokalizacje – plot() i points()

# punkty w klasie data frame + wycinek mapy z sp

# wykres punktowy danych empirycznych z plot() / points()
plot(woj[woj@data$jpt_nazwa_ == "lubelskie", ])
points(firmy$coords.x1, firmy$coords.x2, pch = ".")

# Mapa punktowa – sp - lokalizacje i kolory wartości – spplot()

# punkty z klasy sp, bez konturu
# działa dla zmiennych numerycznych
spplot(firmy.sp["subreg"])

# Mapa punktowa – sp - lokalizacje i wielkość wartości – plot()

# wersja A – bez projekcji punktów
# wybrane zmienne: współrzędne x i y, zatrudnienie, sektor
firmy.sel <- firmy[1:2000, c(58, 59, 22, 3)] # wybrane kolumny
colnames(firmy.sel) <- c("x", "y", "empl", "sector") # zmiana nagłówków
coordinates(firmy.sel) <- c("x", "y") # zmiana klasy

# działa na klasie sp - komenda plot() rysuje punkty
par(mar = c(2, 2, 2, 2)) # marginesy wykresu
plot(firmy.sel, pch = 1, cex = sqrt(firmy.sel$empl) / 3, axes = TRUE)
v <- c(5, 30, 150, 600) # skala legendy
legend("topleft", legend = v, pch = 1, pt.cex = sqrt(v) / 10, bty = "n")
plot(woj[woj@data$jpt_nazwa_ == "lubelskie", ],
     add = TRUE, lwd = 2)
par(mar = c(5, 4, 4, 2))

# wersja B – z projekcją punktów
# wybrane zmienne: współrzędne x i y, zatrudnienie, sektor
firmy.sel <- firmy[1:2000, c(58, 59, 22, 3)] # wybrane kolumny
colnames(firmy.sel) <- c("x", "y", "empl", "sector") # zmiana nagłówków
coordinates(firmy.sel) <- c("x", "y") # zmiana klasy
proj4string(firmy.sel) <- CRS("+proj=longlat +datum=NAD83")
firms.sel <- spTransform(firmy.sel, CRS("+proj=longlat +datum=NAD83"))

# działa na klasie sp - komenda plot() rysuje punkty
par(mar = c(2, 2, 2, 2)) # marginesy wykresu
plot(firmy.sel, pch = 1, cex = sqrt(firmy.sel$empl) / 3, axes = TRUE)
v <- c(5, 30, 150, 600) # skala legendy
legend("topleft", legend = v, pch = 1, pt.cex = sqrt(v) / 10, bty = "n")
plot(woj[woj@data$jpt_nazwa_ == "lubelskie", ], add = TRUE, lwd = 2)
par(mar = c(5, 4, 4, 2))

# Mapa punktowa – sp - lokalizacje i kolor wartości – plot() i findInterval()

zmienna <- firmy$subreg
locs <- firmy[, 58:59]
summary(zmienna)
brks <- c(1, 2, 3, 4)
cols <- c("blue3", "cornflowerblue", "seagreen1", "green2")
# ograniczony kontur, ograniczone punkty
plot(woj[woj@data$jpt_nazwa_ == "lubelskie", ])
points(locs, col = cols[findInterval(zmienna, brks)], 
       pch = 21, cex = 0.7,
       bg = cols[findInterval(zmienna, brks)])
legend("bottomleft", legenda = brks, fill = cols, cex = 0.8, bty = "n")
title(main = "Points - colors by values")

savePlot(filename = "locations and random values", type = "jpg")

# powiaty jako centroidy
zmienna <- dane$XA14[dane$rok == 2015]
summary(zmienna)
crds <- coordinates(pow)
brks <- c(60, 80, 100, 120, 140, 160, 180)
size <- (brks / 100) * 1.2
cols = brewer.pal(7, "Reds")
plot(pow, border = "grey90") #Fig.2.10b
plot(woj, border = "grey50", add = TRUE)
points(crds, col = cols[findInterval(zmienna, brks)],
       cex = size[findInterval(zmienna, brks)],
       pch = 21, bg = cols[findInterval(zmienna, brks)])
    legend("bottomleft", legend = brks, 
           pt.bg = cols, pt.cex = size, bty = "n", pch = 21)
    title(main = "Average salary in Poland = 100% year 2015",
    sub = "In the legend, the interval from ...")

savePlot(filename = "Average salary", type = "jpg")

# Mapa punktowa – sf - lokalizacje i kolor wartości – plot()

# rysuje wszystkie zmienne z zbiorze
plot(firmy.sf)

plot(firmy.sf, max.plot = 2)

# automatyczny dobór kolorów dla wybranej zmiennej
plot(firmy.sf["subreg"], key.pos = 4, pch = "*")

# Mapa punktowa – sf - lokalizacje i kolor wartości – ggplot()

# typowy rysunek punktowy ggplot() - lokalizacje z obiektu w klasie sf
ggplot() + geom_sf(data = firmy.sf, aes(col = subreg))

ggplot() + geom_sf(data = firmy.sf, aes(col = poviat))

# mapa pokładowa w klasie sf

# punkty w województwa lublekiego na pełnej mapie Polski
ggplot() +
    geom_sf(data = WOJ) +
    geom_point(aes(x = coords.x1, y = coords.x2, size = zatr / 5),
               data = firmy, alpha = 0.5, color = "darkred", size = 0.75)


# punkty w województwa lubelskiego na wybranym konturze województwa lubelskiego
lub.WOJ <- WOJ[WOJ$jpt_nazwa_ == "lubelskie", ]
ggplot() + geom_sf(data = lub.WOJ) +
    geom_point(aes(x = coords.x1, y = coords.x2, size = zatr / 5),
               data = firmy, alpha = 0.5, color = "darkred", size = 0.75)

# mapa gęstości punktów (stat_density2d) (punkty z data.frame)
ggplot() + geom_sf(data = lub.WOJ) +
    stat_density2d(aes(x = coords.x1, y = coords.x2,
                       fill = ..level.., alpha = 0.25),
                   size = 0.01, bins = 30, data = firmy, geom = "polygon")


# mapa gęstości punktów (stat_density2d) i izoliniami (geom_density2d)
ggplot() + geom_sf(data = lub.WOJ) +
    stat_density2d(aes(x = coords.x1, y = coords.x2,
                       fill = ..level.., alpha = 0.25),
                   size = 0.01, bins = 30, data = firmy,
                   geom = "polygon") +
    geom_density2d(data = firmy, aes(x = coords.x1, y = coords.x2),
                   size = 0.3)

# mapa z etykietami miast
# wczytywanie danych nt miast
miasta.lub <- read.csv("data/miasta lubelskie.csv",
                       sep = ";", dec = ",", header = TRUE)
summary(miasta.lub)
miasta.lub$label <- paste(miasta.lub$pozycja, miasta.lub$miasto, sep = ".")

# mapa podkładowa z konturem (geom_polygon),
# gęstością punktów (stat_density2d) i
# etykietami (geom_point, geom_label_repel)
library(ggrepel)

ggplot() +
    geom_sf(data = lub.WOJ) +
    stat_density2d(aes(x = coords.x1, y = coords.x2,
                       fill = ..level.., alpha = 0.25),
                   size = 0.01, bins = 30, data = firmy,
                   geom = "polygon") +
    geom_point(aes(x = yy, y = xx, stroke = 2),
               colour = "grey80", data = miasta.lub, size = 0.5) +
    geom_label_repel(aes(x = yy, y = xx, label = miasto),
                     data = miasta.lub, family = "Times",
                     size = 2, box.padding = 0.2,
                     point.padding = 0.3, segment.color = "grey50")

# rysunek zbiorczy dla wielu lat

# dodanie sztucznej zmiennej rok do danych punktowych

firmy$rok <- sample(2012:2020, size = dim(firmy)[1], replace = TRUE)

ggplot(lub.WOJ) +
    geom_point(data = firmy, alpha = 0.5, 
               aes(x = xxe, y = yye, color = roa)) +
    scale_colour_gradient(low = "yellow", high = "red") +
    facet_wrap(~rok) +
    ggtitle("Roa of firms by years") +
    labs(fill = "ROA", x = "", y = "")

# sposób z propSymbolsLayer() z cartpgraphy::

library(sf)
library(cartography)

# wczytanie i konwersja danych punktowych nt. miast w woj.lubelskim
miasta.lub <- read.csv("miasta lubelskie.csv",
                       sep = ";", dec = ",", header = TRUE)
summary(miasta.lub)
miasta.sp <- miasta.lub
coordinates(miasta.sp) <- c("xx", "yy") # zmiana klasy na sp
proj4string(miasta.sp) <- CRS("+proj=longlat +datum=NAD83")
miasta.sp <- spTransform(miasta.sp, CRS("+proj=longlat +datum=NAD27"))
miasta.sf <- st_as_sf(miasta.sp)

lub.WOJ <- WOJ[WOJ$jpt_nazwa_ == "lubelskie", ]
dane15 <- dane[dane$rok == 2015, ]
lub.POW <- POW[dane15$wojew_nazwa == "Lubelskie", ]
lub.POW$podreg72 <- dane15$podreg72_nr[dane15$wojew_nazwa == "Lubelskie"]

# podkład konturowy
plot(st_geometry(lub.POW), col = "grey80", border = "grey")
# warstwa punktowa
propSymbolsLayer(x = miasta.sf, var = "Ludność", inches = 0.25,
                 col = "brown4", legend.pos = "topright",
                 legend.title.txt = "Liczba ludności")
# legendy
layoutLayer(title = "Ludność w miastach województwa lubelskiego",
            sources = "Sources: GUS BDL",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
north(pos = "topleft")

# interaktywne mapy punktowe z mapview::

# z użyciem pakietu mapview::
library(mapview)

# firmy wg sektorów
mapview(firmy.sf, zcol = "SEC_PKD7") # map + variable

# firmy z sektora M
mapview(firmy.sf[firmy.sf$SEC_PKD7 == "M", ], zcol = "roa", cex = "empl")

# firmy z sektora M + mapa podkładowa wojewódzka
mapview(lub.WOJ, zcol = NULL, alpha.regions=0.1) + mapview(firmy.sf[firmy.sf$SEC_PKD7 == "M",], zcol = "roa", cex = "empl")

# firmy z sektora M + mapa podkładowa powiatowa z podziałem na podregiony

mapview(lub.WOJ, zcol = NULL, alpha.regions = 0.1) + mapview(lub.POW, zcol = "podreg72", alpha.regions = 0.1) + mapview(firmy.sf[firmy.sf$SEC_PKD7 == "M",], zcol = "roa", cex = "empl")

# * Mapy konturowe dostępne on-line
library(raster)
ccodes() # pełna lista
ccodes()[181, ] # lista ograniczona do jednego kraju

woj <- getData("GADM", country = "PL", level = 1) # z pakietu raster::
attributes(woj)	# sprawdzenie atrybutów i nazw slotów
woj$NAME_1	# nazwy podregionów
dim(woj) # długość zbioru – liczba regionów

pow <- getData("GADM", country = "PL", level = 2) # inny poziom agregacji
gm <- getData("GADM", country = "PL", level = 3) # inny poziom agregacji
pol <- getData("GADM", country = "PL", level = 0) # inny poziom agregacji

plot(pol, lwd = 3)	# rysunek warstwowy dwóch wyższych poziomów agreg.
plot(woj, add = TRUE)

plot(gm, border = "grey70") # rys. warstw. 2 niższych poziomów agreg.
plot(pow, add = TRUE)

# pobieranie map z www – mapa województw – dane w pliku zip

shpurl <- "http://www.gis-support.pl/downloads/wojewodztwa.zip"
tmp <- tempfile(fileext = ".zip")
download.file(shpurl, destfile = tmp)
files <- unzip(tmp, exdir = getwd())
shp <- readOGR(files[grep(".shp$", files)])
plot(shp)
