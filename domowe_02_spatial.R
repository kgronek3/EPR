# Zadanie 1:
# Przygotuj mapę powiatową z warstwą kolorystyczną – średnia płaca Polska=100.
# Podpisz interaktywnie najbardziej odstające regiony.

# Przygotowywanie danych
dane <- read.csv("data/data_nts4_2019_pl.csv",
                 sep = ";", dec = ",", header = TRUE, encoding = "latin1")
unique(dane$rok)
zmienna <- dane$XA14[dane$rok == 2017]
zmienna
length(zmienna)

min(zmienna)
max(zmienna)
length(seq(60,170,10))
przedzialy <- 12
kolory <- brewer.pal(przedzialy, "BuPu")  # wybór kolorów
klasy <- classIntervals(zmienna, przedzialy, style = "fixed",
                        fixedBreaks = c(60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170))
tabela.kolorów = findColours(klasy, kolory)

pow <- readOGR("./data", "powiaty") # 380 jedn.
pow <- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))

pow.df <- as.data.frame(pow)
pow.df <- pow.df %>%
    lapply(., iconv, from = "latin1", to = "UTF-8") %>%
    tibble::as_tibble()

crds <- coordinates(pow)
identify(crds, labels = as.character(pow.df$jpt_nazwa_))

powiaty_odstajace <- c(59, 98, 151, 171, 205, 304, 321, 360)

# wykres
plot(pow,
     col = tabela.kolorów)
pointLabel(crds[powiaty_odstajace, ],
           as.character(pow.df$jpt_nazwa_[powiaty_odstajace]), cex = 0.6)
legend("bottomleft",
       legend = names(attr(tabela.kolorów, "table")),
       fill = attr(tabela.kolorów, "palette"), cex = 1, bty = "n")
title(main = "Średnia płaca w powiatach w 2017r. (średnia = 100)")

# Zadanie 2:
# Przygotuj mapę powiatową odległości powiatów od ich miast wojewódzkich
# – użyj palety szarości. Wytrzyj kontury granic administracyjnych
# używając koloru transparent. Podpisz miasta wojewódzkie.

zmienna = dane$dist[dane$rok == 2017]
POW$dist <- dane$dist[dane$rok == 2017]
pow <- readOGR("./data", "powiaty") # 380 jedn.
pow <- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
pow.df <- as.data.frame(pow)
pow.df <- pow.df %>%
    lapply(., iconv, from = "latin1", to = "UTF-8") %>%
    tibble::as_tibble()
crds <- coordinates(pow)
stolice_woj <- which(dane$stol_woj[dane$rok == 2017] == 1)

plot(st_geometry(POW), col = NA, border = NA, bg = "white")
choroLayer(x = POW, var = "dist", method = "equal", nclass = 5,
           col = carto.pal(pal1 = "grey.pal", n1 = 5),
           border = NA, lwd = 0.5, legend.pos = "bottomleft",
           legend.title.txt = "Odległości powiatów do stolicy województwa",
           legend.horiz = T,
           add = TRUE)
layoutLayer(title = "Przestrzenna dostępność pośród regionów NTS2, Polska",
            sources = "Źródło: DDL GUS",
#            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE,
            theme = "grey.pal")
north(pos = "topleft") # north arrow
pointLabel(crds[stolice_woj, ],
           as.character(pow.df$jpt_nazwa_[stolice_woj]), cex = 0.6)

# Zadanie 3:
# Narysuj mapę średnich zarobków w wybranym województwie, innym niż
# Podkarpackie. Nanieś etykiety nazw powiatów wykorzystując komendę text().

dane_maz <- dane[dane$rok == 2015, ]
pow <- readOGR("./data", "powiaty") # 380 jedn.
pow <- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
maz.pow <- pow[dane_maz$wojew_nazwa == "Mazowieckie", ]

zmienna <- dane$XA14[dane$rok == 2017]
zmienna
przedzialy <- 12
kolory <- brewer.pal(przedzialy, "BuPu")  # wybór kolorów
klasy <- classIntervals(zmienna, przedzialy, style = "fixed",
                        fixedBreaks = c(60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170))
tabela.kolorów = findColours(klasy, kolory)

plot(woj[woj@data$jpt_nazwa_ == "mazowieckie", ])
plot(maz.pow,
     col = tabela.kolorów)
legend("bottomleft",
       legend = names(attr(tabela.kolorów, "table")),
       fill = attr(tabela.kolorów, "palette"), cex = 1, bty = "n")
title(main = "Średnia płaca w powiecie Mazowieckim w 2017r. (średnia = 100)")

crds <- coordinates(pow)
identify(crds, labels = as.character(pow.df$jpt_nazwa_))


# Zadanie 4:
# Dla danych punktowych, narysuj mapę odległości pomiędzy 5000 losowo wybranych
# punktów i miast powiatowych: Biała Podlaska, Zamość, Chełm, Lublin. Punkty
# połącz z najbliższym miastem. Dodaj kontury powiatowe. Wykorzystaj funkcję
# sp::spDistsN1() lub sf::st_distance().

# Zadanie 5:
# Na mapie podkładowej punktowej (lokalizacje firm z woj.lubelskiego) narysuj
# innym kolorem kontur wybranego powiatu i punkty zlokalizowane w granicach
# tego powiatu. Wykorzystaj funkcję sp::over() lub sf::st_join().

# Zadanie 6:
# Dla wybranych 500 firm, policz po ile firm mają w sąsiedztwie w promieniu
# x i jaka jest sektorowa struktura sąsiadów. Wykorzystaj komendę gBuffer().
