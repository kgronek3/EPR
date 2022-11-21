library(spdep)
library(rgdal)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)
library(maps)
library(sf)
library(ggplot2)
library(cartography)

setwd("/home/oic/IiE/EkonometriaPrzestrzennaR")

# obiekty klasy sp są pisane małymi literami (pow, woj)
# obiekty klasy sf są pisane WIELKIMI LITERAMI (POW, WOJ)

#### mapy sp ####
pow<-readOGR(".", "powiaty") # 380 jedn. 
pow<-spTransform(pow, CRS("+proj=longlat +datum=NAD83"))

POW <- st_read("./data/powiaty.shp")
POW <- st_transform(POW, 4326)

# wczytanie map w klasie sf
POW<-st_read("powiaty.shp")
POW<-st_transform(POW, 4326) 	# konwersja 4326=WGS84, 4267=NAD27 

# macierz wag według kryterium wspólnej granicy

# rozwiązania w sf i sp nie różnią się znacząco

# rozwiązanie w sp
cont.nb<-poly2nb(as(pow, "SpatialPolygons"))		# class nb
# cont.nb <- poly2nb(pow) # alternatywa
cont.listw<-nb2listw(cont.nb, style="W")		# class listw	
cont.listw # wyświetla podusmowanie


# rozwiązanie w sf
cont.sf<- poly2nb(POW)						# class nb
cont.listw<-nb2listw(cont.sf, style="W")		# class listw

# sprawdzenie klas
class(cont.nb)
class(cont.listw)

# jak to obejrzeć i narysować

# wykres sąsiedztwa w sp
crds<-coordinates(pow)	# współrzędne centroidów
plot(pow) # mapa konturowa
plot(cont.nb, crds, add=TRUE)

# wykres sąsiedztwa w sf
crds.sf<-st_centroid(st_geometry(POW)) # współrzędne centroidów
plot(st_geometry(POW)) # contour map
plot(cont.nb, crds.sf, add=TRUE) 

# konwersja do klasy matrix
cont.mat<-nb2mat(cont.nb)
cont.mat[1:6, 1:6]
summary(cont.listw)

#### macierz sąsiedztwa dla k-najbliższych sąsiadów (knn) ####

# macierz knn może być niesymetryczna!
# niesymetryczna macierz nb nie konwertuje się do listw
# macierz trzeba wtedy usymetrycznić (dodać extra powiązania)

# rozwiązanie w sp
crds<-coordinates(pow)	# współrzędne centroidów
pow.knn<-knearneigh(crds, k=1) # obiekt klasy knn k=1
class(pow.knn)		# knn
pow.knn.nb<-knn2nb(pow.knn) 

# usymetrycznienie
is.symmetric.nb(pow.knn.nb)				# FALSE
pow.knn.sym.nb<-make.sym.nb(pow.knn.nb)
is.symmetric.nb(pow.knn.sym.nb)			# TRUE

# tworzenie obiektu klasy listw 
pow.knn.sym.listw<-nb2listw(pow.knn.sym.nb)

# rozwiązanie w sf
crds.sf<-st_centroid(st_geometry(POW)) # współrzędne centroidów
pow.knn.sf<-knearneigh(crds.sf, k=1) 	# k to liczba sąsiadów
pow.knn.nb.sf<-knn2nb(pow.knn.sf)
pow.knn.sym.listw<-nb2listw(make.sym.nb(pow.knn.nb.sf))

# rysowanie powiązań w sp
plot(pow)
plot(knn2nb(pow.knn), crds, add=TRUE)

# rysowanie powiązań w sf
plot(st_geometry(POW))
plot(knn2nb(pow.knn.sf), crds.sf, add=TRUE)

# konwersja do klasy matrix
knn.mat<-nb2mat(pow.knn.sym.nb)
knn.mat[1:6, 1:6]

#### macierz sąsiedztwa dla sąsiadów w promieniu d km ####

# rozwiązanie w sp
crds<-coordinates(pow)	# współrzędne centroidów
conti30.nb<-dnearneigh(crds, 0, 30,longlat=TRUE)	# liczymy w km
class(conti30.nb)			# klasa nb
conti30.listw<-nb2listw(conti30.nb, zero.policy=TRUE)
class(conti30.listw)			# klasa listw i nb

# rozwiązanie w sf
crds.sf<-st_centroid(st_geometry(POW)) # współrzędne centroidów
conti30.nb<-dnearneigh(crds.sf, 0, 30,longlat=TRUE)	# liczymy w km
class(conti30.nb)			# klasa nb
conti30.listw<-nb2listw(conti30.nb, zero.policy=TRUE)
class(conti30.listw)			# klasa listw i nb

# rysowanie powiązań w sp
plot(pow)
plot(conti30.nb, crds, add=TRUE)

# rysowanie liczby powiązań w sp
conti30.m<-nb2mat(conti30.nb, zero.policy=TRUE)
a<-colMeans(t(conti30.m))
pow$a<-a
spplot(pow, "a")


# przy jakiej odległości d wszystkie obszary mają przynajmniej jednego sąsiada?

# domyślnie k=1, lista najbliższych sąsiadów
kkk<-knn2nb(knearneigh(crds))

# dist pomiędzy najbliższym sąsiadami  dostajemy max dist
wszyscy<-max(unlist(nbdists(kkk, crds)))

# sąsiedzi w promieniu d km 
# gwarancja, że każdy region ma przynajmniej 1 sąsiada
wszyscy.nb<-dnearneigh(crds, 0, wszyscy)

summary(wszyscy.nb, crds)
plot(pow, border="grey") 
plot(wszyscy.nb, crds, add=TRUE)

#### macierz odwrotnej odległości ####

# rozwiązanie w sp
# z wykorzystaniem funkcji nb2listwsdist()
# opcja type="idw" – inverse distance weighting
# opcja type="exp" – exponential distance decay
# opcja type="dpd" – double-power distance weights

crds<-coordinates(pow)			# współrzędne centroidów
pow.knn<-knearneigh(crds, k=379)	# knn dla wszystkich regionów 
pow.nb<-knn2nb(pow.knn)

# macierz odwrotnej odległości
inv.dist<-nb2listwdist(pow.nb, pow, type="idw", style="raw", alpha=1, dmax=NULL, longlat=TRUE, zero.policy=NULL) # klasa "listw" "nb"   

# macierz odwrotnej kwadratowej odległości
dpd.dist<-nb2listwdist(pow.nb, pow, type="dpd", style="raw", alpha=1, dmax=200, longlat=TRUE, zero.policy=NULL) # klasa "listw" "nb"   

# macierz wykładniczo wygasającej odległości
exp.dist<-nb2listwdist(pow.nb, pow, type="exp", style="raw", alpha=1, dmax=NULL, longlat=TRUE, zero.policy=NULL) # klasa "listw" "nb"   

# mapa wag dla wybranego regionu
inv.dist.mat<-listw2mat(inv.dist)
dpd.dist.mat<-listw2mat(dpd.dist)
exp.dist.mat<-listw2mat(exp.dist)

summary(inv.dist.mat)# statystyki wag kolejnego regionu
summary(dpd.dist.mat)# statystyki wag kolejnego regionu
summary(exp.dist.mat)# statystyki wag kolejnego regionu

POW$inv.dist.WAW<-inv.dist.mat[,151]	# Warszawa to wiersz 151
POW$dpd.dist.WAW<-dpd.dist.mat[,151]	# Warszawa to wiersz 151
POW$exp.dist.WAW<-exp.dist.mat[,151]	# Warszawa to wiersz 151

choroLayer(POW, var="inv.dist.WAW")
choroLayer(POW, var="dpd.dist.WAW")
choroLayer(POW, var="exp.dist.WAW")

plot(POW["inv.dist.WAW"], key.pos=4)
plot(POW["dpd.dist.WAW"], key.pos=4)
plot(POW["exp.dist.WAW"], key.pos=4)

# rozwiązanie w sp (stare podejście)
crds<-coordinates(pow)			# współrzędne centroidów
pow.knn<-knearneigh(crds, k=379)	# knn dla wszystkich regionów
pow.nb<-knn2nb(pow.knn)
dist<-nbdists(pow.nb, crds) 		# macierz odległośco 
dist1<-lapply(dist, function(x) 1/x)  # obiekt klasy list, odwrotna odle
pow.dist.listw<-nb2listw(pow.nb, glist=dist1)	# obiekt listw


# rysunek odwrotnej odległości dla wybranego regionu
pow.df<-as.data.frame(pow)
pow.df$jpt_nazwa # Warszawa is 151
summary(x)
cols<-rev(heat.colors(11))
brks<-c(0, 0.001, 0.0015, 0.002, 0.0025, 0.003, 0.005, 0.01, 0.015, 0.02, 0.025)
plot(pow, col=cols[findInterval(x, brks)])
plot(woj, add=TRUE, lwd=2)
legend("bottomleft", legend=brks, pt.bg=cols, bty="n", pch=22, cex=0.8)
title(main="Inverse distance weigths", sub="In legend intervals from….%")

#### Podsumowywanie macierzy wag ####

summary(cont.nb) # macierz klasy nb

cont.nb.listw<-nb2listw(cont.nb)
summary(unlist(cont.nb.listw$weights))

cont.nb.mat<-nb2mat(cont.nb)
summary(cont.nb.mat)

table(card(cont.nb))

print(cont.nb.mat[1:6, 1:6])

cont.listw$weights

#### dyfuzja szoków ####

pow.df<-as.data.frame(pow)
head(pow.df)
unique(pow.df$jpt_nazwa_)

pow$SZOK<-rep(0,380)
pow$SZOK[pow.df$jpt_nazwa_=="powiat Szczecin"]<-1 #Szczecin, 292
pow$SZOK[pow.df$jpt_nazwa_=="powiat Warszawa"]<-1 #Warszawa, 148
#pow$SZOK[pow.df$jpt_nazwa_=="powiat Gdynia"]<-1 #Gdynia, 297
#pow$SZOK[pow.df$jpt_nazwa_=="powiat Pozna\xf1"]<-1 #Poznań, 206
names(pow$SZOK)<- "SZOK" 
spplot(pow, "SZOK") 

# opóźnienie przestrzenne zmiennej SZOK – pierwszy rząd
pow$lagg<-lag.listw(cont.listw, pow$SZOK)
summary(pow$lagg)
spplot(pow, "lagg")

# opóźnienie przestrzenne zmiennej SZOK – drugi rząd
powiaty.2.list<-nblag(cont.nb, 2)
powiaty.2.nb<-nblag_cumul(powiaty.2.list)

pow$lagg2<-lag.listw(nb2listw(powiaty.2.nb), pow$SZOK)
spplot(pow, "lagg2") 

# opóźnienie przestrzenne zmiennej SZOK – trzeci rząd
powiaty.3.list<-nblag(cont.nb, 3)
powiaty.3.nb<-nblag_cumul(powiaty.3.list)

pow$lagg3<- lag.listw(nb2listw(powiaty.3.nb), pow$SZOK)
spplot(pow, "lagg3") 

