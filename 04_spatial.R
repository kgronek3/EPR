library(spdep)
library(maptools)
library(sp)
        
# Mapa zjawiska

# rozkład przestrzenny – mapa stopy bezrobocia
dane09<-dane[dane$rok==2009, ]

x<-dane09$XA21 # wyodrębnienie zmiennej

summary(x)

cols<-rev(heat.colors(7))

brks<-(0:6)*5

plot(pow, col=cols[findInterval(x, brks)])

legend("bottomleft", legend=brks, pt.bg=cols, bty="n", pch=22)

title(main="Stopa bezrobocia w 2009r.",sub="W legendzie przedział od …")

# globalna statystyka Morana
dane09<-dane[dane$rok==2009, ]
wynik01<-moran(dane09$XA21, cont.listw, 
               length(cont.nb), Szero(cont.listw))
wynik02<-moran.test(dane09$XA21, cont.listw)
wynik03<-moran.test(dane09$XA21, cont.listw, randomisation=FALSE)
wynik01
wynik02
wynik03

# wartość p-value statystyki Morana (rozkład normalny)
pval.norm<-1-pnorm(wynik03$statistic, mean=0, sd=1)  
pval.norm

# wykres punktowy Morana 
# kod, który nie zadziała – dlaczego???
zmienna<-scale(dane09$XA21)# standaryzacja
moran.plot(zmienna, cont.listw, pch=19, labels=as.character(dane09$powiat))

# kod, który zadziała – dlaczego???
zmienna<-as.data.frame(scale(dane09$XA21))# standaryzacja
moran.plot(zmienna$V1, cont.listw, pch=19, labels=as.character(dane09$powiat))	

# wykres punktowy Morana 
# wersja krok po kroku
x<-dane09$XA21 # wyodrębnienie zmiennej
zx<-scale(x)  #standaryzacja zmiennej
mean(zx) # kontrola średniej
sd(zx) # kontrola odchylenia std.
wzx<-lag.listw(cont.listw, zx) # opóźnienie przestrzenne zmiennej x
morlm<-lm(wzx~zx) # regresja             
slope<-morlm$coefficients[2] # współczynnik kierunkowy
intercept<-morlm$coefficients[1] # stała
par(pty="s") # kwadratowe okno wykresu
plot(zx, wzx, xlab="zx",ylab="opóźnienie przestrzenne zx", pch="*")  
abline(intercept, slope)  # linia regresji
abline(h=0, lty=2) # linia pozioma w  y=0
abline(v=0, lty=2) #linia pionowa w x=0 

# mapa przynależności do ćwiartek wykresu punktowego Morana
# tworzenie zmiennej do analizy
x<-dane09$XA21 # wyodrębnienie zmiennej ze zbioru
zx<-scale(x)  #standaryzacja zmiennej
wzx<-lag.listw(cont.listw, zx) # opóźnienie przestrzenne zmiennej x
cond1<-ifelse(zx>=0 & wzx>=0, 1,0)  # I cwiartka
cond2<-ifelse(zx>=0 & wzx<0, 2,0)  # II cwiartka
cond3<-ifelse(zx<0 & wzx<0, 3,0)  # III cwiartka
cond4<-ifelse(zx<0 & wzx>=0, 4,0)  # IV cwiartka
cond.all<-cond1+cond2+cond3+cond4 # pokazuje przynależność do ćwiartek
cond.all
cond<-as.data.frame(cond.all)
is.data.frame(cond)

# wykres - mapa kolorystyczna
brks<-c(1,2,3,4)
cols<-c("grey25", "grey60", "grey85", "grey45")
plot(pow, col=cols[findInterval(cond$V1, brks)])
legend("bottomleft", legend=c("I ćw - HH - wysokie otoczone wysokimi", "II ćw - LH - niskie otoczone wysokimi", "III ćw - LL - niskie otoczone niskimi", "IV ćw - HL - wysokie otoczone niskimi"), fill=cols, bty="n", cex=0.85)
title(main="Przynależność regionów do ćwiartek 
z wykresu punktowego Morana")

# Obliczenia w pętli
# statystyka I Morana dla wszystkich lat w zbiorze danych

# przygotowanie obiektu do zapisania wyników
moran<-matrix(0, ncol=12, nrow=1)
colnames(moran)<-2006:2017
rownames(moran)<-"Moran’s I"
moran

for(i in 2006:2017){  # pętla wykorzustuje liczby naturalne
    result01<-moran.test(dane$XA21[dane$year==i], cont.listw)
    moran[1,i-2005]<-result01$estimate[1]}
moran

# podstawowy rysunek 
plot(moran[1,])

# udoskonalony rysunek 
plot(moran[1,], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0.3, 0.6))
axis(1, at=1:12, labels=2006:2017)
axis(2)
points(moran[1,], bg="red", pch=21, cex=1.5)
abline(h=c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6), lty=3, lwd=3, col="grey80")
abline(h=0.30+(0:30)*0.01, lty=3, lwd=1, col="grey80")
text(1:12, moran[1,]+0.015, labels=round(moran[1,],2))
title(main="Moran’s I over years
Unemployment rate in 2006-2017 in Poland")


#### Statystyki lokalne – LISA (Local Indicators of Spatial Association) ####
# zmienna do analizy
dane09$zmienna<-dane09$XA21

# lista lokalnych statystyk Morana z przypisaniem nazwy regionu
locM<-localmoran(spNamedVec("zmienna", dane09),cont.listw)
oid1<-order(dane09$ID_mapa_berkely)
printCoefmat(data.frame(locM[oid1,], row.names=dane09$powiat[oid1]), check.names=FALSE)

# mapa istotności statystyk lokalnych Morana
locM<-localmoran(spNamedVec("zmienna", dane09), cont.listw)
locMdf<-as.data.frame(locM)

# wykres kolorystyczny statystyki lokalnej Morana na mapie
brks<-c(min(locMdf[,5]), 0.05000, 0.95000, max(locMdf[,5]))
cols<-c("grey30", "grey90", "grey60")

plot(pow, col=cols[findInterval(locMdf[,5], brks)])
legend("bottomleft", legend=c("otoczony podobnymi wartościami, locM>0", "nieistotne", "otoczony odmiennymi wartościami, locM<0"), fill=cols, bty="n", cex=0.8)
title(main="statystyka Local Moran", cex=0.7)

# statystykia globalna C Geary'ego
geary(dane09$XA06, cont.listw, length(cont.nb), 
      length(cont.nb)-1, Szero(cont.listw))
geary.test(spNamedVec("XA06",dane09),cont.listw)

# testy kolorystyczne - statystyki i testy join-count
# podział graficzny obserwacji badanej zmiennej na trzy grupy wartości
zmienna<-dane09$XA21
# parametry grafiki
brks1<-c(0, 10, 20, 40) 
cols<-c("green", "blue", "red")

# wykres punktowy
plot(zmienna, bg=cols[findInterval(zmienna, brks1)], pch=21)
abline(h=c(10,20,40), lty=3)

# przestrzenny rozkład wartości na trzy wyróżnione grupy
brks1<- c(0,10,20,40) 
cols<-c("green", "blue", "red")
plot(pow, col=cols[findInterval(zmienna, brks1)])

plot(woj, add=TRUE, lwd=2)
title(main=" … ")
legend("bottomleft", legend=c("mało", "średnio", "dużo"), leglabs(brks1), fill=cols, bty="n")

#przygotowanie danych - podział danych na trzy grupy

zmienna.f<-factor(cut(zmienna, breaks=c(0,6, 18, 40), labels=c("mało", "średnio","dużo")))

joincount.test(zmienna.f, cont.listw)
joincount.multi(zmienna.f, cont.listw)
joincount.multi(zmienna.f, cont.listw)










