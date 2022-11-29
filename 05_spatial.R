# starter
library(spdep)
library(rgdal)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)
library(maps)
library(sf)
library(ggplot2)
library(spatialreg)

# ustalenie ścieżki dostępu do Working Directory
setwd("/home/oic/IiE/EkonometriaPrzestrzennaR")

pow<-readOGR("./data", "powiaty") # 380 jedn. 
pow<-spTransform(pow, CRS("+proj=longlat +datum=NAD83"))

POW<-st_read("./data/powiaty.shp")
POW<-st_transform(POW, 4326) 	# konwersja 4326=WGS84, 4267=NAD27 

#wczytywanie danych
dane<-read.csv("data/data_nts4_2019_pl.csv", sep=";", dec=",", header=TRUE) 
dim(dane)
summary(dane)
names(dane)

# Podstawowy model
# Dla 380 powiatów (NTS4) oszacuj model dla wynagrodzeń. Jako zmienne objaśniające wykorzystaj dostępne w zbiorze zmienne rozsądnie tłumaczące zjawisko, a także opóźnienie przestrzenne zmiennej objaśnianej etc. 

# podstawowy model regresji liniowej, rozkład przestrzenny reszt
# y	przeciętne wynagrodzenie Polska=100%		(XA14)
# x1	udział osób zatrudnionych w usługach	(XA18+XA19+XA20/XA15)
# x2	stopa bezrobocia						(XA21)
# x3	liczba spółek per capita Polska=100%	(XA13/mean(XA13))
# x4	odległość powiatu od miasta wojew.		(dist)

# przygotowanie danych
sub<-dane[dane$rok==2016, ]
sub$y<-sub$XA14
sub$x1<-(sub$XA18+sub$XA19+sub$XA20)/sub$XA15
sub$x2<-sub$XA21
sub$x3<-sub$XA13/mean(sub$XA13, na.rm=TRUE)
sub$x4<-sub$dist

# przygotowanie macierzy wag przestrzennych
cont.nb<-poly2nb(as(pow, "SpatialPolygons"))

cont.listw<-nb2listw(cont.nb, style="W")

# szybka mapa zmiennej y
pow$y<-sub$y
range(pow$y)
rng<-seq(0, 175, 25) # from, to, by
cls = brewer.pal(7, "PuBuGn")
spplot(pow, "y", col.regions = cls, at = rng)

# sposoby zapisu modelu
model.lm<-lm(y~x1+x2+x3+x4, data=sub) # opcja A
summary(model.lm)

model.lm2<-lm(sub$y~sub$x1+sub$x2+sub$x3+sub$x4) # opcja B
summary(model.lm2)

# niby najlepsza opcja
eq<-y~x1+x2+x3+x4 # opcja C
model.lm3<-lm(eq, data=sub)
summary(model.lm3)

form<-formula(y~x1+x2+x3+x4) # opcja D
model.lm4<-lm(form, data=sub)
summary(model.lm4)

attributes(model.lm)

# diagnostyka modelu liniowego
install.packages("lmtest")

library(lmtest)
bptest(model.lm) # test na heteroskedastyczność (H1) /homoskedast…(H0)

# test Ramseya na formę funkcyjną
# H1: gdy nieliniowe zmienne (jak potęgi zmiennych) powinny być uwzględnione w modelu (model jest źle wyspecyfikowany mis-specified)
resettest(model.lm, power=2, type="regressor") 	

# przestrzenny rozkład reszt z modelu liniowego MNK
summary(model.lm$residuals)
res<-model.lm$residuals
brks<-c(min(res), mean(res)-sd(res), mean(res), mean(res)+sd(res), max(res))
cols<-c("steelblue4","lightskyblue","thistle1","plum3")
plot(pow, col=cols[findInterval(res,brks)])
title(main="Reszty w modelu MNK")
legend("bottomleft", legend=c("<mean-sd", "(mean-sd, mean)", "(mean, mean+sd)", ">mean+sd"), leglabs(brks1), fill=cols, bty="n")

# kilka testów stosowanych do MNK (OLS)
# testy Morana - ważne – uzasadniają stosowanie metod przestrzennych
lm.morantest(model.lm, cont.listw) # czy reszty są losowe przestrzennie?

moran.test(res, cont.listw)

# test join.count dla reszt (dodatnie vs. ujemne)
# wciąż rozsądne testowanie
reszty<-factor(cut(res, breaks=c(-100, 0, 100), labels=c("ujemne","dodatnie")))
joincount.test(reszty, cont.listw)

# Estymacja modeli przestrzennych z 3,2 lub 1 komponentem przestrzennym

form<-formula(y~x1+x2+x3+x4) # przypomnienie

# Manski model (full specification) – zawiera spatial lag of Y (rho), 
# spatial lag of X (theta), spatial error term (lambda)
# opcja type="sacmixed" aktywuje spatial lags of X
GNS_1<-sacsarlm(form, data=sub, listw=cont.listw, type="sacmixed", method="LU")  # method="LU" przyspiesza znacząco obliczenia
summary(GNS_1)

# SAC / SARAR model - includes spatial lag of Y, spatial error term
SAC_1<-sacsarlm(form, data=sub, listw=cont.listw)
summary(SAC_1)

# SEM - spatial error model
# typically includes spatial error term only (with lambda coefficient)
# option etype="emixed" activates spatial lags of X (with theta coeff.) # what makes spatial Durbin error model

SDEM_1<-errorsarlm(form, data=sub, listw=cont.listw, etype="emixed") # with spatial lags of X
summary(SDEM_1)

SEM_1<-errorsarlm(form, data=sub, listw=cont.listw) # no spat-lags of X
summary(SEM_1)

# SAR - spatial lag model
# normally includes spatial lag of Y only (with rho coefficient)
# option type="mixed" activates spatial lags of X (with theta coeff.)

SDM_1<-lagsarlm(form, data=sub, listw=cont.listw, type="mixed") # with spatial lags of X
summary(SDM_1)

SAR_1<-lagsarlm(form, data=sub, listw=cont.listw) # no spatial lags of X
summary(SAR_1)

# from errorsarlm() library
# an ‘lm’ model augmented with the spatially lagged RHS variables
# RHS variables – right-hand side variables
SLX_1<-lmSLX(form, data=sub, listw=cont.listw)
summary(SLX_1)

# LR (likelihood ratio) test - compares nested restricted model
# H0 – restricted (narrower) model is better
# H1 – unrestricted (wider) model is better
# df in chi2 is the number of restricted parameters
LR.Sarlm(GNS_1, SDM_1)

LR.Sarlm(GNS_1, SDEM_1)

LR.Sarlm(GNS_1, SLX_1)

LR.Sarlm(SDM_1, SAR_1)

LR.Sarlm(SDM_1, SEM_1)

LR.Sarlm(SDM_1, SLX_1)

LR.Sarlm(SDEM_1, SLX_1)

lrtest(model.lm, SLX_1)

# Efekty pośrednie i bezpośrednie (impacts)
# nowa postać modelu
form<-formula(y~x1+x2+x3+x4) # forma modelu
# form2<-update(form, . ~ . -x4) # usuwanie x4 z modelu - PRZYKŁAD

# estymacja modelu opóźnienia przestrzennego
SAR_1<-lagsarlm(form, data=sub, listw=cont.listw) # no spatial lags of X

# rozkład efektów 
# vector of traces of powers of a spatial weights matrix
# converting censored listw to CsparseMatrix form
W.c<-as(as_dgRMatrix_listw(cont.listw), "CsparseMatrix") 
# the default values for the number of powers is 30
trMat<-trW(W.c, type="mult") 

SAR_1_imp<-impacts(SAR_1, tr=trMat, R=2000)
summary(SAR_1_imp, zstats=TRUE, short=TRUE)

# extracting direct & total impacts
a<-SAR_1_imp$res$direct
b<-SAR_1_imp$res$total
a/b # ratio of impacts

# estymacja modelu Durbina 
# option type="mixed" activates spatial lags of X (with theta coeff.)
# model with spatial lags of X
SDM_1<-lagsarlm(form, data=sub, listw=cont.listw, type="mixed") 
summary(SDM_1)

# distribution of total impact 
# vector of traces of powers of a spatial weights matrix
# converting censored listw to CsparseMatrix form
W.c<-as(as_dgRMatrix_listw(cont.listw), "CsparseMatrix") 
# the default values for the number of powers is 30
trMat<-trW(W.c, type="mult") 

SDM_1_imp<-impacts(SDM_1, tr=trMat, R=2000)
summary(SDM_1_imp, zstats=TRUE, short=TRUE)

# extracting direct & total impacts
a<-SDM_1_imp$res$direct
b<-SDM_1_imp$res$total
a/b # ratio of impacts

# Modele z opóźnieniem czasowo-przestrzennym
# dodatnie takie zmiennej znacząco zmienia znaczenie innych zmiennych
# bez zmiennej lag – x to strukturalne determinanty y
# ze zmienną lag – x to czynniki korygujące path-dependence

# pojedyncze opóźnienie czasowo-przestrzenne
sub1<-dane[dane$rok==2015, ] # previous year
sub$XA14.st<-lag.listw(cont.listw, sub1$XA14)

# proste modele przestrzenne dynamiczne
model.lag<-lagsarlm(y~x1+x2+x3+x4+XA14.st, data=sub, cont.listw, tol.solve=3e-30)
summary(model.lag)

model.error<-errorsarlm(y~x1+x2+x3+x4+XA14.st, data=sub, cont.listw, tol.solve=3e-30)
summary(model.error)

# nieco bardziej zaawansowane modele z opóźnieniem czasowo-przestrzennym

# tworzenie opóźnień czasowych(t), przestrzennych (s) i mixed (st)
sub<-dane[dane$rok==2016, ]		# given period
sub1<-dane[dane$rok==2015, ]	# previous period, for temporal lag

# zmienne w t
sub$y<-sub$XA14
sub$x1<-(sub$XA18+sub$XA19+sub$XA20)/sub$XA15
sub$x2<-sub$XA21
sub$x3<-sub$XA13/mean(sub$XA13, na.rm=TRUE)
sub$x4<-sub$dist

# zmienne okres wcześniej (temporal lag x)
sub1$y<-sub1$XA14
sub1$x1<-(sub1$XA18+sub1$XA19+sub1$XA20)/sub1$XA15
sub1$x2<-sub1$XA21
sub1$x3<-sub1$XA13/mean(sub1$XA13, na.rm=TRUE)
sub1$x4<-sub1$dist

sub$y.t<-sub1$y	# dodanie zmiennych do jednolitego zbioru danych
sub$x1.t<-sub1$x1
sub$x2.t<-sub1$x2
sub$x3.t<-sub1$x3
sub$x4.t<-sub1$x4

# opóźnienia przest. zmiennych okres wcześniej (spatio-temporal lag)
sub$y.st<-lag.listw(cont.listw, sub1$y) 	# spatio-temporal lag y
sub$x1.st<-lag.listw(cont.listw, sub1$x1) # spatio-temporal lag x1
sub$x2.st<-lag.listw(cont.listw, sub1$x2) # spatio-temporal lag x2
sub$x3.st<-lag.listw(cont.listw, sub1$x3) # spatio-temporal lag x3
sub$x4.st<-lag.listw(cont.listw, sub1$x4) # spatio-temporal lag x4

# skrajności
# zwykły model OLS bez efektów przestrzennych 
model.ols<-lm(y~x1+x2+x3+x4, data=sub)
summary(model.ols)

# zwykły model przestrzenny statyczny - Manski
model.manski<-sacsarlm(y~x1+x2+x3+x4, data=sub, cont.listw, tol.solve=3e-30, type="sacmixed")
summary(model.manski)

# zwykły spatial Durbin (rhoWy + tetaX), bez lambdaWu
model.SDM<-lagsarlm(y~x1+x2+x3+x4, data=sub, cont.listw, tol.solve=3e-30, type="mixed")
summary(model.SDM)

# zwykły model przestrzenny SAC (rhoWy + LambdaWu)
model.sac<-sacsarlm(y~x1+x2+x3+x4, data=sub, cont.listw, tol.solve=3e-30)
summary(model.sac)

# generalnie opóźnienia przestrzenne są nieistotne

# zobaczmy, czy opóźnienia czasowo-przestrzenne są lepsze
# w modelu SAC mamy domyślnie rhoWy, lambdaWu
# dodajemy ręcznie tetaX1.t+ tetaX2.t+tetaX3.t+tetaX4.t
model.sac2<-sacsarlm(y~x1+x2+x3+x4+x1.st+x2.st+x3.st+x4.st, data=sub, cont.listw, tol.solve=3e-30)
summary(model.sac2)

# wymieniamy rhoWy na rhoWy.t – spatial lag na spatio-temporal lag
# trzeba zmienić domślny typ modelu – errorsarlm (tylko lambdaWu)
# czyli mamy rhoWy(t-1), lambdaWu + opóźnienia czasowo-przestrzenne X
model.sac3<-errorsarlm(y~x1+x2+x3+x4+x1.st+x2.st+x3.st+x4.st+y.st, data=sub, cont.listw, tol.solve=3e-30)
summary(model.sac3)
#  zamiana poprawiła istotności – mamy przyzwoity model – 
#  Ale nie policzymy automatycznie impacts, 
# bo to możliwe tylko w modelach lag
# nie ma dużego problemu z impacts – bo nie ma jednoczesności 
# można interpretować jak w modelu error

moran.test(model.sac3$residuals, cont.listw) # reszty bez autokorelacji
bptest.sarlm(model.sac3) # heteroskedasticity
summary(model.sac3, Hausman=TRUE) # test Hausmana na istotność różnicy między OLS a SAC

# distribution of total impact 
# vector of traces of powers of a spatial weights matrix
# converting censored listw to CsparseMatrix form
W.c<-as(as_dgRMatrix_listw(cont.listw), "CsparseMatrix") 
# the default values for the number of powers is 30
trMat<-trW(W.c, type="mult") 

SAC_2_imp<-impacts(model.sac2, tr=trMat, R=2000)
summary(SAC_2_imp, zstats=TRUE, short=TRUE)















