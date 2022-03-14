## ----
##  HasaR - Hagelschadenschaetzung anhand Radardaten
##  (Statistische Modellierung von Hagelschaeden anhand Radardaten)
##
##  Modell fuer Ereignisse mit ueber 1'000 Schadenmeldungen, keine
##  Unterscheidung anch kleinen oder grossen Events.
##
##  1) Daten aus Gemdat auf Gitter der Radar-Daten der meteoSchweiz (1km2)
##     aggregieren
##  2) Schaetzung der Schadensumme mit dem statistischen Modell
##     (Es werden nur Gitterzellen mit Anzahl Geb > 0 
##     und einer POH von mindestens 80% verwendet)
##  3) PDF - Karten erstellen:
##      - Aggregierte, gegitterte Daten des Gebaeudebestandes darstellen
##      - Radar-Daten der meteoSchweiz darstellen
##      - Modellierte, geschaetzte Schadensumme pro Gitter darstellen
##  4) Leaflet - Karten erstellen:
##      - Radar-Daten der meteoSchweiz darstellen
##      - Modellierte, geschaetzte Schadensumme pro Gitter darstellen
##  5) Schaetzung der Schadensumme nach der INTERPOL Methode
##      - Empirischen Betroffenheitsgraden je Baujahrskategorie und einem
##        Durchschnittsschaden von CHF 5'000.
##
##  Mirco Heidemann, 07/2016 (modified 08/2017)
##  HasaR 2.0, 11/2017
## ----

## HIER DAS DATUM DES RADARBILDS ANGEBEN
eventDat <- '01.08.2017'

## AKTUELLER GEBAEUDEBESTAND ANGEBEN:
# portfolio.path <- 'J:/Naturgefahren/FG2_Datenanalysen/Portfolio gvz/geb?udebestand gvz/2017/'
# portfolio.file <- 'data georef/gebaeudebestand.201701.georef - CHECKED gem schaetzkr.csv'
portfolio.file <- 'gebaeudebestand.201701.georef - CHECKED gem schaetzkr.csv'
## ----

## Arbeitsverzeichnis definieren
# setwd('J:/Naturgefahren/FG2_Datenanalysen/Ereignisanalysen/Hagel/HasaR/data/')
setwd('I:/Statistical Computing/HasaR 2.0/data')
# setwd('/Users/vanBrunkhorst/Documents/Statistical Computing/HasaR 2.0/data/')

## Ordner-Pfade definieren:
pdf.out <- 'I:/Statistical Computing/HasaR 2.0/pdf/'
# pdf.out <- '/Users/vanBrunkhorst/Documents/Statistical Computing/HasaR 2.0/pdf/'

## Funktionen und Packages laden
library(rgdal) ## raster Package laden
#require(maptools)
library(MASS)
library(dplyr)
library(stringr)
library(raster)
library(leaflet)
library(fields) ## interp.surface

## Skript zum Rotieren bzw. Spiegeln vom Radar-Bild laden
source('./functions/rotate.R')
## Funktion um neue Faktor-Levels predict-Datensatz auf NA zu setzen
source('./functions/f.removeMissingLevels.R')

dat <- as.Date(eventDat, "%d.%m.%Y")
in.poh <- paste('poh.', format(dat, "%Y%m%d"),'.tif', sep="")
in.meshs <- sub('poh', 'meshs', in.poh)
## meteoSchweiz schickt die POH als tif-files, die MESHS aber als tiff-Files
in.meshs <- sub('tif', 'tiff', in.meshs)

## load the HasaR model:
load('./Rdata/mLogit.rda')
load('./Rdata/mGamma.rda')
load('./Rdata/mPois.rda')

## GeoTiff lesen
poh <- readGDAL(paste0('./radarTiffs/', in.poh))
meshs <- readGDAL(paste0('./radarTiffs/',in.meshs))

## Datum aus dem Radarfile extrahieren (fuer Grafik)
per.string <- unlist(strsplit(in.poh, '\\poh.'))[2]
per.string <- unlist(strsplit(per.string, '\\.'))[1]
datum <- as.Date(per.string, format='%Y%m%d')

## shapes fuer plots einlesen
sh.gemeinde <- readOGR('./shapes/gemeinden.2016.shp', layer = 'gemeinden.2016')
sh.fluss <-readOGR('./shapes/Fluesse_gross.shp', layer = 'Fluesse_gross')
sh.seen <-readOGR('./shapes/seendet_250.shp', layer = 'seendet_250')
sh.Flug <-readOGR('./shapes/AVZH_Flughafen_Kloten.shp',
                  layer = 'AVZH_Flughafen_Kloten')
sh.bezirke <- readOGR('./shapes/Bezirke.shp', layer = 'Bezirke')
sh.schaetzkreis <- readOGR('./shapes/gvz_schaetzkreise.shp',
                           layer = 'gvz_schaetzkreise')
shp.kanton    <- readOGR("./shapes/Knt_Umriss wgs84.shp",
                         layer = "Knt_Umriss wgs84")

## ----
##  1. GVZ GEBAEUDE AUS GEMDAT AUF RADARGITTER AGGREGIEREN
## ----

## definieren Vektoren fuer x- und y-Koordinaten
## geht auch mit bbox(poh) und gridparameters(poh)
poh.sum <- summary(poh)
x.cellsize <- poh.sum$grid$cellsize[1]
y.cellsize <- poh.sum$grid$cellsize[2]
xmin <- poh.sum$bbox[1,1] + x.cellsize / 2
xmax <- poh.sum$bbox[1,2] - x.cellsize / 2
ymin <- poh.sum$bbox[2,1] + y.cellsize / 2
ymax <- poh.sum$bbox[2,2] - y.cellsize / 2

xs <- seq(xmin, xmax, x.cellsize)
ys <- seq(ymin, ymax, y.cellsize)
## Konvertiere die Daten aus dem GeoTiff in einen 2-dimensionalen Array
## --> POH-Werte
poh.array <- mirror.matrix(as.array(poh))
meshs.array <- mirror.matrix(as.array(meshs))

## Gebaeude-Daten lesen
in.file <- paste0('./tables/', portfolio.file)
gemdat <- read.csv2(in.file, stringsAsFactors = FALSE, sep = ',')
## str(gemdat)
options(digits = 11) ## um die nach-kommastellen nicht zu verliehren
gemdat <- gemdat %>% mutate(versSum = as.numeric(versSum),
                            geox = as.numeric(geoxLV03),
                            geoy = as.numeric(geoyLV03)) %>% 
  filter(!is.na(geox)) ## Wenn keine Koordinaten, raus...

## Referenzierung betrachten:
## Wenn bei der Adresse keine Stasse angegeben ist UND die Referenzierung 
## nicht vom kantonalen Amt vorgenommen wurde, so ist die die Gebaeudelage
## zu ungenau bestimmbar und wird fuer die Anylse nicht beruecksichtigt.

t.logisch <- which(gemdat$strasse=="" & 
                     gemdat$ref!="Ref_Geb_ZH2008")
gemdat <- gemdat[-t.logisch,]

## Geb ausserhalb vom Knt Zh raus...
x.min.zh <- sh.gemeinde@bbox[1]
y.min.zh <- sh.gemeinde@bbox[2]
x.max.zh <- sh.gemeinde@bbox[3]
y.max.zh <- sh.gemeinde@bbox[4]

ind <- which(gemdat$geox >= x.min.zh & gemdat$geox <= x.max.zh)
if (length(ind) > 0) {
  gemdat <- gemdat[ind,]
}
ind <- which(gemdat$geoy >= y.min.zh & gemdat$geoy <= y.max.zh)
if (length(ind) > 0) {
  gemdat <- gemdat[ind,]
}

## Das Gitter vom Radar auf den Bereich vom Knt ZH begrenzen.
ind.x <- which(xs >= (min(gemdat$geox) - x.cellsize) &
                 xs <= (max(gemdat$geox) + x.cellsize))
ind.y <- which(ys >= (min(gemdat$geoy) - y.cellsize) &
                 ys <= (max(gemdat$geoy) + y.cellsize))
if (length(ind.x) > 0 & length(ind.y) > 0) {
  xs <- xs[ind.x]
  ys <- ys[ind.y]
  poh.array <- poh.array[ind.x, ind.y]
  meshs.array <- meshs.array[ind.x, ind.y]
}

## definiere Arrays fuer die Anzahl und Versicherungssumme pro
## Gitterzelle
Anzahl <- VersSumme <- x.coord <- y.coord <- array(dim = c(length(xs), length(ys)))

## Schleife ueber Gitterzellen
for (i in 1:length(xs)) {
  for (j in 1:length(ys)) {
    ind <- which(gemdat$geox > (xs[i] - x.cellsize / 2) &
                   gemdat$geox <= (xs[i] + x.cellsize / 2) &
                   gemdat$geoy > (ys[j] - y.cellsize / 2) &
                   gemdat$geoy <= (ys[j] + y.cellsize / 2))
    if (length(ind) > 0) {
      Anzahl[i,j] <- length(ind)
      VersSumme[i,j] <- sum(gemdat$versSum[ind])
      x.coord[i,j] <- xs[i]
      y.coord[i,j] <- ys[j]
    }
  }
}

## Nur Knt ZH, Kontrolle:
## image(Anzahl)

## Datum aus dem Geb. Daten File extrahieren
per.char <- unlist(strsplit(gsub("([0-9]+).*$", "\\1", in.file),'\\.'))[3]
out.file.char <- paste0('GemdatGitter.',per.char,'.Rdata')
# save(xs, ys, Anzahl, VersSumme, file=out.file.char)

## ----
## Alle gegitterten Daten in einen Dataframe stecken

## In Gitterzellen wo keine Gebaeude stehen (Anzahl = 0) fehlt
## der GVZ die Information zu m?glichen Hagelschaeden.
ind.geb <- which(!is.na(Anzahl)) ## Nur Gitterzellen mit Geb
dat <- data.frame(event = rep(datum, length(ind.geb)),
                  Anzahl = as.numeric(Anzahl[ind.geb]),
                  VersSumme = VersSumme[ind.geb],
                  poh = as.numeric(poh.array[ind.geb]/10), ## poh zw. 0 und 1
                  f.poh = as.factor(poh.array[ind.geb]/10),
                  meshs.class = as.numeric(meshs.array[ind.geb]),
                  f.meshs.class = as.factor(meshs.array[ind.geb]),
                  x.coord = as.numeric(x.coord[ind.geb]),
                  y.coord = as.numeric(y.coord[ind.geb]),
                  stringsAsFactors = FALSE)

## von meshs.class abstufung zu meteoschweiz hagelkorngroesse (meshs)
meshs.tab <- data.frame(meshs.class = seq(from=0, to=10, by=1),
                        meshs = seq(from=1.5, to=6.5, by=0.5))
dat$meshs <- meshs.tab$meshs[match(dat$meshs.class, meshs.tab$meshs.class)]
## hagelkorngroessen kleiner als 2 gibt es nicht in meshs
dat$meshs[which(dat$meshs<2)] <- 0
## meshs zu factor
dat$f.meshs <- as.factor(dat$meshs)
dat <- dat[-c(6:7)]

## nur gitterzellen mit mindestens poh 80%
dat <- dat %>% filter(poh >= 0.8)

## ----
##  2. SCHAETZUNG DER SCHADENSUMME ANHAND DES RADARBILDES (HasaR)
## ----
##  Die Modellierung besteht aus zwei Teilen (bedingte Wahrscheinlichkeit):
##  Im ersten Teil wird unterschieden, ob ein Schaden eintritt (Schaden ja/nein)
##  - Logistische Regression
##
##  Tritt ein Schaden ein, so wird im zweiten Teil der erwartete Schaden
##  pro Gitterzellen mit einer Gamma likelihood (link=log) modelliert.
##  - GLM mit Gamma Verteilung


## fitted(model.glm): predicted values on the original scale
##                    (scale of response, eg the predicted probabilities)
## predict(model.glm, type='response'): same as 'fitted(schadsum.glm)'
##                                      (eg the predicted probabilities)
## predict(model.glm, type='link'): predicted values on the transformed
##                                  scale (scale of linear predictor)

# ## Scatterplot Matrices per event (requires packages'car')
# library(car)
# scatterplotMatrix(dat[,-1], diagonal="density",
#                   pch=19, cex=0.5, spread=F, col.axis='gray50')

## Generate the predicted probabilities with standard errors.
newdat.logit <- cbind(dat, predict(m.logit, newdata = dat, type="link",
                                   se=TRUE))
## Estimates on the link scale and back transform both the predicted
## values and confidence limits into probabilities.
newdat.logit <- within(newdat.logit, {
  EstimatedProb <- plogis(fit)
  LL.prob <- plogis(fit - (1.96 * se.fit))
  UL.prob <- plogis(fit + (1.96 * se.fit))
})

# ## TEST
# pred.m.logit <- predict(m.logit, newdata=dat, type="response")
# pred.m.gamma <- predict(m.gamma, newdata=dat, type="response")
# pred.pois <- predict(m.pois, newdata=dat, type="response")
# ## Gesamtschaden Modell fit
# (fit.sum <- sum(pred.m.gamma * pred.m.logit))
# ## Schadenanzahl Modell fit
# (fit.anzahl<- round(sum(pred.pois * pred.m.logit)))

## Generate the predicted loss with standard errors.
# newdat.gamma <- cbind(dat, predict(m.gamma, newdata = dat, type="link",
#                                    se=TRUE))

## mit der funktion 'removeMissing.Levels' werden alle faktor levels
## die in den original daten (dat) nicht vorkommen auf NA gesetzt
## vorkommen auf NA gesetzt
newdat.gamma <- cbind(dat, predict(m.gamma, newdata=removeMissing.Levels
                                   (fit=m.gamma, test_data=dat),
                                   type="link", se=TRUE))

## Estimates on the link scale and back transform both the predicted
## values and confidence limits into loss
newdat.gamma <- within(newdat.gamma, {
  EstimatedGamma <- exp(fit)
  LL.gam <- exp(fit - (1.96 * se.fit))
  UL.gam <- exp(fit + (1.96 * se.fit))
})

## Generate the predicted number of claims with standard errors.
# newdat.pois <- cbind(dat, predict(m.pois, newdata = dat, type="link",
#                                   se=TRUE))
newdat.pois <- cbind(dat, predict(m.pois, newdata=removeMissing.Levels
                                  (fit=m.pois, test_data=dat),
                                  type="link", se=TRUE))

newdat.pois  <- within(newdat.pois, {
  EstimatedClaims <- exp(fit)
  LL.claims <- exp(fit - (1.96 * se.fit))
  UL.claims <- exp(fit + (1.96 * se.fit))
})

## Alles in einen dataframe packen
newdat <- cbind(newdat.logit, newdat.gamma[c(10:15)])
## faktoren werden nicht mehr gebraucht
newdat <- newdat[,-c(5,9)]
newdat$EstimatedSchad.LL <- newdat$LL.prob * newdat$LL.gam
newdat$EstimatedSchad <- newdat$EstimatedProb * newdat$EstimatedGamma
newdat$EstimatedSchad.UL <- newdat$UL.prob * newdat$UL.gam
newdat$EstimatedClaims.LL <- newdat$LL.prob * newdat.pois$LL.claims
newdat$EstimatedClaims <- newdat$EstimatedProb * newdat.pois$EstimatedClaims
newdat$EstimatedClaims.UL <- newdat$UL.prob * newdat.pois$UL.claims
newdat[is.na(newdat)] <- 0

## definiere Arrays fuer die geschaetzte Schadensumme pro Gitterzelle
EstimatedGamma <- EstimatedProb <- EstimatedLoss <- EstimatedClaims <- 
  array(dim = c(length(xs), length(ys)))
## Schleife ueber Gitterzellen
for (i in 1:length(xs)) {
  for (j in 1:length(ys)) {
    ind <- which(dat$x.coord > (xs[i] - x.cellsize / 2) &
                   dat$x.coord <= (xs[i] + x.cellsize / 2) &
                   dat$y.coord > (ys[j] - y.cellsize / 2) &
                   dat$y.coord <= (ys[j] + y.cellsize / 2))
    if (length(ind) > 0) {
      EstimatedProb[i,j] <- sum(newdat$EstimatedProb[ind])
      EstimatedGamma[i,j] <- sum(newdat$EstimatedGamma[ind])
      EstimatedLoss[i,j] <- sum(newdat$EstimatedSchad[ind])
      EstimatedClaims[i,j] <- sum(newdat$EstimatedClaims[ind])
    }
  }
}

# ## Nur geschaetzte Schadensumme, ohne Bandbreite
# pred.m.logit <- predict(m.logit, newdata=dat, type="response")
# pred.m.gamma <- predict(m.gamma, newdata=dat, type="response")
# ## Schadenschaetzung, Modell Fit
# fit.schad <- pred.m.gamma * pred.m.logit
# ## Gesamt-Schadenschaetzung:
# (fit.sum <- sum(pred.m.gamma * pred.m.logit))

## Schadenschaetzung in DF zusammenfassen:
LL <- (round(sum(newdat$EstimatedSchad.LL)/1e5)/10)
BE <- (round(sum(newdat$EstimatedSchad)/1e5)/10)
UL <- (round(sum(newdat$EstimatedSchad.UL)/1e5)/10)
NR.LL <- (round(sum(newdat$EstimatedClaims.LL)))
NR <- (round(sum(newdat$EstimatedClaims)))
NR.UL <- (round(sum(newdat$EstimatedClaims.UL)))
## Schadenschaetzung DF:
(df <- data.frame(estimated.loss = BE,
                 lowerBound.loss = LL,
                 upperBound.loss = UL,
                 estimated.claims = NR,
                 lowerBound.claims = NR.LL,
                 upperBound.claims = NR.UL))

## ----
##  3. PDF KARTEN ERSTELLEN:
## ----
## prepare the pdf-plot
pdf.file <- paste(pdf.out,'Gemdat.Radar.', as.character(datum, format='%Y%m%d')
                  ,'.pdf', sep = '')
pdf(pdf.file, width = 11.6, height = 8.2, version = '1.6',
    paper = 'a4r', encoding = 'ISOLatin1.enc')
par(mar = c(1, 1, 1.5, 1), mfrow = c(1, 2))

## min-max coordinates
xmin <- sh.gemeinde@bbox[1,1]
xmax <- sh.gemeinde@bbox[1,2]
ymin <- sh.gemeinde@bbox[2,1]
ymax <- sh.gemeinde@bbox[2,2]

## ymax so anpassen, dass es zum Verhaeltnis von A4r-Papier passt
ymax <- ymin + (29.7 * (xmax - xmin) / 21)

txt.x <- xmin + 0.02 * (xmax - xmin)
txt.y <- ymin + 0.96 * (ymax - ymin)
# txt.y2 <- ymin + 0.01 * (ymax - ymin)
txt.y2 <- ymin - 0.08 * (ymax - ymin)

leg.x <- xmin
leg.y <- ymin + 0.7 * (ymax - ymin)

## Aggregierte, gegitterte Daten des Gebaeudebestandes darstellen
breaks.anzahl <- c(0, 10, 20, 50, 100, 200, 500, 1000, max(Anzahl, na.rm = TRUE))
cols.anzahl <- rainbow((length(breaks.anzahl) - 1), start = 0.15, alpha = 0.4)
breaks.summe <- c(0, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9, 5e9,
                  1e6*ceiling(max(VersSumme, na.rm = TRUE)/1e6))
cols.summe <- rainbow((length(breaks.summe) - 1), start = 0.15, alpha = 0.4)
# breaks.mittel <- c(0, 2e5, 4e5, 6e5, 8e5, 1e6, 2e6, 5e6,
#                    1e6*ceiling(max(VersSumme/Anzahl, na.rm = TRUE)/1e6))
# cols.mittel <- rainbow((length(breaks.mittel) - 1), start = 0.15, alpha = 0.4)

## grenze zeichnen, damit nichts abgeschnitten ist
plot(sh.bezirke, lwd = 1)
## Gebaeude plotten
#plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
## fliessende gewaesser plotten
plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
## Seen plotten
plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
## Bezirke des Kantons ZH
plot(sh.bezirke, lwd = 0.5, add = TRUE)
image(xs, ys, Anzahl, ann = FALSE, axes = FALSE,
      col = cols.anzahl, breaks = breaks.anzahl, useRaster=T, add = T)
##abline(v = seq(min(xs), max(xs), 1e3))
##abline(h = seq(min(ys), max(ys), 1e3))
text(txt.x, txt.y2, paste('GemDat per ', per.char,
 ', GG25?swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
                          format(Sys.time(), "%b %Y"), sep=''),
     adj = c(0, 0), cex = 0.5)
leg.txt <- paste(breaks.anzahl[-length(breaks.anzahl)],
                 breaks.anzahl[-1], sep = '-')
legend('topleft', leg.txt[1:4], col = cols.anzahl[1:4],
       pch = 15, cex = 0.7, pt.cex = 1, bty = 'n')
legend('topright', leg.txt[5:8], col = cols.anzahl[5:8],
       pch = 15, cex = 0.7, pt.cex = 1, bty = 'n')
title(main = 'Anzahl Gebaeude', line = 0.4, cex.main = 0.9)
box()

## grenze zeichnen, damit nichts abgeschnitten ist
plot(sh.bezirke, lwd = 1)
## Gebaeude plotten
#plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
## fliessende gewaesser plotten
plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
## Seen plotten
plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
## Bezirke des Kantons ZH
plot(sh.bezirke, lwd = 0.5, add = TRUE)
image(xs, ys, VersSumme, ann = FALSE, axes = FALSE,
      col = cols.summe, breaks = breaks.summe, useRaster=T, add = T)
text(txt.x, txt.y2, paste('GemDat per ', per.char,
 ', GG25?swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
                          format(Sys.time(), "%b %Y"), sep=''),
     adj = c(0, 0), cex = 0.5)
leg.txt <- paste(breaks.summe[-length(breaks.summe)]/1e6,
                 breaks.summe[-1]/1e6, sep = '-')
legend('topleft', leg.txt[1:4], col = cols.summe[1:4],
       pch = 15, cex = 0.7, pt.cex = 1, bty = 'n')
legend('topright', leg.txt[5:8], col = cols.summe[5:8],
       pch = 15, cex = 0.7, pt.cex = 1, bty = 'n')
title(main = 'Versicherungssumme aller Gebaeude in Mio CHF',
      line = 0.4, cex.main = 0.9)
box()


## Radar-Daten der meteoSchweiz darstellen
# MeteoSwiss  POH-Colours
unique.values <- (0:10)
brks.poh <- unique.values + 0.1

## MeteoSwiss POH-Colours
c1 <- rgb(0,100,255,160,maxColorValue=255)
c2 <- rgb(0,150,150,160,maxColorValue=255)
c3 <- rgb(0,200,50,160,maxColorValue=255)
c4 <- rgb(150,255,0,160,maxColorValue=255)
c5 <- rgb(200,255,50,160,maxColorValue=255)
c6 <- rgb(255,255,0,160,maxColorValue=255)
c7 <- rgb(255,200,0,160,maxColorValue=255)
c8 <- rgb(255,160,0,160,maxColorValue=255)
c9 <- rgb(255,125,0,160,maxColorValue=255)
c10 <- rgb(255,22,0,160,maxColorValue=255)
cols.poh <- c(c1, c2, c3, c4, c5, c6, c7,c8, c9, c10)

# ## Hex Code, aber ohne transparenz
# cols.poh <- c('#0066ff', '#009696', '#00c832', '#99ff00',
#               '#c9ff32', '#ffff00', '#ffc800', '#ff9d00',
#               '#ff7b00', '#e11600')

## MESHS: ACHTUNG, es wird MESHS Class dargestellt
##        und nicht die Korngroessen (see meshs.tab)
unique.values <- (0:9)
brks.meshs <- unique.values + 0.1

# MeteoSwiss  MESHS-Colours
c1 <- rgb(0,100,255,160,maxColorValue=255)
c2 <- rgb(0,200,50,160,maxColorValue=255)
c3 <- rgb(150,255,0,160,maxColorValue=255)
c4 <- rgb(200,255,50,160,maxColorValue=255)
c5 <- rgb(255,255,0,160,maxColorValue=255)
c6 <- rgb(255,200,0,160,maxColorValue=255)
c7 <- rgb(255,160,0,160,maxColorValue=255)
c8 <- rgb(255,125,0,160,maxColorValue=255)
c9 <- rgb(255,22,0,160,maxColorValue=255) 
cols.meshs <- c(c1, c2, c3, c4, c5, c6, c7,c8, c9)

# ## Hex Code, aber ohne transparenz
# cols.meshs <- c('#0064ff', '#00c832', '#96ff00',
#                 '#c8ff32', '#ffff00', '#ffc800',
#                 '#ffa000', '#ff7d00', '#ff1600')

## POH pro grid
## grenze zeichnen, damit nichts abgeschnitten ist
plot(sh.bezirke, lwd = 1)
## Gebaeude plotten
#plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
## fliessende gewaesser plotten
plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
## Seen plotten
plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
## Bezirke des Kantons ZH
plot(sh.bezirke, lwd = 0.5, add = TRUE)
image(xs, ys, poh.array, ann = FALSE, axes = FALSE, col = cols.poh,
      breaks = brks.poh, useRaster=T, add = T)
text(txt.x, txt.y2, paste('GemDat per ', per.char,
  ', GG25?swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
                          format(Sys.time(), "%b %Y"), sep=''),
     adj = c(0, 0), cex = 0.5)
leg.txt <- as.character(10*c(1:10))
legend('topleft', leg.txt[1:5], col = cols.poh[1:5],
       pch = 15, cex = 0.7, pt.cex = 1, title ='POH [%]',
       bg = "white", box.col = "white")
legend('topright', leg.txt[6:10], col = cols.poh[6:10],
       pch = 15, cex = 0.7, pt.cex = 1, title ='POH [%]',
       bg = "white", box.col = "white")
title(main = paste('Hagelwahrscheinlichkeit - ',
                   as.character(datum, format='%d.%m.%Y'), sep = ''),
      line = 0.4, cex.main = 0.9)
box()

## MESHS pro grid
## grenze zeichnen, damit nichts abgeschnitten ist
plot(sh.bezirke, lwd = 1)
## Gebaeude plotten
#plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
## fliessende gewaesser plotten
plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
## Seen plotten
plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
## Bezirke des Kantons ZH
plot(sh.bezirke, lwd = 0.5, add = TRUE)
image(xs, ys, meshs.array, ann = FALSE, axes = FALSE, col = cols.meshs,
      breaks = brks.meshs, useRaster=T, add = T)
text(txt.x, txt.y2, paste('GemDat per ', per.char,
                          ', GG25 swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
                          format(Sys.time(), "%b %Y"), sep=''),
     adj = c(0, 0), cex = 0.5)
leg.txt <- as.character(seq(from=2, to=6, by=0.5))
legend('topleft', leg.txt[1:5], col = cols.meshs[1:5],
       pch = 15, cex = 0.7, pt.cex = 1, title ='MESHS [cm]',
       bg = "white", box.col = "white")
legend('topright', leg.txt[6:10], col = cols.meshs[6:10],
       pch = 15, cex = 0.7, pt.cex = 1, title ='MESHS [cm]',
       bg = "white", box.col = "white")
title(main = paste('Maximale Korngroesse am Boden - ',
                   as.character(datum, format='%d.%m.%Y'), sep = ''),
      line = 0.4, cex.main = 0.9)
box()

## Modellierte, geschaetzte Schadensumme pro Gitter darstellen
breaks.estim <- c(0, 10e3, 25e3, 50e3, 100e3, 500e3, 99999e9)
cols.estim <- rev(heat.colors((length(breaks.estim) - 1), alpha = 0.6))

## grenze zeichnen, damit nichts abgeschnitten ist
plot(sh.bezirke, lwd = 1)
## Gebaeude plotten
#plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
## fliessende gewaesser plotten
plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
## Seen plotten
plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
## Bezirke des Kantons ZH
plot(sh.bezirke, lwd = 0.5, add = TRUE)
image(xs, ys, EstimatedLoss, ann = FALSE, axes = FALSE,
#       col = cols.estim, breaks = breaks.estim, useRaster=T, add = T)
      col = c(NA,cols.estim[2:6]), breaks = breaks.estim, useRaster=T, add = T)
text(txt.x, txt.y2, paste('GemDat per ', per.char,
  ', GG25 swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
                          format(Sys.time(), "%b %Y"), sep=''),
     adj = c(0, 0), cex = 0.5)
leg.txt <- as.character(c("-","< 10", "10 - 25", "25 - 50", "50 - 100",
                           "100 - 500", "> 500"))                       
legend('topleft', leg.txt, col = c('white', cols.estim),
       title ='In Tausend CHF', pch = 15,
       cex = 0.7, pt.cex = 1, bty = "n")
title(main = paste('Geschaetzte Gebaeudeschaeden - ',
                   as.character(datum, format='%d.%m.%Y'), sep = ''),
      line = 0.4, cex.main = 0.9)
legend('topright', cex = 0.7, bty = "n",
       legend = paste0('Geschaetzte Schadensumme: ',
                     df$estimated.loss, ' Mio CHF\n',
                     'Geschaetzte Anzahl Schaeden: ',
                     format(df$estimated.claims, big.mark="'")))
box()

## pdf schliessen
dev.off()
par(mfcol = c(1, 1))

# ## ----
# ## Nur fuer Modellvalidierung, nicht fuer externe Abgabe
# ## ----
# ## prepare the pdf-plot
# pdf.file <- paste(pdf.out,'Gemdat.Radar.Model.', as.character(datum, format='%Y%m%d')
#                   ,'.pdf', sep = '')
# pdf(pdf.file, width = 11.6, height = 8.2, version = '1.6',
#     paper = 'a4r', encoding = 'ISOLatin1.enc')
# par(mar = c(1, 1, 1.5, 1), mfrow = c(1, 2))
# 
# ## Binomial Prozess, schaden wahrsch. pro Gitter darstellen
# breaks.prob <- c(0.1, 0.2, 0.3, 0.4, 1)
# cols.prob <- rev(heat.colors((length(breaks.prob) - 1), alpha = 0.6))
# 
# ## grenze zeichnen, damit nichts abgeschnitten ist
# plot(sh.bezirke, lwd = 1)
# ## Gebaeude plotten
# #plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
# ## fliessende gewaesser plotten
# plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
# ## Seen plotten
# plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
# plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
# plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
# ## Bezirke des Kantons ZH
# plot(sh.bezirke, lwd = 0.5, add = TRUE)
# image(xs, ys, EstimatedProb, ann = FALSE, axes = FALSE,
#       col = cols.prob, breaks = breaks.prob, useRaster=T, add = T)
# text(txt.x, txt.y2, paste('GemDat per ', per.char,
#  ', GG25?swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
#                           format(Sys.time(), "%b %Y"), sep=''),
#      adj = c(0, 0), cex = 0.5)
# leg.txt <- as.character(c('0.1 - 0.2', '0.2 - 0.3',
#                           '0.3 - 0.4', '0.4 - 0.5'))
# legend('topleft', leg.txt[1:5], col = cols.prob[1:5],
#        pch = 15, cex = 0.7, pt.cex = 1,
#        title ='Schadenswahrscheinlichkeit',
#        bg = "white", box.col = "white")
# title(main = paste('Logistische Regression (Binomial Prozess) - ',
#                    as.character(datum, format='%d.%m.%Y'), sep = ''),
#       line = 0.5, cex.main = 0.9)
# box()
# 
# ## Gamma Prozess der non-zero pro Gitter darstellen
# breaks.gam <- c(0, 25e3, 50e3, 100e3, 200e3,
#                   1e3*ceiling(max(EstimatedGamma, na.rm = TRUE)/1e3))
# cols.gam <- rev(heat.colors((length(breaks.gam) - 1), alpha = 0.6))
# 
# ## grenze zeichnen, damit nichts abgeschnitten ist
# plot(sh.bezirke, lwd = 1)
# ## Gebaeude plotten
# #plot(sh.gebFl, col = '#656565', border= NA, add = TRUE)
# ## fliessende gewaesser plotten
# plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
# ## Seen plotten
# plot(sh.seen, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
# plot(sh.Flug, col = '#ababab', border= NA, add = TRUE)
# plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
# ## Bezirke des Kantons ZH
# plot(sh.bezirke, lwd = 0.5, add = TRUE)
# image(xs, ys, EstimatedGamma, ann = FALSE, axes = FALSE,
#       col = cols.gam, breaks = breaks.gam, useRaster=T, add = T)
# text(txt.x, txt.y2, paste('GemDat per ', per.char,
#  ', GG25?swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, ',
#                           format(Sys.time(), "%b %Y"), sep=''),
#      adj = c(0, 0), cex = 0.5)
# leg.txt <- as.character(c("-","< 25", "25 - 50", "50 - 100",
#                           "100 - 200", "> 200"))
# legend('topleft', leg.txt[1:5], col = cols.gam[1:5],
#        pch = 15, cex = 0.7, pt.cex = 1,
#        title ='Schadensverteilung',
#        bg = "white", box.col = "white")
# title(main = paste('Gamma GLM (log link) wenn Schaden - ',
#                    as.character(datum, format='%d.%m.%Y'), sep = ''),
#       line = 0.4, cex.main = 0.9)
# box()
# 
# ## pdf schliessen
# dev.off()
# par(mfcol = c(1, 1))

## Schadenschaetzung ausgeben:
message(print(paste0('HASAR - Der geschaetzte Schaden betraegt: ',
                     BE, ' Mio CHF')))
message(print(paste0('HASAR - Der geschaetzte Schaden liegt zwischen: ',
                     LL, ' Mio CHF und ', UL, ' Mio CHF')))


## ----
##  4. LEAFLET-KARTEN:
## ----

## MESHS-Radarbild darstellen
## ----

# meshs@data$band1 <- meshs.tab$meshs[match(meshs@data$band1, meshs.tab$meshs.class)]
# meshs@data$band1[is.na(meshs@data$band1)] <- 0
# ## hagelkorngroessen kleiner als 2 gibt es nicht in meshs
# meshs@data$band1[which(meshs@data$band1<2)] <- 0
# 
# ## rasterwerte groesser 6 oder kleiner 2, raus...
# ind <- which(meshs@data$band1 > 6 | meshs@data$band1 < 2)
# meshs@data$band1[ind] <- NA
# 
# rMeshs <- raster(meshs)

# brksR <- seq(2,6,0.5)
# # vals <- na.omit(values(rMeshs))
# 
# cbR <- colorBin(palette = cols.meshs, domain = values(rMeshs),
#                 bins = brksR, na.color = "transparent")

# previewColors(colorFactor(cols.meshs, domain = NULL),
#               sort(factor(unique(values(rMeshs))),
#                    levels=sort(factor(unique(values(rMeshs))))))
# 

meshs.array[meshs.array == 0] <- NA

rMeshs <- raster(xmn=min(xs)-x.cellsize/2, xmx=max(xs)+x.cellsize/2,
                   ymn=min(ys)-x.cellsize/2, ymx=max(ys)+x.cellsize/2,
                   resolution = x.cellsize,
                   vals=rotate90.matrix(meshs.array))
## ch-coordsyst
crs(rMeshs) <- CRS("+init=epsg:21781") # LV03

cbR.f <-colorFactor(palette = cols.meshs,
                    domain = base::as.factor(sort(unique(values(rMeshs)))),
                    na.color = "transparent")

mRadar <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap") %>% ## change basemap
    setView(lng = 8.54226, lat = 47.37174, zoom = 10) %>%
    addRasterImage(rMeshs, colors = cbR.f, opacity = 0.6, project=F) %>%
    addLegend(position = "bottomright", pal = cbR.f,
              values = values(rMeshs),
              title = "MESHS",
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                p = c("< 2", "2.5", "3", "3.5",
                      "4", "4.5", "5", "5.5", "6")})

## Modellierte, geschaetzte Schadensumme pro Gitter darstellen
## ----

## create a new - not projected - RasterLayer with the
## EstimatedLoss cellnumbers as values

rEstLoss <- raster(xmn=min(xs)-x.cellsize/2, xmx=max(xs)+x.cellsize/2,
                   ymn=min(ys)-x.cellsize/2, ymx=max(ys)+x.cellsize/2,
                   ncols=dim(EstimatedLoss)[1], nrows=dim(EstimatedLoss)[2],
                   resolution = x.cellsize,
                   vals=rotate90.matrix(EstimatedLoss))

## ch-coordsyst
crs(rEstLoss) <- CRS("+init=epsg:21781") # LV03

## bins/breaks anhand quantilen
vals <- na.omit(values(rEstLoss))
prbs <- c(0.5,0.75, 0.90, 0.95, 0.975, 1)
qtls <- quantile(vals, prbs)
bns <- c(0, ceiling(as.numeric(qtls)))

# ## Legende mit Schadenzahlen als Kategoriegrenzen
# cbL <- colorBin(palette = 'YlOrBr', domain = values(rEstLoss),
#                 bins = bns, na.color = 'transparent')

## Legende mit Quantilen als Kategoriegrenzen
cbL <- colorQuantile(palette = 'YlOrBr', domain = vals,
                     n = length(prbs), probs = c(0, prbs),
                     na.color = 'transparent')

mLoss <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    setView(lng = 8.54226, lat = 47.37174, zoom = 10) %>%
    addRasterImage(rEstLoss, colors = cbL, opacity = 0.8) %>%
    addLegend(position = "bottomright", pal = cbL,
              values = values(rEstLoss),
              title = "Schaden", className = "info legend",
              labFormat = labelFormat(big.mark = "'"))


## ----
##  5. SCHADENSCHAETZUNG NACH INTERPOL METHODE:
## ----

## definieren das Objekt mit den Radar-Daten fuer interp.surface
locObj <- cbind(gemdat$geox, gemdat$geoy)
gridObjPoh <- list(x = xs,
                   y = ys,
                   z = poh.array)
gridObjMeshs <- list(x = xs,
                     y = ys,
                     z = meshs.array)

datInterpol <- gemdat %>% dplyr::select(strGebNr, gebNr, gemeinde, versSum,
                                        zweckcode, gebBaujahr, volumen, geox, geoy) %>% 
  ## interpolation der poh und meshs werte auf die gebaeude
  mutate(poh = round(interp.surface(gridObjPoh, locObj),1) *10,
         meshs = round(interp.surface(gridObjMeshs, locObj),1),
         bjKat = ifelse(gebBaujahr < 1960, 1,
                        ifelse(gebBaujahr <= 2002,2,3))) %>%
  ## nur gitterzellen mit mindestens poh 80%
  filter(poh >= 80)

# ## "turn off" scientific notation in write.csv 
# datInterpol$geox <- format(datInterpol$geox, scientific = FALSE)
# datInterpol$geoy <- format(datInterpol$geoy, scientific = FALSE)
# datInterpol$pohInterpol <- format(datInterpol$pohInterpol, scientific = FALSE)
# datInterpol$meshsInterpol <- format(datInterpol$meshsInterpol, scientific = FALSE)
# write.csv2(datInterpol, 'testInterpol3.csv', row.names = F)

## statistische Kennzahl
## Quelle: analyse.hagelevents.xlsx, Nov 2017, MHE
schadMean <- 5e3
betrofKat1 <- 3e-2
betrofKat2 <- 6e-2
betrofKat3 <- 8e-2

# schad <- read.csv2('./Rdata/hagelschad.170802.csv', stringsAsFactors = FALSE)
# 
# dat <- dat %>% mutate(schadSum = schad$indexSchad[match(dat$gebNr, schad$gebnr)],
#                       schadSum = ifelse(is.na(schadSum), 0, schadSum),
#                       schadLogi = ifelse(schadSum == 0, 0, 1),
#                       bjKat = ifelse(gebBaujahr < 1960, 1,
#                                      ifelse(gebBaujahr <= 2002,2,3))) %>% 
#   group_by(bjKat)
# ## mean pro gebjahr kat
# summarise(dat, round(mean(schadSum[schadSum>0]),0))
# ## median pro gebjahr kat
# summarise(dat, round(median(schadSum[schadSum>0]),0))
# ## betroffenheit pro gebjahr kat
# summarise(dat, ceiling(sum(schadLogi == 1)/sum(schadLogi == 0) *100))

## auswahl betroffener Gebaeude
tbl <- data.frame(anzKat1 = round(sum(datInterpol$bjKat == 1) *betrofKat1, 0),
                  anzKat2 = round(sum(datInterpol$bjKat == 2) *betrofKat2, 0),
                  anzKat3 = round(sum(datInterpol$bjKat == 3) *betrofKat3, 0)) %>% 
  mutate(schadKat1 = anzKat1 *schadMean,
         schadKat2 = anzKat2 *schadMean,
         schadKat3 = anzKat3 *schadMean,
         schadHagel = schadKat1 + schadKat2 + schadKat3)

tblOut <- data.frame("BaujahrKat" = c("Baujahr bis 1960",
                                      "Baujahr zwischen 1960 und 2002",
                                      "Baujahr ab 2002"),
                  "AnzahlSchad" = as.integer(t(tbl[1:3])),
                   "Schadensumme" = as.integer(t(tbl[4:6])))




message(print(paste0('INTERPOL - Schadenschaetzung fuer das Hagelereignis vom ',
                     eventDat, ': CHF ',
                     format(tbl$schadHagel/1e6, big.mark="'"), ' Mio')))

