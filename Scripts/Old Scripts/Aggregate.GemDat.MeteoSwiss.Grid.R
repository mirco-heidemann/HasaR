## ----
##  Aggregate the new GVZ Portfolio (GemDat) to the
##  MeteoSwiss Hailradar grid. Save it as R-object.
##
##  For Hailmodelling HasaR
##
## Mirco Heidemann, Dez 17
## ----

## Funktionen und Packages laden
library(rgdal)
library(dplyr)

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## move up one directory
setwd("..")

pth.data <- ("./data/")
pth.tbl <- ("./data/tables/")
pth.func <- ("./data/rfunctions/")
pth.pres <- ("./presentation/")
pth.tif <- ("./data/radarTiffs/")
pth.rdata <- ("./data/rdata/")
pth.shp <- ("./data/shapes/")

## AKTUELLER GEBAEUDEBESTAND ANGEBEN:
portfolio.file <- 'GVZ.GEBAEUDEBESTAND.201801.georef.LV03 - CHECKED gem schaetzkr.csv'

## Gebaeude-Daten lesen
in.file <- paste0(pth.tbl, portfolio.file)
gemdat <- read.csv2(in.file, stringsAsFactors = FALSE, sep = ',')

## str(gemdat)
## datenfelder unterscheiden sich von jahr zu jahr, der DF muss hier angepasst werden
options(digits = 11) ## um die nach-kommastellen nicht zu verliehren
gemdat <- gemdat %>% mutate(versSum = as.numeric(versSum),
                            geox = as.numeric(X),
                            geoy = as.numeric(Y))
  # filter(!is.na(geox), ## Wenn keine Koordinaten, raus...
  #          ## Referenzierung betrachten:
  #          ## Wenn bei der Adresse keine Stasse angegeben ist UND die Referenzierung 
  #          ## nicht vom kantonalen Amt vorgenommen wurde, so ist die die Gebaeudelage
  #          ## zu ungenau bestimmbar und wird fuer die Anylse nicht beruecksichtigt.
  #        !c(strasse == "" & ref != "Ref_Geb_ZH2008"))

## Ein GeoTiff lesen fuer das Gitter
poh <- readGDAL(paste0(pth.tif, 'poh.20120701.tif'))

## ----
##  1. GVZ GEBAEUDE AUS GEMDAT AUF RADARGITTER AGGREGIEREN
## ----
## shapes fuer plots einlesen
sh.gemeinde <- readOGR(paste0(pth.shp, 'gemeinden.2016.shp'), layer = 'gemeinden.2016')

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
# out.file.char <- paste0('GemdatGitter.',per.char,'.Rdata')
out.file.char <- 'GemdatGitter.Rdata'
# save(xs, ys, ind.x, ind.y, x.coord, y.coord, Anzahl, VersSumme, gemdat, per.char,
#      file = paste0(pth.rdata, out.file.char))

