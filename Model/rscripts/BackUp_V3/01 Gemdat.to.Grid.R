## ----
##  Daten aus Gemdat auf Gitter der Radar-Daten aggregieren
##  - Gebaeudebestand und Hagelschaeden der ausgewaehlten
##    Ereignisse
##
##  Mirco Heidemann, 05/2014
##  Modified 03/2016, 11/2017
## ----

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## move up one directory
setwd("..")

pth.data <- ("../data/")
pth.portfol <- ("../data/tables/")
pth.func <- ("../data/rfunctions/")
pth.tif <- ("./radarTiffs/")
pth.rdata <- ("../data/rdata/")
pth.tbl <- ("./tables/")

in.file <- paste0(pth.portfol, 'gebaeudebestand.201701.georef - CHECKED gem schaetzkr.csv')
gemdat <- read.csv2(in.file, stringsAsFactors = FALSE, sep=',')

options(digits = 11) ## um die nach-kommastellen nicht zu verliehren
gemdat <- gemdat %>% mutate(versSum = as.numeric(versSum),
                            geox = as.numeric(geoxLV03),
                            geoy = as.numeric(geoyLV03))

## raster Package laden
library(rgdal)

## Skript zum Rotieren bzw. Spiegeln vom Radar-Bild laden
source(paste0(pth.func, 'rotate.R'))

## GeoTiff-Datei definieren
in.poh <- paste0(pth.tif, 'BZC20120701.tiff')

## GeoTiff lesen
poh <- readGDAL(in.poh)

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
##--> POH-Werte
poh.array <- mirror.matrix(as.array(poh))

## hier am besten erst einmal das Gitter vom Radar auf den Bereich
## vom Kanton Zuerich begrenzen.
ind.x <- which(xs >= (min(gemdat$geox) - x.cellsize) &
  xs <= (max(gemdat$geox) + x.cellsize))
ind.y <- which(ys >= (min(gemdat$geoy) - y.cellsize) &
  ys <= (max(gemdat$geoy) + y.cellsize))
if (length(ind.x) > 0 & length(ind.y) > 0) {
  xs <- xs[ind.x]
  ys <- ys[ind.y]
  poh.array <- poh.array[ind.x, ind.y]
}

## definieren Arrays fuer die Anzahl und Versicherungssumme pro
## Gitterzelle
Anzahl <- VersSumme <- array(dim = c(length(xs), length(ys)))

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
    }
  }
}

## Nur Knt ZH, Kontrolle:
## image(Anzahl)
## image(VersSumme)

## Datum aus dem Geb. Daten File extrahieren
per.char <- unlist(strsplit(gsub("([0-9]+).*$", "\\1", in.file),'\\.'))[2]
out.file.char <- paste0(pth.rdata, 'GemdatGitter', per.char,'.Rdata')
# save(xs, ys, Anzahl, VersSumme, file=out.file.char)

## -----
## Schaden einlesen und auf gleiche Gitter-Dimensionen aggregieren
## -----
library(dplyr)

## liste alle csv files im ordner auf
setwd("./tables")

in.files <- list.files(".", '.csv')

for (f in 1:length(in.files)) {
  schad <- read.csv2(in.files[f], stringsAsFactors = FALSE)
  schad <- schad %>% mutate(indexSchad = as.numeric(indexSchad),
                  geox = as.numeric(geox),
                  geoy = as.numeric(geoy),
                  vs = as.numeric(vs))

  ## Arrays definieren (gleiche Dimension wie oben)
  schad.Anzahl <- schad.VersSumme <- schad.SchadSum <-
    array(dim = c(length(xs), length(ys)))

  ## Schleife ueber Gitterzellen
  for (i in 1:length(xs)) {
    for (j in 1:length(ys)) {
      ind <- which(schad$geox > (xs[i] - x.cellsize / 2) &
        schad$geox <= (xs[i] + x.cellsize / 2) &
        schad$geoy > (ys[j] - y.cellsize / 2) &
        schad$geoy <= (ys[j] + y.cellsize / 2))
      if (length(ind) > 0) {
        schad.Anzahl[i,j] <- length(ind)
        schad.VersSumme[i,j] <- sum(schad$vs[ind])
        schad.SchadSum[i,j] <- sum(schad$indexSchad[ind])
      }
    }
  }
  out.file <- sub('.csv', '.Gitter.Rdata', in.files[f])
  save(xs, ys, schad.Anzahl, schad.VersSumme, schad.SchadSum,
       file = out.file)
}
## R beenden ohne etwas zu speichern
#q('no')
