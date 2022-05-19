## ----
##  Alle gegitterten Schaden- und Radar-Daten in eine Tabelle stecken
##  meteoSchweiz Daten
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

## Skript zum Rotieren bzw. Spiegeln vom Radar-Bild laden
source(paste0(pth.func, 'rotate.R'))

## gegitterte Gemdat-Daten lesen
load(paste0(pth.rdata, 'GemdatGitter_2017.Rdata'))

Anzahl[is.na(Anzahl)] <- 0
ind.geb <- which(Anzahl >= 0)

## Dateien der gegitterten Schaden-Daten und Daten der Ereignisse definieren
in.files <- list.files(pth.tbl, '.Gitter.Rdata')
datum <- paste('20', sub('.Gitter.Rdata', '', sub('hagelschad.', '',
                                                  in.files)), sep = '')

## raster Package laden
library(rgdal)

## Tabelle fuer Daten definieren
grid.data <- data.frame()

for (i in 1:length(in.files)) {
  in.file <- paste0(pth.tbl, in.files[i])
  load(in.file)
  
  ## load POH from GeoTiff
  poh.file <- paste0(pth.tif, 'BZC', datum[i], '.tiff')
  poh <- readGDAL(poh.file)
  
  ## definieren Vektoren fuer x- und y-Koordinaten
  ## geht auch mit bbox(poh) und gridparameters(poh)
  poh.sum <- summary(poh)
  x.cellsize <- poh.sum$grid$cellsize[1]
  y.cellsize <- poh.sum$grid$cellsize[2]
  xmin <- poh.sum$bbox[1,1] + x.cellsize / 2
  xmax <- poh.sum$bbox[1,2] - x.cellsize / 2
  ymin <- poh.sum$bbox[2,1] + y.cellsize / 2
  ymax <- poh.sum$bbox[2,2] - y.cellsize / 2
  
  xs.poh <- seq(xmin, xmax, x.cellsize)
  ys.poh <- seq(ymin, ymax, y.cellsize)
  poh <- mirror.matrix(as.array(poh))
  
  ## reduziere POH zu gleicher Domaine wie Gebaeude- und Schaden-Daten
  ind.x <- which(xs.poh >= min(xs) & xs.poh <= max(xs))
  ind.y <- which(ys.poh >= min(ys) & ys.poh <= max(ys))
  poh <- poh[ind.x, ind.y]
  
  ## load meshs.class from GeoTiff
  meshs.class.file <- paste0(pth.tif, 'MZC', datum[i], '.tiff')
  meshs.class <- readGDAL(meshs.class.file)
  
  ## definieren Vektoren fuer x- und y-Koordinaten
  ## geht auch mit bbox(meshs.class) und gridparameters(meshs.class)
  meshs.class.sum <- summary(meshs.class)
  x.cellsize <- meshs.class.sum$grid$cellsize[1]
  y.cellsize <- meshs.class.sum$grid$cellsize[2]
  xmin <- meshs.class.sum$bbox[1,1] + x.cellsize / 2
  xmax <- meshs.class.sum$bbox[1,2] - x.cellsize / 2
  ymin <- meshs.class.sum$bbox[2,1] + y.cellsize / 2
  ymax <- meshs.class.sum$bbox[2,2] - y.cellsize / 2
  
  xs.meshs.class <- seq(xmin, xmax, x.cellsize)
  ys.meshs.class <- seq(ymin, ymax, y.cellsize)
  meshs.class <- mirror.matrix(as.array(meshs.class))
  
  ## reduziere meshs.class zu gleicher Domaine wie Gebaeude- und Schaden-Daten
  ind.x <- which(xs.meshs.class >= min(xs) & xs.meshs.class <= max(xs))
  ind.y <- which(ys.meshs.class >= min(ys) & ys.meshs.class <= max(ys))
  meshs.class <- meshs.class[ind.x, ind.y]
  
  if (i == 1) {
    grid.data <- data.frame(event = rep(datum[i], length(ind.geb)),
                            Anzahl = Anzahl[ind.geb],
                            VersSumme = VersSumme[ind.geb],
                            schad.Anzahl = schad.Anzahl[ind.geb],
                            schad.VersSumme = schad.VersSumme[ind.geb],
                            schad.SchadSum = schad.SchadSum[ind.geb],
                            poh = poh[ind.geb]/10, ## poh  zwischen 0 und 1
                            meshs.class = meshs.class[ind.geb],
                            stringsAsFactors = FALSE)
  } else {
    data.tmp <- data.frame(event = rep(datum[i], length(ind.geb)),
                           Anzahl = Anzahl[ind.geb],
                           VersSumme = VersSumme[ind.geb],
                           schad.Anzahl = schad.Anzahl[ind.geb],
                           schad.VersSumme = schad.VersSumme[ind.geb],
                           schad.SchadSum = schad.SchadSum[ind.geb],
                           ## poh als Werte zwischen 0 und 1
                           poh = poh[ind.geb]/10,
                           meshs.class = meshs.class[ind.geb],
                           stringsAsFactors = FALSE)
    grid.data <- rbind(grid.data, data.tmp)
  }
}

## Daten von NA auf Null setzen
ind <- which(is.na(grid.data$schad.Anzahl))
if (length(ind) > 0) grid.data$schad.Anzahl[ind] <- 0
ind <- which(is.na(grid.data$schad.VersSumme))
if (length(ind) > 0) grid.data$schad.VersSumme[ind] <- 0
ind <- which(is.na(grid.data$schad.SchadSum))
if (length(ind) > 0) grid.data$schad.SchadSum[ind] <- 0
ind <- which(is.na(grid.data$VersSumme))
if (length(ind) > 0) grid.data$VersSumme[ind] <- 0

## von meshs.class abstufung zu meteoschweiz hagelkorngroesse (meshs)
meshs.tab <- data.frame(meshs.class = seq(from=0, to=9, by=1),
                        meshs = seq(from=1.5, to=6, by=0.5))
grid.data$meshs <- meshs.tab$meshs[match(grid.data$meshs.class, meshs.tab$meshs.class)]
## hagelkorngroessen kleiner als 2 gibt es nicht in meshs
grid.data$meshs[which(grid.data$meshs<2)] <- 0
grid.data <- grid.data[,-8]

# save(grid.data, file=paste0(pth.tbl, "events.schadtabelle.meteoschweiz.2017.Rdata"))

(tbl <- aggregate(grid.data[,c(4,6)], by=list(grid.data$event), sum))
## R beenden ohne etwas zu speichern
##q('no')
