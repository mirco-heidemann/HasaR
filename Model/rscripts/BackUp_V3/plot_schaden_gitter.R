## ---------------------------------------------------------------------
##  Radar-Daten, Anzahl Schadenmeldungen und Schadensumme plotten.
##  Punkte mit Schaden ohne Radar-Signal markieren.
##
##  Mirco Heidemann, 08/2014
## ---------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('J:/Naturgefahren/Datenanalysen/Ereignisanalysen/Hagel/HasaR/data/')

## maptools Package laden
require(maptools)
## raster Package laden
require(rgdal)
## Skript zum Rotieren bzw. Spiegeln vom Radar-Bild laden
source(paste('J:/Naturgefahren/Datenanalysen/Statistische Analysen/',
             'Rscripts meteoSchweiz/rotate.R', sep=''))

## gegitterte Gemdat-Daten lesen
rdata.path <- 'J:/Naturgefahren/Datenanalysen/Ereignisanalysen/Hagel/HasaR/data/Rdata/'
load(paste(rdata.path, 'GemdatGitter201601.Rdata', sep=''))

## verschiedene ordner-Pfade definieren:
pdf.out <- 'J:/Naturgefahren/Datenanalysen/Ereignisanalysen/Hagel/HasaR/pdf/'
rdata.path <- 'J:/Naturgefahren/Datenanalysen/Ereignisanalysen/Hagel/HasaR/data/Rdata/'
shape.path <- 'J:/Naturgefahren/Datenanalysen/Daten-Grundlagen GIS/Grunddaten_KantonZH/'
portfolio.path <- 'J:/Naturgefahren/Datenanalysen/Portfolio gvz/gebäudebestand gvz/2016/'

## shapes fuer plots einlesen
sh.gemeinde <- readShapeSpatial(paste(shape.path,'gemeinden_2015.shp',sep=""))
sh.seen <-readShapeSpatial(paste(shape.path,'seendet_250.shp',sep=""))
sh.bezirke <- readShapeSpatial(paste(shape.path, 'Bezirke', sep=""))
sh.schaetzkreis <- readShapeSpatial(paste(shape.path, 'gvz_schaetzkreise', sep=""))

in.files <- list.files(rdata.path, '-Gitter.Rdata')
datum <- paste('20', sub('-Gitter.Rdata', '',
                         sub('hagelschad', '', in.files)), sep = '')

for (i in 1:length(in.files)) {
##for (i in 1) {
  in.file <- paste(in.files[i], sep = '/')
  load(paste(rdata.path, in.file, sep=""))
  
  ## load POH from GeoTiff
  poh.file <- paste(rdata.path, 'BZC', datum[i], '.tiff', sep = '')
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
  
  ## load MESHS from GeoTiff
  kg.file <- paste(rdata.path, 'MZC', datum[i], '.tiff', sep = '')
  kg <- readGDAL(kg.file)
  
  ## von kg abstufung zu meteoschweiz hagelkorngroesse (meshs)
  meshs <- kg
  meshs.tab <- data.frame(meshs = c(0, seq(from=2, to=6, by=0.5)),
                          kg = seq(from=0, to=9, by=1))
  meshs@data$band1<- as.array(meshs.tab$mesh[match(kg@data$band1,
                                                   meshs.tab$kg)])
  
  ## definieren Vektoren fuer x- und y-Koordinaten
  ## geht auch mit bbox(meshs) und gridparameters(meshs)
  meshs.sum <- summary(meshs)
  x.cellsize <- meshs.sum$grid$cellsize[1]
  y.cellsize <- meshs.sum$grid$cellsize[2]
  xmin <- meshs.sum$bbox[1,1] + x.cellsize / 2
  xmax <- meshs.sum$bbox[1,2] - x.cellsize / 2
  ymin <- meshs.sum$bbox[2,1] + y.cellsize / 2
  ymax <- meshs.sum$bbox[2,2] - y.cellsize / 2
  
  xs.meshs <- seq(xmin, xmax, x.cellsize)
  ys.meshs <- seq(ymin, ymax, y.cellsize)
  meshs <- mirror.matrix(as.array(meshs))
  
  ## reduziere meshs zu gleicher Domaine wie Gebaeude- und Schaden-Daten
  ind.x <- which(xs.meshs >= min(xs) & xs.meshs <= max(xs))
  ind.y <- which(ys.meshs >= min(ys) & ys.meshs <= max(ys))
  meshs <- meshs[ind.x, ind.y]  
  
  # MeteoSwiss  POH-Colours
  # unique.values <- sort(unique(as.vector(poh)))
  unique.values <- (0:10)
  brks.poh <- unique.values + 0.1
  
  # MeteoSwiss  POH-Colours
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
  
  ## meshs
  #unique.values <- sort(unique(as.vector(meshs)))
  unique.values <- c(0, seq(from=2, to=6, by=0.5))
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
  
  dat.character <- format(as.Date(datum[i],"%Y%m%d"),"%d.%m.%Y")
  
  pdf.file <- paste(pdf.out, datum[i], '.pdf', sep = '')
  pdf(pdf.file, width = 8.5, height = 5)
  par(mar = c(1, 1, 2, 1), mfcol = c(1, 2))
  
  ## POH pro grid
  plot(sh.gemeinde,  border='gray80', add = F, lwd=1)
  plot(sh.seen, col='lightsteelblue2', border='lightsteelblue2',
       add = TRUE)
  ## Bezirke des Kantons ZH
  plot(sh.bezirke, lwd = 0.5, add = TRUE)
  image(xs, ys, poh, ann = FALSE, axes = FALSE, col = cols.poh,
        breaks = brks.poh, useRaster=T, add = T)

  ## Gitterzellen ohne POH aber mit Schaden markieren
  ind <- which(poh < 1 & Schaden.Anzahl > 0, arr.ind = TRUE)
  if (length(ind) > 0) {
    points(xs[ind[,1]], ys[ind[,2]], cex = 0.5)
    n <- length(ind)
  } else {
    n <- 0
  }
  leg.txt <- as.character(10*c(1:10))
  legend('bottomright', leg.txt, col = cols.poh, pch = 15,
         title ='POH [%]', pt.cex = 1, cex=0.5,bg = 'white', 
         box.col = 'white')
  title(main = paste('Ereignis vom ', dat.character,
  ' - Hagelwahrscheinlichkeit\n(Gitterzellen ohne POH aber mit Schaden = ',
                     n, ')', sep = ''), line = 0.4, cex.main = 0.7)
  box()
  
  ## MESHS pro grid
  plot(sh.gemeinde,  border='gray80', add = F, lwd=1)
  plot(sh.seen, col='lightsteelblue2', border='lightsteelblue2',
       add = TRUE)
  ## Bezirke des Kantons ZH
  plot(sh.bezirke, lwd = 0.5, add = TRUE)
  image <- image(xs, ys, meshs, ann = FALSE, axes = FALSE,
                 col = cols.meshs, breaks = brks.meshs,
                 useRaster=T, add = T)
  
  ## Gitterzellen ohne meshs aber mit Schaden markieren
  ind <- which(meshs < 1 & Schaden.Anzahl > 0, arr.ind = TRUE)
  if (length(ind) > 0) {
    points(xs[ind[,1]], ys[ind[,2]], cex = 0.5)
    n <- length(ind)
  } else {
    n <- 0
  }
  leg.txt <- as.character(seq(from=2, to=6, by=0.5))
  legend('bottomright', leg.txt, col = cols.meshs,
         title ='MESHS [cm]', pch = 15, cex = 0.5, pt.cex = 1,
         bg = 'white', box.col = 'white')
  title(main = paste('Ereignis vom ', dat.character,
  ' - Maximale Korngrösse am Boden\n(Gitterzellen ohne MESHS aber mit Schaden = ',
                     n, ')', sep = ''), line = 0.4, cex.main = 0.7)
  box()
  
  ## 2. Seite Schadensumme und schadenanzahl
  breaks.anzahl <- sort(c(0, 5, 50, 
                     max(Schaden.Anzahl, na.rm = TRUE)))
  
  cols.anzahl <- rev(heat.colors((length(breaks.anzahl) - 1), alpha = 0.6))
  
  breaks.summe <- sort(c(0, 25e3, 200e3,
                    1e3*ceiling(max(Schaden.SchadSum, na.rm = TRUE)/1e3)))
  
  cols.summe <- rev(heat.colors((length(breaks.summe) - 1), alpha = 0.6))
  
  ## Anzahl Schaeden pro grid
  plot(sh.gemeinde,  border='gray80', add = F, lwd=1)
  plot(sh.seen, col='lightsteelblue2', border='lightsteelblue2',
       add = TRUE)
  ## Bezirke des Kantons ZH
  plot(sh.bezirke, lwd = 0.5, add = TRUE)
  image(xs, ys, Schaden.Anzahl, ann = FALSE, axes = FALSE,
        col = cols.anzahl, breaks = breaks.anzahl, useRaster = T, add = T)
  
  leg.txt <- as.character(c("-", "bis 5", "5 - 50",">50"))
  legend('bottomright', leg.txt, col = c('white', cols.anzahl),
         title ='Anzahl Sch?den', pch = 15, cex = 0.5, pt.cex = 1,
         bg = 'white', box.col = 'white')

  title(main = paste('Ereignis vom ', dat.character, ' - Anzahl Sch?den',sep = ''),
        line = 0.4, cex.main = 0.7)
  box()
  
  ## Schadensumme pro grid
  plot(sh.gemeinde,  border='gray80', add = F, lwd=1)
  plot(sh.seen, col='lightsteelblue2', border='lightsteelblue2',
       add = TRUE)
  ## Bezirke des Kantons ZH
  plot(sh.bezirke, lwd = 0.5, add = TRUE)
  image(xs, ys, Schaden.SchadSum, ann = FALSE, axes = FALSE,
        col = cols.summe, breaks=breaks.summe, useRaster = T, add =T)
  
  leg.txt <- as.character(c("-","bis 25'000", "25'000 - 200'000",
                            ">200'000"))
  legend('bottomright', leg.txt, col = c('white', cols.summe),
         title ='Schadensumme', pch = 15,
         cex = 0.5, pt.cex = 1, bg = 'white', box.col = 'white')
  
  title(main = paste('Ereignis vom ', dat.character, ' - Schadensumme',
                     sep=''), line = 0.4, cex.main = 0.7)
  box()
  
  dev.off()
  par(mfcol = c(1, 1))
}


## R beenden ohne etwas zu speichern
##q('no')
