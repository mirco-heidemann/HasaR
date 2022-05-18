## ----
##  Aggregate the new GVZ Portfolio (GemDat, GemDat/Rubin) to the
##  MeteoSwiss Hailradar grid. Save it as R-object.
##
##  For Hailmodelling HasaR
##
## Mirco Heidemann, Dez 17
## Modified in May 2022
## ----

## Funktionen und Packages laden
library(rgdal)
library(tidyverse)
library(here)

pth_data <- here("Data/")
pth_tbl <- here("Data/tables/")
pth_func <- here("Data/rfunctions/")
pth_pres <- here("Output/")
pth_tif <- here("Data/radarTiffs/")
pth_rdata <- here("Data/rdata/")
pth_shp <- here("Data/shapes/")

## AKTUELLER GEBAEUDEBESTAND ANGEBEN:
## (Versicherungssumme und Koordianten)
portfolio_file <- 'GVZ_Exposure_202201.csv'

## Gebaeude-Daten lesen
input_csv <- paste0(pth_tbl, portfolio_file)
#df_exp <- read.csv2(input_csv, stringsAsFactors = FALSE, sep = ',')

df_exp <- read_delim(input_csv, delim = ";",
                  locale = locale(encoding = 'UTF-8',
                                  decimal_mark = ".",
                                  grouping_mark = "'"),
                  col_names = TRUE,
                  skip_empty_rows = TRUE,
                  na = c("", "NA"),
                  col_types = cols_only(
                    Versicherungssumme = col_number(),
                    KoordinateNord = col_number(),
                    KoordinateOst = col_number())
                  ) %>%
  ## Wenn keine Koordinaten, raus...
  filter(!is.na(KoordinateNord)) %>%
  mutate(versSum = Versicherungssumme,
         geox = KoordinateOst,
         geoy = KoordinateNord) %>% 
  dplyr::select(-c(Versicherungssumme, KoordinateNord, KoordinateOst))
  
# summary(df_exp)

## Ein GeoTiff lesen fuer das Gitter
#poh <- readGDAL(paste0(pth_tif, 'poh.20120701.tif'))
poh <- readGDAL(paste0(pth_tif, 'poh.20210628.tif'))

## ----
##  1. GVZ EXPOSURE AUF RADARGITTER AGGREGIEREN
## ----
## shapes fuer plots einlesen
sh.gemeinde <- readOGR(paste0(pth_shp, 'gemeinden.2016.shp'),
                       layer = 'gemeinden.2016')

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

ind <- which(df_exp$geox >= x.min.zh & df_exp$geox <= x.max.zh)
if (length(ind) > 0) {
  df_exp <- df_exp[ind,]
}
ind <- which(df_exp$geoy >= y.min.zh & df_exp$geoy <= y.max.zh)
if (length(ind) > 0) {
  df_exp <- df_exp[ind,]
}

## Das Gitter vom Radar auf den Bereich vom Knt ZH begrenzen.
ind.x <- which(xs >= (min(df_exp$geox) - x.cellsize) &
                 xs <= (max(df_exp$geox) + x.cellsize))
ind.y <- which(ys >= (min(df_exp$geoy) - y.cellsize) &
                 ys <= (max(df_exp$geoy) + y.cellsize))
if (length(ind.x) > 0 & length(ind.y) > 0) {
  xs <- xs[ind.x]
  ys <- ys[ind.y]
}

## definiere Arrays fuer die Anzahl und Versicherungssumme pro
## Gitterzelle
Anzahl <- VersSumme <- x.coord <- y.coord <- array(dim = c(length(xs),
                                                           length(ys)))

## Schleife ueber Gitterzellen
for (i in 1:length(xs)) {
  for (j in 1:length(ys)) {
    ind <- which(df_exp$geox > (xs[i] - x.cellsize / 2) &
                   df_exp$geox <= (xs[i] + x.cellsize / 2) &
                   df_exp$geoy > (ys[j] - y.cellsize / 2) &
                   df_exp$geoy <= (ys[j] + y.cellsize / 2))
    if (length(ind) > 0) {
      Anzahl[i,j] <- length(ind)
      VersSumme[i,j] <- sum(df_exp$versSum[ind])
      x.coord[i,j] <- xs[i]
      y.coord[i,j] <- ys[j]
    }
  }
}

## Nur Knt ZH, Kontrolle:
image(Anzahl)

## Datum aus dem Geb. Daten File extrahieren
per_char <- unlist(strsplit(gsub("([0-9]+).*$", "\\1", input_csv),'\\.'))[3]
# out_file_char <- paste0('GemdatGitter.',per_char,'.Rdata')
out_file_char <- 'GemdatGitter.Rdata'
# save(xs, ys, ind.x, ind.y, x.coord, y.coord, Anzahl,
#      VersSumme, gemdat, per_char,
#      file = paste0(pth_rdata, out_file_char))

