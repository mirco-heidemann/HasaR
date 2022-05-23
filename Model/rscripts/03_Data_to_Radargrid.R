## ----
##  Daten aus Gemdat auf Gitter der Radar-Daten aggregieren
##  - Gebaeudebestand (Exposure) und Hagelschaeden der ausgewaehlten
##    Ereignisse
##
##  Mirco Heidemann, 05/2014
##  Modified 03/2016, 11/2017, 05/2022
## ----

## Funktionen und Packages laden
library(rgdal)
library(tidyverse)
library(here)
library(sf)

pth_data <- here("Model/data/")
pth_func <- here("Data/rfunctions/")
pth_tif <- here("Model/radarTiffs/")
pth_rdata <- here("Data/rdata/")
pth_tbl <- here("Model/tables")

## Skript zum Rotieren bzw. Spiegeln vom Radar-Bild laden
source(here(pth_func, 'rotate.R'))

## AKTUELLER GEBAEUDEBESTAND ANGEBEN:
## (Versicherungssumme und Koordianten)
portfolio_file <- 'GVZ_Exposure_202201.csv'

## Die MeteoSchweiz Radar Tiffs sind noch in der alten Landesvermessung LV03 -
## Bezugssystem CH1903 - codiert!!
## Die Exposure Daten werden deshalb vom Bezugssystem CH1903+ in der 
## Landesvermessung LV95 in das alte Bezugssystem CH1903 transformiert.

## Gebaeude-Daten lesen
input_csv <- here(pth_data, portfolio_file)

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
  rename(versSum = Versicherungssumme)

## Coord transform with the sf-package:
## ---

## First define coord.system, ...
df_point_exp <- df_exp %>%
  st_as_sf(coords = c("KoordinateOst", "KoordinateNord"), crs = 2056)
## ... then transform into the LV03 coordinat system
df_exp_lv03 <- st_transform(df_point_exp, 21781)

## Write point geometry coords in separate columns
df_exp_lv03 <- df_exp_lv03 %>%
  mutate(geox = unlist(map(df_exp_lv03$geometry,1)),
         geoy = unlist(map(df_exp_lv03$geometry,2)))

## Convert geo feature tibble into data frame
df_exp_lv03 <- data.frame(df_exp_lv03) %>% 
  dplyr::select(-geometry)

# summary(df_exp_lv03)
## ---

## GeoTiff-Datei definieren
in_poh <- here(pth_tif, 'BZC20120701.tiff')

## GeoTiff lesen
poh <- readGDAL(in_poh)

## GVZ EXPOSURE AUF RADARGITTER AGGREGIEREN

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
ind.x <- which(xs >= (min(df_exp_lv03$geox) - x.cellsize) &
  xs <= (max(df_exp_lv03$geox) + x.cellsize))
ind.y <- which(ys >= (min(df_exp_lv03$geoy) - y.cellsize) &
  ys <= (max(df_exp_lv03$geoy) + y.cellsize))
if (length(ind.x) > 0 & length(ind.y) > 0) {
  xs <- xs[ind.x]
  ys <- ys[ind.y]
  poh.array <- poh.array[ind.x, ind.y]
}

# ## definieren Arrays fuer die Anzahl und Versicherungssumme pro
# ## Gitterzelle
# Anzahl <- VersSumme <- array(dim = c(length(xs), length(ys)))

## definiere Arrays fuer die Anzahl und Versicherungssumme pro
## Gitterzelle
Anzahl <- VersSumme <- x.coord <- y.coord <- array(dim = c(length(xs),
                                                           length(ys)))

## Schleife ueber Gitterzellen
for (i in 1:length(xs)) {
  for (j in 1:length(ys)) {
    ind <- which(df_exp_lv03$geox > (xs[i] - x.cellsize / 2) &
      df_exp_lv03$geox <= (xs[i] + x.cellsize / 2) &
      df_exp_lv03$geoy > (ys[j] - y.cellsize / 2) &
      df_exp_lv03$geoy <= (ys[j] + y.cellsize / 2))
    if (length(ind) > 0) {
      Anzahl[i,j] <- length(ind)
      VersSumme[i,j] <- sum(df_exp_lv03$versSum[ind])
      x.coord[i,j] <- xs[i]
      y.coord[i,j] <- ys[j]
    }
  }
}

## Nur Knt ZH, Kontrolle:
## image(Anzahl)
## image(VersSumme)

## Datum aus dem Geb. Daten File extrahieren
per_char <- str_extract(input_csv, "[[:digit:]]+")
out_file_char <- here(pth_rdata, paste0(per_char, '_Exposure_Grid', '.Rdata'))

save(xs, ys, ind.x, ind.y, x.coord, y.coord, Anzahl,
     VersSumme, df_exp_lv03, per_char,
     file = out_file_char)

## -----
## Schaden einlesen und auf gleiche Gitter-Dimensionen aggregieren
## -----

in_files <- list.files(pth_tbl, '.csv')

for (f in 1:length(in_files)) {
  file_name <- here(pth_tbl, in_files[f])
  schad <- read_delim(file_name, delim = ';',
                      show_col_types = FALSE) %>%
    mutate(indexSchad = as.numeric(schad_index),
           geox = as.numeric(geox),
           geoy = as.numeric(geoy),
           vs = as.numeric(Versicherungssumme))
  
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
  
  out_file <- here(pth_rdata, sub('.csv', '_Gitter.Rdata', in_files[f]))
  # save(xs, ys, schad.Anzahl, schad.VersSumme, schad.SchadSum, file = out_file)
  
  cat("\n")
  cat("Gridded Rdata-file saved:", here(pth_rdata, out_file))
}

