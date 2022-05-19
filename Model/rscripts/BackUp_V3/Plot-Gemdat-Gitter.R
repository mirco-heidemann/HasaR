## ---------------------------------------------------------------------
##  Aggregierte gegitterte Daten des Gebaeudebestandes darstellen
##
##  Mirco Heidemann, 05/2014
## ---------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('J:/MHE/03 Statistische Analysen/Radar Hagelschaden/meteoSchweiz/')

## gegitterte Daten lesen
in.file <- 'Rdata/GemdatGitter.Rdata'
load(in.file)

## maptools Package laden
library(maptools)

## Gemeinde-Grenzen lesen
shape.path <- 'ShapeFiles'
gemeinden <- readShapePoly(paste(shape.path, 'g1g98', sep = '/'))
## define polygons for Zurich
ind.zh <- which(gemeinden$KT < 2)
gemeinden <- gemeinden[ind.zh,]

breaks.anzahl <- c(0, 10, 20, 50, 100, 200, 500, 1000,
                   max(Anzahl, na.rm = TRUE))
cols.anzahl <- rainbow((length(breaks.anzahl) - 1), start = 0.15)

breaks.summe <- c(0, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9, 5e9,
                  1e6*ceiling(max(VersSumme, na.rm = TRUE)/1e6))
cols.summe <- rainbow((length(breaks.summe) - 1), start = 0.15)

breaks.summe <- c(0, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9, 5e9,
                  1e6*ceiling(max(VersSumme, na.rm = TRUE)/1e6))
cols.summe <- rainbow((length(breaks.summe) - 1), start = 0.15)

breaks.mittel <- c(0, 2e5, 4e5, 6e5, 8e5, 1e6, 2e6, 5e6,
                   1e6*ceiling(max(VersSumme/Anzahl, na.rm = TRUE)/1e6))
cols.mittel <- rainbow((length(breaks.mittel) - 1), start = 0.15)

pdf.file <- 'pdf/GemdatGitter-plots_v1.pdf'
pdf(pdf.file, width = 11, height = 5)

par(mar = c(1, 1, 2, 1), mfcol = c(1, 3))

image(xs, ys, Anzahl, ann = FALSE, axes = FALSE, col = cols.anzahl,
      breaks = breaks.anzahl)
##abline(v = seq(min(xs), max(xs), 1e3))
##abline(h = seq(min(ys), max(ys), 1e3))
plot(gemeinden, add = TRUE)
leg.txt <- paste(breaks.anzahl[-length(breaks.anzahl)],
                 breaks.anzahl[-1], sep = '-')
legend('topleft', leg.txt[1:4], col = cols.anzahl[1:4],
       pch = 15, pt.cex = 1.8, bty = 'n')
legend('topright', leg.txt[5:8], col = cols.anzahl[5:8],
       pch = 15, pt.cex = 1.8, bty = 'n')
title(main = 'Anzahl Gebäude', line = 0.4)
box()

image(xs, ys, VersSumme, ann = FALSE, axes = FALSE, col = cols.summe,
      breaks = breaks.summe)
plot(gemeinden, add = TRUE)
leg.txt <- paste(breaks.summe[-length(breaks.summe)]/1e6,
                 breaks.summe[-1]/1e6, sep = '-')
legend('topleft', leg.txt[1:4], col = cols.summe[1:4], pch = 15,
       pt.cex = 1.8, bty = 'n')
legend('topright', leg.txt[5:8], col = cols.summe[5:8], pch = 15,
       pt.cex = 1.8, bty = 'n')
title(main = 'Versicherungs-Summe aller Gebaeude in Mio CHF', line = 0.4)
box()

image(xs, ys, VersSumme/Anzahl, ann = FALSE, axes = FALSE,
      col = cols.mittel, breaks = breaks.mittel)
plot(gemeinden, add = TRUE)
leg.txt <- paste(breaks.mittel[-length(breaks.mittel)]/1e6,
                 breaks.mittel[-1]/1e6, sep = '-')
legend('topleft', leg.txt[1:4], col = cols.mittel[1:4], pch = 15,
       pt.cex = 1.8, bty = 'n')
legend('topright', leg.txt[5:8], col = cols.mittel[5:8], pch = 15,
       pt.cex = 1.8, bty = 'n')
title(main = 'Mittlere Versicherungs-Summe in Mio CHF', line = 0.4)
box()

dev.off()


## R beenden ohne etwas zu speichern
##q('no')
