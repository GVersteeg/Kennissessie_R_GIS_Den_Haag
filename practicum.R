# laad de benodigde libraries
require(tidyverse)
require(sf)
require(sp)
require(raster)
require(rgdal)
require(tmap)
require(ggmap)
require(grid)
require(rgeos)
require(maptools)

#######################################################################################
# oefening 1: vector data
#######################################################################################

# inputfile enegielabels
f_energielabels <- "Energielabels_Postcode_5_niveau_Den_Haag_2016.shp"
# layers in de energielabels file
fc_list <- ogrListLayers(f_energielabels)
# lees shapefile energielabels in
energielabels <- readOGR(f_energielabels,layer=fc_list[1])

# plot de kaart van Den Haag met gemiddelde energielabels per p5 gebied
tm_shape(energielabels) +
  tm_polygons("MEAN_label", style="jenks", alpha=.5, palette=colorRampPalette(c("green", "red"))(5)) +
  tm_compass(type="arrow", position=c("right", "top"), fontsize = 2 ) + 
  tm_scale_bar()

# de trend is duidelijk: de binnenstad heeft minder energiezuinige panden dan de buitenwijken
# het zijn echter wel erg veel en kleine gebiedjes
# maak eenzelfde kaart, maar dan voor p4 gebieden
# natuurlijk zou je dat eigenlijk met de originele data moeten doen (per kadasterkavel)
# maar die hebben we niet, dus gebruik deze keer het gemiddelde van het gemiddelde

# maak van p5 p4 met unionSpatialPolygons, (pas op, doet niets met de data)


# maaklege data om in energielabels_p4 te zetten.


# bereken mean per p4 gebied
# natuurlijk zou je dat eigenlijk met de originele data moeten doen (per kadasterkavel)
# maar die hebben we niet, dus gebruik deze keer het gemiddelde van het gemiddelde


# plak de data en layer aan elkaar vast


# plot de kaart van Den Haag met gemiddelde energielabels per p4 gebied

#######################################################################################
# oefening 2: raster data
#######################################################################################
# Ik heb landsat data gedownload, van een mooie dag in de lente van 2015
# Deze data bevat 7 verschillende bands (sensoren)
# we gaan twee daarvan gebruiken om de bekijken of de vegetatie in Den Haag gezond is.

# unzip de tar
dir <- getwd()
setwd("landsat")
untar("landsat/LC081990242016060901T1-SC20180205032516.tar.gz")
setwd(dir)

# lees de banden 3 en 4 in
band3 <- raster("landsat/LC08_L1TP_199024_20160609_20170324_01_T1_sr_band3.tif")
band4 <- raster("landsat/LC08_L1TP_199024_20160609_20170324_01_T1_sr_band4.tif")

# plot deze banden om even te kijken hoe dat er nou uitziet
par(mfrow=c(1,2))
plot(band3)
plot(band4)
par(mfrow=c(1,1))

# Normalized Difference Vegetation Index
# https://en.wikipedia.org/wiki/Normalized_difference_vegetation_index
# NVDI: returns values from -1 to 1, where  
#    values closer to 1 indicate more productive vegetation;
#    negative values are probably not vegetation.

# bereken de NVDI en sla op in een nieuw raster
ndvi <- (band3 - band4) / (band3 + band4)
ndvi
plot(ndvi)

# crop het raster tot de extent van energielabels
# 1. reproject energielabels op dezelfde manier als landsat

# 2. crop het raster tot de extent van energielabels


# waardes onder 0 zijn zeer waarschijnlijk geen vegetatie
# & de ndvi neemt rond de 0 vaak water mee
# negeer daarom alle waardes < 0.2 (maak ze NA)

# plot de nvdi met een toepasselijke kleurschaal

# met een basemap en interactief

# en laten we voor de grap de p4 grenzen er nog even overheen plotten

#######################################################################################
# BONUS: combineer de vector en raster data in één layer
#######################################################################################
# Om je op weg te helpen, de methode die je nodig hebt is: raster::extract
# tidyverse heeft een methode die ook extract heet, en overschrijft die van  raster
# met raster:: dwing je R om de juiste methode te gebruiken.
# de output die de methode geeft is een matrix (met 1 kolom), en niet numeric.
# je weet ws wel dat je een gemiddelde alleen kan gebruiken voor normaal verdeelde data.
# misschien wil je dus eerst een histogrammetje maken?

# enne, pas op: raster::extract duurt best lang!

# en plot het resultaat

# conclusie: gemiddeld zijn de verschillen erg klein
# we hebben echter in de originele schaal gezien dat er ook uitschieters naar boven en beneden zijn.
# laten we de hele riedel nog een keer doen, maar dan met max

# en plot het resultaat



