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

# de trend is duidelijk: binnenstad is minder energiezuinig dan de buitenwijken
# het zijn echter wel erg veel en kleine gebiedjes
# maak eenzelfde kaart, maar dan voor p4 gebieden

# van p5 p4 maken, maar doet niets met de data
energielabels@data$PC4CODE <- substring(energielabels@data$PC5CODE,1,4)
energielabels_p4 <- unionSpatialPolygons(energielabels, energielabels@data$PC4CODE )

# lege data maken om in energielabels_p4 te zetten.
mean_label <- data_frame( unique(substring(energielabels@data$PC5CODE,1,4)))
colnames(mean_label) <- c("PC4CODE")
rownames(mean_label) <- unique(substring(energielabels@data$PC5CODE,1,4))
energielabels_p4 <- SpatialPolygonsDataFrame(energielabels_p4,mean_label)

# bereken mean per p4 gebied
summ_energie <- energielabels@data %>%
  group_by(PC4CODE) %>%
  summarise(mean = mean(MEAN_label,na.rm=TRUE), n = n())

energielabels_p4@data <- inner_join(energielabels_p4@data,summ_energie)

# plot de kaart van Den Haag met gemiddelde energielabels per p4 gebied
energielabels_p4@data
plot(energielabels_p4)
tm_shape(energielabels_p4) +
  tm_polygons("mean", style="jenks", alpha=.5, palette=colorRampPalette(c("green", "red"))(5)) +
  tm_compass(type="arrow", position=c("right", "top"), fontsize = 2 ) + 
  tm_scale_bar()


# opdracht 2
# Ik heb landsat data gedownload, van een mooie dag in de lente in 2015
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

par(mfrow=c(1,2))
plot(band3)
plot(band4)
par(mfrow=c(1,1))

# NVDI: returns values from -1 to 1, where  
#    values closer to 1 indicate more productive vegetation;
#    negative values are probably not vegetation.

# bereken de NVDI en sla op in een nieuw raster
ndvi <- (band3 - band4) / (band3 + band4)
ndvi
plot(ndvi)

# crop het raster tot de extent van energielabels
# reproject energielabels op dezelfde manier als landsat
energielabels_ndvi <- spTransform(energielabels, crs(ndvi) )
# crop het raster tot de extent van energielabels
crop_ndvi <- crop(ndvi, energielabels_ndvi)
# plot
plot(crop_ndvi)

values(crop_ndvi)

# alle waardes < 0.2 negeren (NA)
crop_ndvi_vegi <- crop_ndvi
values(crop_ndvi_vegi) <- ifelse(values(crop_ndvi)<0.2,NA,values(crop_ndvi))
plot(crop_ndvi_vegi)

# plot de nvdi
tm_shape(crop_ndvi_vegi) +
  tm_raster(n=10,palette=colorRampPalette(c("grey", "green"))(10)) +
  tm_compass(type="arrow", position=c("right", "top"), fontsize = 2 ) + 
  tm_scale_bar()

# met een basemap en interactief
tmap_mode("view")
tm_shape(crop_ndvi_vegi) +
  tm_raster(n=10,alpha=.95,palette=colorRampPalette(c("grey", "green"))(10)) +
  tm_compass(type="arrow", position=c("right", "top"), fontsize = 2 ) + 
  tm_scale_bar()

# en laten we voor de grap de p4 grenzen er nog even overheen
tm_shape(energielabels_p4) +
  tm_polygons(alpha=.1,border.col = "black") +
  tm_shape(crop_ndvi_vegi) +
  tm_raster(n=10,alpha=.95,palette=colorRampPalette(c("grey", "green"))(10)) +
  tm_compass(type="arrow", position=c("right", "top"), fontsize = 2 ) + 
  tm_scale_bar()

