library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  
library(ggthemes) 
library(stringr)

# import your raster file here:
raster <- raster("data.tif")

raster_spdf <- as(raster, "SpatialPixelsDataFrame")
raster_df <- as.data.frame(raster_spdf)

ewbrks <- seq(-64,-63,0.2)
nsbrks <- seq(-13,12,0.2)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x > 0, paste(toString(x), "E"), ifelse(x < 0, paste(str_sub(toString(x), 2,), "W"),toString(x)))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(str_sub(toString(x), 2,), "S"), ifelse(x > 0, paste(toString(x), "N"),toString(x)))))


p <- ggplot() +  
  geom_tile(data=raster_df, aes(x=x, y=y, fill=data), alpha=0.8) +
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))+
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme(axis.text = element_text(size=12))
p
