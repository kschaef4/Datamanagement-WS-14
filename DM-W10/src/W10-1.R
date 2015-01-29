inpath <- "C:/Rdata/Data_Management/Raster"
setwd(inpath)

#install.packages('sp')
#install.packages('RColorBrewer')
#install.packages('raster')
#install.packages('rgdal')
#install.packages('latticeExtra')

library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)
library(latticeExtra)


#1
thermo.tif <- raster("LC82100502014328LGN00_B10.tif")
inpath <- "C:/Rdata/Data_Management/Vector"
setwd(inpath)
field <- readOGR("data_2014_subset1.shp", "data_2014_subset1")
field <- spTransform(field, CRS(projection(thermo.tif)))
vegetation <- field


a <- function (raster, vector){
  vector_classes <- cut(vector@data$COVRG, c(0, 20, 40, 60, 80, 100, 120))
  vector_colors <- colorRampPalette(brewer.pal(6,"Greens"))(6)
  min <- max(mean(getValues(raster)) - sd(getValues(raster)), 0)
  max <- mean(getValues(raster)) + sd(getValues(raster))
  
  breaks <- seq(min, max, length.out = 256)
  yat = seq(extent(raster)@ymin, 
            extent(raster)@ymax, length.out = 5)
  xat = seq(extent(raster)@xmin, 
            extent(raster)@xmax, length.out = 5)
  
  
  
  plt <- spplot(raster, col.regions = gray.colors(256), at = breaks,
                key = list(space = 'left', text = list(levels(vector_classes)), 
                           points = list(pch = 21, cex = 2, fill = vector_colors)),
                colorkey=list(space="right"),
                panel = function(...){
                  panel.levelplot(...)
                  panel.abline(h = yat, v = xat, col = "grey0", lwd = 0.8, lty = 3) 
                }
  )
  
  orl <- spplot(vector, zcol = "COVRG", col.regions = vector_colors, 
                cuts = c(0, 20, 40, 60, 80, 100, 120))
  
  plt + as.layer(orl)
}


a(thermo.tif, vegetation)