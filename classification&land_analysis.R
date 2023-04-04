# Install and Load Packages
packages <- c('sf', 'terra','raster','RStoolbox','rgdal','rasterVis','gridExtra',
              'caret','mapview','plotly','mapedit','RColorBrewer','leaflet','leaflet.providers')
new_packages <- packages[!packages%in%install.packages()[,'Package']]

# installs packages which are not already installed
if(length(new_packages)){install.packages(new_packages)}

# Loads installed packages
sapply(packages, require, character.only=TRUE)

# set your working directory
setwd('C:/Users/HP/Desktop/Classification')

# Load Area of interest
aoi <- readOGR('C:/Users/HP/Desktop/Classification/aoi/aberdares.shp')

# assign a projection
proj4string(aoi) <- CRS('+proj=longlat +datum=WGS84')
aoi

# view aoi
plot(aoi)
df <- fortify(aoi)
ggplot()+geom_polygon(data = df, aes(x= long, y= lat, group = group))

# Load your satellite image or images
sent2 <- "/satellite_images_datasets/sentinel-2"
sent2 <- list.files(recursive = TRUE, full.names = TRUE, pattern = 'B0[2348]_10m.jp2$')
head(sent2)
sent2 <- lapply(1:length(sent2), function(x){raster(sent2[x])})
head(sent2)

# set Layout parameters
options(repr.plot.width = 41, repr.plot.height =20)
lyt <- rbind(c(1,2))
layout(lyt)

# stack the images
sent2_stack <- stack(sent2)
sent2_stack
plot(sent2_stack)
plotRGB(sent2_stack,r=3,g=2,b=1,scale=maxValue(sent2[[6]]), stretch='hist')
