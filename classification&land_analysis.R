# Install and Load Packages
packages <- c('sf', 'terra','raster','RStoolbox','rgdal','rasterVis','gridExtra',
              'caret','mapview','plotly','mapedit','RColorBrewer','leaflet','leaflet.providers')
new_packages <- packages[!packages%in%install.packages()[,'Package']]
# installs packages which are not already installed
if(length(new_packages)){install.packages(new_packages)}
# Loads installed packages
sapply(packages, require, character.only=TRUE)

# set seed
set.seed(12345)

# set your working directory
setwd('C:/Users/Chris/Desktop/Classification')

# Load Area of interest
aoi <- readOGR('C:/Users/Chris/Desktop/Classification/aoi/shapefile/Shapefile_1.shp')
# aoi

# view aoi
plot(aoi)

#------------------------------------------------------------------------------------------------------------------------------#
#                                 year = 2019
#------------------------------------------------------------------------------------------------------------------------------#

# Load your satellite image or images
sent2 <- "/new_sent"
sent2_19 <- list.files(recursive = TRUE, full.names = TRUE, pattern = '11_B0[2,3,4,8]_10m.jp2$')
sent2_19 <- lapply(1:length(sent2_19), function(x){raster(sent2_19[x])})
# length(sent2_19)

# stack the images
sent2_19_stack <- stack(sent2_19)
sent2_19_stack
# plot(sent2_stack)

# crop stacked images
sent2_19_croped <- crop(sent2_19_stack, aoi)
plotRGB(sent2_19_croped, r=3, g=2, b=1, stretch='hist')

# Load training data
training_data_19 <- readOGR("C:/Users/Chris/Desktop/Classification/training_data/training_data_19.shp")
# training_data_19$class_id

#  convert the polygons to points and extract values
training_data_19_points <- list()
for(i in unique(training_data_19$class_id)){
  message(paste0("Sampling points from polygons with class_id=", i))

  # sample points for polygons of class_id = i
  training_data_19_sf <- st_as_sf(training_data_19)
  training_data_19_points[[i]] <- st_sample(
    x = training_data_19_sf[training_data_19_sf$class_id == i,],
    size = 100
  )
  training_data_19_points[[i]] <- st_as_sf(training_data_19_points[[i]])
  training_data_19_points[[i]]$class_id <- i
}
training_data_19_points <- do.call(rbind, training_data_19_points)
head(training_data_19_points)

# now that we have points we need to extract features
# we can use our RS data for this since we do not have any ground-truth from site
sent2_19_croped <- normImage(sent2_19_croped)
# plot(sent2_19_croped)
sent2_19_croped <- rescaleImage(sent2_19_croped, ymin = 0, ymax = 1)
# plot(sent2_19_croped)

# extract features and label them with our class_id
extract_features_19 <- extract(sent2_19_croped, training_data_19_points, df = T)
# head(extract_features_19)
extract_features_19 <- extract_features_19[,-1] # no ID column needed
head(extract_features_19)

extract_features_19 <- cbind(
  class_id = training_data_19_points$class_id,
  extract_features_19
)
# head(extract_features_19)

# remove duplicates (in case multiple points fall into the same pixel)
dupl <- duplicated(extract_features_19)
which(dupl)
length(which(dupl)) # number of duplicates in labeled features that we need to remove!
extract_features_19 <- extract_features_19[!dupl,]

# extracted features
x <- extract_features_19[,2:ncol(extract_features_19)] # remove ID column
y <- as.factor(extract_features_19$class_id) #we want caret to treat this as categories, thus factor
levels(y) <- paste0("class_", levels(y))

# Split the training data and build Random forest model
RFmodel_19 <- train(
  x = x,
  y = y,
  trControl = trainControl(
    p = 0.7, # % of training samples, 0.3% for validation
    method  = "cv", #cross validation
    number  = 5, # 5-fold
    verboseIter = TRUE, # progress update per iteration
    classProbs = TRUE # probabilities for each example
  ),
  method = "rf" # used algorithm
)

# Model Accuracy and Performance
RFmodel_19
confusion_matrix <- confusionMatrix(RFmodel_19)
confusion_matrix

# classify
classified_19 <- predict(sent2_19_croped, RFmodel_19, type = 'raw')

# save your classified image
writeRaster(classified_19, filename = "C:/Users/Chris/Desktop/Classification/classified/Bweri_19.tif", overwrite=TRUE)

cols <- c("blue", "darkgreen", "red", "sandybrown","green")

# mapview
mapview(classified_19, col.regions = cols)

# define legend labels
legend_labels <- c('water','forest','urban','bareland','agriculture')

# ggplot
ggplot() + ggR(classified_19, geom_raster = T, ggLayer = T) +
  scale_fill_manual(values = cols, name = "Land Use", labels =legend_labels) +
  labs(title = "Classified Map of Bweri 2019")+coord_sf(crs = st_crs(sent2_19_croped), datum = st_crs(4326)) +
  theme(plot.title = element_text(hjust = 0.5))
# save your plot
ggsave("C:/Users/Chris/Desktop/Classification/classified/classified_map_bweri_2019.png",
       width = 10, height = 10, dpi = 300)

#-----------------------------------------------------------------------------------------------------------------------------#
#                                 year = 2022
#-----------------------------------------------------------------------------------------------------------------------------#

sent2_22 <- list.files(recursive = TRUE, full.names = TRUE, pattern = '21_B0[2,3,4,8]_10m.jp2$')
sent2_22 <- lapply(1:length(sent2_22), function(x){raster(sent2_22[x])})
# length(sent2_22)

# stack the images
sent2_22_stack <- stack(sent2_22)
sent2_22_stack
# plot(sent2_stack)

# crop stacked images
sent2_22_croped <- crop(sent2_22_stack, aoi)
plotRGB(sent2_22_croped, r=3, g=2, b=1, stretch='hist')

# Load training data
training_data_22 <- readOGR("C:/Users/Chris/Desktop/Classification/training_data/training_data_22.shp")
# training_data_19$class_id

#  convert the polygons to points and extract values
training_data_22_points <- list()
for(i in unique(training_data_22$class_id)){
  message(paste0("Sampling points from polygons with class_id=", i))

  # sample points for polygons of class_id = i
  training_data_22_sf <- st_as_sf(training_data_22)
  training_data_22_points[[i]] <- st_sample(
    x = training_data_22_sf[training_data_22_sf$class_id == i,],
    size = 100
  )
  training_data_22_points[[i]] <- st_as_sf(training_data_22_points[[i]])
  training_data_22_points[[i]]$class_id <- i
}
training_data_22_points <- do.call(rbind, training_data_22_points)
head(training_data_22_points)

# now that we have points we need to extract features
# we can use our RS data for this since we do not have any ground-truth from site
sent2_22_croped <- normImage(sent2_22_croped)
# plot(sent2_22_croped)
sent2_22_croped <- rescaleImage(sent2_22_croped, ymin = 0, ymax = 1)
# plot(sent2_22_croped)

# extract features and label them with our class_id
extract_features_22 <- extract(sent2_22_croped, training_data_22_points, df = T)
# head(extract_features_19)
extract_features_22 <- extract_features_22[,-1] # no ID column needed
head(extract_features_22)

extract_features_22 <- cbind(
  class_id = training_data_22_points$class_id,
  extract_features_22
)
# head(extract_features_22)

# remove duplicates (in case multiple points fall into the same pixel)
dupl <- duplicated(extract_features_22)
which(dupl)
length(which(dupl)) # number of duplicates in labeled features that we need to remove!
extract_features_22 <- extract_features_22[!dupl,]

# extracted features
x <- extract_features_22[,2:ncol(extract_features_22)] # remove ID column
y <- as.factor(extract_features_22$class_id) #we want caret to treat this as categories, thus factor
levels(y) <- paste0("class_", levels(y))

# Split the training data and build Random forest model
RFmodel_22 <- train(
  x = x,
  y = y,
  trControl = trainControl(
    p = 0.7, # % of training samples, 0.3% for validation
    method  = "cv", #cross validation
    number  = 5, # 5-fold
    verboseIter = TRUE, # progress update per iteration
    classProbs = TRUE # probabilities for each example
  ),
  method = "rf" # used algorithm
)

# Model Accuracy and Performance
RFmodel_22
confusion_matrix <- confusionMatrix(RFmodel_22)
confusion_matrix

# classify
classified_22 <- predict(sent2_22_croped, RFmodel_22, type = 'raw')

# save your classified image
writeRaster(classified_22, filename = "C:/Users/Chris/Desktop/Classification/classified/Bweri_22.tif", overwrite=TRUE)

cols <- c("blue", "darkgreen", "red", "sandybrown","green")

# mapview
mapview(classified_22, col.regions = cols)

# define legend labels
legend_labels <- c('water','forest','urban','bareland','agriculture')

# ggplot
ggplot() + ggR(classified_22, geom_raster = T, ggLayer = T) +
  scale_fill_manual(values = cols, name = "Land Use", labels =legend_labels) +
  labs(title = "Classified Map of Bweri 2022")+coord_sf(crs = st_crs(sent2_22_croped), datum = st_crs(4326)) +
  theme(plot.title = element_text(hjust = 0.5))
# save your plot
ggsave("C:/Users/Chris/Desktop/Classification/classified/classified_map_bweri_2022.png",
       width = 10, height = 10, dpi = 300)

#------------------------------------------------------------------------------------------------------------------------------#
#                                 Post Classification
#------------------------------------------------------------------------------------------------------------------------------#
classified_19x10 <- classified_19$layer*10
classified_19_22 <- classified_19x10 + classified_22$layer
head(classified_19_22)

color_pal <- colorRampPalette(c('#00007F','blue','darkgreen','red', 'grey','sandybrown','green', 'yellow','#366c36'))

# ggplot
ggplot() + ggR(classified_19_22, geom_raster = T, ggLayer = T) +
  scale_fill_gradientn(colors = color_pal(100)) +
  labs(title = "Post Classified Map of Bweri")+coord_sf(crs = st_crs(sent2_19_croped), datum = st_crs(4326)) +
  theme(plot.title = element_text(hjust = 0.5))

# save your plot
ggsave("C:/Users/Chris/Desktop/Classification/classified/post_classified_map_bweri.png",
       width = 10, height = 10, dpi = 300)


#------------------------------------------------------------------------------------------------------------------------------#
#                              Land Cover Change Classification
#------------------------------------------------------------------------------------------------------------------------------#

Landcoverchange <- classified_22-classified_19

color_pal <- colorRampPalette(c('#00007F','blue','darkgreen','red', 'grey','sandybrown','green', 'yellow','#366c36'))

# ggplot
ggplot() + ggR(Landcoverchange, geom_raster = T, ggLayer = T) +
  scale_fill_gradientn(colors = color_pal(100)) +
  labs(title = "Post Classified Map of Bweri")+coord_sf(crs = st_crs(Landcoverchange), datum = st_crs(4326)) +
  theme(plot.title = element_text(hjust = 0.5))

# save your plot
ggsave("C:/Users/Chris/Desktop/Classification/classified/Landcoverchange.png",
       width = 10, height = 10, dpi = 300)
