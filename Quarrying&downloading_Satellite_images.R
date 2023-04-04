# First install github
library(devtools)
install.packages('htmltools')
devtools::install_github('16EAGLE/getSpatialData')
library(getSpatialData)
set_archive('C:/Users/HP/Desktop/Classification/satellite_images')

# Load Area of interest
aoi <- readOGR('C:/Users/HP/Desktop/Classification/aoi/aberdares.shp')

# assign a projection
proj4string(aoi) <- CRS('+proj=longlat +datum=WGS84')
aoi

set_aoi(aoi) #if you have an aoi you can place it in the bracket and load

view_aoi()

# Time duration of your dataset
time_range <- c('2022-01-01', '2022-12-30')

# login
login_CopHub(username = 'christobaltobbin')

get_products() #this is to see the data you can query
# query
records <- get_records(time_range = time_range,
                       products = 'sentinel-2')
View(records) #this displays your queried data

# filter by level
records <- records[records$level == 'Level-2A',]

view_records(records)

# filter by cloud cover
records <- records[records$cloudcov <= 10,]
# nrow(records) $ class(record$cloud_cover)

records <- get_previews(records)
view_previews(records)

# filter by area of the tile
# check area of actual data per record
records$area <- sf::st_area(records)
# class(records$area)
records$area <- units::set_units(records$area, 'km^2') #converts from m^2 to km^2
records <- records[as.numeric(records$area)> 1000,] #to grep all tiles with area above 1000km^2

#check whether records are ready for download
records <-check_availability(records)

#order missing data
records <- order_data(records)#,wait_to_complete = T, allows the code to wait still approval then it would continue to run

# download data
records <- get_data(records)
