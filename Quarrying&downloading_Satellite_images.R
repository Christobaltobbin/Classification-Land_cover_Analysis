# Install Packages
library(devtools)
install.packages('htmltools')
devtools::install_github('16EAGLE/getSpatialData')
library(getSpatialData)
set_archive('C:/Users/HP/Desktop/Lectures/Introduction_to_programming/Rstatistics/FridayPractice/gSD')

set_aoi() # load area of interest if you have any else;

view_aoi() #select your area of interest in the viewer

# Time duration of your dataset
time_range <- c('2022-07-01', '2022-07-30')

# login
login_CopHub(username = 'christobaltobbin') # type in your password to login

get_products() # displays the data you can query

# querying images
records <- get_records(time_range = time_range,
                       products = 'sentinel-2')
View(records) # displays queried images

# filter by level
records <- records[records$level == 'Level-2A',]

view_records(records)

# filter by cloud cover
records <- records[records$cloudcov <= 15,]
# nrow(records) $ class(record$cloud_cover)

records <- get_previews(records)
view_previews(records)

# filter by area of the tile
# check area of actual data per record
records$area <- sf::st_area(records)
# class(records$area)
records$area <- units::set_units(records$area, 'km^2') #converts from m^2 to km^2
records <- records[as.numeric(records$area) > 1000] #to grep all tiles with area above 1000km^2

#check whether records are ready for download
records <-check_availability(records)

#order missing data
records <- order_data(records)#,wait_to_complete = T, allows the code to wait still approval then it would continue to run

# download data
records <- get_data(records)
