#load required R packages (need to be installed first)
require(sp) 
require(rgdal) 
require(ggplot2) 
require(mapproj)
library(ggsn)
library(dplyr)
require(plyr)

#Read in the data files
#expenses contains the data of expenditures for the 114th senate.
expenses <- read.csv(file="C:/Users/Peter/Desktop/Utica/DataVisualization-611/Module8/114_sdoc7_senate_data.csv",
                     header=TRUE, sep=",")
#states is a mapping of senators to the states they serve
states <- read.csv(file="C:/Users/Peter/Desktop/Utica/DataVisualization-611/Module8/2015senateDataStates.csv",
                   header=TRUE, sep=",")

#Filter the data to only include the columns to be used.
#columns used: senator_name, start_date, end_date, description, amount, salary_flag
expenses <- select(expenses, senator_name, start_date, end_date, description, amount, salary_flag)

#eliminate null values in the senator_name column
expenses <- expenses[!expenses$senator_name == "",]

#merge the states and states abbreviations in 
expenses <- merge(expenses, states, all = TRUE)
expenses$amount <- as.numeric(gsub(",","",expenses$amount))

#Change factor to date for date related fields
expenses$start_date <- as.Date(expenses$start_date, "%m/%d/%y")
expenses$end_date <- as.Date(expenses$end_date, "%m/%d/%y")

#choropleth data
cpdata <- select(expenses, State, amount)

senex1 <- aggregate(amount~senator_name, expenses, sum)
senex2 <- aggregate(amount~senator_name, expenses, length)
names(senex2) <- c("senator_name", "number_expenses")
senex1 <- merge(senex1, senex2, all = TRUE)

stateEx <- aggregate(amount~State, expenses, sum)

fn <- file.path(tempdir(), "cb_2017_us_state_5m.zip", fsep = "\\")
download.file("http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_5m.zip", fn)
utils::unzip(fn, exdir = tempdir())
geom <- readOGR(dsn = file.path(tempdir(), "cb_2017_us_state_5m.shp"))
#geom <- readOGR("C:/Users/Peter/Desktop/Utica/DataVisualization-611/Module8/cb_2017_us_state_20m/cb_2017_us_state_20m.shp")

#join attribute data to geometries
geom@data$id <- rownames(geom@data)   #create an identifier for each census block (running number)
geoforti <- fortify(geom)             #create fortify object 
geoforti <- join(geoforti, geom@data, by="id")    #join geometries to fortify object

geoforti <- geoforti[geoforti$long < -50,]
geoforti <- merge(geoforti, stateEx, by.x="NAME", by.y="State", all.x=T, a..ly=F)

# create the map layers
ggp <- ggplot(data=geoforti, aes(x=long, y=lat, group=group))   #create ggplot object
ggp <- ggp + geom_polygon(aes(fill=amount))  # draw polygons
ggp <- ggp + geom_path(color="grey", linestyle=2)  # draw boundaries
ggp <- ggp + coord_equal() 
ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "red",
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar") # specify colors
ggp
