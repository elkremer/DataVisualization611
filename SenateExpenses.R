#load required R packages (need to be installed first)
require(sp) 
require(ggplot2) 
require(mapproj)
library(ggsn)
library(dplyr)


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

expenses$description <- gsub("STAFF TRANSPORTATION.*","STAFF TRANSPORTATION", expenses$description) %>%
                        gsub("SENATOR TRANSPORTATION.*","SENATOR TRANSPORTATION", .) %>%
                        gsub(".*INTERN.*", "INTERN", .) %>%
                        gsub(".*PER DIEM.*", "PER DIEM", .) %>%
                        gsub(".*INCIDENTALS.*", "INCIDENTALS", .) %>%
                        gsub(".*STAFF ASSISTANT.*", "STAFF ASSISTANT", .) %>%
                        gsub("FROM\\s\\w{3}\\.\\s?\\d+", "", ., ignore.case = TRUE) %>%
                        gsub("TO\\s\\w{3}\\.\\s?\\d+", "", ., ignore.case = TRUE)

senex1 <- aggregate(amount~senator_name, expenses, sum)
senex2 <- aggregate(amount~senator_name, expenses, length)
names(senex2) <- c("senator_name", "number_expenses")
senex1 <- merge(senex1, senex2, all = TRUE)

#Function to get the data and create the choropleth
createSenatorChoroplethMap <- function() {
  #Libraries required for choropleth map
  require(rgdal)
  library(ggsn)
  require(plyr)
  
  #choropleth data
  stateEx <- aggregate(amount~State, expenses, sum)

  #Create a path for the download, download it and unzip it
  fn <- file.path(getwd(), "cb_2017_us_state_5m.shp", fsep = "\\")
  #Don't download the file if we already have it
  if (!file.exists(fn)) {
    download.file("http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_5m.zip", fn)
    utils::unzip(fn, exdir = tempdir())
  }
  
  #Read in the shape file of the US
  geom <- readOGR(dsn = file.path(tempdir(), "cb_2017_us_state_5m.shp"))
#geom <- readOGR("C:/Users/Peter/Desktop/Utica/DataVisualization-611/Module8/cb_2017_us_state_20m/cb_2017_us_state_20m.shp")

  #join attribute data to geometries
  geom@data$id <- rownames(geom@data)   #Create an identifier for each census block (running number)
  geoforti <- fortify(geom)             #Create fortify object 
  geoforti <- join(geoforti, geom@data, by="id")    #Join geometries to fortify object

  #Make it just contiguous US for better visibility
  #Alaska, Hawaii and US territories made for a map too small to be of use
  geoforti <- geoforti[geoforti$long < -50 & geoforti$long > -125,]
  geoforti <- geoforti[geoforti$lat > 25 & geoforti$lat < 50,]

  #Merge the State data data with the shape data
  geoforti <- merge(geoforti, stateEx, by.x="NAME", by.y="State", all.x=T, a..ly=F)

  #Create the map layers
  ggp <- ggplot(data=geoforti, aes(x=long, y=lat, group=group))  #Create ggplot object
  ggp <- ggp + geom_polygon(aes(fill=amount))  #Draw polygons
  ggp <- ggp + geom_path(color="grey")  #Draw boundaries
  ggp <- ggp + coord_equal() 
  ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "darkred",
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar") # specify colors

  ggp <- ggp + labs(title="Senator Spending by State") #Add a title
  #Add scalebar 500 km
  ggp <- ggp + scalebar(data = geoforti, dd2km = TRUE, 
                        dist=500, location="bottomright", st.size=2)

  north2(ggp, x=.10, y=.30,scale = 0.1, symbol = 1) #Add North arrow
}

createExpensesByMonthAnimation <- function() {
  p <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
    geom_point(aes(size = pop, frame = year, ids = country)) +
    scale_x_log10()
  
  p <- ggplotly(p)
}
createSenatorChoroplethMap()
