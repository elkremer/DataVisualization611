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
expenses <- read.csv(file="C:/Users/santo/github/DataVisualization611/DataVisualization611/114_sdoc7_senate_data.csv",
                     header=TRUE, sep=",")
#states is a mapping of senators to the states they serve
states3 <- read.csv(file="C:/Users/santo/github/DataVisualization611/DataVisualization611/2015senateDataStates.csv",
                    header=TRUE, sep=",")

#states2 is a mapping of the GEOIDs to states
states2 <- read.csv(file="C:/Users/santo/github/DataVisualization611/DAtaVisualization611/statelatlon.csv",
                    header=TRUE, sep=",", colClasses = c("character", "character", "character", "character", "character"))

states <- merge(states3, states2, by="State")

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

us <- map_data("state")

exp <- aggregate(amount~State, expenses, sum)

exp$region <- tolower(exp$State)

arr <- select(exp, region, amount)

gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=arr, map=us,
                    aes(fill=amount, map_id=region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='thistle2', high='darkred', 
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg