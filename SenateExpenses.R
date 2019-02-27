#load required R packages (need to be installed first)
require(sp) 
require(ggplot2) 
require(mapproj)
library(ggsn)
library(dplyr)

###############################################################################
## Functions
###############################################################################
#Function to prepare and format the expense data
prepareExpenseData <- function(exData) {
  #Filter the data to only include the columns to be used.
  #columns used: senator_name, start_date, end_date, description, amount, salary_flag
  exData <- select(exData, senator_name, start_date, end_date, description, amount, salary_flag)

  #eliminate null values in the senator_name column
  exData <- exData[!exData$senator_name == "",]

  #merge the states and states abbreviations in 
  exData <- merge(exData, states, all = TRUE)
  exData$amount <- as.numeric(gsub(",","",exData$amount))

  #Change factor to date for date related fields
  exData$start_date <- as.Date(exData$start_date, "%m/%d/%y")
  exData$end_date <- as.Date(exData$end_date, "%m/%d/%y")

  exData$description <- gsub("STAFF TRANSPORTATION.*","STAFF TRANSPORTATION", exData$description) %>%
                        gsub("SENATOR TRANSPORTATION.*","SENATOR TRANSPORTATION", .) %>%
                        gsub(".*INTERN.*", "INTERN", .) %>%
                        gsub(".*PER DIEM.*", "PER DIEM", .) %>%
                        gsub(".*INCIDENTALS.*", "INCIDENTALS", .) %>%
                        gsub(".*STAFF ASSISTANT.*", "STAFF ASSISTANT", .) %>%
                        gsub("FROM\\s\\w{3}\\.\\s?\\d+", "", ., ignore.case = TRUE) %>%
                        gsub("TO\\s\\w{3}\\.\\s?\\d+", "", ., ignore.case = TRUE)
  exData
}

createSenatorChoroplethMap <- function(expenses) {
  #Libraries required for choropleth map
  require(rgdal)
  library(ggsn)
  require(plyr)
  
  us <- map_data("state")
  exp <- aggregate(amount~State, expenses, sum)
  exp$region <- tolower(exp$State)
  arr <- select(exp, region, amount)
  
  ggp <- ggplot() +geom_map(data=us, map=us,
                          aes(x=long, y=lat, map_id=region),
                          fill="#ffffff", color="#ffffff", size=0.15)
  
  ggp <- ggp + geom_map(data=arr, map=us,
                      aes(fill=amount, map_id=region),
                      color="#ffffff", size=0.15)

  ggp <- ggp + geom_path(color="grey")  #Draw boundaries
  ggp <- ggp + coord_equal() 
  ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "darkred",
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar") # specify colors
  
  ggp <- ggp + labs(title="Senator Spending by State") #Add a title
  #Add scalebar 500 km
  ggp <- ggp + scalebar(data = us, dd2km = TRUE, 
                        dist=500, location="bottomright", st.size=2)
  ggp <- ggp + theme(axis.ticks = element_blank())
  ggp <- ggp + theme(panel.background = element_blank())
  
  north2(ggp, x=.10, y=.30,scale = 0.1, symbol = 1) #Add North arrow
}


createExpensesByMonthAnimation <- function(expenses) {
  p <- ggplot(expenses, aes(start_date, amount, color = Party)) +
    geom_point(aes(size = amount, frame = start_date, ids = State))
  
  p <- ggplotly(p)
  
  Sys.setenv("plotly_username"="UserNameHere")
  Sys.setenv("plotly_api_key"="ApiKeyHere")
  
  chart_link = api_create(p, filename="expenses over time")
  chart_link
}

###############################################################################
## Program begins
###############################################################################
#Create a path for the downloads
f1 <- file.path(getwd(), "114_sdoc7_senate_data.csv", fsep = "\\")
f2 <- file.path(getwd(), "2015senateDataStates.csv", fsep = "\\")

#Don't download the file if we already have it
if (!file.exists(f1)) {
  download.file("https://raw.githubusercontent.com/elkremer/DataVisualization611/master/114_sdoc7_senate_data.csv", f1)
}
#expenses contains the data of expenditures for the 114th senate.
expenses <- read.csv(file="114_sdoc7_senate_data.csv", header=TRUE, sep=",")

#Don't download the file if we already have it
if (!file.exists(f2)) {
  download.file("https://raw.githubusercontent.com/elkremer/DataVisualization611/master/2015senateDataStates.csv", f2)
}
#states is a mapping of senators to the states they serve
states <- read.csv(file="2015senateDataStates.csv", header=TRUE, sep=",")

expenses <- prepareExpenseData(exData = expenses)

senex1 <- aggregate(amount~senator_name, expenses, sum)
senex2 <- aggregate(amount~senator_name, expenses, length)
names(senex2) <- c("senator_name", "number_expenses")
senex1 <- merge(senex1, senex2, all = TRUE)

#Scatterplot R version
ggplot(data = senex1) + 
    geom_point(mapping = aes(x = amount, y = number_expenses, color = Party))

#Choropleth map of US
createSenatorChoroplethMap(expenses)

#Currently fails because data is too large, must remove superfluous data and try again
createExpensesByMonthAnimation(expenses)

#Write out filtered csv files for use by Tableau
write.csv(expenses, "filteredExpenses.csv", row.names = FALSE)
write.csv(senex1, "senex1.csv", row.names = FALSE)
