library(tidyverse)
library(magrittr)
library(ggplot2)
library(patchwork)
library(sandwich)
library(lmtest)

# Trips data source
# https://data.bts.gov/Research-and-Statistics/Trips-by-Distance/w96p-f2qv/data

# Define column classes based on the column description from the source.
column_classes <- c("character", "Date", "character", "character", 
                    "character", "character", "integer", "integer", "integer",
                    "integer", "integer", "integer", "integer",
                    "integer", "integer", "integer", "integer",
                    "integer", "integer", "character")

# Read the input file with the trips information in the sates of California, 
# Oregon and Washington.
alltrips <- read.csv('Trips_by_Distance.csv', header = TRUE, colClasses= column_classes,
                     stringsAsFactors = FALSE)

# Description of alltrips
str(alltrips)

# Unique values in Level
unique(alltrips$Level)

# Subset the data set to only county rows as we are interested at the county level
# and by the required date range
alltripscounty <- 
  alltrips %>%
  filter(Level == "County" & Date >= as.Date("2021-05-14") & Date <= as.Date("2021-05-21"))

# summary of all trips by county
summary(alltripscounty)


# a function which takes in a column as input and provides a vector of positions with NA
# napositions <- function(df, column) {
#   navalues <- which(is.na(df$column))
#   return(navalues, df$column)
#   #return(df$column)
# }

# Check all columns and list vector of NA positions
for (i in 1:ncol(alltripscounty)){
  print(colnames(alltripscounty)[i])
  print(which(is.na(alltripscounty[,i])))
}

# NA positions are same in the different columns
# Drop rows with NA value based on one of the columns
alltripscounty <- alltripscounty %>%
  drop_na(Number.of.Trips.50.100)

# We can see there are no more NA values
summary(alltripscounty)

# Checking the sample size of number of counties
length(unique(alltripscounty$County.FIPS))

# Creating a new column to sum the total long distance trips
# All trips greater than 50 miles are considered long distance trips
alltripscounty$Number.of.Long.Trips <- 
  alltripscounty$Number.of.Trips.50.100 + alltripscounty$Number.of.Trips.100.250 + 
  alltripscounty$Number.of.Trips.250.500 + alltripscounty$Number.of.Trips...500

# Check all column names
for (i in colnames(alltripscounty)){
  print(i)
}

# Creating a dataframe with only the required columns
alltripscountyfinal <- alltripscounty %>% 
  select(County.FIPS, 
         County.Name,
         Date,
         Number.of.Long.Trips
         )

# Taking 8-day average of the long trips by each county
# Dataset for merginf with Covid Vaccination percentage
County.Trip.Covid <- 
  aggregate(
  Number.of.Long.Trips ~ County.FIPS + County.Name, data = alltripscountyfinal , 
  FUN=mean)

head(County.Trip.Covid)

# Load the Covid vaccination percentage file for CA, OR & WA
covvac <- read.csv('COVID-19_Vaccinations_CA_OR_WA.csv', header = TRUE,
                     stringsAsFactors = FALSE)

# Creating a dataframe with only the required columns
covvac <- covvac %>%
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct,
         Series_Complete_Yes, Completeness_pct)

# Format the date column
covvac <- covvac %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%Y"))

# Rename FIPS column to County.FIPS
covvac <- covvac %>%
  rename(County.FIPS = FIPS)

str(covvac)

# Filter only the vaccination data on date 5/1/2021
covvac <- covvac %>%
  filter(Date == as.Date("2021-05-01"))

# Inner Join Trip dataframe and Covid vaccination dataframe by County.FIPS
County.Trip.Covid <- dplyr::inner_join(County.Trip.Covid, covvac, by = "County.FIPS")

head(County.Trip.Covid)

summary(County.Trip.Covid)

# Since 0% vaccinated data in a county will most likely mean no data available
# We exclude those rows
County.Trip.Covid <- County.Trip.Covid %>%
  filter(Series_Complete_Pop_Pct != 0.00)


# Calculating the county population using percent vaccinated and
# total number of vaccinated people
County.Trip.Covid$County.POP = 
  County.Trip.Covid$Series_Complete_Yes/County.Trip.Covid$Series_Complete_Pop_Pct

summary(County.Trip.Covid)