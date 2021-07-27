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
#getwd()
# setwd("/home/jovyan/W203/Lab2/V1.1/lab-2-section8-team5-lab2-avv/src/data")

# on anand's pc:
setwd("/home/rstudio/w203/lab-2-section8-team5-lab2-avv/src/data")

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
# Dataset for merging with Covid Vaccination percentage
County.Trip.Covid <- 
  aggregate(
    Number.of.Long.Trips ~ County.FIPS + County.Name, data = alltripscountyfinal, 
    FUN=mean)

# Round the mean trips with 0 decimal places
County.Trip.Covid$Number.of.Long.Trips = 
  round(County.Trip.Covid$Number.of.Long.Trips,0)

head(County.Trip.Covid)

# Load the Covid vaccination percentage file for CA, OR & WA
covvac <- read.csv('COVID-19_Vaccinations_CA_OR_WA.csv', header = TRUE,
                   stringsAsFactors = FALSE)

# Creating a dataframe with only the required columns
covvac <- covvac %>%
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct,
         Series_Complete_Yes)

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
  County.Trip.Covid$Series_Complete_Yes*100/County.Trip.Covid$Series_Complete_Pop_Pct

# Round population to 0 decimal places
County.Trip.Covid$County.POP = round(County.Trip.Covid$County.POP,0)

# Check if both county column names are exactly the same for 125 remaining
sum(County.Trip.Covid$County.Name==County.Trip.Covid$Recip_County)==125

# Drop date and one of the county names column after merge
County.Trip.Covid$Date <- NULL
County.Trip.Covid$County.Name <- NULL

summary(County.Trip.Covid)

# Load the Data for County Median Income. First create the datatype for the csv else the FIPS gets loaded as integer rather than character
df_income_datatype <- c("character", "character", "character", 
                    "integer", "numeric", "integer")

df_income = read.csv("Median_Income.csv", header = TRUE, colClasses= df_income_datatype,
                     stringsAsFactors = FALSE)

# Join Median Income with County.Trip.Covid dataframe
df_county_ot_cov1_3 <- dplyr::inner_join(County.Trip.Covid, df_income, by = "County.FIPS")

# Sanity Check Data
# head(df_income)
# str(df_income)
#length(unique(df_income$County.FIPS))

# Load the Data for Party Affiliation
df_PreferredParty_datatype <- c("character", "character", "character", 
                        "integer", "integer", "integer", "numeric", "numeric", "numeric")

df_PreferredParty = read.csv("Party_Inclination_County_v2.csv", header = TRUE, colClasses= df_PreferredParty_datatype,
                     stringsAsFactors = FALSE)

# Rename df_PreferredParty$county_fips column to County.FIPS
df_PreferredParty <- df_PreferredParty %>%
  rename(County.FIPS = county_fips)

str(df_PreferredParty)
# Create a new column for Party Affiliation DF and run the logic to identify the party inclination parameter
df_PreferredParty <- df_PreferredParty %>%
  select (
    state_name, County.FIPS, county_name, votes_gop, votes_dem, total_votes, diff,
    per_gop, per_dem, per_point_diff
  ) %>%
  mutate(
    party_affiliate = case_when(
      votes_gop > votes_dem ~ "1",
      TRUE                  ~ "0"
    )
  )

# Sanity Check Data
head(df_PreferredParty)
str(df_PreferredParty)
length(unique(df_PreferredParty$county_fips))

# Join Party Affiliation with Previous Dataframe
df_county_ot_cov1_2_3 <- dplyr::inner_join(df_county_ot_cov1_3, df_PreferredParty, by = "County.FIPS")

# Validate if any rows got dropped.
length(unique(County.Trip.Covid$County.FIPS))
length(unique(df_county_ot_cov1_3$County.FIPS))
length(unique(df_county_ot_cov1_2_3$County.FIPS))
str(df_county_ot_cov1_2_3)

# Remove all the unwanted column to create the final dataframe
df_aftercleanup <- df_county_ot_cov1_2_3 %>%
  select (County.FIPS, Number.of.Long.Trips, Recip_County, Recip_State.x, Series_Complete_Pop_Pct, Series_Complete_Yes,
          County.POP, County_Median_Income, Income_CountyMedian_vs_StateMedian, Recip_State_Median_Income, party_affiliate)

# rename party_affiliate to isRepublican
df_aftercleanup <- df_aftercleanup %>%
  rename(isRepublican = party_affiliate)

write.csv(df_aftercleanup,'final_data_v0.csv')