# initialize dataset
covid <- read.csv("Abbreviations.csv")

# import data from COVID tracking project
# https://covidtracking.com/data/api
ds1 <- read.csv("states.csv")
# subset to February
ds1$date <- as.Date(ds1$date, "%m/%d/%Y")
ds1.1 <- subset(ds1, date == "2021-02-28")
ds1.2 <- subset(ds1, date == "2021-02-09") # for Wyoming test data
ds1 <- subset(ds1, date == "2021-01-31")
# find increase in variables over month of February
ds1.3 <- ds1
ds1.3$death <- ds1.1$death - ds1$death
ds1.3$positive <- ds1.1$positive - ds1$positive
ds1.3$totalTestResults <- ds1.1$totalTestResults - ds1$totalTestResults
# find lower bound for increase in test results in Wyoming
ds1.3$totalTestResults[56] <- ds1.1$totalTestResults[56] - ds1.2$totalTestResults[56]
# reduce dataset to variables of interest
ds1 <- ds1.3[c("state","death","positive","totalTestResults")]
# rename columns
colnames(ds1) <- c("Abbreviation","Deaths","Cases","Tests")

# import mask wearing data (self-report)
# https://delphi.cmu.edu/covidcast/export/
ds2 <- read.csv("masks.csv")
# subset to twelve middle days of February
ds2 <- subset(ds2, time_value < "2021-02-21" & time_value > "2021-02-08")
# average by state
ds2 <- aggregate(ds2$value, by=list(ds2$geo_value), FUN=mean)
# rename columns
colnames(ds2) <- c("Abbreviation","Masks")
# capitalize abbreviations
ds2$Abbreviation <- toupper(ds2$Abbreviation)

# import mobility data
# https://www.google.com/covid19/mobility/
ds3 <- read.csv("mobility.csv")
# subset dataset 3 to include only states
ds3 <- subset(ds3, sub_region_2 == "")
ds3 <- subset(ds3, sub_region_1 != "")
# subset to February
ds3$date <- as.Date(ds3$date, "%m/%d/%Y")
ds3 <- subset(ds3, date < "2021-03-01" & date > "2021-01-31")
# average across transit
ds3 <- aggregate(ds3$transit_stations_percent_change_from_baseline, by=list(ds3$sub_region_1), FUN=mean)
# rename columns
colnames(ds3) <- c("State","Mobility")

# import excess deaths data
# https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm
ds4 <- read.csv("excessdeaths.csv")
# subset to February
colnames(ds4)[1] <- "date"
ds4$date <- as.Date(ds4$date, "%m/%d/%Y")
ds4 <- subset(ds4, date < "2021-03-01" & date > "2021-01-31")
# remove NAs
# North Carolina is removed because data is not available for February
ds4 <- subset(ds4, !(is.na(ds4$Observed.Number)))
# average excess deaths for February
ds4 <- aggregate(ds4$Observed.Number, by=list(ds4$State), FUN=mean)
# add North Carolina back to the dataset with value of NA
ds4 <- rbind(ds4, c("North Carolina", NA))
ds4$x <- as.numeric(ds4$x)
# rename columns
colnames(ds4) <- c("State","ExcessDeaths")

# import vaccinations data
# https://ourworldindata.org/us-states-vaccinations
ds5 <- read.csv("vaccinations.csv")
# subset to February 14
ds5$Day <- as.Date(ds5$Day, "%m/%d/%Y")
ds5.1 <- subset(ds5, Day == "2021-02-28")
ds5 <- subset(ds5, Day == "2021-01-31")
ds5$people_fully_vaccinated <- ds5.1$people_fully_vaccinated - ds5$people_fully_vaccinated
# subset to variables of interest
ds5 <- ds5[c("Entity","people_fully_vaccinated")]
# rename columns
colnames(ds5) <- c("State","Vaccinations")
# rename New York State
ds5[ds5=="New York State"] <- "New York"

# import GDP data
# https://www.bea.gov/data/gdp/gdp-state
ds6 <- read.csv("gdp.csv", strip.white = TRUE)
# take most recent GDP data
ds6 <- ds6[c(1,9)]
# subset to states
ds6 <- subset(ds6, X %in% covid$State)
# change data type to numeric
ds6$X.7 <- as.numeric(gsub(",","",ds6$X.7))
# rename columns
colnames(ds6) <- c("State","GDP")

# import population data
# https://worldpopulationreview.com/state-rankings/state-densities
ds7 <- read.csv("population.csv")
# reduce dataset to variables of interest
ds7 <- ds7[1:3]
# rename columns
colnames(ds7) <- c("State","PopDensity","Population")

# put together dataset
covid <- read.csv("Abbreviations.csv")
covid <- merge(covid, ds1, by="Abbreviation")
covid <- merge(covid, ds7, by="State")
covid <- merge(covid, ds6, by="State")
covid <- merge(covid, ds4, by="State")
covid <- merge(covid, ds2, by="Abbreviation")
covid <- merge(covid, ds3, by="State")
covid <- merge(covid, ds5, by="State")

# change totals to rates
covid$Deaths <- covid$Deaths/covid$Population*1000000
covid$Cases <- covid$Deaths/covid$Population*1000000
covid$Tests <- covid$Tests/covid$Population*1000000
covid$GDP <- covid$GDP/covid$Population*1000000
covid$ExcessDeaths <- covid$ExcessDeaths/covid$Population*1000000
covid$Vaccinations <- covid$Vaccinations/covid$Population*1000000

# rearrange columns
covid <- covid[c(1:4,9:12,5,8,6,7)]

# write CSV file
write.csv(covid, "covid.csv")
