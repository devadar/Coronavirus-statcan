library(data.table)
library(dplyr)
library(ggplot2)

coronadata <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  setDT()

coronadata$date <- as.Date(coronadata$date)



dataGraph <- coronadata#[location %in% c("Sweden","Canada","United States","Ireland")]


canadaSeulement <- read.csv("https://sante-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  setDT()

canadaPop <- read.csv("https://www150.statcan.gc.ca/t1/tbl1/en/dtl!downloadDbLoadingData-nonTraduit.action?pid=1710000901&latestN=5&startDate=&endDate=&csvLocale=en&selectedMembers=%5B%5B10%2C6%2C1%2C4%2C7%2C8%2C3%2C12%2C14%2C2%2C5%2C9%2C11%2C15%5D%5D") %>%
  setDT()
canadaPop <- canadaPop[REF_DATE == "2020-01", .(GEO, VALUE)][, .(location = GEO, popTotale = VALUE)]

canadaSeulement <- merge(canadaSeulement,canadaPop,
                         by.x = "prname", by.y = "location")
canadaSeulement[, date := as.Date(date, format= "%d-%m-%y")]
canadaSeulement[, total_deaths_per_million := numdeaths / popTotale *1000000]

rm(canadaPop)

canadaSeulement <- canadaSeulement[, .(location = prname,
                                       date,
                                       numdeaths,
                                       total_deaths_per_million,
                                       popTotale)]
canadaSansQuebec <- canadaSeulement[location %in% c("Canada","Quebec")]

canadaSansQuebec  <- dcast.data.table(canadaSansQuebec, 
                                      formula =  date ~location,
                                      fun.aggregate = ,
                                      value.var = "numdeaths", sep = "-") %>%
  droplevels()
canadaSansQuebec <- canadaSansQuebec[]

canadaSansQuebec[, numdeaths := Canada-Quebec]

canadaSansQuebec <- canadaSansQuebec[numdeaths > 0, .(date,numdeaths,location = "CanadaSansQuebec")]

popCanadaSansQuebec <- canadaSeulement[location == "Canada", popTotale][0:1] - canadaSeulement[location == "Quebec", popTotale][0:1]

canadaSansQuebec[, popTotale := popCanadaSansQuebec]

canadaSansQuebec <- canadaSansQuebec[, .(location,
                                         date,
                                         numdeaths,
                                         total_deaths_per_million = numdeaths /popTotale * 1000000,
                                         popTotale)]
#On est prêt pour le bind.

Canada <- rbind(canadaSeulement,canadaSansQuebec)


Canada[,joursEcoules := (1:.N), by = (location)]

dataGraph <- dataGraph[location != "Canada"]

dataGraph <- dataGraph[, .(
  location,
  date,
  numdeaths = total_deaths,
  total_deaths_per_million
 
)]


dataGraph <- rbind(dataGraph, Canada[, .(location, date, numdeaths, total_deaths_per_million)])

rm(list = c("Canada","canadaSansQuebec","canadaSeulement","popCanadaSansQuebec"))

