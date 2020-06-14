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
  total_deaths_per_million,
  population
 
)]


dataGraph <- rbind(dataGraph, Canada[, .(location, date, numdeaths, total_deaths_per_million,population = popTotale)])

dataGraph[, nouveauxMorts := numdeaths - shift(numdeaths, 1L, type = "lag")]
dataGraph[, nouveauxMortsRelatif := nouveauxMorts / population]
rm(list = c("Canada","canadaSansQuebec","canadaSeulement","popCanadaSansQuebec"))



#########
# STATCAN: données immature actuellement. On s'en servira quand ça sera prêt.


# 
# download.file(url="https://www150.statcan.gc.ca/n1/en/tbl/csv/13100781-eng.zip?st=wFXx6L5K",
#               destfile='13100781-eng.zip', method='libcurl')
# statCanRaw <- read.csv(unzip("13100781-eng.zip","13100781.csv")) %>%
#   setDT()
# 
# 
# 
# statCanCleaner <- dcast.data.table(statCanRaw,
#                                    formula = Case.identifier.number ~Case.information ,
#                                    value.var = "VALUE")
# rm(statCanRaw)
# #Ok, on ramène ça en données intéressantes.
# #les âges.
# statCanCleaner[`Age group` == 1, groupeAge := "0-19"]
# statCanCleaner[`Age group` == 2, groupeAge := "20-29"]
# statCanCleaner[`Age group` == 3, groupeAge := "30-39"]
# statCanCleaner[`Age group` == 4, groupeAge := "40-49"]
# statCanCleaner[`Age group` == 5, groupeAge := "50-59"]
# statCanCleaner[`Age group` == 6, groupeAge := "60-69"]
# statCanCleaner[`Age group` == 7, groupeAge := "70-79"]
# statCanCleaner[`Age group` == 8, groupeAge := "80 et +"]
# statCanCleaner[`Age group` == 99, groupeAge := "Indéterminé"]
# statCanCleaner[,groupeAge := as.factor(groupeAge)]
# #genre
# statCanCleaner[ Gender == 1, genre := "Homme"]
# statCanCleaner[ Gender == 2, genre := "Femme"]
# statCanCleaner[ Gender == 3, genre := "Non-binaire"]
# statCanCleaner[ Gender == 9, genre := "Indéterminé"]
# statCanCleaner[, genre := as.factor(genre)]
# 
# #Mort ou pas
# statCanCleaner[`Death` == 1, `mort` := "Mort"]
# statCanCleaner[`Death` == 2, `mort` := "Vivant"]
# #statCanCleaner[`Death` == 9, `mort` := "Indéterminé"] #Peut-on vraiment être indéterminé?
# statCanCleaner[`Death` == 9, `mort` := "Vivant"]
# statCanCleaner[, mort := as.factor(mort)]
# 
# #Transmission
# statCanCleaner[`Transmission` == 1, transmission := "Voyage"]
# statCanCleaner[`Transmission` == 2, transmission := "Communautaire"]
# statCanCleaner[`Transmission` == 3, transmission := "Indéterminée"]
# statCanCleaner[, transmission := as.factor(transmission)]
# 
# 
# statCan <- statCanCleaner[,
#                           .(idUnique = Case.identifier.number,
#                             groupeAge,
#                             genre,
#                             mort,
#                             #transmission,
#                             semaine.Episode = `Episode week`,
#                             #dateUpdate,
#                             #etatAvant,
#                             #etatMaintenant
#                             #soinsIntensifs
#                             )
#                           ]
# statCan[groupeAge == '20-29', age := 25]
# statCan[groupeAge == "0-19", age := 12]
# statCan[groupeAge == "30-39", age := 35]
# statCan[groupeAge == "40-49", age := 45]
# statCan[groupeAge == "50-59", age := 55]
# statCan[groupeAge == "60-69", age := 65]
# statCan[groupeAge == "70-79", age := 75]
# statCan[groupeAge == "80 et +", age := 85]
# 
# statCan[etatMaintenant == 'Hospitalisé' & soinsIntensifs == FALSE, etatBrute := as.factor('Hospitalisé')]
# statCan[etatMaintenant == 'Hospitalisé' & soinsIntensifs == TRUE, etatBrute := as.factor('Soins Intensifs')]
# statCan[etatMaintenant == 'À la maison', etatBrute := as.factor('À la maison')]
# 
# rm(statCanCleaner)
# 
# 
# 
# 












