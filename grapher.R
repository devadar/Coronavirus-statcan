#les Graphs

source("SetupData.R")


locationInteressante <- c("Canada","Quebec","CanadaSansQuebec","Ontario", "Sweden", "Austria", "Norway", "Danemark", "Germany")

dataOffSetDeathsPM <- 8

dataGrapher <- dataGraph[total_deaths_per_million >= dataOffSetDeathsPM &
                           location %in% locationInteressante 
                           , joursEcoules := (1:.N), by = location]
dataGrapher <- dataGrapher[joursEcoules > 0]

graph <- ggplot(droplevels(dataGrapher),
                aes(joursEcoules,
                    total_deaths_per_million,
                    colour = location))
graph + geom_point() + geom_line()
