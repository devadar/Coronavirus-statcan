#les Graphs

source("SetupData.R")


locationInteressante <- c("Canada","Quebec","CanadaSansQuebec","United States",  "Sweden", "Germany", "Italy")

dataOffSetDeathsPM <- 10

dataGrapher <- dataGraph[total_deaths_per_million >= dataOffSetDeathsPM &
                           location %in% locationInteressante 
                           , joursEcoules := (1:.N), by = location]
dataGrapher <- dataGrapher[joursEcoules > 0]

dernièreEntreeQueb <- max(dataGrapher$joursEcoules)

graph <- ggplot(droplevels(dataGrapher[joursEcoules < dernièreEntreeQueb]),
                aes(joursEcoules,
                    total_deaths_per_million,
                    colour = location))

graph + geom_point() + geom_line()+
  ggtitle("COVID19","Nb de morts par million selon le nb de jours") +
  ylab("Nb de morts par million d'habitants") + 
  xlab("Nb de jours depuis 10 morts/1M d'habitants") +
  xlim(1,dernièreEntreeQueb)

  
