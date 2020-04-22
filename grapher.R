#les Graphs

source("SetupData.R")
library("viridis")

locationInteressante <- c("Canada","Quebec","CanadaSansQuebec","Switzerland",  "Sweden", "Germany", "Italy")

dataOffSetDeathsPM <- 10

dataGrapher <- dataGraph[total_deaths_per_million >= dataOffSetDeathsPM &
                           location %in% locationInteressante 
                           , joursEcoules := (1:.N), by = location]
dataGrapher <- dataGrapher[joursEcoules > 0]

dernièreEntreeQueb <- max(dataGrapher[#location == "Quebec"
                                      , joursEcoules]) 

graph <- ggplot(droplevels(dataGrapher[joursEcoules < dernièreEntreeQueb]),
                aes(joursEcoules,
                    total_deaths_per_million,
                    colour = location))
graph + geom_point() + geom_line()+
  ggtitle("COVID19","Nb de morts par million selon le nb de jours") +
  ylab("Nb de morts par million d'habitants") + 
  xlab("Nb de jours depuis 10 morts/1M d'habitants") +
  xlim(1,dernièreEntreeQueb) +
  scale_color_viridis(discrete = TRUE)


graph2 <- ggplot(data = statCan[dateUpdate < "2020-04-21" & mort == "Mort"],
                 aes(groupeAge)
)
graph2 + geom_bar() +facet_wrap("etatAvant") +
  ggtitle("Nb de mort de la Covid-19 au Canada",
          "État d'hospitalisation avant l'évènement")




statCanner <- droplevels(statCan[age > 0 & mort %in% c("Mort","Vivant")])
cdplot(statCanner$age,statCanner$mort)
