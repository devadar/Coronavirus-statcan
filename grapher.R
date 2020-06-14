#les Graphs
  
  source("SetupData.R")
  library("viridis")
  
  locationInteressante <- c("Canada","Quebec","CanadaSansQuebec","Netherlands",  "Sweden", "Italy", "United Kingdom", "United States","Portugal", "Israel")
  #locationInteressante <- c("Quebec","Sweden","Italy")
  
  dataOffSetDeathsPM <- 10
  
 # dateGraph = "2020-05-10"
  
  dataGrapher <- dataGraph[total_deaths_per_million >= dataOffSetDeathsPM &
                             location %in% locationInteressante 
                             , joursEcoules := (1:.N), by = location]
    dataGrapher <- dataGrapher[joursEcoules > 0]
  
  dernièreEntreeQueb <- max(dataGrapher[location == "Quebec"
                                        , joursEcoules]) +15
  #dataGrapher[location == "Quebec", joursEcoules := as.integer(joursEcoules -2) ]
  
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

graphNouveauxMorts <- ggplot(droplevels(dataGrapher[joursEcoules < dernièreEntreeQueb]),
                             aes(joursEcoules,
                                 nouveauxMortsRelatif, 
                                 colour = location)
                             )
graphNouveauxMorts + geom_smooth(se = F)# + stat_smooth()# +scale_y_continuous( trans = "log")
  
  
#############
  # STATCAN: Données immatures. L'analys devra attendre.
  
  
  
  
# graphIst <- ggplot(data = statCan[mort == "Mort" & semaine.Episode != 99], aes(semaine.Episode))
# 
# graphIst + geom_histogram() + xlim(9,18)
# 
#   
# 
# 
# 
# 
# graph2 <- ggplot(data = statCan[dateEpisode <= dateGraph & mort == "Mort" & !is.na(etatAvant)],
#                  aes(groupeAge, fill = etatAvant )
# )
# graph2 + geom_bar( )  +
#   ggtitle("Nb de mort de la Covid-19 au Canada",
#           "État d'hospitalisation avant l'évènement")
# 
# graph3 <- ggplot(data = statCan[dateEpisode <= dateGraph],
#                  aes(groupeAge, fill = mort))
# graph3 + geom_bar() +
#   ggtitle("Suivi des cas confirmés de COVID19 par groupes d'âge")
# 
# graph4 <- ggplot(data = statCan[dateEpisode <= dateGraph & !is.na(etatBrute)],
#                  aes(groupeAge, fill = etatBrute))
# graph4 + geom_bar() +
#   ggtitle("État d'hospitalisation des cas confirmés au Canada")
# 
# 
# #graphOut <- ggplot(donneesCours, aes(MGS, ..count.., fill = reussite))
# #graphOut + geom_density(position = "fill") + 
#  # ggtitle("Densité conditionnelle pour la réussite selon la MGS") +
# #  ylab("Densité relative") 
# 
# 
# 
# 
# graphSoinsNécessaires <- ggplot(statCan[dateEpisode <= dateGraph & !is.na(etatBrute)],
#                                 aes(age, ..count.., fill = etatBrute))
# 
# graphSoinsNécessaires + geom_density(position = "fill")
# 
# cdplot(statCan[age > 0 & dateEpisode <= "2020-07-21" & !is.na(etatBrute)]$age,
#        
#        statCan[age > 0 & dateEpisode <= "2020-07-21" & !is.na(etatBrute)]$etatBrute)
# 
# statCanner <- droplevels(statCan[age > 0 & mort %in% c("Mort","Vivant")])
# cdplot(statCanner$age,statCanner$mort)
# 
# statCanner <- droplevels(statCan[age > 0 & `etatAvant` == 'À la maison' & `etatMaintenant` != 'Indéterminé'])
# cdplot(statCanner$age,statCanner$etatMaintenant)
# 
# statCanner <- droplevels(statCan[age > 0 & !is.na(soinsIntensifs)])
# statCanner$soinsIntensifs <- as.factor(statCanner$soinsIntensifs)
# cdplot(statCanner$age, statCanner$soinsIntensifs)
# 
# cdplot(statCanner$age,statCanner$etatMaintenant) 

