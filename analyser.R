source("SetupData.R")

dateGraph <- "2020-04-24"
nbCas <- statCan[dateEpisode <= dateGraph,
                 .(nbCas = .N), 
                 by = groupeAge ][order(groupeAge)]

nbMorts <- statCan[dateEpisode <= dateGraph & mort == "Mort",
                  .(nbMorts = .N),
                  by = groupeAge ][order(groupeAge)]

deces <- merge(nbCas, nbMorts,
               all.x = T, all.y = T)

deces[, tauxMortalité := nbMorts/nbCas*100]
