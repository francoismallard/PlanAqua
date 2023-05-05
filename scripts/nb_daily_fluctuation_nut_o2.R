
library(ggplot2)

#### Importation des données du scoring et les fluctuations journalières 

daily_std <- read.table(file = "output_files/daily_fluctuations_WTR.txt", header = TRUE, sep= "\t")
scoring <- read.table(file = "output_files/scoring_03052023.txt", header = TRUE, sep="\t")
scoring <- subset(scoring, obs!="MR")

#### Moyenne pour chaque paramètres et tableau avec les moyennes 

temp <- tapply(scoring$fish, scoring$lake, mean, na.rm=TRUE)
scoring_mean <- data.frame(lake= names(temp), fish=as.numeric(temp))

temp <- tapply(scoring$frog, scoring$lake, mean, na.rm=TRUE)
scoring_mean$frog = as.numeric(temp)

temp <- tapply(scoring$algae, scoring$lake, mean, na.rm= TRUE)
scoring_mean$algae = as.numeric(temp)

temp <- tapply(scoring$other_biol_waste, scoring$lake, mean, na.rm=TRUE)
scoring_mean$other_biol_waste = as.numeric(temp)

temp <- tapply(scoring$wtr_level, scoring$lake, mean, na.rm= TRUE)
scoring_mean$wtr_level = as.numeric(temp)

scoring_mean$lake <- as.numeric(scoring_mean$lake)



#### Ajout de la colonne water level au daily_std

daily_std$wtr <- scoring_mean_lake$wtr_level
daily_std$algae <- scoring_mean_lake$algae


#### Modèle linéaire 

modele_lineaire  <- lm(daily_sd ~  algae + nut + wtr ,data=subset(daily_std,is_surface))
summary(modele_lineaire)
modele_lineaire_interactions <- lm(daily_std$daily_sd ~ daily_std$wtr*daily_std$algae)

modele_lineaire_fond  <- lm(daily_sd ~  algae + nut,data=subset(daily_std,!is_surface))
summary(modele_lineaire_fond)
