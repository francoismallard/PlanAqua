
library(ggplot2)

#### Importation des données du scoring 

scoring <- read.table(file= "data/scoring_03052023.txt", header = TRUE, sep="\t")
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

#### ACP pour les lacs, couleur selon les lignes

scoring_mean_lake <- subset(scoring_mean, lake<= 17)#mask 

scoring_mean_lake<- merge(scoring_mean_lake, unique(scoring[, c("lake", "column", "raw")]))

pca_lake<- prcomp(scoring_mean_lake[, 2:6])
plot( pca_lake$x[,1], pca_lake$x[,2], col = rainbow(4)[scoring_mean_lake$raw], pch=16, main = "Analyse en composantes principales" )


scoring_mean_lake$PC1 = pca_lake$x[,1]
scoring_mean_lake$PC2 = pca_lake$x[,2]
scoring_mean_lake$column = as.factor(scoring_mean_lake$column)
scoring_mean_lake$raw = as.factor(scoring_mean_lake$raw)

ggplot(scoring_mean_lake) + aes(x = PC1, y= PC2) + geom_point(color = rainbow(4)[scoring_mean_lake$raw]) + labs(
  title    = "PC 1 en fonction de PC 2 pour chaque lac à partir de l'ACP ",
  subtitle = "Scoring réalisé le 03/05/23 vers 15h",
  x        = "PC 1",
  y        = "PC 2")

#### ACP pour les lacs, couleurs en fonctions des colonnes

ggplot(scoring_mean_lake) + aes(x = PC1, y= PC2, color = raw) + geom_point() + labs(
  title    = "PC 1 en fonction de PC 2 pour chaque lac à partir de l'ACP ",
  subtitle = "Scoring réalisé le 03/05/23 vers 15h",
  x        = "PC 1",
  y        = "PC 2")


ggplot(scoring_mean_lake) + aes(x = PC1, y= PC2, color=column) + geom_point() + labs(
  title    = "PC 1 en fonction de PC 2 pour chaque lac à partir de l'ACP ",
  subtitle = "Scoring réalisé le 03/05/23 vers 15h",
  x        = "PC 1",
  y        = "PC 2")
#### Test de Fisher 

scoring_mean_lake$nut = c(1:16)%in%c(1,3,6,8,9,11,14,16)

scoring_mean_lake$fish <- scoring_mean_lake$fish>0.5
scoring_mean_lake$algae <- scoring_mean_lake$algae>0.5
fisher.test(scoring_mean_lake$algae,scoring_mean_lake$fish)

fisher.test(scoring_mean_lake$wtr_level,scoring_mean_lake$fish)

fisher.test(scoring_mean_lake$wtr_level,scoring_mean_lake$algae)

fisher.test(as.factor(scoring_mean_lake$column),scoring_mean_lake$wtr_level)

fisher.test(as.factor(scoring_mean_lake$raw),scoring_mean_lake$wtr_level)

fisher.test(as.factor(scoring_mean_lake$column),scoring_mean_lake$algae)

fisher.test(as.factor(scoring_mean_lake$raw),scoring_mean_lake$algae)

fisher.test(as.factor(scoring_mean_lake$nut),scoring_mean_lake$algae)

fisher.test(as.factor(scoring_mean_lake$nut),scoring_mean_lake$wtr_level)

fisher.test(as.factor(scoring_mean_lake$nut),scoring_mean_lake$frog)

fisher.test(as.factor(scoring_mean_lake$raw),scoring_mean_lake$other_biol_waste)

