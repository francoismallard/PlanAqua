###Etude de la meteo sur le mois de mars et avril 2023

library(ggplot2)
meteo0305 <- read.table("data/data_MTO_SYNOP_ORLY_dailly_202303to202305.txt",sep="\t",h=TRUE)

meteo0305$day2=as.character(meteo0305$day)
meteo0305$time=as.POSIXct(meteo0305$day2,tz="GMT",format("%Y%m%d"))

library(cowplot)

#graphes des precipitations et temperature sur mars et avril 2023

pluie <- ggplot(data=meteo0305, aes(x=time,y=rain))+geom_line()+ggtitle("Données des précipitations au cours des mois de mars et avril")
temperature <- ggplot(data=meteo0305, aes(x=time,y=temp))+geom_line()+ggtitle("Données de température au cours des mois de mars et d'avril")

plot_grid(pluie, temperature, ncol=1,nrow=2)

