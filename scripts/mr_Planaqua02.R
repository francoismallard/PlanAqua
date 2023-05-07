#MR
#Script 2023-05-05
#programme analyse données O2 lacs en fonction des traitements
#panel1 = rien, panel2= perches/sans nutriments, panel3 = nutriments/sans perches, panel4 = tout

library(ggplot2)
library(cowplot)


Sys.setenv(TZ = "GMT") #set l'heure en TU

lac = read.table ("output_files/May_2023_O2_data.txt",sep="\t",h=TRUE)
lac$datetime = as.POSIXct(lac$datetime,tz="GMT",format=c("%Y-%m-%d %H:%M:%OS"))


#enlève les valeurs extrêmes
lac=subset(lac, do.obs > (-50) & do.obs<50) 

#créer nouvelle colonne ds dataframe qui stocke toutes les mesures de surfaces (pour tous les lacs)
table(lac$lac)
lac$is_surface = substring(lac$lac,3,3)!="F"
table(lac$is_surface)

#créer nouvelle colonne lac_nb avec la vrai nombre des lacs en numérique
idx_lac_nb=data.frame(lac_nb=rep(1:16,2),lac=c(1:16,paste0("0",1:9,"F"),paste0(10:16,"F")))
lac = merge(lac,idx_lac_nb)

#créer colonne panel
lac$panel=NA
#ajoute 1 ds panel si les 2 nombres sont 2,7,12 ou 13...
lac[lac$lac_nb%in%c(2,7,12,13),]$panel=1
lac[lac$lac_nb%in%c(4,5,10,15),]$panel=2
lac[lac$lac_nb%in%c(1,8,11,14),]$panel=3
lac[lac$lac_nb%in%c(3,6,9,16),]$panel=4

#force à mettre panel en numérique
lac$panel=as.factor(lac$panel)

#dataframe de la moy de DO par jour par lac pour le mois de mai
lac$day <- substring(lac$date,1,10)
lac$lac_day <- paste(lac$lac,lac$day,sep='_')
mean_lac_day <- tapply(lac$do.obs,lac$lac_day,mean)
mean_lac_day <- data.frame(lac_day=names(mean_lac_day),mean_O2=as.numeric(mean_lac_day))
mean_lac_day <- merge(mean_lac_day,unique(lac[,c("lac_day","lac","day","is_surface","lac_nb")]))
head(mean_lac_day)
mean_lac_day$day = as.POSIXct(mean_lac_day$day,tz="GMT",format=c("%Y-%m-%d"))


#Créer colonnes perches et nutriments
mean_lac_day$perches=NA
mean_lac_day$nutriments=NA
#ajoute 1 ds panel si les 2 nombres sont 2,7,12 ou 13...
mean_lac_day[mean_lac_day$lac_nb%in%c(2,7,12,13,1,8,11,14),]$perches=FALSE
mean_lac_day[mean_lac_day$lac_nb%in%c(4,5,10,15,3,6,9,16),]$perches=TRUE

mean_lac_day[mean_lac_day$lac_nb%in%c(1,8,11,14,3,6,9,16),]$nutriments=TRUE
mean_lac_day[mean_lac_day$lac_nb%in%c(2,7,12,13,4,5,10,15),]$nutriments=FALSE


#graph moyenneDO/jour/lac divisé en panel mois de mai
mean_lac_day$lac=factor(mean_lac_day$lac,levels=c(1:16,paste0("0",1:9,"F"),paste0(10:16,"F")))
mean_lac_day_p1 = subset(mean_lac_day,lac_nb%in%c(2,7,12,13))
mean_lac_day_p2 = subset(mean_lac_day,lac_nb%in%c(4,5,10,15))
mean_lac_day_p3 = subset(mean_lac_day,lac_nb%in%c(1,8,11,14))
mean_lac_day_p4 = subset(mean_lac_day,lac_nb%in%c(3,6,9,16))


p1 <- ggplot(data=mean_lac_day_p1, aes(x=day, y=mean_O2, color=is_surface)) + geom_point() + geom_smooth() + labs(x="Date",y="DO (en µg/L)") + scale_color_discrete(name = "Profondeur", labels = c("Fond", "Surface"))
p2 <- ggplot(data=mean_lac_day_p2, aes(x=day, y=mean_O2, color=is_surface)) + geom_point() + geom_smooth() + labs(x="Date",y="DO (en µg/L)") + scale_color_discrete(name = "Profondeur", labels = c("Fond", "Surface"))
p3 <- ggplot(data=mean_lac_day_p3, aes(x=day, y=mean_O2, color=is_surface)) + geom_point() + geom_smooth() + labs(x="Date",y="DO (en µg/L)") + scale_color_discrete(name = "Profondeur", labels = c("Fond", "Surface"))
p4 <- ggplot(data=mean_lac_day_p4, aes(x=day, y=mean_O2, color=is_surface)) + geom_point() + geom_smooth() + labs(x="Date",y="DO (en µg/L)") + scale_color_discrete(name = "Profondeur", labels = c("Fond", "Surface"))
plot_grid(p1,p2,p3,p4, labels=c("A","B","C","D"), ncol=2, nrow=2)

#subdivision fond surface
mean_lacF_day=subset(mean_lac_day,!is_surface)
mean_lacS_day=subset(mean_lac_day,is_surface)

#créer colonne panel
mean_lacF_day$panel=NA
#ajoute 1 ds panel si les 2 nombres sont 2,7,12 ou 13...
mean_lacF_day[mean_lacF_day$lac_nb%in%c(2,7,12,13),]$panel=1
mean_lacF_day[mean_lacF_day$lac_nb%in%c(4,5,10,15),]$panel=2
mean_lacF_day[mean_lacF_day$lac_nb%in%c(1,8,11,14),]$panel=3
mean_lacF_day[mean_lacF_day$lac_nb%in%c(3,6,9,16),]$panel=4
#force à mettre panel en numérique
mean_lacF_day$panel=as.factor(mean_lacF_day$panel)

# DO fond à partir du 17 avril
mean_lacF_day_17a = subset(mean_lacF_day,day>as.POSIXct("2023-04-16 23:59:59"))
summary(mean_lacF_day_17a)

mean_lacF_day_17a$jour = as.numeric(mean_lacF_day_17a$day - min(mean_lacF_day_17a$day))/3600/24

#modèle linéaire perhes et nutriments
modele_lineaire_pn <- lm(mean_O2 ~ (perches*nutriments):jour+perches*nutriments, data=mean_lacF_day_17a)
summary(modele_lineaire_pn)
ggplot(mean_lacF_day_17a,aes(x=day,y=mean_O2))  + geom_smooth(method='lm',aes(color=panel)) + geom_line(aes(color=panel,group=lac)) + labs(x="Date", y="DO moyenne (en µg/L)", title = "Représentation graphique du modèle linéaire testant l'effet des \ntraitements sur la DO", subtitle = "(superposée aux variations journalières de chaque lacs)")

