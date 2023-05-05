###Etude des donnees obtenues avec la sonde multi parametres

rm(list=ls())
library(ggplot2)
library(emmeans)

multip_may <- read.table("data/multiparametre_may.txt",sep="\t",h=TRUE)

multip_may$Lac=as.factor(multip_may$Lac)
multip_may$Profondeur..m.=as.factor(multip_may$Profondeur..m.)

multip_may$traitement=c(1,0,3,2,2,3,0,1,3,2,1,0,0,1,2,3)

###Etude du pH 

multip_may$traitement=as.factor(multip_may$traitement)
ggplot(data=multip_may,mapping=aes(x=Profondeur..m.,y=pH.18F103729,color=traitement)) +ylim(6,10)+geom_point()+ggtitle("Etude du pH en fonction de la profondeur selon les traitements")+xlab("Profondeur (m)")+ylab("pH")+scale_color_discrete(name = "Traitements" , labels = c("Rien","Nutriments","Prédateurs","Nutriments et prédateurs"))

modelepH=lm(pH.18F103729~Profondeur..m.* traitement,data=multip_may)
summary(modelepH)

pairs(emmeans(modelepH,~traitement))
pairs(emmeans(modelepH, ~Profondeur..m.))

###Etude de la chlorophylle (complementaire de l'etude des donnees de la sonde BBE)

ggplot(data=multip_may,mapping=aes(x=Profondeur..m.,y=Chl.RFU.18J104246,color=traitement))+geom_point()

modelechl=lm(Chl.RFU.18J104246~Profondeur..m.* traitement,data=multip_may)
pairs(emmeans(modelechl,~traitement))
pairs(emmeans(modelechl, ~Profondeur..m.))

###Etude de la turbidite

ggplot(data=multip_may,aes(x=Profondeur..m.,y=FNU.18F102856))+geom_point(aes(color=traitement))+geom_line(aes(color=traitement,group=Lac))
head(multip_may)
str(multip_may)

multip_may$Profondeur..m.=as.numeric(as.character(multip_may$Profondeur..m.))


plot(jitter(multip_may$Profondeur..m.),multip_may$FNU.18F102856,pch=16,col=c("grey","cornflowerblue","darkgoldenrod1",'darkgreen')[as.numeric(as.factor(multip_may$traitement))],xlab="Profondeur",ylab="Turbidité (fnu)",las=1,bty="n",main='Turbidité de l\'eau')
legend(0.4,14,c("Aucun traitement","Nutriments",'Perches',"Nutriments+Perches"),col=c("grey","cornflowerblue","darkgoldenrod1",'darkgreen'),pch=16)

# analyse plus precise pour la profondeur 2.5m

mod=lm(FNU.18F102856~traitement,data=subset(multip_may,Profondeur..m.==2.5))
pairs(emmeans(mod,~traitement))
summary(mod)

# séparer les effets perches et nutriments

multip_may$perche = "noP"
multip_may$perche[multip_may$traitement%in%c(2:3)]="P"

multip_may$nut = "noN"
multip_may$nut[multip_may$traitement%in%c(1,3)]="N"

# etude de la correlation des effets
mod=lm(FNU.18F102856~perche*nut,data=subset(multip_may,Profondeur..m.==2.5))
summary(mod)

#comparaison des deux effets
mod2=lm(FNU.18F102856~perche+nut,data=subset(multip_may,Profondeur..m.==2.5))
summary(mod2)
pairs(emmeans(mod2,~perche+nut))



                                   
