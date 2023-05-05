#Upload of the data of the chlorophyll content in different algae types over observations in 2016-2023
bbe_all <- read.delim("data/BBE_all.txt")

#Change in pigments and algae concentration over years in different lakes
#lac1
lac1 <- subset(bbe_all, Lac == "LAC 1")
lac_1 <- ggplot(data = lac1, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac1") +
  ylim(0,100) + ylab("Algae concentration") 


#lac2
lac2 <- subset(bbe_all, Lac == "LAC 2")
lac_2 <-ggplot(data = lac2, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac2") +
  ylim(0,100) + ylab("Algae concentration")

#lac3
lac3 <- subset(bbe_all, Lac == "LAC 3")
lac_3 <-ggplot(data = lac3, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac3") +
  ylim(0,100) + ylab("Algae concentration") 

#lac4
lac4 <- subset(bbe_all, Lac == "LAC 3")
lac_4 <-ggplot(data = lac4, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac4") +
  ylim(0,100) + ylab("Algae concentration") 

#Common graph for 1-4 lakes
grid.arrange(lac_1, lac_2, lac_3, lac_4, nrow = 4)

#lac5
lac5 <- subset(bbe_all, Lac == "LAC 5")
lac_5 <- ggplot(data = lac5, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac5") +
  ylim(0,100) + ylab("Algae concentration") 

#lac6
lac6 <- subset(bbe_all, Lac == "LAC 6")
lac_6 <-ggplot(data = lac6, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac6") +
  ylim(0,100) + ylab("Algae concentration") 

#lac7
lac7 <- subset(bbe_all, Lac == "LAC 7")
lac_7 <-ggplot(data = lac7, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac7") +
  ylim(0,100) + ylab("Algae concentration") 


#lac8
lac8 <- subset(bbe_all, Lac == "LAC 8")
lac_8 <-ggplot(data = lac8, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac8") +
  ylim(0,100) + ylab("Algae concentration") 

#Common graph for 5-8 lakes
grid.arrange(lac_5, lac_6, lac_7, lac_8, nrow = 4)

#updated lac6
lac6 <- subset(bbe_all, Lac == "LAC 6")
lac_6 <-ggplot(data = lac6, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac6") +
  ylim(0,100) + ylab("Algae concentration") 
#lac9
lac9 <- subset(bbe_all, Lac == "LAC 9")
lac_9 <- ggplot(data = lac9, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac9") +
  ylim(0,100) + ylab("Algae concentration") 

#lac10
lac10 <- subset(bbe_all, Lac == "LAC 10")
lac_10 <-ggplot(data = lac10, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac10") +
  ylim(0,100) + ylab("Algae concentration") 

#lac11
lac11 <- subset(bbe_all, Lac == "LAC 11")
lac_11 <-ggplot(data = lac11, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac11") +
  ylim(0,100) + ylab("Algae concentration") 


#lac12
lac12 <- subset(bbe_all, Lac == "LAC 12")
lac_12 <-ggplot(data = lac12, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac12") +
  ylim(0,100) + ylab("Algae concentration") 

#Common graph for 9-12 lakes
grid.arrange(lac_9, lac_10, lac_11, lac_12, nrow = 4)

#lac13
lac13 <- subset(bbe_all, Lac == "LAC 13")
lac_13 <- ggplot(data = lac13, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac13") +
  ylim(0,100) + ylab("Algae concentration") 

#lac14
lac14 <- subset(bbe_all, Lac == "LAC 14")
lac_14 <-ggplot(data = lac13, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac13") +
  ylim(0,100) + ylab("Algae concentration") 

#lac15
lac15 <- subset(bbe_all, Lac == "LAC 15")
lac_15 <-ggplot(data = lac15, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac15") +
  ylim(0,100) + ylab("Algae concentration") 


#lac16
lac16 <- subset(bbe_all, Lac == "LAC 16")
lac_16 <-ggplot(data = lac16, aes(x= date_1, group=1)) + geom_line(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y = Green_Algae_ug.L, color="Green algae"))+ geom_point(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_point(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + geom_line(aes(y=Bluegreen_ug.L, color="Blue green algae")) + 
  geom_line(aes(y=Diatoms_ug.L, color="Diatoms")) + geom_line(aes(y=Cryptophyta_ug.L, color="Cryptophyta")) + geom_line(aes(y=Yellow.sub_ug.L, color="Yellow")) + ggtitle("Lac16") +
  ylim(0,100) + ylab("Algae concentration") 

#Common graph for 13-16 lakes
grid.arrange(lac_13, lac_14, lac_15, lac_16, nrow = 4)

#Classify the nutrient supply and distribution of it in different lakes

vals14 <- c("LAC 1","LAC 2", "LAC 3", "LAC 4")
vals58 <- c("LAC 5","LAC 6", "LAC 7", "LAC 8")
vals912 <- c("LAC 9","LAC 10", "LAC 11", "LAC 12")
vals1316 <- c("LAC 13","LAC 14", "LAC 15", "LAC 16")

lacs14 <- bbe_all[bbe_all$Lac %in% vals14,]
lacs58 <- bbe_all[bbe_all$Lac %in% vals58,]
lacs912 <- bbe_all[bbe_all$Lac %in% vals912,]
lacs1316 <- bbe_all[bbe_all$Lac %in% vals1316,]

#Lacs14
qplot(date_1, Total_conc_ug.L, colour = treatment, shape = Lac,
      data = lacs14, ylim = c(0,100))
#Lacs58
qplot(date_1, Total_conc_ug.L, colour = treatment, shape = Lac, 
      data = lacs58, ylim = c(0,100))
#Lacs912
qplot(date_1, Total_conc_ug.L, colour = treatment, shape = Lac, 
      data = lacs912, ylim = c(0,100))

#Lacs1316
qplot(date_1, Total_conc_ug.L, colour = treatment, shape = Lac, 
      data = lacs1316, ylim = c(0,100))

#ANOVA analysis to find if there are differences in treatment

ggplot(bbe_all, aes(treatment, Diatoms_ug.L)) + geom_boxplot() + ylim(0,20) + ggtitle("Effect of treatment on distribution of algae concentration") + xlab("Treatment") + ylab("Total concentration")

mod = lm(Diatoms_ug.L ~ treatment, data = bbe_all)

library(emmeans)
pairs(emmeans(mod,~treatment))


#Diatoms abundance according to different treatments

ggplot(data = bbe_all, aes(x= date_1, group=1, y=Diatoms_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point() 

ggplot(data = lacs14, aes(x= date_1, group=1, y=Diatoms_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point() + xlab("Date") + ylab("Green algae concentration")
ggplot(data = lacs58, aes(x= date_1, group=1, y=Diatoms_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs912, aes(x= date_1, group=1, y=Diatoms_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs1316, aes(x= date_1, group=1, y=Diatoms_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()

#Green algae abundance according to different treatments

ggplot(data = bbe_all, aes(x= date_1, group=1, y=Green_Algae_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point() 

ggplot(data = lacs14, aes(x= date_1, group=1, y=Green_Algae_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point() + xlab("date") + ylab("Green algae concentration") + ggtitle("Green algae abundance according to different treatments")
ggplot(data = lacs58, aes(x= date_1, group=1, y=Green_Algae_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs912, aes(x= date_1, group=1, y=Green_Algae_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs1316, aes(x= date_1, group=1, y=Green_Algae_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()

#Blue green algae abundance according to different treatments
ggplot(data = lacs14, aes(x= date_1, group=1, y=Bluegreen_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point() + xlab("date") + ylab("Green algae concentration") + ggtitle("Green algae abundance according to different treatments")
ggplot(data = lacs58, aes(x= date_1, group=1, y=Bluegreen_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs912, aes(x= date_1, group=1, y=Bluegreen_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs1316, aes(x= date_1, group=1, y=Bluegreen_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()

#Cryptophyta abundance according to different treatments
ggplot(data = lacs14, aes(x= date_1, group=1, y=Cryptophyta_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point() + xlab("date") + ylab("Green algae concentration") + ggtitle("Green algae abundance according to different treatments")
ggplot(data = lacs58, aes(x= date_1, group=1, y=Cryptophyta_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs912, aes(x= date_1, group=1, y=Cryptophyta_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()
ggplot(data = lacs1316, aes(x= date_1, group=1, y=Cryptophyta_ug.L, colour = Lac, shape = treatment)) + geom_line() + geom_point()

ggplot(bbe_all, aes(treatment, Total_conc_ug.L)) + geom_boxplot() + ylim(0,150) + ggtitle("Effect of treatment on distribution of algae concentration") + xlab("Treatment") + ylab("Total concentration")

mod = lm(Total_conc_ug.L ~ treatment, data = bbe_all)

library(emmeans)
pairs(emmeans(mod,~treatment))

gen <- read.csv2("data/Mesures BBE lacs 2023.csv")

j1 <- gen [1:48, ]
j2  <- gen [53:100, ]                    
f <- gen [105:152, ]
mr <- gen [157:204, ]
ma <- gen [209:224, ]


#BBE analysis of last month (our analysis)
#trials to find the diagnostic group of algae to differentiate different types of treatments in lake
mai <- read.csv2("data/datasets/mai.csv")
qplot(Green.Algae, Total.conc., colour = X, 
      data = ma)
qplot(Green.Algae, Total.conc., colour = X, shape = Treatment,
      data = mai)
qplot(Green.Algae, Diatoms, colour = X, shape = Treatment,
      data = mai)
qplot(Green.Algae, Bluegreen, colour = X, shape = Treatment,
      data = mai)
qplot(Diatoms, Bluegreen, colour = X, shape = Treatment,
      data = mai)
qplot(Yellow.substances, Bluegreen, colour = X, shape = Treatment,
      data = mai)
qplot(Diatoms, Total.conc., colour = X, shape = Treatment,
      data = mai)
qplot(Bluegreen, Total.conc., colour = X, shape = Treatment,
      data = mai)
qplot(Bluegreen, Total.conc., colour = X, shape = Treatment,
      data = mai)

#ANOVA analysis of last month - effect of treatment on algae concentration (our measurements)
mod = lm(Total.conc. ~ Treatment, data = mai)
pairs(emmeans(mod,~Treatment))

#General graph with results of monthly treatment
greenalgae <- qplot(Green.Algae, Total.conc., colour = X, shape = Treatment, main = "Effect of treatment on the green algae concentration", xlab = "Green algae concentration", ylab = "General algae concentration",
                    data = mai)
bluegreenalgae <- qplot(Bluegreen, Total.conc., colour = X, shape = Treatment, main = "Effect of treatment on the blue-green algae concentration", xlab = "Blue-green algae concentration", ylab = "General algae concentration",
                        data = mai)

grid.arrange(greenalgae, bluegreenalgae, ncol = 2)

#Seasonal algae concentration dynamics in 2023
final_dyn <- read.csv2("data/Mesures BBE lacs 2023_update.csv")
final_dyn[['date']] <- as.POSIXct(final_dyn[['Date.Time']],
                                  format = "%d/%m/%Y %H:%M", tz="Europe/Stockholm")

final_dyn$Treatment[final_dyn$Lac == "LAC 2"| final_dyn$Lac =="LAC 7" | final_dyn$Lac =="LAC 12" | final_dyn$Lac =="LAC 13"] <- 0
final_dyn$Treatment[final_dyn$Lac == "LAC 1"| final_dyn$Lac =="LAC 8" | final_dyn$Lac =="LAC 11" | final_dyn$Lac =="LAC 14"] <- 1
final_dyn$Treatment[final_dyn$Lac == "LAC 4"| final_dyn$Lac =="LAC 5" | final_dyn$Lac =="LAC 10" | final_dyn$Lac =="LAC 15"] <- 2
final_dyn$Treatment[final_dyn$Lac == "LAC 3"| final_dyn$Lac =="LAC 6" | final_dyn$Lac =="LAC 9" | final_dyn$Lac =="LAC 16"] <- 3
final_dyn$Treatment = as.factor(final_dyn$Treatment)

qplot(date, Total.conc., colour = Lac, shape = Treatment,
      data = final_dyn, geom=c("point", "line"))

qplot(date, Total.conc., colour = Treatment,
      data = final_dyn, geom=c("line"))

val14 <- c("LAC 1","LAC 2", "LAC 3", "LAC 4")
val58 <- c("LAC 5","LAC 6", "LAC 7", "LAC 8")
val912 <- c("LAC 9","LAC 10", "LAC 11", "LAC 12")
val1316 <- c("LAC 13","LAC 14", "LAC 15", "LAC 16")

lacss14 <- final_dyn[final_dyn$Lac %in% vals14,]
lacss58 <- final_dyn[final_dyn$Lac %in% vals58,]
lacss912 <- final_dyn[final_dyn$Lac %in% vals912,]
lacss1316 <- final_dyn[final_dyn$Lac %in% vals1316,]

#Lacs14

lacs14_graph <- qplot(date, Total.conc., colour = Treatment, main = "Lacs 1-4",
                      data = lacss14, geom=c("line"))
#Lacs58

lacs58_graph <- qplot(date, Total.conc., colour = Treatment, main = "Lacs 5-8",
                      data = lacss58, geom=c("line"))
#Lacs912

lacs912_graph <- qplot(date, Total.conc., colour = Treatment, main = "Lacs 9-12",
                       data = lacss912, geom=c("line"))

#Lacs1316
lacs1316_graph <-qplot(date, Total.conc., colour = Treatment, main = "Lacs 13-16",
                       data = lacss1316, geom=c("line"))

grid.arrange(lacs14_graph, lacs58_graph, lacs912_graph, lacs1316_graph, ncol = 2, nrow=2)






