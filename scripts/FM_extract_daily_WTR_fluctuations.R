rm(list=ls());gc()


# Script 2023-05-03 - François Mallard - extract daily fluctuations from the O2 measurements of April 2023
# Separate the O2 in seasonnal (here daily) and trend using the decompose function


library(gplots)
library(ggplot2)
library(emmeans)

Sys.setenv(TZ="GMT")

data_may=read.table(file='output_files/May_2023_O2_data.txt',sep='\t',h=TRUE)
data_may$datetime = as.POSIXct(data_may$datetime,format='%Y-%m-%d %H:%M:%S',TZ='GMT')  

data_may_F = subset(data_may,!is_surface)
data_may_S = subset(data_may,is_surface)

ggplot(data_may_F, aes(x = datetime, y = wtr)) +
  geom_line(aes(color = lac), alpha = 0.5)

## Create a time series object for the lakes separately (surface here)
ts_data=subset(data_may_S,lac==unique(data_may_S$lac)[1])$wtr

for(i in unique(data_may_S$lac)[2:16]){
  ts_data <- cbind(ts_data,subset(data_may_S,lac==i)$wtr)
}

### Extract the daily fluctuations for each lake

# Number of observation per day (10 mn interval)
freq_ts = 6*24
ts_F <- ts(ts_data[,1],frequency=freq_ts)
seasonal <- decompose(ts_F)$seasonal

for(i in 2:15){
  ts_F <- ts(ts_data[,i],frequency=freq_ts)
  seasonal <- cbind(seasonal,decompose(ts_F)$seasonal)
}

#heatmap.2(cor(seasonal), Rowv=TRUE, Colv=TRUE, notecol="black",col=bluered(100),notecex=0.7, density.info="none", trace="none", key=T, keysize=1.5,labRow=1:16 ,labCol= 1:16)

# Extract the SD
daily_sd  <- mean(tapply(seasonal[,1],subset(data_may_S,lac==1)$day,sd)[2:32])
for(i in 2:15){
  daily_sd  <- c(daily_sd,mean(tapply(seasonal[,i],subset(data_may_S,lac==i)$day,sd)[2:32]))
}
# Create a data frame
daily_fluctuations_S <- data.frame(lake=1:16,daily_sd=c(daily_sd,NA),perch=(1:16)%in%c(3:6,9,10,15,16),nut=(1:16)%in%c(1,3,6,8,9,11,14,16))
boxplot(daily_fluctuations_S$daily_sd~daily_fluctuations_S$nut)

###### Same analysis with the bottom

ts_data=subset(data_may_F,lac==unique(data_may_F$lac)[1])$wtr

for(i in unique(data_may_F$lac)[2:16]){
  ts_data <- cbind(ts_data,subset(data_may_F,lac==i)$wtr)
}
dim(ts_data)

freq_ts = 6*24
ts_F <- ts(ts_data[,1],frequency=freq_ts)
seasonal <- decompose(ts_F)$seasonal

for(i in 2:16){
  ts_F <- ts(ts_data[,i],frequency=freq_ts)
  seasonal <- cbind(seasonal,decompose(ts_F)$seasonal)
}

#plot(decompose(ts_F))
#heatmap.2(cor(seasonal), Rowv=TRUE, Colv=TRUE, notecol="black",col=bluered(100),notecex=0.7, density.info="none", trace="none", key=T, keysize=1.5,labRow=1:16 ,labCol= 1:16)

daily_sd  <- mean(tapply(seasonal[,1],subset(data_may_F,lac==sort(unique(data_may_F$lac))[1])$day,sd)[2:32])
for(i in 2:16){
  daily_sd  <- c(daily_sd,mean(tapply(seasonal[,i],subset(data_may_F,lac==sort(unique(data_may_F$lac))[i])$day,sd)[2:32]))
}

daily_fluctuations_B <- data.frame(lake=c(1:16),daily_sd=c(daily_sd),perch=(1:16)%in%c(3:6,9,10,15,16),nut=(1:16)%in%c(1,3,6,8,9,11,14,16))
boxplot(daily_fluctuations_B$daily_sd~daily_fluctuations_B$nut)

plot(daily_fluctuations_B[,2],daily_fluctuations_S[,2])

daily_fluctuations_B$is_surface=FALSE
daily_fluctuations_S$is_surface=TRUE

daily_fluctuations_all=rbind(daily_fluctuations_S,daily_fluctuations_B) 

## Output the table for stats in another script
write.table(daily_fluctuations_all,file="output_files/daily_fluctuations_WTR.txt",sep='\t',row.names=FALSE,quote=FALSE)

daily_fluctuations_all$nut_surf=paste(daily_fluctuations_all$nut,daily_fluctuations_all$is_surface)
ggplot(daily_fluctuations_all, aes(x = nut_surf, y = daily_sd)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)

names(daily_fluctuations_all)
mod = lm(daily_sd ~ is_surface * (nut+perch),data=daily_fluctuations_all)
summary(mod)

## Only surface
mod = lm(daily_sd ~ (nut+perch),data=subset(daily_fluctuations_all,is_surface))
summary(mod)

mod = lm(daily_sd ~ (nut+perch),data=subset(daily_fluctuations_all,!is_surface))
summary(mod)

daily_fluctuations_all$lac=daily_fluctuations_all$lake
data_may_S=merge(data_may_S,unique(daily_fluctuations_all[daily_fluctuations_all$is_surface,c("lac","nut","perch")]))
dim(data_may_S)

data_may_last = subset(data_may,datetime>as.POSIXct(c("2023-05-01 23:59:59"),format='%Y-%m-%d %H:%M:%S',TZ='GMT'))
data_may_last = subset(data_may_last,datetime<as.POSIXct(c("2023-05-02 23:59:59"),format='%Y-%m-%d %H:%M:%S',TZ='GMT'))
data_may_last_F = subset(data_may_last,!is_surface)
data_may_last_S = subset(data_may_last,is_surface)

data_may_last_S=merge(data_may_last_S,unique(daily_fluctuations_all[daily_fluctuations_all$is_surface,c("lac","nut","perch")]))
tmp=unique(daily_fluctuations_all[daily_fluctuations_all$is_surface,c("lac","nut","perch")])
tmp$lac=paste0(tmp$lac,"F")
tmp$lac[1:9]=paste0("0",tmp$lac[1:9])
data_may_F=merge(data_may_F,tmp)
#data_may_last_S$lac_nut <- paste(data_may_last_S$lac,data_may_last_S$nut)
data_may_S$nutriment=data_may_S$nut
ggplot(data_may_S, aes(x = datetime, y = wtr)) +
  geom_line(aes(group=lac,color = nutriment), alpha = 0.5)+xlab("Date")+ylab('Température de l\'eau')+ggtitle("Température de l'eau - Avril 2023")

ggplot(data_may_F, aes(x = datetime, y = wtr)) +
  geom_line(aes(group=lac,color = nut), alpha = 0.5)


#data_may_S$perch_nut = paste(data_may_S$perch,data_may_S$nut,sep='_')

ggplot(data_may_F, aes(x = datetime, y = do.obs)) +
  geom_line(aes(group=lac,color = nut,linetype=perch), alpha = 0.5)

ggplot(data_may_last_S, aes(x = datetime, y = wtr)) +
  geom_line(aes(group=lac,color = nut), alpha = 0.5)

