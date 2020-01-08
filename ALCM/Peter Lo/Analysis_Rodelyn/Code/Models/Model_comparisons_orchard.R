rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(e1071)
library(randomForest)
library(rpart)

#Read in Data



#----------------------------------------------------------------------------------------------------------------------

traps_DF <- read.csv("Data/Trap Data/ALCMdata_CLEANED.csv",stringsAsFactors = F) 
traps_DF$week <- NULL
traps_DF <- traps_DF %>% group_by(District,Season) %>% mutate(sum = cumsum(ALCM_count)) %>% as.data.frame()
traps_DF$week_dat <- as.Date(traps_DF$week_dat, format='%Y-%m-%d')
traps_DF$cal_week <- strftime(traps_DF$week_dat, format = "%V")
traps_DF$rate <- traps_DF$ALCM_count/traps_DF$No_traps
traps_DF <- traps_DF %>% group_by(District, Season) %>% mutate(sumCGDD = cumsum(weekly_CGDD)) %>% as.data.frame()

traps_DF$Season <- as.factor(traps_DF$Season)
traps_DF$Season2 <- as.numeric(traps_DF$Season)

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(prop_count = ALCM_count/max(sum)) %>%as.data.frame()

#Remove these are Sandy road, St Georges road, Robinson and maybe Lawn rd. 
traps_DF <- traps_DF[!traps_DF$Orchard %in% c("Sandy Rd", "St Georges Rd", "Robinson", "Lawn Rd"),]

#Organic OrchardsTe Koha, Falls, Norton road, Te Aute
traps_DF$Organic <- ifelse(traps_DF$Orchard %in% c("Te Koha", "Falls", "Norton road", "Te Aute"),1,0)
traps_DF[traps_DF$Orchard %in% "Te Koha",]

#keep only last 5 monitoring years
traps_DF <- traps_DF[traps_DF$Season2 %in% 9:15,]

#calculating the lag for each orchard
traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(lag_count = lag(ALCM_count)) %>% as.data.frame()

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(lag_count2 = lag(ALCM_count,2)) %>% as.data.frame()



#----------------------------------------------------------------------------------------------------------------------

#Dealing with missing climate data
#----------------------------------------------------------------------------------------------------------------------
#impute missing rainfall and temp data as previous weeks measurements

clim_miss <- traps_DF[which(rowSums(is.na(traps_DF[,c("Total_Rainfall","Mean_mintemp","Mean_maxtemp","Mean_meantemp")]))==4),]

for(i in 1:length(clim_miss[,1])){
week <- c(clim_miss$monitoring_week[i]-1,clim_miss$monitoring_week[i])
traps_DF[traps_DF$Orchard%in% clim_miss$Orchard[i] & traps_DF$Season%in% clim_miss$Season[i] & traps_DF$monitoring_week %in% week,]

traps_DF[traps_DF$Orchard%in% clim_miss$Orchard[i] & traps_DF$Season%in% clim_miss$Season[i] & traps_DF$monitoring_week %in% week[2],
         c("Total_Rainfall","Mean_mintemp","Mean_maxtemp","Mean_meantemp")] <-
traps_DF[traps_DF$Orchard%in% clim_miss$Orchard[i] & traps_DF$Season%in% clim_miss$Season[i] & traps_DF$monitoring_week %in% week[1],
         c("Total_Rainfall","Mean_mintemp","Mean_maxtemp","Mean_meantemp")] }

#crosses Rd
traps_DF[traps_DF$Orchard %in% "Crosses Road" & traps_DF$Season2 %in% 10,
         c("weekly_CGDD","Mean_mintemp","Mean_maxtemp","Mean_meantemp")] <-
traps_DF[traps_DF$station %in% "HAV" & traps_DF$Season2 %in% 10 & 
           traps_DF$cal_week %in% c(38) & traps_DF$Orchard %in% "Mt Erin", c("weekly_CGDD","Mean_mintemp","Mean_maxtemp","Mean_meantemp")]

#----------------------------------------------------------------------------------------------------------------------

#--------------------#
#--      Trees     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
#doing this by Orchard
#CV
traps_DF$tree_pred <- NA
traps_DF$tree_pred_rate <- NA
traps_DF$tree_pred_prop <- NA

levels(traps_DF$station)
traps_DF$Orchard <- as.factor(traps_DF$Orchard)
traps_DF$cal_week <- as.numeric(traps_DF$cal_week)

#LVOOCV
for(j in 1:length(levels(traps_DF$Orchard))){
df <- traps_DF[traps_DF$Orchard %in% levels(traps_DF$Orchard)[j],]
n_length <- dim(df)[1]
tree_pred <- tree_pred_rate <- tree_pred_prop<- numeric()

for(i in 1:n_length){

    train_df <- df[-i,]
    test_df <- df[i,]
    tree_mod <- rpart(ALCM_count~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + cal_week, data=train_df)
    tree_pred[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2","cal_week")])
  
    
    tree_mod <- rpart(rate~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + cal_week, data=train_df)
    tree_pred_rate[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2","cal_week")])
    
    tree_mod <- rpart(prop_count~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + cal_week, data=train_df)
    tree_pred_prop[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2","cal_week")])
    
    
    }

traps_DF[traps_DF$Orchard %in% levels(traps_DF$Orchard)[j],c("tree_pred","tree_pred_rate","tree_pred_prop")] <- cbind(tree_pred,tree_pred_rate,tree_pred_prop)

}

#---------

#doing this by Orchard
#CV
traps_DF$tree_pred_di <- NA
traps_DF$tree_pred_rate_di <- NA
traps_DF$tree_pred_prop_di <- NA

levels(traps_DF$station)
traps_DF$Orchard <- as.factor(traps_DF$Orchard)
traps_DF$cal_week <- as.numeric(traps_DF$cal_week)

#LVOOCV
for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- dim(df)[1]
  tree_pred_di <- tree_pred_rate_di <- tree_pred_prop_di<- numeric()
  
  for(i in 1:n_length){
    
    train_df <- df[-i,]
    test_df <- df[i,]
    tree_mod <- rpart(ALCM_count~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + as.numeric(cal_week)+
                        Orchard+lag_count + lag_count2, data=train_df)
    tree_pred_di[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2",
                                                           "cal_week","Orchard","lag_count","lag_count2")])
    
    
    tree_mod <- rpart(rate~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + as.numeric(cal_week) + 
                        Orchard + lag_count + lag_count2, data=train_df)
    tree_pred_rate_di[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2",
                                                                "cal_week","Orchard","lag_count","lag_count2")])
    
    tree_mod <- rpart(prop_count~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + as.numeric(cal_week)
                      +Orchard + lag_count + lag_count2, data=train_df)
    tree_pred_prop_di[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2",
                                                                "cal_week","Orchard","lag_count","lag_count2")])
    
    
  }
  
  traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],c("tree_pred_di","tree_pred_rate_di","tree_pred_prop_di")] <- cbind(tree_pred_di,tree_pred_rate_di,tree_pred_prop_di)
  
}


ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week,y=ALCM_count,group=Orchard)) + geom_line() +
  geom_line(aes(x=monitoring_week,y=tree_pred_di,group=Orchard),col="red")+ facet_wrap(~Orchard)

par(mfrow=c(2,1))
plot(traps_DF[traps_DF$Season2 %in% 12 & traps_DF$Orchard %in% unique(traps_DF$Orchard)[12],"ALCM_count"],type = "S")
plot(traps_DF[traps_DF$Season2 %in% 12 & traps_DF$Orchard %in% unique(traps_DF$Orchard)[12],"tree_pred_rate"],type = "S")

#-------------------------------------------------------------------------------------------------

#--------------------#
#--      RFore     --#
#--------------------#
#WARNING: THIS TAKES AGES
#----------------------------------------------------------------------------------------------------
#CV
traps_DF$for_pred <- NA
traps_DF$for_pred_rate <- NA
traps_DF$for_pred_prop <- NA


#using total counts
a1 <- Sys.time()
for(j in 1:length(levels(traps_DF$Orchard))){
  
  print("WARNING: THIS TAKES AGES") 
  df <- traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[j],]
  n_length <- dim(df)[1]
  
  print(c(j, (n_length)))
  for_pred <- for_pred_rate <- for_pred_prop <- numeric()
  
  for(i in 1:n_length){
    
    train_df <- df[-i,]
    test_df <- df[i,]
    tree_mod <- randomForest(ALCM_count~ CGDD +  Total_Rainfall + Mean_meantemp + Season2 + cal_week +lag_count+lag_count2,
                             data=train_df,ntree=1000,na.action = na.roughfix)
    for_pred[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2","cal_week","lag_count","lag_count2")])
    
    
    tree_mod <- randomForest(rate~ CGDD +  Total_Rainfall + Mean_meantemp + Season2 + cal_week +lag_count+lag_count2, 
                             data=train_df,ntree=1000,na.action = na.roughfix)
    for_pred_rate[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2","cal_week","lag_count","lag_count2")])
    
    
    tree_mod <- randomForest(prop_count~ CGDD +  Total_Rainfall + Mean_meantemp + Season2 + cal_week +lag_count+lag_count2, 
                             data=train_df,ntree=1000,na.action = na.roughfix)
    for_pred_prop[i] <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2","cal_week","lag_count","lag_count2")])
    
  }
  
  traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[j],c("for_pred","for_pred_rate","for_pred_prop")] <- cbind(for_pred,for_pred_rate,for_pred_prop)
  
}
a2 <- Sys.time()
print(a1-a2)

x_seq <- 1000:1100
plot(traps_DF$rate[x_seq],type="l",lty=2)
lines(traps_DF$for_pred_rate[x_seq],col="red")

plot(traps_DF$ALCM_count[x_seq],type="S",lty=2)
lines(traps_DF$for_pred[x_seq],col="red",type="S")


#-------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------


#--------------------#
#--    Results     --#
#--------------------#

#----------------------------------------------------------------------------------------------------

#trees
ggplot(traps_DF[traps_DF$Season2 %in% 9,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 10,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 11,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 12,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 13,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_di, group=Orchard),col="red")


#-----------




ggplot(traps_DF[traps_DF$Season2 %in% 9,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 10,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 11,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 12,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 13,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred, group=Orchard),col="red")
