library(reshape2)

LF_path = paste("Data/Leafwetness/",
                list.files(path="\\\\lin-file/HOME$/hrlrxj/My Documents/ALCM/Peter Lo/Analysis_Rodelyn/Data/Leafwetness/",pattern="*.csv"),sep="")
leafwetness <- lapply(LF_path, read.csv)
leafwetness_og <- do.call(rbind,leafwetness[1:15])

leafwetness_og[,3:8] <- sapply(leafwetness_og[,3:8],  as.character)
leafwetness_og[,3:8] <- sapply(leafwetness_og[,3:8],  as.numeric)
                                                                 
leafwetness_og[,1] <- as.POSIXct(leafwetness_og[,1], format = "%Y-%m-%d %H:%M:%S")
leafwetness_og[,2] <- as.POSIXct(leafwetness_og[,2], format = "%Y-%m-%d %H:%M:%S")

leafwetness_LF <- melt(leafwetness_og, id.vars = colnames(leafwetness_og)[1:2])
colnames(leafwetness_LF)[3:4] <- c("station","hours_wet")
leafwetness_LF$Year <- format(leafwetness_LF$Record_Start_Time, "%Y")

table(leafwetness_LF$Year)

leafwetness2 <- leafwetness[[16]]
leafwetness2[,1] <- as.POSIXct(leafwetness2[,1], format = "%Y-%m-%d %H:%M:%S")
leafwetness2$Year <- format(leafwetness2[,1], "%Y")

table(leafwetness2$Year)

leafwetness_LF$Day <- cut(leafwetness_LF$Record_Start_Time, breaks = "1440 min")
LF_agg <- aggregate(x=leafwetness_LF[,"leafwetness"],by= leafwetness_LF[c("Day", "station","Year")], FUN=function(x) sum(x>50,na.rm = T))
colnames(LF_agg)[4] <- "hours_wet"

table(leafwetness2$Year)
table(LF_agg$Year)

head(LF_agg)
head(leafwetness2)
#write.csv(leafwetness2, file="Data/Leafwetness/LF_day.csv",row.names=F)
