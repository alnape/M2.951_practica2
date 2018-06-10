getwd()
setwd("E:/TCVD")
getwd()
mhm <- read.csv("Melbourne_housing_FULL.csv")
#Lectura de dades
head(mhm)
str(mhm)
sapply(mhm, function(x) class(x))
kable(data.frame(variables=names(mhm),clase=as.vector(mhm)))
mhm <- mhm[,-2]
str(mhm)
mhm <- mhm[,-6]
str(mhm)
res <- sapply(mhm,class)
res
suburb_pcode <- mhm[,c('Suburb','Postcode')]
head(suburb_pcode)
suburb_pcode <- unique(suburb_pcode)
suburb_pcode
pcode_council <- mhm[,c('Postcode','CouncilArea')]
pcode_council <- unique(pcode_council)
pcode_council
sapply(mhm, function(x) sum(is.na(x)))
mhm[which(mhm$games!="0"),]
mhm[which(is.na(mhm$Price))]
mhm[is.na(mhm$Price)]
mhm[mhm[is.na(mhm$Price)]]
mhm$Price
mhm[is.na(mhm$Price),]
mhm[!is.na(mhm$Price),]
summary(mhm[!is.na(mhm$Price),])
str(mhm[!is.na(mhm$Price),])
mhm <- mhm[!is.na(mhm$Price),]
sapply(mhm, function(x) sum(is.na(x)))
install.packages("VIM")
mhm$Bedroom2 <- kNN(mhm)$Bedroom2
load(VIM)
library("VIM", lib.loc="C:/Program Files/R/R-3.4.2/library")
mhm$Bedroom2 <- kNN(mhm)$Bedroom2
sapply(mhm, function(x) sum(is.na(x)))
mhm$Bathroom <- kNN(mhm)$Bathroom
mhm$Car <- kNN(mhm)$Car
sapply(mhm, function(x) sum(is.na(x)))
mhm$Landsize <- kNN(mhm)$Landsize
mhm$BuildingArea <- kNN(mhm)$BuildingArea
mhm$YearBuilt <- kNN(mhm)$YearBuilt
mhm$Lattitude <- kNN(mhm)$Lattitude
mhm$Longtitude <- kNN(mhm)$Longtitude
sapply(mhm, function(x) sum(is.na(x)))
boxplot(mhm$Rooms)
help(boxplot)
boxplot(mhm$Distance)
summary(mhm)
boxplot(mhm$Bedroom2)
boxplot(mhm$Bathroom)
boxplot(mhm$Car)
boxplot(mhm$Landsize)
boxplot(mhm$BuildingArea)
boxplot(mhm$YearBuilt)
boxplot(mhm$Lattitude)
boxplot(mhm$Landsize)
savehistory("E:/TCVD/practica2_R.txt")
str(mhm)
as.numeric(mhm$Distance)
mhm$Distance
unique(mhm$Distance)
as.numeric(unique(mhm$Distance))
as.int(unique(mhm$Distance))
as.integer(unique(mhm$Distance))
str(mhm$Distance)
mhm$Distance[mhm$Distance=="#N/A"]
> str(mhm)
str(mhm)
mhm$Distance[which([mhm$Distance=="#N/A"),]
mhm$Distance[mhm$Distance=="#N/A"] <- NA
str(mhm)
mhm$Distance[mhm$Distance=="#N/A"]
mhm$Distance[mhm$Distance==NA]
mhm$Distance
sum(is.na(mhm$%Distance))
sum(is.na(mhm$Distance))
mhm$Postcode[mhm$Postcode=="#N/A"] <- NA
mhm$CouncilArea[mhm$CouncilArea=="#N/A"] <- NA
mhm$RegionName[mhm$RegionName=="#N/A"] <- NA
mhm$CouncilArea[mhm$CouncilArea=="#N/A"] <- NA
> mhm$PropertyCount[mhm$PropertyCount=="#N/A"] <- NA
mhm$PropertyCount[mhm$PropertyCount=="#N/A"] <- NA
sum(is.na(mhm$%Distance))
sum(is.na(mhm$Distance))
sum(is.na(mhm$Postcode))
sum(is.na(mhm$CouncilArea))
sum(is.na(mhm$Regionname))
is.na(mhm$Postcode)
is.na(mhm$Postcode==TRUE)
mhm$Postcode[is.na(mhm$Postcode==TRUE)]
str(mhm)
mhm_backup <- mhm
mhm_backup$Distance <- as.numeric(as.character(mhm_backup$Distance))
str(mhm_backup)
mhm_backup$Distance
median(mhm_backup$Distance)
str(mhm_backup)
mhm_backup
summary(mhm)
str(mhm)
dim(mhm)
sapply(mhm, function(x) sum(is.na(x)))
mhm$Distance <- kNN(mhm)$Distance
sapply(mhm, function(x) sum(is.na(x)))
mhm$YearBuilt[mhm$YearBuilt==2019]
sum(mhm$YearBuilt[mhm$YearBuilt==2019])
median((mhm$YearBuilt))
mhm$YearBuilt[mhm$YearBuilt==2019] <- median((mhm$YearBuilt))
str(mhm$YearBuilt)
mhm$YearBuilt
summary(mhm$YearBuilt)
sum(mhm$Rooms>8)
mhm$Rooms[mhm$Rooms>8] <- 8
sum(mhm$Rooms>8)
sum(mhm$Bedroom2>8)
mhm$Bedroom2[mhm$Bedroom2>8] <- 8
sum(mhm$Bedroom2>5)
mhm$Bathroom[mhm$Bathroom>5] <- 5
sum(mhm$Bathroom>5)
sum(mhm$Car>5)
mhm$Car[mhm$Car>5] <- 5
sum(mhm$Car>5)
sum(mhm$Landsize>50000)
sum(mhm$BuildingArea>50000)
sum(mhm$BuildingArea>10000)
sum(mhm$BuildingArea>5000)
write.csv(mhm, "Mhm_data_clean.csv")
savehistory("E:/TCVD/practica2_R.txt")
par(mfrow=c(2,2))
for(i in 1:ncol(mhm)) {
if (is.numeric(mhm[,i])){
qqnorm(mhm[,i],main = paste("Normal Q-Q Plot for ",colnames(mhm)[i]))
qqline(mhm[,i],col="red")
hist(mhm[,i],
main=paste("Histogram for ", colnames(mhm)[i]),
xlab=colnames(mhm)[i], freq = FALSE)
}
}
str(mhm)
for(i in 1:ncol(mhm)) {
if (is.numeric(mhm[,i])){
colnames(mhm)[i]
}
}
colnames(mhm)
par(mfrow=c(4,2))
for(i in 1:ncol(mhm)) {
if (is.numeric(mhm[,i])){
qqnorm(mhm[,i],main = paste("Normal Q-Q Plot for ",colnames(mhm)[i]))
qqline(mhm[,i],col="red")
hist(mhm[,i],
main=paste("Histogram for ", colnames(mhm)[i]),
xlab=colnames(mhm)[i], freq = FALSE)
}
}
shapiro.test(mhm$Lattitude)
shapiro.test(mhm$Rooms)
shapiro.test(mhm$Lattitude[0:5000])
savehistory("E:/TCVD/practica2_R.txt")
