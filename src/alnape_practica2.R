getwd()
setwd("E:/TCVD")
getwd()

#Lectura de dades
mhm <- read.csv("Melbourne_housing_FULL.csv")
head(mhm)
sapply(mhm, function(x) class(x))
str(mhm)

#Substitucions de #N/A per Nuls
levels(mhm$Distance) <- sub("#N/A", NA, levels(mhm$Distance))
levels(mhm$Postcode) <- sub("#N/A", NA, levels(mhm$Postcode))
levels(mhm$CouncilArea) <- sub("#N/A", NA, levels(mhm$CouncilArea))
levels(mhm$Regionname) <- sub("#N/A", NA, levels(mhm$Regionname))
levels(mhm$Propertycount) <- sub("#N/A", NA, levels(mhm$Propertycount))
#Comprobacions
sum(is.na(mhm$Distance))
sum(is.na(mhm$Postcode))
sum(is.na(mhm$CouncilArea))
sum(is.na(mhm$Regionname))
sum(is.na(mhm$Propertycount))

#Conversió de tipus d'atributs
mhm$Distance <- as.numeric(as.character(mhm$Distance))
mhm$Propertycount <- as.numeric(as.character(mhm$Propertycount))
mhm$Date <- as.Date(mhm$Date, format = "%d/%m/%Y")

#Esborrem columna Address
mhm <- mhm[,-2]
#Esborrem columna SellerG
mhm <- mhm[,-6]

#Colocació de l'atribut Price com a última columna
col_price <- grep("Price", names(mhm))
mhm <- mhm[, c((1:ncol(mhm))[-col_price], col_price)]

sapply(mhm, function(x) sum(is.na(x)))
#Esborrem registres sense Price
mhm <- mhm[!is.na(mhm$Price),]
dim(mhm)
#Dataframe final
str(mhm)

# Tractament de Nuls
install.packages("VIM")
load(VIM)
library("VIM", lib.loc="C:/Program Files/R/R-3.4.2/library")
mhm$Bedroom2 <- kNN(mhm)$Bedroom2
mhm$Bathroom <- kNN(mhm)$Bathroom
mhm$Car <- kNN(mhm)$Car
mhm$Postcode <- kNN(mhm)$Postcode
mhm$Regionname <- kNN(mhm)$Regionname
mhm$Landsize <- kNN(mhm)$Landsize
mhm$BuildingArea <- kNN(mhm)$BuildingArea
mhm$YearBuilt <- kNN(mhm)$YearBuilt
mhm$CouncilArea <- kNN(mhm)$CouncilArea
mhm$Lattitude <- kNN(mhm)$Lattitude
mhm$Longtitude <- kNN(mhm)$Longtitude
mhm$Propertycount <- kNN(mhm)$Propertycount

sapply(mhm, function(x) sum(is.na(x)))
str(mhm)
summary(mhm)

# Correcció de YearBuilt
mhm$YearBuilt[mhm$YearBuilt==2019] <- median((mhm$YearBuilt))
summary(mhm$YearBuilt)

# Boxplots i correccions
boxplot(mhm$Rooms)
mhm$Rooms[mhm$Rooms>8] <- 8
boxplot(mhm$Bedroom2)
mhm$Bedroom2[mhm$Bedroom2>8] <- 8
boxplot(mhm$Bathroom)
mhm$Bathroom[mhm$Bathroom>5] <- 5
boxplot(mhm$Car)
mhm$Car[mhm$Car>5] <- 5
boxplot(mhm$Landsize)
sum(mhm$Landsize>50000)
boxplot(mhm$BuildingArea)
sum(mhm$BuildingArea>5000)

# Analisi de Dades
suburb_pcode <- mhm[,c('Suburb','Postcode')]
suburb_pcode <- unique(suburb_pcode)
head(suburb_pcode,20)

pcode_council <- mhm[,c('Postcode','CouncilArea')]
pcode_council <- unique(pcode_council)
head(pcode_council,20)

# Normalitat de dades
library(nortest)
alpha = 0.05
col.names = colnames(mhm)
for (i in 1:ncol(mhm)) {
    if (is.integer(mhm[,i]) | is.numeric(mhm[,i])) {
        p_val = ad.test(mhm[,i])$p.value
        if (p_val < alpha) {
            cat("-> ",col.names[i])
			cat("\n")
        }
    }
}

# Normal Q-Q Plot & Histograms of Integer variables
par(mfrow=c(3,2))
for(i in 1:ncol(mhm)) {
 if (is.integer(mhm[,i])) {
	qqnorm(mhm[,i],main = paste("Normal Q-Q Plot for ",colnames(mhm)[i]))
	qqline(mhm[,i],col="red")
	hist(mhm[,i], main=paste("Histogram for", colnames(mhm)[i]), xlab=colnames(mhm)[i], freq = FALSE)
 }
}

# Variància de les dades
# Test de Fligner-Killeen
fligner.test(Price ~ Type, data = mhm)
plot(Price ~ Type, data = mhm)
fligner.test(Price ~ Suburb, data = mhm)
fligner.test(Price ~ Regionname, data = mhm)

# Correlacio d'Spearman
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
for (i in 1:(ncol(mhm) - 1)) {
    if (is.integer(mhm[,i]) | is.numeric(mhm[,i])) {
        spearman_test = cor.test(mhm[,i], mhm[,length(mhm)], method = "spearman")
        corr_coef = spearman_test$estimate
        p_val = spearman_test$p.value
        # Add row to matrix
        pair = matrix(ncol = 2, nrow = 1)
        pair[1][1] = corr_coef
        pair[2][1] = p_val
        corr_matrix <- rbind(corr_matrix, pair)
        rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(mhm)[i]
    }
}
print(corr_matrix)

write.csv(mhm, "Mhm_data_clean.csv")


