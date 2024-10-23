
### Structural Estimation Low Debt, Classify Subjects According to Types

## last date 2/2/2022

### This code classifies participants into optimal, low lambda, high lambda

rm(list = ls())

# load data ----

mainData <- read.csv("C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/mainData.csv")

# for now, keep only low debt

data_set <- subset(mainData, subset = treatment %in% "Low Debt")
rm(mainData)
# 344 observations, 344/4=86 subjects
data_set <- subset(data_set, subset = day %in% "Day 4")

# Let's start with just one subject

# matrix preallocation to store all the results ----
MSE_lowDebt <- matrix(data=NA,nrow = length(unique(data_set$idturk)),ncol = 5)
colnames(MSE_lowDebt) <- c("participantid","treatment","zeroLambdaMSE","lowLambdaMSE","highLambdaMSE")

ids <- as.vector(unique(data_set$idturk))

for(i in 1:length(unique(data_set$idturk))) {
  aux <- data_set[data_set$idturk==ids[i],]

  
#aux <- subset(data_set, subset = idturk %in% "AJMUCGUKNKE74")
# 4 observations

# tested A11BSFO4LMHPXQ  A13FUEPWBCLBUY A16G6PPH1INQL8  A17K1CHOI773VZ  A1BNR1ZAF1QGWG A1BQCRF5Q76YFY
# tested A21HUPW67XA7UJ A239EUWY6SNDEZ AJMUCGUKNKE74


# keep only useful variables ----

myvars <- c("batch","day","idturk","treatment","initialendowment", 
            "oneshotcontrol_s1", "oneshotcontrol_s2", "oneshotcontrol_s3","oneshotcontrol_s4",
            "oneshotdebt_s1", "oneshotdebt_s2","oneshotdebt_d1","oneshotdebt_d2",
            "oneshothigh_s1", "oneshothigh_s2","oneshothigh_d1","oneshothigh_d2")

aux <- aux[myvars]

# change data form so each one shot scenario is one observation
aux2 <- aux
aux3 <- aux

aux <- rbind.data.frame(aux,aux2,aux3)
rm(aux2,aux3)

aux$day <- as.character(aux$day)
aux$day[1] <- "No Debt"
aux$day[2] <- "Low Debt"
aux$day[3] <- "High Debt"

# Create new balance variables ----

# format initial balances ----
aux$initialBalance1 <- NA
aux$initialBalance2 <- NA
aux$initialBalance3 <- NA
aux$initialBalance4 <- NA

aux$initialBalance1[aux$day=="No Debt"] <- 200
aux$initialBalance2[aux$day=="No Debt"] <- 100
aux$initialBalance3[aux$day=="No Debt"] <- 300
aux$initialBalance4[aux$day=="No Debt"] <- 200

aux$initialBalance1[aux$day=="Low Debt"] <- 1000
aux$initialBalance2[aux$day=="Low Debt"] <- 1200
aux$initialBalance3[aux$day=="Low Debt"] <- -600
aux$initialBalance4[aux$day=="Low Debt"] <- -800

aux$initialBalance1[aux$day=="High Debt"] <- 2000
aux$initialBalance2[aux$day=="High Debt"] <- 2400
aux$initialBalance3[aux$day=="High Debt"] <- -1700
aux$initialBalance4[aux$day=="High Debt"] <- -1900

aux$initialendowment <- 1000

# ormat points allocated
aux$pointssaving1[aux$day=="No Debt"] <- aux$oneshotcontrol_s1[aux$day=="No Debt"]
aux$pointssaving2[aux$day=="No Debt"] <- aux$oneshotcontrol_s2[aux$day=="No Debt"]
aux$pointsdebt1[aux$day=="No Debt"] <- aux$oneshotcontrol_s3[aux$day=="No Debt"]
aux$pointsdebt2[aux$day=="No Debt"] <- aux$oneshotcontrol_s4[aux$day=="No Debt"]

aux$pointssaving1[aux$day=="Low Debt"] <- aux$oneshotdebt_s1[aux$day=="Low Debt"]
aux$pointssaving2[aux$day=="Low Debt"] <- aux$oneshotdebt_s2[aux$day=="Low Debt"]
aux$pointsdebt1[aux$day=="Low Debt"] <- aux$oneshotdebt_d1[aux$day=="Low Debt"]
aux$pointsdebt2[aux$day=="Low Debt"] <- aux$oneshotdebt_d2[aux$day=="Low Debt"]

aux$pointssaving1[aux$day=="High Debt"] <- aux$oneshothigh_s1[aux$day=="High Debt"]
aux$pointssaving2[aux$day=="High Debt"] <- aux$oneshothigh_s2[aux$day=="High Debt"]
aux$pointsdebt1[aux$day=="High Debt"] <- aux$oneshothigh_d1[aux$day=="High Debt"]
aux$pointsdebt2[aux$day=="High Debt"] <- aux$oneshothigh_d2[aux$day=="High Debt"]

# best fit across all three decisions, although no debt does not allow to distingish between models
# it is useful to identify noisyness.
# I shoudl also look at allocation in the one shot treatments, compare low debt account 1 and 2

myvars <- c("batch","day","idturk","initialendowment",
            "initialBalance1", "initialBalance2", "initialBalance3","initialBalance4",
            "pointssaving1", "pointssaving2","pointsdebt1","pointsdebt2")
aux <- aux[myvars]
# 12 variables

# Calculate Shares for each type ----

# original shares ----
aux$originalShare1 <- aux$pointssaving1/aux$initialendowment
aux$originalShare2 <- aux$pointssaving2/aux$initialendowment
aux$originalShare3 <- aux$pointsdebt1/aux$initialendowment
aux$originalShare4 <- aux$pointsdebt2/aux$initialendowment
aux$originalShareCheck <- aux$originalShare1+aux$originalShare2+aux$originalShare3+aux$originalShare4
table(aux$originalShareCheck)

# this should be all ones

# Zero Lambda shares ----
aux$zeroLambdaShare1 <- 1
aux$zeroLambdaShare2 <- 0
aux$zeroLambdaShare3 <- 0
aux$zeroLambdaShare4 <- 0
aux$zeroLambdaShareCheck <- aux$zeroLambdaShare1+aux$zeroLambdaShare2+aux$zeroLambdaShare3+aux$zeroLambdaShare4
table(aux$zeroLambdaShareCheck)

# Low Lambda shares  ----

aux$lowLambdaShare1 <- NA
aux$lowLambdaShare2 <- NA
aux$lowLambdaShare3 <- NA
aux$lowLambdaShare4 <- NA

aux$lowLambdaShare2 <- 0
aux$lowLambdaShare4 <- 0

# No Debt
aux$lowLambdaShare1[aux$day=="No Debt"] <- 1
aux$lowLambdaShare3[aux$day=="No Debt"] <- 0
# Low Debt
aux$lowLambdaShare1[aux$day=="Low Debt"] <- 0.4
aux$lowLambdaShare3[aux$day=="Low Debt"] <- 0.6
# High Debt
aux$lowLambdaShare1[aux$day=="High Debt"] <- 0
aux$lowLambdaShare3[aux$day=="High Debt"] <- 1

# share check
aux$lowLambdaShareCheck <- aux$lowLambdaShare1+aux$lowLambdaShare2+aux$lowLambdaShare3+aux$lowLambdaShare4
table(aux$lowLambdaShareCheck)

# High Lambda shares, 1 mixed case ----

aux$highLambdaShare1 <- NA
aux$highLambdaShare2 <- NA
aux$highLambdaShare3 <- NA
aux$highLambdaShare4 <- NA

aux$highLambdaShare2 <- 0

# No Debt
aux$highLambdaShare1[aux$day=="No Debt"] <- 1
aux$highLambdaShare3[aux$day=="No Debt"] <- 0
aux$highLambdaShare4[aux$day=="No Debt"] <- 0
# Low Debt
aux$highLambdaShare1[aux$day=="Low Debt"] <- 0
aux$highLambdaShare3[aux$day=="Low Debt"] <- 0.6
aux$highLambdaShare4[aux$day=="Low Debt"] <- 0.4
# High Debt
aux$highLambdaShare1[aux$day=="High Debt"] <- 0
aux$highLambdaShare3[aux$day=="High Debt"] <- 1
aux$highLambdaShare4[aux$day=="High Debt"] <- 0

# share check
aux$highLambdaShareCheck <- aux$highLambdaShare1+aux$highLambdaShare2+aux$highLambdaShare3+aux$highLambdaShare4
table(aux$highLambdaShareCheck)

# MSE ----

aux$zeroLambdaMSE <- NA
aux$lowLambdaMSE <- NA
aux$highLambdaMSE <- NA

aux$zeroLambdaMSE <- ((aux$originalShare1-aux$zeroLambdaShare1)^2+(aux$originalShare2-aux$zeroLambdaShare2)^2+
  (aux$originalShare3-aux$zeroLambdaShare3)^2+(aux$originalShare4-aux$zeroLambdaShare4)^2)/4

aux$lowLambdaMSE <- ((aux$originalShare1-aux$lowLambdaShare1)^2+(aux$originalShare2-aux$lowLambdaShare2)^2+
                        (aux$originalShare3-aux$lowLambdaShare3)^2+(aux$originalShare4-aux$lowLambdaShare4)^2)/4

aux$highLambdaMSE <- ((aux$originalShare1-aux$highLambdaShare1)^2+(aux$originalShare2-aux$highLambdaShare2)^2+
                       (aux$originalShare3-aux$highLambdaShare3)^2+(aux$originalShare4-aux$highLambdaShare4)^2)/4



MSE_lowDebt[i,1] <- as.vector(aux$idturk[1])
MSE_lowDebt[i,3] <- mean(aux$zeroLambdaMSE)
# 0.1418336
MSE_lowDebt[i,4] <- mean(aux$lowLambdaMSE)
# 0.0010521
MSE_lowDebt[i,5] <- mean(aux$highLambdaMSE)
# 0.3084021

# so far it works well

}

# find best fit ----
rm (aux, i, ids, myvars)

MSE_lowDebt <- as.data.frame(MSE_lowDebt)
MSE_lowDebt$treatment <- "Low Debt"
MSE_lowDebt$zeroLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$zeroLambdaMSE))
MSE_lowDebt$lowLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$lowLambdaMSE))
MSE_lowDebt$highLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$highLambdaMSE))
mean(MSE_lowDebt$zeroLambdaMSE)
# 0.1071026
mean(MSE_lowDebt$lowLambdaMSE)
# 0.1469607
mean(MSE_lowDebt$highLambdaMSE)
# 0.185253

MSE_lowDebt$type <- NA
MSE_lowDebt$typeMSE <- NA

# find min value
MSE_lowDebt$typeMSE <- pmin(MSE_lowDebt$zeroLambdaMSE, MSE_lowDebt$lowLambdaMSE,  MSE_lowDebt$highLambdaMSE)

# type 1 zero, 2 low, 3 high
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$zeroLambdaMSE] <- 1
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$lowLambdaMSE] <- 2
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$highLambdaMSE] <- 3

table(MSE_lowDebt$type)
# 44 22 20
# 51% 25% 23%

# 49 28 9
# 56% 33% 10%
# probably very high correlation since type distribution is so similar

save(MSE_lowDebt, file = "C:/Users/Alejandro/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/oneShotTypes.RData")



# Calculate avg. lambda ----
aux <- as.vector(table(MSE_lowDebt$type))

lowLambda <- 1.2/1.15
highLambda <- 1.2/1.05
# avg. lambda no thresold
meanLambda <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt)
# 1.029106

# avg. lambda with thresold
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.054]))

meanLambdaThreshold <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=0.054,])
# 1.02559

thresholdValues <- unique(MSE_lowDebt$typeMSE)
thresholdValues <- sort(thresholdValues)
lambdaMatrix <- matrix(data=NA,nrow = length(thresholdValues),ncol = 2)
colnames(lambdaMatrix) <- c("lambda","threshold")


for (i in 1:length(thresholdValues)) {
  threshold <- thresholdValues[i]
  aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=threshold]))
  lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
  lambdaMatrix[i,2] <- threshold
}


lambdaMatrix <- as.data.frame(lambdaMatrix)

write.table(lambdaMatrix, "C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/lambdaMatrixOneShot.txt",
            sep="\t", row.names = F)

