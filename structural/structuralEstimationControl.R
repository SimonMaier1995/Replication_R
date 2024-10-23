
### Structural Estimation Low Debt, Classify Subjects According to Types

## last date 2/2/2022

### This code classifies participants into optimal, low lambda, high lambda

rm(list = ls())

# load data ----

mainData <- read.csv("C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/mainData.csv")

# keep only no debt

data_set <- subset(mainData, subset = treatment %in% "No Debt")
rm(mainData)
# 344 observations, 344/4=86 subjects
data_set <- subset(data_set, !(subset = day %in% "Initial"))

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

myvars <- c("batch","day","idturk","treatment","initialendowment", "pointssaving1","pointssaving2",
            "pointssaving3","pointssaving4","pointsdebt1","pointsdebt2", "pointsthisdecision","pointsnextdecision",
            "additionalpointssaving1","additionalpointssaving2","additionalpointssaving3","additionalpointssaving4",
            "additionalpointsdebt1", "additionalpointsdebt2","balance_ia_a1","balance_ia_a2","balance_ia_a3",
            "balance_ia_a4", "balance_aa_a1", "balance_aa_a2", "balance_aa_a3","balance_aa_a4")

aux <- aux[myvars]


# Create new balance variables ----

# initial balances ----
aux$initialBalance1 <- NA
aux$initialBalance2 <- NA
aux$initialBalance3 <- NA
aux$initialBalance4 <- NA

aux$initialBalance1[aux$day=="Day 1"] <- 1100
aux$initialBalance2[aux$day=="Day 1"] <- 700
aux$initialBalance3[aux$day=="Day 1"] <- 900
aux$initialBalance4[aux$day=="Day 1"] <- 1500

aux$initialBalance1[aux$day=="Day 2"] <- aux$balance_aa_a1[aux$day=="Day 1"]
aux$initialBalance2[aux$day=="Day 2"] <- aux$balance_aa_a2[aux$day=="Day 1"]
aux$initialBalance3[aux$day=="Day 2"] <- aux$balance_aa_a3[aux$day=="Day 1"]
aux$initialBalance4[aux$day=="Day 2"] <- aux$balance_aa_a4[aux$day=="Day 1"]

aux$initialBalance1[aux$day=="Day 3"] <- aux$balance_aa_a1[aux$day=="Day 2"]
aux$initialBalance2[aux$day=="Day 3"] <- aux$balance_aa_a2[aux$day=="Day 2"]
aux$initialBalance3[aux$day=="Day 3"] <- aux$balance_aa_a3[aux$day=="Day 2"]
aux$initialBalance4[aux$day=="Day 3"] <- aux$balance_aa_a4[aux$day=="Day 2"]

aux$initialBalance1[aux$day=="Day 4"] <- aux$balance_aa_a1[aux$day=="Day 3"]
aux$initialBalance2[aux$day=="Day 4"] <- aux$balance_aa_a2[aux$day=="Day 3"]
aux$initialBalance3[aux$day=="Day 4"] <- aux$balance_aa_a3[aux$day=="Day 3"]
aux$initialBalance4[aux$day=="Day 4"] <- aux$balance_aa_a4[aux$day=="Day 3"]

# no debt adjustment
aux$initialBalance3 <- aux$initialBalance3-1800
aux$initialBalance4 <- aux$initialBalance4-3000

# additional balances 

aux$additionalBalance1 <- NA
aux$additionalBalance2 <- NA
aux$additionalBalance3 <- NA
aux$additionalBalance4 <- NA

aux$additionalBalance1 <- aux$balance_ia_a1
aux$additionalBalance2 <- aux$balance_ia_a2
aux$additionalBalance3 <- aux$balance_ia_a3
aux$additionalBalance4 <- aux$balance_ia_a4

myvars <- c("batch","day","idturk","treatment","initialendowment", "pointssaving1","pointssaving2",
            "pointssaving3","pointssaving4","pointsdebt1","pointsdebt2", "pointsthisdecision","pointsnextdecision",
            "additionalpointssaving1","additionalpointssaving2","additionalpointssaving3","additionalpointssaving4",
            "additionalpointsdebt1", "additionalpointsdebt2","initialBalance1","initialBalance2","initialBalance3",
            "initialBalance4", "additionalBalance1", "additionalBalance2", "additionalBalance3","additionalBalance4")

aux <- aux[myvars]

# Calculate Shares for each type ----

# original shares ----
aux$originalShare1 <- aux$pointssaving1/aux$initialendowment
aux$originalShare2 <- aux$pointssaving2/aux$initialendowment
aux$originalShare3 <- aux$pointssaving3/aux$initialendowment
aux$originalShare4 <- aux$pointssaving4/aux$initialendowment
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

# Low Lambda shares, 1 mixed case  ----

aux$lowLambdaShare1 <- NA
aux$lowLambdaShare2 <- NA
aux$lowLambdaShare3 <- NA
aux$lowLambdaShare4 <- NA

aux$lowLambdaShare2 <- 0
aux$lowLambdaShare4 <- 0

# Day 1 is always the same
aux$lowLambdaShare3[aux$day=="Day 1"] <- 1
aux$lowLambdaShare1[aux$day=="Day 1"] <- 1- aux$lowLambdaShare3[aux$day=="Day 1"]

# Case when no debt balance in debt 1
aux$lowLambdaShare3[aux$initialBalance3 >= 0] <- 0
aux$lowLambdaShare1[aux$initialBalance3 >= 0] <- 1- aux$lowLambdaShare3[aux$initialBalance3 >= 0]


# Case when debt balance is bigger than endowment
aux$lowLambdaShare3[abs(aux$initialBalance3)>=aux$initialendowment & aux$initialBalance3 < 0] <- 1
aux$lowLambdaShare1[abs(aux$initialBalance3)>=aux$initialendowment & aux$initialBalance3 < 0] <- 
  1 - aux$lowLambdaShare3[abs(aux$initialBalance3)>=aux$initialendowment & aux$initialBalance3 < 0]

 
# Case when debt balance is smaller than endowment but still below 0

# abs(aux$initialBalance3) < aux$initialendowment & aux$initialBalance3<0 & aux$day!="Day 1"
  aux$lowLambdaShare3[abs(aux$initialBalance3) < aux$initialendowment & aux$initialBalance3<0 & aux$day!="Day 1"] <- 
    abs(aux$initialBalance3[abs(aux$initialBalance3) < aux$initialendowment & aux$initialBalance3<0 & aux$day!="Day 1"])/
    aux$initialendowment[abs(aux$initialBalance3) < aux$initialendowment & aux$initialBalance3<0 & aux$day!="Day 1"]
  
  aux$lowLambdaShare1[abs(aux$initialBalance3) < aux$initialendowment & aux$initialBalance3<0 & aux$day!="Day 1"] <- 
    1- aux$lowLambdaShare3[abs(aux$initialBalance3) < aux$initialendowment & aux$initialBalance3<0 & aux$day!="Day 1"]

# share check
aux$lowLambdaShareCheck <- aux$lowLambdaShare1+aux$lowLambdaShare2+aux$lowLambdaShare3+aux$lowLambdaShare4
table(aux$lowLambdaShareCheck)


# High Lambda shares, 2 mixed cases  ----

aux$highLambdaShare1 <- NA
aux$highLambdaShare2 <- NA
aux$highLambdaShare3 <- NA
aux$highLambdaShare4 <- NA

aux$highLambdaShare2 <- 0

# Day 1 is always the same, all in 
aux$highLambdaShare1[aux$day=="Day 1"] <- 0
aux$highLambdaShare3[aux$day=="Day 1"] <- 1
aux$highLambdaShare4[aux$day=="Day 1"] <- 0


# (1) Case 1 d1=0 and d2=0    No debt remaining ----
aux$highLambdaShare1[aux$initialBalance3 >= 0 & aux$initialBalance4>=0] <- 1
aux$highLambdaShare3[aux$initialBalance3 >= 0 & aux$initialBalance4>=0] <- 0
aux$highLambdaShare4[aux$initialBalance3 >= 0 & aux$initialBalance4>=0] <- 0


# (2) Case 2 d1>0 ----


# (2.1) d1>>0 Case when large debt balance in debt 1, corner solution
aux$highLambdaShare1[aux$initialBalance3 < 0 & abs(aux$initialBalance3)>=aux$initialendowment] <- 0
aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)>=aux$initialendowment] <- 1
aux$highLambdaShare4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)>=aux$initialendowment] <- 0

# (2.2) d1>0 & d2<=0 Case when small debt balance in debt 1 and debt 2 <=0, mixed strategy

aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment & aux$initialBalance4 >= 0] <- 
  abs(aux$initialBalance3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment & aux$initialBalance4 >= 0])/
  aux$initialendowment[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment & aux$initialBalance4 >= 0]

aux$highLambdaShare4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment & aux$initialBalance4 >= 0] <- 0

aux$highLambdaShare1[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment & aux$initialBalance4 >= 0] <- 
  1 - aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment & aux$initialBalance4 >= 0]


# (2.3) d1>0 and d2>>0 Case when small debt balance in debt 1 and balance in debt 2 large, mixed strategy

aux$highLambdaShare1[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)>=aux$initialendowment ] <- 0

aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)>=aux$initialendowment ] <- 
  abs(aux$initialBalance3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                            aux$initialBalance4 < 0 & abs(aux$initialBalance4)>=aux$initialendowment ])/
  aux$initialendowment[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
    aux$initialBalance4 < 0 & abs(aux$initialBalance4)>=aux$initialendowment ]

aux$highLambdaShare4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)>=aux$initialendowment ] <- 
  1-aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                           aux$initialBalance4 < 0 & abs(aux$initialBalance4)>=aux$initialendowment ]

# (2.4) d1>0 and d2>0 Case when small debt balance in debt 1 and small balance in debt 2, mixed strategy

# two cases d1+d2>endowment and d1+d2<endowment


# case 1: d1+d2>=endowment, then x1 is zero
aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment & 
                       (abs(aux$initialBalance3)+abs(aux$initialBalance4))>=aux$initialendowment]<- 
  abs(aux$initialBalance3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                            aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment & 
                            (abs(aux$initialBalance3)+abs(aux$initialBalance4))>=aux$initialendowment])/
  aux$initialendowment[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                         aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment & 
                         (abs(aux$initialBalance3)+abs(aux$initialBalance4))>=aux$initialendowment]

aux$highLambdaShare4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment & 
                       (abs(aux$initialBalance3)+abs(aux$initialBalance4))>=aux$initialendowment] <- 
  1-aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                           aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment & 
                           (abs(aux$initialBalance3)+abs(aux$initialBalance4))>=aux$initialendowment]

aux$highLambdaShare1[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment & 
                       (abs(aux$initialBalance3)+abs(aux$initialBalance4))>=aux$initialendowment] <- 0


# case 2: d1+d2<endowment, then x1 is non zero
aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                       (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment] <- 
  abs(aux$initialBalance3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                            aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                            (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment])/
  aux$initialendowment[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                         aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                         (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment]

aux$highLambdaShare4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                       (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment] <- 
  abs(aux$initialBalance4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                            aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                            (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment])/
  aux$initialendowment[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                         aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                         (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment]

aux$highLambdaShare1[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                       aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                       (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment] <- 
  1-aux$highLambdaShare3[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                           aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                           (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment] -
  aux$highLambdaShare4[aux$initialBalance3 < 0 & abs(aux$initialBalance3)<aux$initialendowment &
                         aux$initialBalance4 < 0 & abs(aux$initialBalance4)<aux$initialendowment &
                         (abs(aux$initialBalance3)+abs(aux$initialBalance4))<aux$initialendowment]

# (3) Case 3 Remaining cases when, d2>0 ----

# (3.1) d1=0 & d2>>0,  no debt balance in debt 1 and big enough balance in debt 2, corner solution
aux$highLambdaShare1[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)>=aux$initialendowment] <- 0
aux$highLambdaShare3[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)>=aux$initialendowment] <- 0
aux$highLambdaShare4[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)>=aux$initialendowment] <- 1

# (3.2) d1=0 & d2>0, no debt balance in debt 1 and small balance in debt 2,  mixed strategy
aux$highLambdaShare4[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)<aux$initialendowment & aux$initialBalance4<0] <- 
  abs(aux$initialBalance4[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)<aux$initialendowment & aux$initialBalance4<0])/aux$initialendowment[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)<aux$initialendowment & aux$initialBalance4<0]

aux$highLambdaShare3[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)<aux$initialendowment & aux$initialBalance4<0] <- 0

aux$highLambdaShare1[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)<aux$initialendowment & aux$initialBalance4<0] <- 
  1- aux$highLambdaShare4[aux$initialBalance3 >= 0 & abs(aux$initialBalance4)<aux$initialendowment & aux$initialBalance4<0] 

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
# 0.05988828
mean(MSE_lowDebt$lowLambdaMSE)
# 0.2576355
mean(MSE_lowDebt$highLambdaMSE)
# 0.3252143

MSE_lowDebt$type <- NA
MSE_lowDebt$typeMSE <- NA

# find min value
MSE_lowDebt$typeMSE <- pmin(MSE_lowDebt$zeroLambdaMSE, MSE_lowDebt$lowLambdaMSE,  MSE_lowDebt$highLambdaMSE)

# type 1 zero, 2 low, 3 high
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$highLambdaMSE] <- 3
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$lowLambdaMSE] <- 2
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$zeroLambdaMSE] <- 1


table(MSE_lowDebt$type)
# 65 12 9
# 75% 14% 10%

table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.0546993563])/nrow(MSE_lowDebt)
# 52 1
# 60% 1%

# Calculate avg. lambda ----

lowLambda <- 1.2/1.15
highLambda <- 1.2/1.05
# avg. lambda no thresold
aux <- as.vector(table(MSE_lowDebt$type))
meanLambda <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt)
# 1.021017

# avg. lambda with thresold
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<0.054]))
# 1


# check random threshold
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.054]))

meanLambdaThreshold <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=0.054,])
#1.00433
rm(meanLambda, meanLambdaThreshold)

# first thresold for a type 3 subject is 0.0767939801 

thresholdValues <- unique(MSE_lowDebt$typeMSE)
thresholdValues <- sort(thresholdValues)
lambdaMatrix <- matrix(data=NA,nrow = length(thresholdValues),ncol = 2)
colnames(lambdaMatrix) <- c("lambda","threshold")


for (i in 1:length(thresholdValues)) {
threshold <- thresholdValues[i]
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=threshold]))
  if (threshold<0.054) {
    lambdaMatrix[i,1] <- (aux[1]*1)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold 
  } 


if (threshold<=0767 & threshold>0.054) {
    lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*lowLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold 
  } 
if (threshold>=0.0767 ) {
    lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold 
  }
 
}

lambdaMatrix <- as.data.frame(lambdaMatrix)

write.table(lambdaMatrix, "C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/lambdaMatrixControl.txt",
            sep="\t", row.names = F)

