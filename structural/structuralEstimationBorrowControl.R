
### Structural Estimation borrowControl, Classify Subjects According to Types

## last date 7/6/2022

### This code classifies participants into optimal, low lambda, high lambda

rm(list = ls())

# load data ----

mainData <- read.csv("C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/borrowingData.csv")

# for now, keep only control
table(mainData$treatment)
data_set <- subset(mainData, subset = treatment %in% "B Control")
rm(mainData)
# 320 observations, 320/4=80 subjects
data_set <- subset(data_set, !(subset = day %in% "Initial"))


# matrix preallocation to store all the results ----
MSE_lowDebt <- matrix(data=NA,nrow = length(unique(data_set$idturk)),ncol = 5)
colnames(MSE_lowDebt) <- c("participantid","treatment","zeroLambdaMSE","lowLambdaMSE","highLambdaMSE")

ids <- as.vector(unique(data_set$idturk))


for(i in 1:length(unique(data_set$idturk))) {
  aux <- data_set[data_set$idturk==ids[i],]

  # Let's start with just one subject
#aux <- data_set[data_set$idturk=="APRZ7BR8C0ZMQ",]

# 4 observations 

# tested A13OOAT2ORKH6V A143XRCI1YXAFE A154X03NKVZZL1 APRZ7BR8C0ZMQ


# keep only useful variables ----

myvars <- c("batch","day","idturk","treatment","initialendowment", "pointssaving1","pointssaving2",
            "pointssaving3","pointssaving4", "borrowsaving5","borrowsaving6",
"additionalpointssaving1","additionalpointssaving2","additionalpointssaving3",
"additionalpointssaving4",
"balance_ia_a1","balance_ia_a2","balance_ia_a3",  "balance_ia_a4",
"balance_borrow_a1","balance_borrow_a2",
"balance_aa_a1", "balance_aa_a2", "balance_aa_a3","balance_aa_a4") 


aux <- aux[myvars]

# Calculate Shares for each type ----

# original shares ----

aux$originalShare1 <- aux$pointssaving1/(aux$initialendowment+aux$borrowsaving5+aux$borrowsaving6)
aux$originalShare2 <- aux$pointssaving2/(aux$initialendowment+aux$borrowsaving5+aux$borrowsaving6)
aux$originalShare3 <- aux$pointssaving3/(aux$initialendowment+aux$borrowsaving5+aux$borrowsaving6)
aux$originalShare4 <- aux$pointssaving4/(aux$initialendowment+aux$borrowsaving5+aux$borrowsaving6)
aux$originalShareCheck <- aux$originalShare1+aux$originalShare2+aux$originalShare3+aux$originalShare4
table(aux$originalShareCheck)
# this should be all ones

aux$originalShare5[aux$day=="Day 1"] <- aux$borrowsaving5[aux$day=="Day 1"]/(900) 
aux$originalShare6[aux$day=="Day 1"] <- aux$borrowsaving6[aux$day=="Day 1"]/(1500) 

aux$originalShare5[aux$day=="Day 2"] <- aux$borrowsaving5[aux$day=="Day 2"]/
  (900-aux$balance_borrow_a1[aux$day=="Day 1"])
aux$originalShare5[aux$day=="Day 3"] <- aux$borrowsaving5[aux$day=="Day 3"]/
  (900-aux$balance_borrow_a1[aux$day=="Day 2"])
aux$originalShare5[aux$day=="Day 4"] <- aux$borrowsaving5[aux$day=="Day 4"]/
  (900-aux$balance_borrow_a1[aux$day=="Day 3"])

aux$originalShare6[aux$day=="Day 2"] <- aux$borrowsaving6[aux$day=="Day 2"]/
  (1500-aux$balance_borrow_a2[aux$day=="Day 1"])
aux$originalShare6[aux$day=="Day 3"] <- aux$borrowsaving6[aux$day=="Day 3"]/
  (1500-aux$balance_borrow_a2[aux$day=="Day 2"])
aux$originalShare6[aux$day=="Day 4"] <- aux$borrowsaving6[aux$day=="Day 4"]/
  (1500-aux$balance_borrow_a2[aux$day=="Day 3"])

#aux$originalShare5[aux$day!="Day 1"] <- aux$borrowsaving5[aux$day!="Day 1"]/(900-aux$balance_borrow_a1[aux$day!="Day 1"])
#aux$originalShare6[aux$day!="Day 1"] <- aux$borrowsaving6[aux$day!="Day 1"]/(1500-aux$balance_borrow_a2[aux$day!="Day 1"])

aux$originalShare5[aux$day!="Day 1" & is.na(aux$originalShare5)] <- 0
aux$originalShare6[aux$day!="Day 1" & is.na(aux$originalShare6)] <- 0


# Zero Lambda shares ----
aux$zeroLambdaShare1 <- 1
aux$zeroLambdaShare2 <- 0
aux$zeroLambdaShare3 <- 0
aux$zeroLambdaShare4 <- 0
aux$zeroLambdaShareCheck <- aux$zeroLambdaShare1+aux$zeroLambdaShare2+aux$zeroLambdaShare3+aux$zeroLambdaShare4
table(aux$zeroLambdaShareCheck)

aux$zeroLambdaShare5 <- 1
# after day 1, if balance is exahusted, the lambda share is 0
aux$zeroLambdaShare5[aux$day=="Day 2" & aux$balance_borrow_a1[aux$day=="Day 1"]==900] <- 0
aux$zeroLambdaShare5[aux$day=="Day 3" & aux$balance_borrow_a1[aux$day=="Day 2"]==900] <- 0
aux$zeroLambdaShare5[aux$day=="Day 4" & aux$balance_borrow_a1[aux$day=="Day 3"]==900] <- 0

aux$zeroLambdaShare6 <- 1
aux$zeroLambdaShare6[aux$day=="Day 2" & aux$balance_borrow_a2[aux$day=="Day 1"]==1500] <- 0
aux$zeroLambdaShare6[aux$day=="Day 3" & aux$balance_borrow_a2[aux$day=="Day 2"]==1500] <- 0
aux$zeroLambdaShare6[aux$day=="Day 4" & aux$balance_borrow_a2[aux$day=="Day 3"]==1500] <- 0


# Low Lambda shares ----

aux$lowLambdaShare1 <- 1
aux$lowLambdaShare2 <- 0
aux$lowLambdaShare3 <- 0
aux$lowLambdaShare4 <- 0

aux$lowLambdaShare5 <- 0
aux$lowLambdaShare6 <- 1
# if balance of debt2 is exahusted the day before, then the share is 0

aux$lowLambdaShare6[aux$day=="Day 2" & aux$balance_borrow_a2[aux$day=="Day 1"]==1500] <- 0
aux$lowLambdaShare6[aux$day=="Day 3" & aux$balance_borrow_a2[aux$day=="Day 2"]==1500] <- 0
aux$lowLambdaShare6[aux$day=="Day 4" & aux$balance_borrow_a2[aux$day=="Day 3"]==1500] <- 0

# High Lambda shares ----

aux$highLambdaShare1 <- 1
aux$highLambdaShare2 <- 0
aux$highLambdaShare3 <- 0
aux$highLambdaShare4 <- 0

aux$highLambdaShare5 <- 0
aux$highLambdaShare6 <- 0

# MSE ----

aux$zeroLambdaMSE <- NA
aux$lowLambdaMSE <- NA
aux$highLambdaMSE <- NA

aux$zeroLambdaMSE <- ((aux$originalShare1-aux$zeroLambdaShare1)^2+(aux$originalShare2-aux$zeroLambdaShare2)^2+
  (aux$originalShare3-aux$zeroLambdaShare3)^2+(aux$originalShare4-aux$zeroLambdaShare4)^2+
  (aux$originalShare5-aux$zeroLambdaShare5)^2+(aux$originalShare6-aux$zeroLambdaShare6)^2)/6

aux$lowLambdaMSE <- ((aux$originalShare1-aux$lowLambdaShare1)^2+(aux$originalShare2-aux$lowLambdaShare2)^2+
                        (aux$originalShare3-aux$lowLambdaShare3)^2+(aux$originalShare4-aux$lowLambdaShare4)^2+
                       (aux$originalShare5-aux$lowLambdaShare5)^2+(aux$originalShare6-aux$lowLambdaShare6)^2)/6

aux$highLambdaMSE <- ((aux$originalShare1-aux$highLambdaShare1)^2+(aux$originalShare2-aux$highLambdaShare2)^2+
                       (aux$originalShare3-aux$highLambdaShare3)^2+(aux$originalShare4-aux$highLambdaShare4)^2+
                        (aux$originalShare5-aux$highLambdaShare5)^2+(aux$originalShare6-aux$highLambdaShare6)^2)/6



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
MSE_lowDebt$treatment <- "Borrow Control"
MSE_lowDebt$zeroLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$zeroLambdaMSE))
MSE_lowDebt$lowLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$lowLambdaMSE))
MSE_lowDebt$highLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$highLambdaMSE))
mean(MSE_lowDebt$zeroLambdaMSE)
# 0.1276045
mean(MSE_lowDebt$lowLambdaMSE)
# 0.1138619
mean(MSE_lowDebt$highLambdaMSE)
# 0.119357

MSE_lowDebt$type <- NA
MSE_lowDebt$typeMSE <- NA

# find min value
# add epsilon to break ties toward zerolambda
MSE_lowDebt$typeMSE <- pmin(MSE_lowDebt$zeroLambdaMSE, MSE_lowDebt$lowLambdaMSE,  MSE_lowDebt$highLambdaMSE)

# type 1 zero, 2 low, 3 high
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$highLambdaMSE] <- 3
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$lowLambdaMSE] <- 2
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$zeroLambdaMSE] <- 1


table(MSE_lowDebt$type)
# 48 8 24
# 60% 10% 30%

table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.057232])/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=0.057232,])
# 35 2 2
# 90% 5% 5%

# Calculate avg. lambda ----
lowLambda <- 1.2/1.15
highLambda <- 1.2/1.05
# avg. lambda no thresold
aux <- as.vector(table(MSE_lowDebt$type))
meanLambda <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt)
# 1.0472

# avg. lambda with thresold
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<0.054]))
meanLambdaThreshold <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=0.054,])
# 1.009556

rm(meanLambda, meanLambdaThreshold)

# first thresold for a type 3 subject is 0.0767939801 

thresholdValues <- unique(MSE_lowDebt$typeMSE)
thresholdValues <- sort(thresholdValues)
lambdaMatrix <- matrix(data=NA,nrow = length(thresholdValues),ncol = 2)
colnames(lambdaMatrix) <- c("lambda","threshold")

threshold <- thresholdValues[3]
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=threshold]))


for (i in 1:length(thresholdValues)) {
threshold <- thresholdValues[i]
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=threshold]))
if (threshold<=0.0416666666 ) {
    lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold 
  } 
if (threshold>0.0416666666 ) {
    lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold 
  }
 
}

lambdaMatrix <- as.data.frame(lambdaMatrix)

write.table(lambdaMatrix, "C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/lambdaMatrixborrowControl.txt",
            sep="\t", row.names = F)


