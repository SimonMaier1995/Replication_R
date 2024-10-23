
### Structural Estimation Low Debt, Classify Subjects According to Types

## last date 8/3/2022

### This code classifies participants into optimal, low lambda, high lambda

rm(list = ls())

# load data ----

mainData <- read.csv("C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/redistributionData.csv")
#mainData <- read.csv("C:/Users/Alejandro/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/redistributionData.csv")

# for now, keep only low debt
table(mainData$treatment)
data_set <- subset(mainData, subset = treatment %in% "No Debt")
rm(mainData)
data_set <- subset(data_set, !(subset = day %in% 0))

# 324 observations, 324/4=81 subjects

# Let's start with just one subject

# matrix preallocation to store all the results ----
MSE_lowDebt <- matrix(data=NA,nrow = length(unique(data_set$idturk)),ncol = 5)
colnames(MSE_lowDebt) <- c("participantid","treatment","zeroLambdaMSE","lowLambdaMSE","highLambdaMSE")

ids <- as.vector(unique(data_set$idturk))


for(i in 1:length(unique(data_set$idturk))) {
  aux <- data_set[data_set$idturk==ids[i],]

  
  
#aux <- subset(data_set, subset = idturk %in% "A11S8IAAVDXCUS")
# 4 observations
 
# excel  
# tested 

# keep only useful variables ----

myvars <- c("batch","day","idturk","treatment","initialendowment", "initialendowmentupdated", 
            "pointssaving1","pointssaving2","pointssaving3", "pointssaving4","balance_a1","balance_a2",
            "balance_a3","balance_a4")

aux <- aux[myvars]

# df = dataframe
# old.var.name = The name you don't like anymore
# new.var.name = The name you want to get

names(aux)[names(aux) == 'pointssaving3'] <- 'pointsdebt1'
names(aux)[names(aux) == 'pointssaving4'] <- 'pointsdebt2'


# Create new balance variables ----

# change day variable
aux$day[aux$day==1] <- "Day 1"
aux$day[aux$day==2] <- "Day 2"
aux$day[aux$day==3] <- "Day 3"
aux$day[aux$day==4] <- "Day 4"


# initial balances ---
aux$initialBalance1 <- NA
aux$initialBalance2 <- NA
aux$initialBalance3 <- NA
aux$initialBalance4 <- NA

aux$initialBalance1[aux$day=="Day 1"] <- 2000
aux$initialBalance2[aux$day=="Day 1"] <- 400
aux$initialBalance3[aux$day=="Day 1"] <- 600
aux$initialBalance4[aux$day=="Day 1"] <- 4300

aux$initialBalance1[aux$day=="Day 2"] <- aux$balance_a1[aux$day=="Day 1"]
aux$initialBalance2[aux$day=="Day 2"] <- aux$balance_a2[aux$day=="Day 1"]
aux$initialBalance3[aux$day=="Day 2"] <- aux$balance_a3[aux$day=="Day 1"]
aux$initialBalance4[aux$day=="Day 2"] <- aux$balance_a4[aux$day=="Day 1"]

aux$initialBalance1[aux$day=="Day 3"] <- aux$balance_a1[aux$day=="Day 2"]
aux$initialBalance2[aux$day=="Day 3"] <- aux$balance_a2[aux$day=="Day 2"]
aux$initialBalance3[aux$day=="Day 3"] <- aux$balance_a3[aux$day=="Day 2"]
aux$initialBalance4[aux$day=="Day 3"] <- aux$balance_a4[aux$day=="Day 2"]

aux$initialBalance1[aux$day=="Day 4"] <- aux$balance_a1[aux$day=="Day 3"]
aux$initialBalance2[aux$day=="Day 4"] <- aux$balance_a2[aux$day=="Day 3"]
aux$initialBalance3[aux$day=="Day 4"] <- aux$balance_a3[aux$day=="Day 3"]
aux$initialBalance4[aux$day=="Day 4"] <- aux$balance_a4[aux$day=="Day 3"]

# no debt adjustment
aux$initialBalance3 <- aux$initialBalance3-1200
aux$initialBalance4 <- aux$initialBalance4-8600


# final balances 

aux$finalBalance1 <- aux$balance_a1
aux$finalBalance2 <- aux$balance_a2
aux$finalBalance3 <- aux$balance_a3
aux$finalBalance4 <- aux$balance_a4

# no debt adjustment
aux$finalBalance3 <- aux$finalBalance3-1200
aux$finalBalance4 <- aux$finalBalance4-8600


# allocation 

aux$allocation1 <- aux$pointssaving1
aux$allocation2 <- aux$pointssaving2
aux$allocation3 <- aux$pointsdebt1
aux$allocation4 <- aux$pointsdebt2

# allocation auxiliar

aux$allocationAux1 <- aux$finalBalance1-aux$initialBalance1
aux$allocationAux2 <- aux$finalBalance2-aux$initialBalance2
aux$allocationAux3 <- aux$finalBalance3-aux$initialBalance3
aux$allocationAux4 <- aux$finalBalance4-aux$initialBalance4

# borrowed amount
aux$borrowedAmount <- NA

aux$borrow1 <- NA
aux$borrow1[aux$allocationAux1<0] <- aux$allocationAux1[aux$allocationAux1<0]
aux$borrow1[aux$allocationAux1>=0] <- 0
aux$borrow2 <- NA
aux$borrow2[aux$allocationAux2<0] <- aux$allocationAux2[aux$allocationAux2<0]
aux$borrow2[aux$allocationAux2>=0] <- 0
aux$borrowedAmount <- aux$borrow1+aux$borrow2

# endowment auxiliar
aux$endowmentAux <- NA
aux$endowmentAux <- aux$initialendowment-aux$borrowedAmount


myvars <- c("day","idturk","treatment","initialendowment", "initialBalance1", "initialBalance2", "initialBalance3","initialBalance4",
            "finalBalance1", "finalBalance2", "finalBalance3", "finalBalance4",
            "allocation1", "allocation2", "allocation3", "allocation4", "endowmentAux",
            "allocationAux1","allocationAux2","allocationAux3","allocationAux4","borrowedAmount")
            
aux <- aux[myvars]
# 22 variables

# Calculate Shares for each type ----

# original shares ----

aux$originalShare1 <- NA
aux$originalShare2 <- NA
aux$originalShare3 <- NA
aux$originalShare4 <- NA

aux$originalShare1[aux$allocationAux1<0] <-  aux$allocationAux1[aux$allocationAux1<0]/aux$initialBalance1[aux$allocationAux1<0]
aux$originalShare1[aux$allocationAux1>=0] <-  aux$allocationAux1[aux$allocationAux1>=0]/aux$endowmentAux[aux$allocationAux1>=0]

aux$originalShare2[aux$allocationAux2<0] <-  aux$allocationAux2[aux$allocationAux2<0]/aux$initialBalance2[aux$allocationAux2<0]
aux$originalShare2[aux$allocationAux2>=0] <-  aux$allocationAux2[aux$allocationAux2>=0] /aux$endowmentAux[aux$allocationAux2>=0] 

aux$originalShare3 <-  aux$allocationAux3/aux$endowmentAux
aux$originalShare4 <-  aux$allocationAux4/aux$endowmentAux


aux$originalShareCheck <- aux$originalShare1+ aux$originalShare2+aux$originalShare3+aux$originalShare4
if( aux$originalShare1[aux$day=="Day 1"]<0)
{aux$originalShareCheck[aux$day=="Day 1"] <- aux$originalShareCheck[aux$day=="Day 1"] +aux$originalShare1[aux$day=="Day 1"]
}
if( aux$originalShare2[aux$day=="Day 1"]<0)
{aux$originalShareCheck[aux$day=="Day 1"] <- aux$originalShareCheck[aux$day=="Day 1"] -aux$originalShare2[aux$day=="Day 1"]
}
if( aux$originalShare1[aux$day=="Day 2"]<0)
{aux$originalShareCheck[aux$day=="Day 2"] <- aux$originalShareCheck[aux$day=="Day 2"] +aux$originalShare1[aux$day=="Day 2"]
}
if( aux$originalShare2[aux$day=="Day 2"]<0)
{aux$originalShareCheck[aux$day=="Day 2"] <- aux$originalShareCheck[aux$day=="Day 2"] -aux$originalShare2[aux$day=="Day 2"]
}
if( aux$originalShare1[aux$day=="Day 3"]<0)
{aux$originalShareCheck[aux$day=="Day 3"] <- aux$originalShareCheck[aux$day=="Day 3"] +aux$originalShare1[aux$day=="Day 3"]
}
if( aux$originalShare2[aux$day=="Day 3"]<0)
{aux$originalShareCheck[aux$day=="Day 3"] <- aux$originalShareCheck[aux$day=="Day 3"] -aux$originalShare2[aux$day=="Day 3"]
}
if( aux$originalShare1[aux$day=="Day 4"]<0)
{aux$originalShareCheck[aux$day=="Day 4"] <- aux$originalShareCheck[aux$day=="Day 4"] +aux$originalShare1[aux$day=="Day 4"]
}
if( aux$originalShare2[aux$day=="Day 4"]<0)
{aux$originalShareCheck[aux$day=="Day 4"] <- aux$originalShareCheck[aux$day=="Day 4"] -aux$originalShare2[aux$day=="Day 4"]
}

table(aux$originalShareCheck)
# this should not necessary be all ones, actually it depends on the redistribution

# Zero Lambda shares ----
aux$zeroLambdaShare1 <- 1
aux$zeroLambdaShare2 <- NA
aux$zeroLambdaShare3 <- 0
aux$zeroLambdaShare4 <- 0

aux$zeroLambdaShare2[aux$initialBalance2>0] <- -1
aux$zeroLambdaShare2[aux$initialBalance2==0] <- 0

aux$zeroLambdaShareCheck <- aux$zeroLambdaShare1+aux$zeroLambdaShare2+aux$zeroLambdaShare3+aux$zeroLambdaShare4


if(aux$zeroLambdaShare2==-1)
{aux$zeroLambdaShareCheck[aux$zeroLambdaShare2==-1] <- aux$zeroLambdaShareCheck[aux$zeroLambdaShare2==-1]-aux$zeroLambdaShare2[aux$zeroLambdaShare2==-1]
}
table(aux$zeroLambdaShareCheck)

# Low Lambda shares, 1 mixed case  ----

aux$lowLambdaShare1 <- NA
aux$lowLambdaShare2 <- NA
aux$lowLambdaShare3 <- NA
aux$lowLambdaShare4 <- 0

aux$lowLambdaShare2[aux$initialBalance2>0] <- -1
aux$lowLambdaShare2[aux$initialBalance2==0] <- 0

# Case 3 d1>=s1+s2+endowment (full borrow from s1 and s2)

aux$lowLambdaShare1[-aux$initialBalance3 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment] <- -1
aux$lowLambdaShare3[-aux$initialBalance3 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment] <- 1

# Case 1 s1+s2+endowment>d1>=s2+endowment (partial borrow from s1 and full from s2)

aux$lowLambdaShare3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment > -aux$initialBalance3 & -aux$initialBalance3>= aux$initialBalance2+aux$initialendowment] <- 1
aux$lowLambdaShare1[aux$initialBalance1+aux$initialBalance2+aux$initialendowment > -aux$initialBalance3 & -aux$initialBalance3>= aux$initialBalance2+aux$initialendowment] <- (-aux$initialBalance3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment > -aux$initialBalance3 & -aux$initialBalance3>= aux$initialBalance2+aux$initialendowment]
    -aux$initialBalance2[aux$initialBalance1+aux$initialBalance2+aux$initialendowment > -aux$initialBalance3 & -aux$initialBalance3>= aux$initialBalance2+aux$initialendowment]
    -aux$initialendowment[aux$initialBalance1+aux$initialBalance2+aux$initialendowment > -aux$initialBalance3 & -aux$initialBalance3>= aux$initialBalance2+aux$initialendowment]
    )/-aux$initialBalance1[aux$initialBalance1+aux$initialBalance2+aux$initialendowment > -aux$initialBalance3 & -aux$initialBalance3>= aux$initialBalance2+aux$initialendowment]

  
# Case 2, d1<=endowment (mixed)

# case 2.1 if s2==0, (mixed between d2 and s1)
aux$lowLambdaShare3[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment]  <- 
  -aux$initialBalance3[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment] /aux$initialendowment[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment] 

aux$lowLambdaShare1[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment]  <- 
  (aux$initialendowment[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment] 
   +aux$initialBalance3[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment] 
   )/aux$initialendowment[aux$initialBalance2==0 & -aux$initialBalance3<=aux$initialendowment] 


# case 2.2 if s2>0, (mixed between d2 and s1, full borrow from s2)

aux$lowLambdaShare3[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0] <- 
  -aux$initialBalance3[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0]/(
    aux$initialendowment[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0]+
      aux$initialBalance2[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0])

aux$lowLambdaShare1[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0] <- (
  aux$initialendowment[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0]+
    aux$initialBalance2[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0]+
    aux$initialBalance3[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0])/(
      aux$initialendowment[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0]+
        aux$initialBalance2[aux$initialBalance2>0 & -aux$initialBalance3<=aux$initialendowment & aux$initialBalance3<0])

# Case 4, s2+endow> d1 > endowm and  s2>0 (mixed to s1 and d1) (s2=0 collapses to case 1)


aux$lowLambdaShare3[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment] <- -aux$initialBalance3[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment]/(
  aux$initialBalance2[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment]
+aux$initialendowment[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment])


aux$lowLambdaShare1[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment] <- (aux$initialBalance2[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment]
  +aux$initialendowment[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment]
  +aux$initialBalance3[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment]
  )/(aux$initialBalance2[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment]
     +aux$initialendowment[aux$initialBalance2>=0 & aux$initialendowment+aux$initialBalance2>-aux$initialBalance3 & -aux$initialBalance3>aux$initialendowment])

# Case 5, d1 >= 0
aux$lowLambdaShare3[aux$initialBalance3>=0] <- 0
aux$lowLambdaShare1[aux$initialBalance3>=0] <- 1

# share check

aux$lowLambdaShareCheck <- aux$lowLambdaShare1+aux$lowLambdaShare2+aux$lowLambdaShare3+aux$lowLambdaShare4

if(aux$lowLambdaShare2==-1)
{aux$lowLambdaShareCheck[aux$lowLambdaShare2==-1] <- aux$lowLambdaShareCheck[aux$lowLambdaShare2==-1]-aux$lowLambdaShare2[aux$lowLambdaShare2==-1]
}

if(aux$lowLambdaShare1<0)
{aux$lowLambdaShareCheck[aux$lowLambdaShare1<0] <- aux$lowLambdaShareCheck[aux$lowLambdaShare1<0]-aux$lowLambdaShare1[aux$lowLambdaShare1<0]
}

table(aux$zeroLambdaShareCheck)


# High Lambda shares, X mixed cases  ----

aux$highLambdaShare1 <- NA
aux$highLambdaShare2 <- NA
aux$highLambdaShare3 <- NA
aux$highLambdaShare4 <- NA

aux$highLambdaShare2[aux$initialBalance2>0] <- -1
aux$highLambdaShare2[aux$initialBalance2==0] <- 0

# Case 1 -d1-d2>=s1+s2+endowment (full borrow from s1 and s2 fully) ---

# Case 1.1 if -d1>0

aux$highLambdaShare1[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0] <- -1
aux$highLambdaShare3[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0] <- -aux$initialBalance3[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]/(
  aux$initialBalance1[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
    aux$initialBalance2[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
    aux$initialendowment[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0])

aux$highLambdaShare4[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0] <- (aux$initialBalance1[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
       aux$initialBalance2[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
         aux$initialendowment[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
         aux$initialBalance3[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0])/(
           aux$initialBalance1[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
             aux$initialBalance2[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
             aux$initialendowment[-aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0])


# Case 1.2 if d1>=0
aux$highLambdaShare1[-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0] <- -1
aux$highLambdaShare3[-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0] <- 0
aux$highLambdaShare4[-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0] <- 1

# Case 1.3 if -d1> = s1+s2+endowment 
aux$highLambdaShare1[-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment] <- -1
aux$highLambdaShare3[-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment] <- 1
aux$highLambdaShare4[-aux$initialBalance4 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment & -aux$initialBalance3 >= aux$initialBalance1+aux$initialBalance2+aux$initialendowment] <- 0

# Case 2.1 s1+s2+endowment>=-d1-d2>s2+endowment & -d1 > 0 (borrow partially from s1 and s2 fully) ---

aux$highLambdaShare1[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0] <- -(aux$initialBalance2[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]+
                       aux$initialendowment[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                                              -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]-
                         aux$initialBalance3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                                               -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]-
                         aux$initialBalance4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                                               -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0])/
  aux$initialBalance1[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                        -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]


aux$highLambdaShare3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0] <- -aux$initialBalance3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]/(
                       -aux$initialBalance3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]-
                       aux$initialBalance4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0])

aux$highLambdaShare4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0] <- -aux$initialBalance4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]/(
                       -aux$initialBalance3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0]-
                       aux$initialBalance4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & -aux$initialBalance3>0])


# Case 2.2 s1+s2+endowment>=-d2>s2+endowment & d1>=0 (borrow partially from s1 and s2 fully) 

aux$highLambdaShare1[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0] <- -(-aux$initialBalance4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                                                                                                                                            -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0]-
                       aux$initialBalance2[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                                             -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0]-
                         aux$initialendowment[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                                                -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0])/
  aux$initialBalance1[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                        -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0]


aux$highLambdaShare3[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0] <- 0
aux$highLambdaShare4[aux$initialBalance1+aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 >= aux$initialBalance2+aux$initialendowment & aux$initialBalance3>=0] <- 1

# Case 3.1 s2+endowment>=-d1-d2>endowment & -d1 > 0 (borrow only from s2 fully) ---

aux$highLambdaShare1[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0] <- (aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]+
                       aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]+
                        aux$initialBalance3[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]+
                       aux$initialBalance4[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0])/(
                       aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]+
                       aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0])



aux$highLambdaShare3[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0] <- -aux$initialBalance3[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]/(
                        aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                        -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]+
                        aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                   -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]) 

aux$highLambdaShare4[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0] <- -aux$initialBalance4[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]/(
                       aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]+
                       aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & 
                       -aux$initialBalance3-aux$initialBalance4 > aux$initialendowment & aux$initialBalance3<0]) 

# Case 3.2 s2+endowment>=-d2>endowment & d1 > 0 (borrow only from s2 fully) ---

aux$highLambdaShare1[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0] <- (aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0]+
                       aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0]+
                       aux$initialBalance4[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0])/(
                       aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0]+
                       aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0])

aux$highLambdaShare3[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0] <- 0

aux$highLambdaShare4[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0] <- -aux$initialBalance4[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0]/(
                       aux$initialBalance2[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0]+
                       aux$initialendowment[aux$initialBalance2+aux$initialendowment>=-aux$initialBalance4 & 
                       -aux$initialBalance4 > aux$initialendowment & aux$initialBalance3>0])

# Case 4.1 endowment>=d1-d2>endowment & d1 <= 0 (borrow only from s2 fully) ---

aux$highLambdaShare1[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0] <- (aux$initialendowment[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]+
                      aux$initialBalance2[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]+
                      aux$initialBalance3[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]+
                      aux$initialBalance4[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0])/(
                      aux$initialendowment[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]+
                      aux$initialBalance2[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0])


aux$highLambdaShare3[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0] <- -aux$initialBalance3[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]/(
  aux$initialBalance2[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]+
    aux$initialendowment[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0])

aux$highLambdaShare4[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0] <- -aux$initialBalance4[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]/(
  aux$initialBalance2[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0]+
    aux$initialendowment[aux$initialendowment>=-aux$initialBalance3-aux$initialBalance4 & aux$initialBalance3<=0])

# Case 4.2 endowment>=-d2>endowment & d1 > 0 (borrow only from s2 fully) --


aux$highLambdaShare1[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0] <- (aux$initialendowment[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0]+
                     aux$initialBalance2[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0]+
                     aux$initialBalance4[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0])/(
                     aux$initialendowment[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0]+
                     aux$initialBalance2[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0])


aux$highLambdaShare3[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0] <- 0

aux$highLambdaShare4[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0] <- -aux$initialBalance4[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0]/(
  aux$initialBalance2[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0]+
    aux$initialendowment[aux$initialendowment>=-aux$initialBalance4 & aux$initialBalance3>0])


# if s1 is exhausted is then 0
aux$highLambdaShare1[aux$initialBalance1==0] <- 0


# share check

aux$highLambdaShareCheck <- aux$highLambdaShare1+aux$highLambdaShare2+aux$highLambdaShare3+aux$highLambdaShare4

if(aux$highLambdaShare2==-1)
{aux$highLambdaShareCheck[aux$highLambdaShare2==-1] <- aux$highLambdaShareCheck[aux$highLambdaShare2==-1]-aux$highLambdaShare2[aux$highLambdaShare2==-1]
}

if(aux$highLambdaShare1<0)
{aux$highLambdaShareCheck[aux$highLambdaShare1<0] <- aux$highLambdaShareCheck[aux$highLambdaShare1<0]-aux$highLambdaShare1[aux$highLambdaShare1<0]
}

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
MSE_lowDebt$treatment <- "Redistribution No Debt"
MSE_lowDebt$zeroLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$zeroLambdaMSE))
MSE_lowDebt$lowLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$lowLambdaMSE))
MSE_lowDebt$highLambdaMSE <- as.numeric(as.vector(MSE_lowDebt$highLambdaMSE))
mean(MSE_lowDebt$zeroLambdaMSE)
# 0.4085696
mean(MSE_lowDebt$lowLambdaMSE)
# 0.3773699
mean(MSE_lowDebt$highLambdaMSE)
# 0.9345395

MSE_lowDebt$type <- NA
MSE_lowDebt$typeMSE <- NA

# find min value
MSE_lowDebt$typeMSE <- pmin(MSE_lowDebt$zeroLambdaMSE, MSE_lowDebt$lowLambdaMSE,  MSE_lowDebt$highLambdaMSE)

# type 1 zero, 2 low, 3 high
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$highLambdaMSE] <- 3
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$lowLambdaMSE] <- 2
MSE_lowDebt$type[MSE_lowDebt$typeMSE==MSE_lowDebt$zeroLambdaMSE] <- 1

table(MSE_lowDebt$type)
# 43 28 10
table(MSE_lowDebt$type)/nrow(MSE_lowDebt)
# 53% 34% 12%

table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.0546993563])
# 28 1
table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.0546993563])/nrow(MSE_lowDebt)
# 34%, .1%,
# noise threshold from control 0.0546993563

# Calculate avg. lambda ----
aux <- as.vector(table(MSE_lowDebt$type))
# avg. lambda no thresold
lowLambda <- 1.20/1.15
highLambda <- 1.20/1.05
meanLambda <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt)
# 1.032666

# avg. lambda with thresold
aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=0.054]))

meanLambdaThreshold <- (aux[1]*1+aux[2]*lowLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=0.054,])
# 1.001499


thresholdValues <- unique(MSE_lowDebt$typeMSE)
thresholdValues <- sort(thresholdValues)
lambdaMatrix <- matrix(data=NA,nrow = length(thresholdValues),ncol = 2)
colnames(lambdaMatrix) <- c("lambda","threshold")

  
for (i in 1:length(thresholdValues)) {
  threshold <- thresholdValues[i]
  aux <- as.vector(table(MSE_lowDebt$type[MSE_lowDebt$typeMSE<=threshold]))
  if(threshold<0.22){
    lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*lowLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold
    
  } else {
    
    lambdaMatrix[i,1] <- (aux[1]*1+aux[2]*lowLambda+aux[3]*highLambda)/nrow(MSE_lowDebt[MSE_lowDebt$typeMSE<=threshold,])
    lambdaMatrix[i,2] <- threshold  
    
  }

}


lambdaMatrix[1,1] <- 1
lambdaMatrix[2,1] <- 1
lambdaMatrix[3,1] <- 1

lambdaMatrix <- as.data.frame(lambdaMatrix)

write.table(lambdaMatrix, "C:/Users/am04817/Dropbox/Behavioral debt experiment/NewDesign/StructuralEstimation/lambdaMatrixRedistributionNoDebt.txt",
            sep="\t", row.names = F)

