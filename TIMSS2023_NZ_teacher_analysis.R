library(tidyverse)
library(haven)
library(plyr)
library(missRanger)
library(mltools)
library(data.table)
library(dbarts)
library(Rcpp)

set.seed(123)
setDTthreads(1)

load("data/bcgnzlm8.rdata")
load("data/bsgnzlm8.rdata")
load("data/bstnzlm8.rdata")
load("data/btmnzlm8.rdata")
load("data/btsnzlm8.rdata")

BCG <- BCGNZLM8 #School context data files
BSG <- BSGNZLM8 #Student context data files
BST <- BSTNZLM8 #Student-teacher linkage files
BTM <- BTMNZLM8 #Mathematics teacher context data files
BTS <- BTSNZLM8 #Science teacher context data files 

#Merging Student-teacher linkage and Mathematics teacher context data
BST_BTM <- merge(BST, BTM, by = "IDTEALIN")

#Ordering BST_BTM by Student ID (IDSTUD) and time spent on math homework (BTBM14) for tie breaker
order1 <- as.numeric(as.character(BST_BTM$IDSTUD)) 
order2 <- as.numeric(as.character(BST_BTM$BTBM14))
order2 <- ifelse(is.na(order2), 0, order2)*-1

BST_BTM <- BST_BTM[order(order1, order2), ]

#Removing repititive IDSTUD
support_teachers<-c()
for(i in 2:(length(BST_BTM$IDSTUD)))
{
  if(BST_BTM$IDSTUD[i] == BST_BTM$IDSTUD[i-1])
  {
    support_teachers<-c(support_teachers, i)
  }
}
BST_BTM<-BST_BTM[-support_teachers,]

#Each row now has student context data 
BSG_BST_BTM<-merge(BSG, BST_BTM, by="IDSTUD", all.x=T)
#Each row now has school context data 
BSG_BST_BTM_BCG<-merge(BSG_BST_BTM, BCG, by="IDSCHOOL")

#Merging Student-teacher linkage and Science teacher context data
BST_BTS <- merge(BST, BTS, by = "IDTEALIN")

#Ordering BST_BTM by Student ID (IDSTUD) and time spent on science homework (BTBS14) for tie breaker
order1<-as.numeric(as.character(BST_BTS$IDSTUD))
order2<-as.numeric(as.character(BST_BTS$BTBS14))
order2<-ifelse(is.na(order2), 0, order2)*-1

BST_BTS<-BST_BTS[order(order1, order2),]

#Removing repititive IDSTUD
support_teachers<-c()

for(i in 2:(length(BST_BTS$IDSTUD)))
{
  if(BST_BTS$IDSTUD[i] == BST_BTS$IDSTUD[i-1])
  {
    support_teachers<-c(support_teachers, i)
  }
}

BST_BTS<-BST_BTS[-support_teachers,]

#Each row now has student context data 
BSG_BST_BTS<-merge(BSG, BST_BTS, by="IDSTUD", all.x=T)
#Each row now has the school context data 
BSG_BST_BTS_BCG<-merge(BSG_BST_BTS, BCG, by="IDSCHOOL")

maths_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
              "BSBG08A", "BSBG08B",
              "BSBG09A", "BSDGEDUP", "BSBGHER",
              "BSBGSSB", "BSBGSB", "BSBGICM",
              "BSBGSCM", "BSBGSVM",
              
              "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
              "BSBG11A", "BSBG11B", "BSBG10",
              
              "BTBG01", "BTBG02", "BTBG03", "BTBG10",
              "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDMMME",
              
              "BCBGDAS", "BCBGEAS", "BCBGMRS",
              "BCDGSBC",
              
              "BTBM20BA", "BTBM20BB", "BTBM20BC", "BTBM20BD", "BTBM20BE", "BTBM14")

maths_other<-c("BSMMAT01.x", "BSMMAT02.x", "BSMMAT03.x", 
               "BSMMAT04.x", "BSMMAT05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")


maths_treatment<-c("BTBM20A") #Replacing BSBM26AA with BSBM30A

science_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                "BSBG08A", "BSBG08B",
                "BSBG09A", "BSDGEDUP", "BSBGHER",
                "BSBGSSB", "BSBGSB", "BSBGICS",
                "BSBGSCS", "BSBGSVS",
                
                "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
                "BSBG11A", "BSBG11B", "BSBG10",
                
                "BTBG01", "BTBG02", "BTBG03", "BTBG10",
                "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDSMSE",
                
                "BCBGDAS", "BCBGEAS", "BCBGSRS",
                "BCDGSBC",
                
                "BTBS23BA", "BTBS23BB", "BTBS23BC", "BTBS23BD", "BTBS23BE", "BTBS14")

science_other<-c("BSSSCI01.x", "BSSSCI02.x", "BSSSCI03.x",
                 "BSSSCI04.x", "BSSSCI05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")

science_treatment<-c("BTBS23A")

XYM<-BSG_BST_BTM_BCG[,c(maths_vars, maths_other, maths_treatment)]
XYS<-BSG_BST_BTS_BCG[,c(science_vars, science_other, science_treatment)]
#------------------------------------------------------------------------------
label <- c()
filtered_XY <- cbind(XYM[, c(2:9, 16:25, 27:28, 34, 38:43, 53)], XYS$BTDSMSE)
for (i in 1:(ncol(filtered_XY)))
{
  label <- c(label, attr(filtered_XY[, i], "labels"))
}
from <- unique(names(label))
to <- c(0, 1, 2, NA,
        3, 2, 1, 0,
        5, 20, 50, 150, 200,
        2, 3, 4, 5, 6, 8,
        1, 0, NA, NA, 
        5, 4, 3, 2, 1, 0,
        5, 2,
        4, 3, 2, 1, 0,
        0, 1,
        25, 29, 39, 49, 59, 66,
        2, 1, 1, 0, 0,
        3, 2, 1,
        2, 0,
        0, 0.5, 1.5, 3.5,
        2, 1, 1)

mapna <- function(df)
{
  for (i in 1:53)
  {
    df_NA <- attr(df[, i], "na_values")
    df[, i] <- replace(df[, i], df[, i] %in% df_NA, NA)
    if (i %in% c(2:9, 16:25, 27:28, 34, 38:43, 53))
    {
      df_label <- attr(df[, i], "labels")
      df[, i] <- names(df_label)[match(df[, i], df_label)]
    }
  }
  return(df)
}

mymap<-function(x)
{
  x<-as.numeric(as.character(x))
  
  return(x)
}

mymap2<-function(x)
{
  x<-mapvalues(x,
               from=from,
               to=to)
  
  x<-as.numeric(as.character(x))
  
  return(x)
}

XYM <- mapna(XYM)
XYS <- mapna(XYS)

XYM<-mutate_at(XYM, c(2:9, 16:25, 27:28, 34, 38:43, 53), mymap2)
XYS<-mutate_at(XYS, c(2:9, 16:25, 27:28, 34, 38:43, 53), mymap2)

XYM<-mutate_at(XYM, c(1, 10:15, 26, 29:33, 35:37, 44:52), mymap)
XYS<-mutate_at(XYS, c(1, 10:15, 26, 29:33, 35:37, 44:52), mymap)

XY <- merge(XYM, XYS, by = "IDSTUD")
NA_percentage <- sum(is.na(XY))/prod(dim(XY))

complete_XY <- XY[complete.cases(XY),]
impute_XY <- missRanger(XY, num.threads=6, num.trees=50, pmm.k = 3)

x_variable <- c(
  "BSDAGE.x",   "BSBG01.x",   "BSBG03.x",   "BSBG04.x",   "BSBG07.x",   "BSBG08A.x",  "BSBG08B.x",  "BSBG09A.x", 
  "BSDGEDUP.x", "BSBGHER.x",  "BSBGSSB.x",  "BSBGSB.x",   "BSBGICM",    "BSBGSCM",    "BSBGSVM",    "BSBG05A.x", 
  "BSBG05B.x",  "BSBG05C.x",  "BSBG05D.x",  "BSBG05E.x",  "BSBG05F.x",  "BSBG05G.x",  "BSBG11A.x",  "BSBG11B.x", 
  "BSBG10.x",   "BTBG01.x",   "BTBG02.x",   "BTBG03.x",   "BTBG10.x",   "BTBGTJS.x",  "BTBGSOS.x",  "BTBGLSN.x", 
  "BTBGEAS.x",  "BTDMMME",    "BCBGDAS.x",  "BCBGEAS.x",  "BCBGMRS",    "BCDGSBC.x",  "BTBM20BA", "BTBM20BB", 
  "BTBM20BC", "BTBM20BD", "BTBM20BE",   "BTBM14",     "BSBGICS",    "BSBGSCS",    "BSBGSVS",    "BTBG01.y",  
  "BTBG02.y",   "BTBG03.y",   "BTBG10.y",   "BTBGTJS.y",  "BTBGSOS.y",  "BTBGLSN.y",  "BTBGEAS.y",  "BTDSMSE",   
  "BCBGSRS",    "BTBS23BA", "BTBS23BB", "BTBS23BC", "BTBS23BD", "BTBS23BE",   "BTBS14")

X<-as.matrix(one_hot(as.data.table(impute_XY[,x_variable])))

X<-X[,!colnames(X) %in% c("BTBS23BB", "BTBS23BC", "BTBS23BD", "BTBS23BE",
                          "BTBM20BB", "BTBM20BC", "BTBM20BDD", "BTBM20BE")]

FrequencyM<-impute_XY$BTBM20A
FrequencyS<-impute_XY$BTBS23A

#If we're going with the suggested category
Z1 <- ifelse(FrequencyM == 0.5, 1, 0)
Z2 <- ifelse(FrequencyS == 0.5, 1, 0)

Z3 <- ifelse(FrequencyM == 1.5, 1, 0)
Z4 <- ifelse(FrequencyS == 1.5, 1, 0)

Z5 <- ifelse(FrequencyM == 3.5, 1, 0)
Z6 <- ifelse(FrequencyS == 3.5, 1, 0)

Z7 <- ifelse(FrequencyM == 5, 1, 0)
Z8 <- ifelse(FrequencyS == 5, 1, 0)

#Get propensity scores
p_mod<-bart(x.train = X, y.train = Z1, k=3)
p<-colMeans(pnorm(p_mod$yhat.train))
p_mod2<-bart(x.train = X, y.train = Z2, k=3)
p2<-colMeans(pnorm(p_mod2$yhat.train))
p_mod3<-bart(x.train = X, y.train = Z3, k=3)
p3<-colMeans(pnorm(p_mod3$yhat.train))
p_mod4<-bart(x.train = X, y.train = Z4, k=3)
p4<-colMeans(pnorm(p_mod4$yhat.train))
p_mod5<-bart(x.train = X, y.train = Z5, k=3)
p5<-colMeans(pnorm(p_mod5$yhat.train))
p_mod6<-bart(x.train = X, y.train = Z6, k=3)
p6<-colMeans(pnorm(p_mod6$yhat.train))
p_mod7<-bart(x.train = X, y.train = Z7, k=3)
p7<-colMeans(pnorm(p_mod7$yhat.train))
p_mod8<-bart(x.train = X, y.train = Z8, k=3)
p8<-colMeans(pnorm(p_mod8$yhat.train))

Z1<-cbind(Z1, Z2) #Frequency less than once per week
Z2<-cbind(Z3, Z4) #Frequency 1-2 times a week
Z3<-cbind(Z5, Z6) #Frequency 3-4 times a week 
Z4<-cbind(Z7, Z8) #Frequency 5 times a week
#Reference level for duration "My teacher never gives me homework in..." 
#Reference level for frequency "Rarely/Never" which is from merging the category "Never" and "Less than once per week"

#Set number of trees and iterations

n_tree_mu<-60
n_tree_tau<-20
n_iter<-5000
n_burn<-2500

#Change 01 to 02, 03, 04, and 05 for different chains.
Y<-cbind(impute_XY$BSMMAT01.x, impute_XY$BSSSCI01.x)

X1<-cbind(X, p, p2, p3, p4, p5, p6, p7, p8)

sourceCpp(file = "E:\\Summer Research\\BCF\\HomeworkModelGitHub.cpp")

group_id<-as.integer(as.factor(impute_XY$IDCLASS.x.x)) - 1

group_id_test<- group_id

my_mod <- fast_bart(X1, 
                    Y, 
                    Z1,
                    Z2,
                    Z3,
                    Z4,
                    X,
                    X1[1:3,],
                    X[1:3,],
                    0.95, 
                    2, 
                    0.25, 
                    3, 
                    diag((1)^2/n_tree_mu, 2), 
                    diag((0.3)^2/n_tree_tau, 2), 
                    1, 
                    diag(1, 2), 
                    n_iter, 
                    n_tree_mu, 
                    n_tree_tau, 
                    1, 
                    group_id,
                    group_id_test,
                    diag(0.1, 2),
                    matrix(0, nrow=2, ncol=1),
                    diag(0.01, 2),
                    1,
                    n_burn,
                    2)
#save results
save(my_mod, file = "ModelResultsChain1.RData", compress = "xz")

