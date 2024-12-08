library(irr)
library(ggplot2)
## define functions
get_Metrics <- function(diagnosis,rater){# generate the confusion matrix and metrics
  TP <- sum(diagnosis == 1 & rater == 1)
  FP <- sum(diagnosis == 0 & rater == 1)
  TN <- sum(diagnosis == 0 & rater == 0)
  FN <- sum(diagnosis == 1 & rater == 0)
  sumr1 <- sum(TP,FN)
  sumr2 <- sum(FP,TN)
  sumc1 <- sum(TP,FP)
  sumc2 <- sum(FN,TN)
  sum <- sum(TP,FP,FN,TN)
  table <- matrix(c(TP,FN,sumr1,FP,TN,sumr2,sumc1,sumc2,sum),
                  nrow=3,
                  byrow=TRUE)
  colnames(table) <- (c('PosTest','NegTest','Total'))
  rownames(table) <- (c('Diseased','Healthy','Total'))
  # df <- data.frame(table)
  output <- list(confusion_matrix=table,
                 Sensitivity=TP/sumr1,
                 Specificity=TN/sumr2,
                 PPV=TP/sumc1,
                 NPV=TN/sumc2,
                 Accuracy=(TP+TN)/sum)
  return (output)
}

get_2dim_matrix <- function(confmatrix){# turn a 3 dim matrix to a 2 dim one
  table <- matrix(c(confmatrix[1,1],confmatrix[1,2],confmatrix[2,1],confmatrix[2,2]),
                  nrow = 2,
                  byrow = TRUE,
                  dimnames = list("Rater1" = c("Positive", "Negative"),
                                  "Rater2" = c("Positive", "Negative")))
  return(table)
}

## main
# import data
data <- read.csv("data_task1.csv")
# have a brief look
labels <- c('Positive','Negative')
diagnosis <- c(sum(data$Patient_Diagnosis[which(data$Patient_Diagnosis==1)]),
               nrow(data)-sum(data$Patient_Diagnosis[which(data$Patient_Diagnosis==1)]))
rater1 <- c(sum(data$Rater1[which(data$Rater1==1)]),
            nrow(data)-sum(data$Rater1[which(data$Rater1==1)]))
rater2 <- c(sum(data$Rater2[which(data$Rater2==1)]),
            nrow(data)-sum(data$Rater1[which(data$Rater1==1)]))
pie(diagnosis,labels)
pie(rater1,labels)
pie(rater2,labels)

# metrics and confusion matrix 
# between diagnosis and raters
test1 <- get_Metrics(data$Patient_Diagnosis,data$Rater1)
print(test1$confusion_matrix)
test2 <- get_Metrics(data$Patient_Diagnosis,data$Rater2)
print(test2$confusion_matrix)
# between two raters
test_agree <- get_Metrics(data$Rater1,data$Rater2)
agree_matrix <- test_agree$confusion_matrix
print(agree_matrix)

# kappa value 
data_2col <- data.frame(Rater1 = data$Rater1,Rater2 = data$Rater2)
result <- kappa2(data_2col)
print(result)

# McNemar's test
# binom.test(c(agree_matrix[2,1],agree_matrix[1,2]))
agree_matrix_2dim <- get_2dim_matrix(agree_matrix)
result <- mcnemar.test(agree_matrix_2dim)
print(result)

# Test difference in sensitivity/specificity
# For sensitivity, look at the subpopulation of diseased subjects
subpop_diseased <- data.frame(data[which(data$Patient_Diagnosis == 0),])
test_subdis <- get_Metrics(subpop_diseased$Rater1,subpop_diseased$Rater2)
subdis_matrix_2dim <- get_2dim_matrix(test_subdis$confusion_matrix)
print(subdis_matrix_2dim)
result <- mcnemar.test(subdis_matrix_2dim)
print(result)

# For specificity, look at the subpopulation of healthy subjects.
subpop_healthy <- data.frame(data[which(data$Patient_Diagnosis == 1),])
test_subheal <- get_Metrics(subpop_healthy$Rater1,subpop_healthy$Rater2)
subheal_matrix_2dim <- get_2dim_matrix(test_subheal$confusion_matrix)
print(subheal_matrix_2dim)
result <- mcnemar.test(subheal_matrix_2dim)
print(result)                      
                                  