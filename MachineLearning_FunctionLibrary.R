#########################################
### Machine Learning Function Library ###
#########################################
library(e1071)
library(randomForest)
library(pROC)

###########################################
### BUILD 10 FOLDS FOR CROSS VALIDATION ###
###########################################
buildFolds <-
        function( mydata, k = 10 ) {
        #k - number of folds (default 10)
        #mydata - data frame for future analysis
        
        #number of samples
        n        <- dim(mydata)[1]
        
        #Repeat 1:k as many times as you need to cover the whole data set and then some 
        #When n = 18147, folds.1 has 18150 rows
        folds.1  <- suppressWarnings( cbind( rep( 1:k, ceiling( n/k) ), 1:n ) )
        
        # Grab the first n rows of this slightly bigger than needed matrix
        # n = 18147. Extra 3 rows are discarded
        folds.2  <- as.data.frame( folds.1[1:n,] )
        
        #make a factor to split the data set into 10 groups based on the Fold number 
        f.1        <- as.factor( folds.2$V1 )
        
        #randomize the pattern in the ordering of the integers
        folds.2$V1 <- as.factor( sample( f.1, length(f.1), replace = FALSE ) )
        
        #split the folds into separate lists of indices. 
        f       <- folds.2$V1
        folds.3 <- split( folds.2, f )
        
        folds <- 
                lapply( folds.3, function(x) 
                {
                        x[,2]
                } )
        
        return(folds)
}


#################################
### LOGISTIC REGRESSION MODEL ###
#################################
LRModel_Results <- 
        function(mydata, r = 3) {
        #r      - how many decimal places to round the output?
        #mydata - data frame containing outcome variable "survStat_28Day"
                #consider changing the outcome variable from a hardcoded variable to a parameter of the function
        
        coding1_2Indicator <- all( as.numeric( names( table(mydata$survStat_28Day) ) ) == c(1,2) )
        
        #if outcome is coded as 1/2, then change it to 0,1
        if (coding1_2Indicator) 
        {
                mydata$survStat_28Day <- mydata$survStat_28Day - 1
        }
        
        ###########################################
        ### BUILD 10 FOLDS FOR CROSS VALIDATION ###
        ###########################################
        
        folds <- buildFolds(mydata, k = 10)
        
        ### Check Distribution of NM in each fold ###
        sapply( folds, function(x) 
        {
                fold <- mydata[x,]
                mortalityDistr <- table( fold[,"survStat_28Day"] )
                mortalityDistr[2]/dim(fold)[1]
        }
        )
        
        #        1.1        2.1        3.1        4.1        5.1        6.1        7.1        8.1        9.1       10.1 
        # 0.04738292 0.04242424 0.04077135 0.03966942 0.04352617 0.04132231 0.03856749 0.03748622 0.03969129 0.04575524 
        
        ###########################################
        ### CROSS VALIDATION AND MODEL BUILDING ###
        ###########################################
        
        #calculate predicted probability of death for each child
        print("Cross-Validating") 
        
        probabilityDF <- rep( NA, length( mydata$survStat_28Day ) )
        for ( i in 1:10)
        {
                cat("Fold:", i, "\n" )
                holdOut <- folds[[i]]
                train   <- mydata[ -holdOut, ] #all indices not in the "hold out" fold are the training set	
                test    <- mydata[  holdOut, ] #all indices in the "hold out" fold are the test set
                
                lr.obj      <- glm( formula = survStat_28Day~., data = train, family = "binomial" )
                probability <- predict( lr.obj, test, type = "response" )	
                probabilityDF[ holdOut ] <- probability 
                
        }
        
        
        #################
        ### BUILD ROC ###
        #################
        
        #Build AND plot ROC w/ predicted probability of death and actual outcome
        print("Building ROC")
        roc.obj <- roc( mydata$survStat_28Day, probabilityDF, plot = TRUE)
        sens    <- roc.obj$sensitivities
        spec    <- roc.obj$specificities
        
        
        ### Calculate sens, spec, ppv, and yield for 90% - 99% sensitivity levels ###
        thresholds        <- roc.obj$thresholds
        results           <- matrix( rep(NA, 5*10) , 10, 5)
        colnames(results) <- c("Sens","Spec","PPV","NPV","Yield")
        for (i in 0:9)
        {
                set_sensitivity <- 0.9 + i/100
                
                ### What is the smallest threshold probability level that gives a sensitivity of 9[0-9]% 
                ind <- which(  abs( sens - set_sensitivity) == min( abs( sens - set_sensitivity) ) )[1]
                threshold <- thresholds[ind] 
                
                #Make vector of predicted class labels based on pre-determined threshold value
                predClass <- rep(1, length(mydata$survStat_28Day) )
                #Where probability of death is less than threshold, label baby as "0" i.e low risk
                predClass[ probabilityDF < threshold ] <- 0 
                
                ### Build Confusion Matrix ###
                confusion_matrix <- table( predClass, mydata$survStat_28Day ) 
                
                TN <- confusion_matrix[1,1] #true negative:  baby did not die - predicted low risk
                TP <- confusion_matrix[2,2] #true positive:  baby died        - predicted high risk
                FN <- confusion_matrix[1,2] #false negative: baby died        - predicted low risk 
                FP <- confusion_matrix[2,1] #false positive: baby did not die - predicted high risk
                
                ### Calculate Sensitivity, Specificity, PPV, NPV, and Yield from confusion matrix ###
                Sensitivity <- round( TP/(TP + FN), r )
                Specificity <- round( TN/(TN + FP), r )
                PPV         <- round( TP/(FP + TP), r )
                NPV         <- round( TN/(TN + FN), r )
                Yield       <- round( (TP + FP) / (TP + FP + TN + FN), r )
                
                results[i+1,] <-  c(Sensitivity, Specificity, PPV, NPV, Yield) 
                
        }
        print( roc.obj$auc ) 
        print( results )
        
        output <- list(as.numeric( roc.obj$auc) , results)
        names(output) <- c("auc","results")
        return(output)
}

###########################
### RANDOM FOREST MODEL ###
###########################
RFModel_Results <- 
        function(mydata, r = 3) {
        #r      - how many decimal places to round the output?
        #mydata - data frame containing outcome variable "survStat_28Day"
        #consider changing the outcome variable from a hardcoded variable to a parameter of the function
        
        folds <- buildFolds(mydata, k = 10)
        
        probabilityDF <- rep( NA, length( mydata$survStat_28Day ) )
        predClassDF   <- rep( NA, length( mydata$survStat_28Day ) )
        for ( i in 1:10)
        {
                print(i)
                holdOut <- folds[[i]]
                train   <- mydata[ -holdOut, ]	
                test    <- mydata[  holdOut, ]
                
                rf.obj               <- randomForest( formula = survStat_28Day~., data = train, norm.votes = TRUE )
                probability[holdOut] <- predict( rf.obj, test, type = "prob" )[,1]
                
                
        }
        
        
        #################
        ### BUILD ROC ###
        #################
        
        #Build AND plot ROC w/ predicted probability of death and actual outcome
        print("Building ROC")
        roc.obj <- roc( mydata$survStat_28Day, probabilityDF, plot = TRUE)
        sens    <- roc.obj$sensitivities
        spec    <- roc.obj$specificities
        
        
        ### Calculate sens, spec, ppv, and yield for 90% - 99% sensitivity levels ###
        thresholds        <- roc.obj$thresholds
        results           <- matrix( rep(NA, 5*10) , 10, 5)
        colnames(results) <- c("Sens","Spec","PPV","NPV","Yield")
        for (i in 0:9)
        {
                set_sensitivity <- 0.9 + i/100
                
                ### What is the smallest threshold probability level that gives a sensitivity of 9[0-9]% 
                ind <- which(  abs( sens - set_sensitivity) == min( abs( sens - set_sensitivity) ) )[1]
                threshold <- thresholds[ind] 
                
                #Make vector of predicted class labels based on pre-determined threshold value
                predClass <- rep(1, length(mydata$survStat_28Day) )
                #Where probability of death is less than threshold, label baby as "0" i.e low risk
                predClass[ probabilityDF < threshold ] <- 0 
                
                ### Build Confusion Matrix ###
                confusion_matrix <- table( predClass, mydata$survStat_28Day ) 
                
                TN <- confusion_matrix[1,1] #true negative:  baby did not die - predicted low risk
                TP <- confusion_matrix[2,2] #true positive:  baby died        - predicted high risk
                FN <- confusion_matrix[1,2] #false negative: baby died        - predicted low risk 
                FP <- confusion_matrix[2,1] #false positive: baby did not die - predicted high risk
                
                ### Calculate Sensitivity, Specificity, PPV, NPV, and Yield from confusion matrix ###
                Sensitivity <- round( TP/(TP + FN), r )
                Specificity <- round( TN/(TN + FP), r )
                PPV         <- round( TP/(FP + TP), r )
                NPV         <- round( TN/(TN + FN), r )
                Yield       <- round( (TP + FP) / (TP + FP + TN + FN), r )
                
                results[i+1,] <-  c(Sensitivity, Specificity, PPV, NPV, Yield) 
                
        }
        print( roc.obj$auc ) 
        print( results )
        
        output <- list(as.numeric( roc.obj$auc) , results)
        names(output) <- c("auc","results")
        return(output)
}


########################
### LINEAR SVM MODEL ###
########################
SVML_Model_Results <-
        function( mydata, r = 3 ) {
                
                #r      - how many decimal places to round the output?
                #mydata - data frame containing outcome variable "survStat_28Day"
                #consider changing the outcome variable from a hardcoded variable to a parameter of the function
                
                folds <- buildFolds(mydata, k = 10)
                
                probabilityDF <- rep( NA, length( mydata$survStat_28Day ) )
                predClassDF   <- rep( NA, length( mydata$survStat_28Day ) )
                for ( i in 1:10)
                {
                        print(i)
                        holdOut <- folds[[i]]
                        train   <- mydata[ -holdOut, ]	
                        test    <- mydata[  holdOut, ]
                
                        svmLinear <- svm( formula = outcome ~. , data = train, kernel = "linear", probability = TRUE )
                        
                        predictors                 <- predict(svmLinear, test, probability= TRUE)
                        probabilities              <- attr( predictors, "probabilities" )[,1] 
                        probabilityDF[ holdOut ]   <- test$outcome
                }
                
                #################
                ### BUILD ROC ###
                #################
                
                #Build AND plot ROC w/ predicted probability of death and actual outcome
                print("Building ROC")
                roc.obj <- roc( mydata$survStat_28Day, probabilityDF, plot = TRUE)
                sens    <- roc.obj$sensitivities
                spec    <- roc.obj$specificities
                
                
                ### Calculate sens, spec, ppv, and yield for 90% - 99% sensitivity levels ###
                thresholds        <- roc.obj$thresholds
                results           <- matrix( rep(NA, 5*10) , 10, 5)
                colnames(results) <- c("Sens","Spec","PPV","NPV","Yield")
                for (i in 0:9)
                {
                        set_sensitivity <- 0.9 + i/100
                        
                        ### What is the smallest threshold probability level that gives a sensitivity of 9[0-9]% 
                        ind <- which(  abs( sens - set_sensitivity) == min( abs( sens - set_sensitivity) ) )[1]
                        threshold <- thresholds[ind] 
                        
                        #Make vector of predicted class labels based on pre-determined threshold value
                        predClass <- rep(1, length(mydata$survStat_28Day) )
                        #Where probability of death is less than threshold, label baby as "0" i.e low risk
                        predClass[ probabilityDF < threshold ] <- 0 
                        
                        ### Build Confusion Matrix ###
                        confusion_matrix <- table( predClass, mydata$survStat_28Day ) 
                        
                        TN <- confusion_matrix[1,1] #true negative:  baby did not die - predicted low risk
                        TP <- confusion_matrix[2,2] #true positive:  baby died        - predicted high risk
                        FN <- confusion_matrix[1,2] #false negative: baby died        - predicted low risk 
                        FP <- confusion_matrix[2,1] #false positive: baby did not die - predicted high risk
                        
                        ### Calculate Sensitivity, Specificity, PPV, NPV, and Yield from confusion matrix ###
                        Sensitivity <- round( TP/(TP + FN), r )
                        Specificity <- round( TN/(TN + FP), r )
                        PPV         <- round( TP/(FP + TP), r )
                        NPV         <- round( TN/(TN + FN), r )
                        Yield       <- round( (TP + FP) / (TP + FP + TN + FN), r )
                        
                        results[i+1,] <-  c(Sensitivity, Specificity, PPV, NPV, Yield) 
                        
                }
                print( roc.obj$auc ) 
                print( results )
                
                output <- list(as.numeric( roc.obj$auc) , results)
                names(output) <- c("auc","results")
                return(output)
                        
        }
        


