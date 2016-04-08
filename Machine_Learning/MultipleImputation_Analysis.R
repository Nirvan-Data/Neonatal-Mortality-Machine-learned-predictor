################################################
### Running analyses on multiple imputations ###
################################################



setwd("/home/mhealth/Documents/R/")
mice_output     <- readRDS(file = "Datasets/mice_output100.rds")
survStat_28Day  <- readRDS(file = "Datasets/survStat_28Day.rds")  

num_imps <- 10 #however many ( out of 100 )
output <- vector("list", num_imps )

for (i in 1:num_imps)
{
        #################################
        ### Pre-processing data frame ###
        #################################
        cat("\n", "Building Data Frame:", i, "\n")
        single_imputation.1  <- complete( mice_output, action = i, include = FALSE )
        allVariables1        <- cbind( survStat_28Day, single_imputation.1 )
        ##Standarize all numeric variables by scaling and centering
        ##For this we will have to eliminate those variables that had a very high outlier value such as time to breastfeeding
        ##Removing nawash, naclotht, nabfedt, nabruisd, naumbbd, naumbpd
        allVariables2 <- select(allVariables1, -nawash, -naclotht, -nabfedt, -nabruisd, -naumbbd, -naumbpd, -survStat_28Day)
        ##Variables that need to be standardized
        #nafntd, naweight, naheight, namuac, nahc, nacc, ivprgmth, ivdiard, ivdysd, GA, ageOfMother, parity, gravid, siblings
        allVariables3 <- select(allVariables2, nafntd, naweight, naheight, namuac, nahc, nacc, ivdiard, ivdysd, GA, ageOfMother, parity, 
                                gravid, siblings )
        allVariables4 <- stdize(allVariables3, center=TRUE, scale=TRUE, binary = "omit")
        allVariables5 <- select(allVariables2, -nafntd, -naweight, -naheight, -namuac, -nahc, -nacc, -ivprgmth, -ivdiard, -ivdysd, -GA, -ageOfMother, 
                                -parity, -gravid, -siblings )
        allVariables <- cbind(allVariables4, allVariables5, survStat_28Day)
        
        ################
        ### Analysis ###
        ################
        
        output[[i]] <- LRModel_Results(mydata) 
        
}

### Name Each Element of List as "Imp1", "Imp2", ... "Imp num_imps" ###
names(output) <- paste(rep("Imp", num_imps), 1:num_imps, sep ="" )

### Vector of AUCs from each imputation ###
sapply( output, function(x) {
         x$auc
})

### List of "RESULTS" matrices from each imputation ### 
sapply( output, function(x) {
        x$results
})
