########################
### Function Library ###
########################

#takes a vector, convert it  to the specified class (factor, character, or numeric) 
classConv <- function(input.vector, varClass) {
        
        output.vector <- 
                switch(varClass,
                       factor  = as.factor(  input.vector ),
                       numeric = as.numeric( input.vector ),
                       integer = as.integer( input.vector )
                )
        
        output.vector
        
}


# ### Take a list of variable names and classes and convert the respective ...
# ### ... vectors to the prescribed class according to the list

classConvDF <- function( DF, vectorVarClasses ) 
{
        nVars    <- dim(DF)[ 2 ]
        nSamples <- dim(DF)[ 1 ]
                
        outputDF <- as.data.frame( matrix( rep( NA, nVars*nSamples ), nSamples, nVars ) )
        for( i in 1:nVars )
        {
                input.vector <- DF[,i]
                varClass     <- vectorVarClasses[i]
                outputDF[,i] <- classConv( input.vector, varClass )        
        }
        outputDF
}


timeConv <- function(data.vector, units, hrs2min = TRUE) {
        
        if( hrs2min ) {
                
                suppressWarnings( units        <- as.numeric( units ) )
                suppressWarnings( data.vector  <- as.numeric( data.vector ) )
                hours.indicator                <- which( units == 2 ) 
                data.vector[ hours.indicator ] <- data.vector[hours.indicator] * 60
                
                data.vector
                
        } else {
                units       <- as.numeric( units )
                data.vector <- as.numeric( data.vector  )
                minutes.indicator <- which( units == 1 ) 
                data.vector[minutes.indicator] <- data.vector[minutes.indicator] / 60
                
                data.vector
                
        }
}



cat2bin <-
        function(feature){
                
                levels      <- levels(feature)
                num_levels  <- length(levels)
                num_samples <- length(feature) 
                
                #Binary features, n columns for each category of the original categorical feature
                binary_features <- matrix(data<-NA, nrow<- num_samples, ncol<-num_levels)
                
                for (i in 1:num_levels){
                        level <- levels[i]
                        binary_features[which(feature==level),i]  <- 1
                        binary_features[-which(feature==level),i] <- 0
                        binary_features[which(is.na(feature)),i]  <- NA
                }
                
                colnames(binary_features)<-c(levels(feature))
                outputDF <- as.data.frame( binary_features )
                return(outputDF)
                
        }

replCat2bin <- 
        function( feature, DF ) {
                ### Function takes a data frame and a specifed multiclass categorical feature.
                ### Replaces the single feature with a Data Frame of binary feature
                ### outputs a data frame with multiclass categorical feature replaced ... 
                ###... with a data frame of binary features.
                
                feat_Numb     <- which(  names( DF ) == feature ) #What is the Index of the categorical variable to be replaced
                L             <- length( names( DF ) )
                
                binDF         <- cat2bin( feature = as.factor( DF[,feature] ) )
                
                appendNumbers <- levels(as.factor(DF[,feature]))
                newNames      <- paste( feature, appendNumbers, sep ="" )
                names(binDF)  <- newNames 
                
                finalDF       <- cbind( select( DF, 1:( feat_Numb - 1 ) ), binDF , select( DF, ( feat_Numb + 1 ): L ) )  
        }



dk2na <- function(data.vector, DKcode) {
        
        data.vector[ data.vector == DKcode ] <- NA
        suppressWarnings( data.vector <- as.numeric( data.vector ) ) #why as numeric?
        return( data.vector )
}



dk2naDF <- function(df){ 
        as.data.frame(
                stringsAsFactors = TRUE, #default
                sapply(df, function(x) 
                        
                        if( sum(x == 999, na.rm = T) >= 1 ){
                                
                                DKcode = 999
                                data.vector <- x
                                dk2na(data.vector, DKcode)
                                
                        } else if (sum(x == 99.9, na.rm = T) >= 1){
                                
                                DKcode = 99.9
                                data.vector <- x
                                dk2na(data.vector, DKcode)
                                
                        } else if (sum(x == 99, na.rm = T) >= 1){
                                
                                DKcode = 99
                                data.vector <- x
                                dk2na(data.vector, DKcode)
                                
                        } else if (sum(x == 9.99, na.rm = T) >= 1) {
                                
                                DKcode = 9.99
                                data.vector <- x
                                dk2na(data.vector, DKcode)
                                
                        } else if (sum(x == 9, na.rm = T) >= 1) {
                                
                                DKcode = 9
                                data.vector <- x
                                dk2na(data.vector, DKcode)
                                
                        } else {
                                data.vector <- x
                                suppressWarnings( as.numeric( data.vector ) )
                        }
                )
        )
}


loadGoogleSheets <- function() {
        print("Loading List of Selected Variables Directly from Google Spreadsheet")
        ### This function grabs the data in the google spreadsheet, ...
        ### "Codes for Neonatal Mortality" in the Data Analysis/Codebooks folder 
        ### The variables selected for analysis from the entire data set are ...
        ### in the sheet "Selected Variables" sheet 2
        ### The code is hardcoded to look for the exact title "Selected_Variables" 
        
        #R API for Google Sheets
        if(!require(googlesheets)) { install.packages("googlesheets"); require(googlesheets)} 
        
        ### Make a registered google spreadsheet (i.e make a googlesheet object) ### 
        key <- "1EXazFu0YaXnl2T17Rnyyv6NrUGxbEdSugqbUxs290CU" #found in url to spreadsheet
        gsObj <- gs_key(key) 
        
        ### Import the data into an R object ### 
        selectedVariables <- gs_read( ss = gsObj, 
                                      ws = "Selected_Variables") #Import the data from the second worksheet
        
        
        ### Save a copy of the data on the local machine ###
        saveRDS(selectedVariables, file = "googlesheetsVariables.Rds")
        
        ### Read the Data into R ###
        ### this is the output of the loadGoogleSheets function ##
        readRDS("googlesheetsVariables.Rds")
}



loadSelectedDatasets <- function(dataset_names, fromGoogleDoc ) {
        
        print("Loading Datasets with Selected Variables", quote = FALSE)
        print("Datasets to Load:", quote = FALSE)
        print(dataset_names, quote = FALSE)
        
        setwd("C:/Users/Bhavna/Google Drive/NMR Study/Data Work/JiVitA Datasets/")
        library(dplyr)
        #################################
        ### Import Google Spreadsheet ###
        #################################
        
        if( fromGoogleDoc == TRUE ) {
                selectedVariables <- loadGoogleSheets() 
        } else {
                selectedVariables <- readRDS("googlesheetsVariables.Rds")
        }
        
        ######################################################
        ### Grab the selected variable NAMES and the FORMS ###
        ######################################################
        
        forms         <- selectedVariables$Form; 
        varNames      <- selectedVariables$Variable.Code 
        formsVarNames <- data.frame(forms = forms, 
                                    varNames = varNames,  
                                    stringsAsFactors = FALSE) 
        
        
        ### Corresponding Variables *This is only relevant for IVBA at the moment
        pairedVars <-
                select(selectedVariables, 
                       Paired.Variable.Form, 
                       Corresponding.Variable.Code..surviving.neonates.
                )
        
        ### Split the Variable Names according to the Form ###
        f    <- as.factor(forms)
        datasets <- split(formsVarNames, f)
        
        loadedDatasets <- list()
        loadedDatasets$selectedVariables <- selectedVariables
        
        ### Come back to this segment of code and make it scalable.
        ### Make it flexible to more datasets, expression, eval, paste $, etc
        ### Why didn't it work? 
        
        ### I3MOP ###
        if( "i3mop" %in% dataset_names){
                if  (!exists("i3mop") ) 
                        {
                        print("Loading I3MOP")
                        load("i3mop.rdata")
                        }
                i3VarNames <- 
                        with(pairedVars, 
                             Corresponding.Variable.Code..surviving.neonates.[
                                     Paired.Variable.Form == "i3mop" &
                                             !is.na( Paired.Variable.Form )
                                     ] )
                i3mopSelected <- i3mop[,c("compid","childid", i3VarNames)]
                loadedDatasets$i3mopSelected <- i3mopSelected 
                
                print("I3MOP Loaded")
                
        }
        
        
        ### IVBA ###
        if ( "ivba" %in% dataset_names )  {
                if ( !exists("ivba") ) {
                        print("Loading IVBA")
                        load("ivba.rdata")
                        ivbaVarNames <- datasets$iv$varNames
                        ivbaSelected <- ivba[,c("compid","childid", ivbaVarNames)]
                        loadedDatasets$ivbaSelected <- ivbaSelected  
                        
                        print("IVBA Loaded", quote=F)
                        
                } else {
                        print("Message: IVBA Already loaded", quote = F)
                }
                
        }
        
        ### Kidtrak ###
        if( "kidtrak" %in% dataset_names ) {
                if ( !exists("kidtrak") ) {
                        load("kidtrak.rdata")
                        ktVarNames <- datasets$kidtrak$varNames
                        ktSelected <- kidtrak[,c("compid","childid", "deadaged", ktVarNames)]
                        loadedDatasets$kidtrakSelected <- kidtrakSelected
                        
                        print("Kidtrak Loaded", quote=F)
                        
                } else {
                        print("Message: Kidtrak Already Loaded", quoted=F)
                }
        }
        
        ### Naff ###
        if( "naff" %in% dataset_names )
        {
                if ( !exists("naff") ) {
                        load("naff.rdata")
                        naffVarNames <- datasets$naff$varNames
                        naffSelected <- naff[,c("compid","childid", naffVarNames)]
                        loadedDatasets$naffSelected <- naffSelected  
                        
                        print("naffSelected Loaded", quote = FALSE)
                        
                } else {
                        naffVarNames <- datasets$naff$varNames
                        naffSelected <- naff[,c("compid","childid", naffVarNames)]
                        loadedDatasets$naffSelected <- naffSelected
                        
                        print("naffSelected Loaded", quote = FALSE)
                        
                }
        }
        
        
        ### Ndaf ###
        if( "ndaf" %in% dataset_names ){
                if ( !exists("ndaf") ) {
                        load("ndaf.rdata")
                        ndafVarNames <- datasets$nd$varNames
                        ndafSelected <- ndaf[,c("compid","childid", ndafVarNames)]
                        loadedDatasets$ndafSelected <- ndafSelected
                } else {
                        print("Message: NDAF already loaded", quote = F)
                }
        }
        
        ### Pregtrak ###
        if( "pregtrak" %in% dataset_names ) {
                if( !exists("pregtrak") ) {
                        load("pregtrak.rdata")
                        ptVarNames <- datasets$pregtrak$varNames
                        ptSelected <- pregtrak[,c("compid","outc", ptVarNames)]
                        loadedDatasets$pregtrakSelected <- pregtrakSelected
                        
                        print("Message: Pregtrak already loaded")
                        
                } else {
                        
                }
        }
        
        loadedDatasets
}


optionTallyBin <- function( DF, feature, n_vars=3, opts_range ) {
        
        n_vars_tot <- dim(DF)[2]
        #create the names of the features to be added up ... 
        names   <- paste( feature, as.character( 1:n_vars), sep = "" )
        #...and find their column positions in the data frame ...  
        col_pos <- which( names( DF ) %in% names )
        #... and then select those variables from the DF
        varDF   <- as.data.frame( t( select( DF, col_pos ) ) ) 
        # transposed, gives you a sample oriented view of the data rather than a variable oriented view
        
        ### What are the names of the factor levels of ( response option 1 ) ?###
        #if( !exists(opts_range) ) {
        # opts <- as.numeric( levels( as.factor( DF[, names[ 1 ] ] ) ) )
        # opts_range <- c( min( opts ), max( opts ) )
        #}
        categories <- as.character( opts_range[1]:opts_range[2] )
        
        
        tempDF <-
                sapply(varDF, function(x){
                        varCat <- x
                        as.numeric( categories %in% varCat )
                })
        
        tempDF.2 <- as.data.frame( t(tempDF) )
        names(tempDF.2) <- paste( feature, categories, sep="" )
        
        
        
        
        finalDF <- cbind( select(DF, 1:(col_pos[1]-1 ) ) , 
                          tempDF.2, 
                          select( DF, (col_pos[ n_vars ] + 1):n_vars_tot ) )
        
}
