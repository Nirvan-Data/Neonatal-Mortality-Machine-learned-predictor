########################################
#### SET FILE PATH FOR LOCAL MACHINE ###
########################################

filepath <- "ENTER FILE PATH TO directory 'Pre-Processing Workflow' "

setwd(filepath)
source("FunctionLibrary_Recoding.r")


#############################################################################
########################## LOAD KIDTRAK #####################################
#############################################################################

#install dplyr package if not installed
if( !require(dplyr) ){ install.packages(dplyr) }
#load package if not already loaded
if( !"package:dplyr" %in% search() ) {require(dplyr)}


load("kidtrak.rdata")
kt <- select(kidtrak, 
             compid, 
             childid, 
             sex, 
             deadaged, 
             siblings)

kt     <- rename(kt, ageAtDeath = deadaged)

###############################################################
############ NEONATAL MORTALITY OUTCOME VARIABLE ##############
###############################################################

survStat_28Day <- rep(0, length(kt$ageAtDeath))
survStat_28Day[with(kt, ageAtDeath < 28 & !is.na(ageAtDeath))] <- 1

## Note 
## day 0 --> death in first 24 hours
## --> deadaged 28 is 29th day. Use < 28

survStat_28Day <- as.factor(survStat_28Day)
kt <- cbind(kt, survStat_28Day)


##############################################################################
########################## LOAD PREGTRAK #####################################
##############################################################################
load("pregtrak.rdata")
pt <- select(pregtrak, 
             compid, 
             AGE_URT, 
             bgga, 
             parity, 
             gravid)

pt <- rename(pt, 
             ageOfMother         = AGE_URT, 
             GA                  = bgga)

#Add Pre-term/Full Term binary variable
pt$preterm <- cut(pt$GA, 
                  breaks = c(27,37,44), 
                  labels = c("Yes","No"))
## (a,b] interval
#***!!! check interval inclusive or not in definition of preterm 37 weeks


##############################################################################
########################## MERGE DATA SETS ###################################
##############################################################################
### inner join, only keep records that have matches in both forms ###

#checked that merged data set has 42185 records. Same number of records as in kidtrak
#This means all of the live born kids in pregtrak are tracked in kidtrak
df1 <- merge(pt, kt, by = "compid", all = FALSE) 

#checked that twin records are merged correctly
#merge.check <- subset(df, survStat_28Day.P == "07", select = c("compid", "childid"))
# > head(merge.check)
#      compid     childid
# 97  00210951       1
# 98  00210951       2
# 177 00300741       1
# 178 00300741       2
# 234 00410021       2
# 235 00410021       1

IDs <- select(df1, compid,  childid)
df2 <- select(df1, -compid, - childid, -ageAtDeath)
df3 <- df2[,c("GA","preterm","ageOfMother","parity","gravid","sex","siblings", "survStat_28Day")]

#check that all the classes of the variables are correct (numeric, character, factor)
#sapply(df, class)
#just curious about range of dates
#range(df$DOB) #"2001-09-09" "2006-10-12"

###########################################################
########### RECODING DON'T KNOWS AS MISSING ###############
###########################################################

### Age of Mother ###
### None of the mother's age at UrT+ were coded as 99
# > sum(df$ageOfMother == 99, na.rm=T)
# [1] 0

### Parity ###
# > sum(df$parity == 99, na.rm=T)
# [1] 7
df3$parity[df3$parity == 99] <- NA
# > sum(df$parity == 99, na.rm=T)
# [1] 0

### Gravidity ###
# > sum(df$gravid == 99, na.rm=T)
# [1] 9
df3$gravid[df3$gravid == 99] <- NA
# > sum(df$gravid == 99, na.rm=T)
# [1] 0

### Sex ###
### LEAVE "DON'T KNOW"S IN SEX AS "DON'T KNOW", DON'T RECODE THEM AS "MISSING"
# > sum(df$sex == 9)
# [1] 5

ktptFinal <- cbind( IDs, df3 )

##########################################
### FINISHED WITH KIDTRAK AND PREGTRAK ###
##########################################


############################
### Merge IVBA and I3MOP ### 
############################
#####
data.files         <- c("i3mopSelected.rds", "ivbaSelected.rds")
files.in.directory <- list.files() 

filepathIVBA <- "C:/Users/Bhavna/Google Drive/NMR Study/Data Work/JiVitA Datasets/ivbaSelected.rds"
filepathI3MOP <- "C:/Users/Bhavna/Google Drive/NMR Study/Data Work/JiVitA Datasets/i3mopSelected.rds"


### If i3mopSelected and ivbaSelected are already in the directory then move on ###
### otherwise, load the selected variables by referencing the the googlespreadsheets ###
if( all( data.files %in% files.in.directory ) ) {
        
        ivbaSelected  <- readRDS(file = filepathIVBA)
        i3mopSelected <- readRDS(file = filepathI3MOP)
        
        if (exists("ivbaSelected") & exists("i3mopSelected") ) {
                print("IVBA and I3MOP selected variables already loaded", quote = FALSE)
        }
        
} else {
        
        loadedDatasets <- loadSelectedDatasets( c( "ivba","i3mop" ), fromGoogleDoc = TRUE )
        ivbaSelected   <- loadedDatasets$ivba
        i3mopSelected  <- loadedDatasets$i3mop
        saveRDS( object = ivbaSelected,  file = "ivbaSelected.rds" )
        saveRDS( object = i3mopSelected,  file = "i3mopSelected.rds" )
}
#####

ivbaSelectedNoIDs  <- select(ivbaSelected, -compid, -childid)
ivbaIDs            <- select(ivbaSelected, compid, childid)

i3mopSelectedNoIDs <- select(i3mopSelected, -compid, -childid)
i3mopIDs           <- select(i3mopSelected, compid, childid)

### Recoding "Don't Knows" to "Missing" i.e "NA"
ivbaSelected.2  <- cbind( ivbaIDs, dk2naDF( ivbaSelectedNoIDs ) )
i3mopSelected.2 <- cbind( i3mopIDs, dk2naDF( i3mopSelectedNoIDs ) )

levels( ivbaSelected.2$ivbrn ) <- c( "0", "1" )

#selectedVariables <- loadedDatasets$selectedVariables

### Recode Variables before merging ###
### Following variables are not one to one mapping between ivba and i3mop ###
###                 ivba:    i3mop:
###
###   Convulsions   ivunc    i3conv
###                 ivconvb  i3conv 
### 
###   Breathing     ivresp   i3pneum
###                 ivcgh    i3pneum
###                 ivbreath i3pneum
###   
#####

### IVBA ###
#####

#0 - no  cough/no  breathing difficulty
#1 - yes cough/yes breathing difficulty

ivbaSelected.3 <- ivbaSelected.2

resp   <- as.numeric( as.character( ivbaSelected.3$ivresp ) )
respNA <- is.na( resp )

cough  <- as.numeric( as.character( ivbaSelected.3$ivcgh ) ) 
breath <- as.numeric( as.character( ivbaSelected.3$ivbreath ) )


#Cough or Breath
ivrespRC <- rep( NA, 3971 )
# If baby has either  breathing difficulty or coughing, then code ivrespRC as "1"
ivrespRC[ ( cough + breath >  0 ) ] <- 1
# If baby has neither breathing difficulty or coughing, then code ivrespRC as "0"
ivrespRC[ ( cough + breath == 0 ) ] <- 0
# Where ivresp data is not missing, keep it
# ivresp (V1) given preference over ivcgh/ivbreath (v2) 
ivrespRC[!respNA] <- resp[!respNA]

#remove the original variables
ivbaSelected.3 <- select( ivbaSelected.3, -ivresp, -ivcgh, -ivbreath )
#...and replace it with the combined version
ivbaSelected.3$ivrespRC <- as.factor( ivrespRC )
#reorder the variables to reflect the original order
ivbaSelected.3 <- select(ivbaSelected.3, compid:ivskin, ivrespRC, ivbrfast:ivfood)
#####

### I3MOP ###
#####
i3mopSelected.2         <- select( i3mopSelected.2, -i3pneum.1,  -i3pneum.2 )

#####

### Convulsions ###
### i3mop: 
### i3conv   -- "Did the child have any convulsions in the first 7 days after birth?"
###
### ivba:
### ivunc   --  "Did the infant have any convulsions/seizures in the 2 days after birth?"
### ivconvb --  "Did the infant have any convulsions/seizures in the 3-28 days after birth?
### -- > did the infant have any convultions/seizures in the 28 days after birth?


### IVBA ###
#####

ivbaSelected.4         <- ivbaSelected.3


#if either convb or unc == 0 --> no convulsions
#if either convb or unc == 1 --> yes convulsions
#else --> NA missing
conv2    <- ivbaSelected.4$ivunc
conv3_28 <- ivbaSelected.4$ivconvb
#indicator for where both 2 and 3_28 are missing
conv0_28NA  <- as.logical( is.na( conv2  )  & is.na( conv3_28 ) )
conv0_28YES <- as.logical( conv2 == 1 | conv3_28 == 1 )
conv0_28NO  <- as.logical( ( conv2   == 0 & conv3_28 == 0 ) | #both are 0
                                   ( is.na(conv2) & conv3_28 == 0 ) | # 0, NA
                                   ( conv2   == 0 & is.na(conv3_28) ) # NA, 0
)



ivconv <- rep(NA, 3971)
ivconv[conv0_28YES] <- 1
ivconv[conv0_28NO]  <- 0
ivconv[conv0_28NA]  <- NA

ivbaSelected.4$ivconvRC <- as.factor( ivconv )

#reorder the variable names to reflect the original ordering and remove ivconvb and ivunc
ivbaSelected.4 <- select(ivbaSelected.4, compid:ivyeye, ivconvRC, ivhfvr:ivfood)
#####

### I3MOP ###
#####
i3mopSelected.3         <- i3mopSelected.2
i3mopSelected.3         <- select( i3mopSelected.3, -i3conv.1)
#####



### IVSIZE and I3SIZE ###
### size of infant at birth
###
###  ivsize          i3mop
###
### Very small (4)    small (1)
### small      (3)    small (1)
### medium     (2)   medium (2)
### large      (1)   large  (3)

### Recode "very small" babies as "small" 
ivbaSelected.5 <- ivbaSelected.4
ivbaSelected.5 <- within( ivbaSelected.5, ivsize[ ivsize == 4] <- 3 )

### Re-order the ivsize numbering 
### Smaller to larger numbers according to smaller to larger size
### Switch (3) and (1) in ivsize 

temp         <- factor( ivbaSelected.5$ivsize )         #define temporary vector 
temp         <- droplevels( temp )                      #remove unused levels

temp.2       <- factor( temp, 
                        levels = levels(temp)[3:1],     #reorder the levels
                        ordered = TRUE )                #define factor as ordered 

levels(temp.2) <- c( "1", "2", "3" )
ivbaSelected.5$ivsize <- temp.2

### ivprgmth ###
ivbaSelected.5$ivprgmth <- as.numeric( as.character( ivbaSelected.5$ivprgmth ) )
finalIVBA               <- ivbaSelected.5

### M3BXMO + ivprgmth ###
### How many months running pregnant were you? ###

## Merge the m3mop variable with i3mopSelected.3 (or w/e the latest one is) ##
m3bxmo        <- readRDS(file = "m3bxmo.rds")
m3bxmo$m3bxmo <- dk2na( data.vector = m3bxmo$m3bxmo, DKcode = 99 )
finalI3M3.1   <- merge( m3bxmo, i3mopSelected.3, by = c("compid"), all = FALSE )
finalI3M3     <- select( finalI3M3.1, compid, childid, i3size, m3bxmo, i3brth:i3food)

#only 3% of the m3mop's w/ 90 day surviving live born children are missing...,
#"how many running months pregnant were you when you had the child"?
#whereas 16% of the ivprgmth from IVBA are missing this variable
#how then do we get to 40% missing in the final variable?


# ### Class ###
# namesDF  <- names( finalIVBA )
# classDF  <- rep( "factor", length( names(finalIVBA ) ) )
# metaData <- data.frame( varNames = namesDF, class = classDF )
# metaData <- metaData[-c(1,2),]



### Full outer Join of ivbaSelected and i3Selected ###
#####
ivi3 <- 
        merge( 
                finalIVBA, 
                finalI3M3, 
                by = c("compid", "childid"), 
                all = TRUE )
#####
IDs <- select( ivi3, compid, childid )

### If IVBA exists and I3MOP doesn't exist, use IVBA data
### If I3MOP exists and IVBA doesn't exist, use I3MOP data
### If both I3MOP and IVBA exists, then use I3MOP except with ivhfvr
########################################################
### Do the combining with one pair, then scale it up ###
########################################################

varNamePairs.1 <- as.data.frame( rbind(names( finalIVBA ), names( finalI3M3 ) ) )
varNamePairs   <- select( varNamePairs.1, -V1, -V2 ) #remove compid and childid

merged_i3iv.1 <- 
        as.data.frame(
                sapply( varNamePairs, 
                        function(x) {
                                #variable codes for ivba and i3mop
                                iv <- as.vector(x)[1] 
                                i3 <- as.vector(x)[2]
                                
                                #data vectors
                                ivba_variable  <- as.numeric( as.character( ivi3[, iv] ) )
                                i3mop_variable <- as.numeric( as.character( ivi3[, i3] ) )
                                
                                mergedVar <- rep( NA, length(ivi3$compid) )
                                
                                if( iv == "ivhfvr" ) 
                                {
                                        #Which records have an I3MOP entry? 
                                        i3Exist <- !is.na( i3mop_variable  )
                                        mergedVar[ i3Exist ] <- i3mop_variable[ i3Exist ]
                                        
                                        #Which records have an IVBA entry? 
                                        ivExist <- !is.na( ivba_variable  )
                                        mergedVar[ ivExist ] <- ivba_variable[ ivExist ]
                                        
                                } else {
                                        
                                        #Which records have an IVBA entry? 
                                        ivExist <- !is.na( ivba_variable  ) 
                                        mergedVar[ ivExist ] <- ivba_variable[ ivExist ]
                                        
                                        #Which records have an I3MOP entry? 
                                        i3Exist <- !is.na( i3mop_variable  )
                                        mergedVar[ i3Exist ] <- i3mop_variable[ i3Exist ]
                                        
                                        
                                }
                                mergedVar
                        }
                )
        )

### Case 1: Baby dies w/in 90 days  - only has IVBA
### IVBA - (I3MOP -intersect- IVBA)
### 3971 -         2089               = 1882
intersection <- 
        merge( 
                finalIVBA[,c("compid","childid")], 
                finalI3M3[,c("compid","childid")], 
                by = c("compid", "childid"), 
                all = FALSE )
dim( intersection )#2089 

### Case 2: Baby dies after 90 days - has I3MOP and IVBA
### (IVBA -intersect- I3MOP)           2089


### Case 3: Baby survives           - has only I3MOP
### I3MOP - (I3MOP -intersect- IVBA)
#40130    -           2089           = 38041
#dim(finalI3M3) 
#40120
#dim(i3mopSelected)
#40130

#10 live born babies tracked in i3mop have mothers with no m3mop
#10/40130 = 0.00025

merged_i3iv <- cbind( IDs, merged_i3iv.1 )
names( merged_i3iv ) <- names( finalIVBA )


IDs           <- select( merged_i3iv, compid, childid )
merged_i3iv.2 <- select( merged_i3iv, -compid, -childid )



### Recode 4 Ivbreathing variables ###
#####
### PROBLEM:
### ivbrfast, ivbrwhee, ivbrgrnt, ivbrindr all show "NA" when ivrespRC is 0
### i.e when there is "no" breathing difficulty or cough/cold, there is missing data for the four specific types of breathing difficulties. 
### SOLUTION: 
### Replace the missing 4-breathing-problem-variable values with 0 when ivresp == 0.
### --> ivbrfast, ivbrwhee, ivbrgrnt, ivbrindr all recoded as "no" if ivrespRC is "no" 
### "No breathing difficulty or cough/cold --> no fast breathing, grunting, wheezing, or indrawing
### EXCEPT in a few cases where there are not missing values
### where 4 specific breathing difficulties are missing and general breathing difficulty is "no", 
###...recode 4 specific breathing difficulties as "no" 


# No breathing difficulty and Missing data for 4 specific ailments.
recode_NA_0 <- with( merged_i3iv.2, !is.na(ivrespRC) & ivrespRC == 0 & is.na(ivbrfast) ) 
merged_i3iv.2[ recode_NA_0, c("ivbrfast", "ivbrwhee", "ivbrgrnt", "ivbrindr") ] <- 0 
#####


### Breastfeeding Variables ###
#####
merged_i3iv.3 <- merged_i3iv.2

# Breastfed yes/no (nabfed, not using ivbfed)
# why was the baby no breastfed? (nabfedw)

### How many days was the baby breastfed? 

### ivfood   - was anything other than breastmilk offered to the infant within 3 days after birth?
### i3food   - was anything other than mother's breastmilk offered to the child within 3 days after birth? 


### nabfed   - was the baby breastfed after birth?
### nabfedt  - how soon after birth was the baby breastfed? (minutes/hours) [recode not breastfed as some big no. ]***
### nabfedw  - why was the baby not breastfed after birth? 
### nabfclst - was the infant fed colostrum after birth? 
#####

### Dysentery and Diarrhea ###
### ivdiar  - did the child have diarrhea in the 7 days before death/any time?
### ivdiard - how many days?
### ivdiarf - highest frequency on the worst day?

### where ivdiar == 0, recode ivdiard = 0 and ivdiarf = 0 i.e ...
### where the baby did not have diarrhea recode the # of days and the frequency on the worst day as 0

merged_i3iv.3 <- within( merged_i3iv.3, ivdiard[ ivdiar == 0 ] <- 0 )
merged_i3iv.3 <- within( merged_i3iv.3, ivdiarf[ ivdiar == 0 ] <- 0 )

### ivdys  - did the child have dysentery in the last 7 days before death/any time?
### ivdysd - how many days?
### where ivdys == 0, recode ivdysd as 0 i.e
### where the child did not have dysentery, change the # of days to 0

merged_i3iv.3 <- within( merged_i3iv.3, ivdysd[ ivdys == 0 ] <- 0 )

### immunizations ###
### ivimmun - was the infant ever given vaccinations?
### ivbcg   - 
### ivdpt   -
### ivpoli0 - 
### where ivimmun == 1, recode ivbgc, ivdpt, and ivpolio = 1 i.e
### where the baby was not given any immunizations, recode the three immunizations from NA to "not given" 

merged_i3iv.3 <- within( merged_i3iv.3, ivbcg[ ivimmun == 0 ] <- 0 )
merged_i3iv.3 <- within( merged_i3iv.3, ivdpt[ ivimmun == 0 ] <- 0 )
merged_i3iv.3 <- within( merged_i3iv.3, ivpolio[ ivimmun == 0 ] <- 0 )


### Limb Movement ### 
merged_i3iv.4 <- merged_i3iv.3
merged_i3iv.4$ivlimb <-  factor( merged_i3iv.4$ivlimb, ordered = TRUE )

### Infant's Size at Birth ###
### Ordered Factor         ###
temp <- factor( merged_i3iv.4$ivsize, ordered = TRUE )
merged_i3iv.4$ivsize <- temp 

#### Recode at a later time, remove for now ####
merged_i3iv.5 <- select( merged_i3iv.4, -ivsuck, -ivbfstop, -ivbfsta, -ivsbfeds1)

merged_i3iv_final <- cbind( IDs, merged_i3iv.5 )

###############################
### Recoding NAFF Variables ###
###############################

######################################################
### Load Selected Datasets with selected Variables ###
######################################################
#####
if( !"naffSelected.rds" %in% list.files() ) 
{
        loadedDatasets <- loadSelectedDatasets("naff", fromGoogleDoc = TRUE)
        naffSelected   <- loadedDatasets$naffSelected
        
} else {
        
        filename <- 
                "C:/Users/Bhavna/Google Drive/NMR Study/Data Work/JiVitA Datasets/naffSelected.rds"
        naffSelected <- readRDS( file = filename )
        
}

##################################################
### Mean/Medians of Anthropometry Measurements ###
##################################################
#####

naffSelected.2 <- select(naffSelected, -compid, -childid)

### Weight ### 
### Recode "refused" 6.66 to NA

naffSelected.2$naweight <- dk2na( data.vector = naffSelected.2$naweight, DKcode = 6.66)

### Height ###
naffSelected.2 <-
        mutate( naffSelected.2, 
                naheight = round( (naheight1 + naheight2 + naheight3)/3, 1 ) ) 

naffSelected.2 <- 
        select( naffSelected.2, -naheight1, -naheight2, -naheight3 ) 

naffSelected.2$naheight <- dk2na( data.vector = naffSelected.2$naheight, DKcode = 66.6)

### M Upper Arm Circumference ### 
naffSelected.2 <- 
        mutate( naffSelected.2, 
                namuac = round( (namuac1 + namuac2 + namuac3)/3, 1 ) )
naffSelected.2 <- 
        select( naffSelected.2, -namuac1, -namuac2, -namuac3 )

naffSelected.2$namuac <- dk2na( data.vector = naffSelected.2$namuac, DKcode = 66.6)

### Head Circumference ###
naffSelected.2 <- 
        mutate( naffSelected.2, 
                nahc = round( (nahc1 + nahc2 + nahc3)/3, 1 ) ) 
naffSelected.2 <- 
        select( naffSelected.2, -nahc1, -nahc2, -nahc3 )

naffSelected.2$nahc <- dk2na( data.vector = naffSelected.2$nahc, DKcode = 66.6)

### Chest Circumference ###
naffSelected.2 <- 
        mutate( naffSelected.2, 
                nacc = round( (nacc1 + nacc2 + nacc3)/3, 1) ) 
naffSelected.2 <- 
        select( naffSelected.2, -nacc1, -nacc2, -nacc3 )

naffSelected.2$nacc <- dk2na( data.vector = naffSelected.2$nacc, DKcode = 66.6)

#############################
### Recode the DKs to NAs ###
#############################
#####

Names     <- names( naffSelected.2 )
naffMeta  <- data.frame( Names = Names )

naffSelected.3   <- dk2naDF( df = naffSelected.2 )
naffSelected.3DF <- as.data.frame(naffSelected.3, stringsAsFactors = TRUE) 

#####      

##########################
### Recode the classes ###
##########################
#####
#sapply(naffSelected.3DF, class)
### Check the classes of the data ###
Class <- sapply(naffSelected.2, class) #.2 is before the DK recoding  
Class[Class == "character"] <- "factor"
naffMeta$Class <- Class


# # Transposed Meta Data, variable is "variable name", data is "DK codes", "class"
naffMeta.T <- as.data.frame(t(naffMeta), stringsAsFactors = TRUE)
names(naffMeta.T) <- names(naffSelected.2)
# 

naffSelected.4 <-naffSelected.3DF
invisible(
        sapply( naffMeta.T, #Variables x attributes of variables
                function(Var) {
                        
                        Class <- as.character( Var["Class"] )
                        Name  <- as.character( Var["Names"] )
                        data.vector <- naffSelected.4[,Name]
                        naffSelected.4[,Name] <<- classConv( input.vector = data.vector,
                                                             varClass = Class )
                })
)

#sapply(naffSelected.4, class)

################################
### convert hours to minutes ###
################################
#####

naffSelected.5 <- naffSelected.4
naffSelected.5 <- as.data.frame(naffSelected.5)

### Function to convert hours to minutes ###
###Inputs
#vector of data
#vector of units

# Function Tested: It is Functional


if( !exists("naff") ) load("naff.rdata") 
naffSelected.5[,"nawash"]   <- timeConv( data.vector = naffSelected.5$nawash,   units = naff$nawashu )
naffSelected.5[,"naclotht"] <- timeConv( data.vector = naffSelected.5$naclotht, units = naff$naclothtu )
naffSelected.5[,"nabfedt"]  <- timeConv( data.vector = naffSelected.5$nabfedt,  units = naff$nabfedtu )
#####


################################################################################
####################################
### Recoding/Combining variables ### 
####################################
################################################################################
#####

### 1) Fontanelle ###
#####

naffSelected.6 <- naffSelected.5
# nafnt  is y/n bulging fontanelle
# nafntd is how many days

# where nafnt == 0, recode nafntd = 0, i.e
# where the baby has no bulging fontanelle, recode the # of days with bulging fontanelle as 0

naffSelected.6 <- within( naffSelected.6, nafntd[ nafnt == 0 ] <- 0)

## Fontanelle variable recoded ##
#####

#2) Bathing the Baby .7
#####

naffSelected.7 <- naffSelected.6

#nawash  00- not washed, 0-97 - time, 98- 98 or more
#nawashw what was used

#nawash 
#recode 0-"not washed" to 100000, i.e a large number ( like infinity, washed at time = infinity )
naffSelected.7 <- within( naffSelected.7, nawash[ nawash == 0 ] <- 100000 )

#nawashw
#where nawash is 1000 (i.e baby not washed), recode nawashw as 0, i.e another category
levels(naffSelected.7$nawashw) <- c("1", "2", "3", "4", "5", "6","0")
naffSelected.7 <- within( naffSelected.7, nawashw[ nawash == 100000 ] <- 0 )


### Converting nawashW categories into binaries ###
naffSelected.7 <- replCat2bin( feature = "nawashw", naffSelected.7 )
naffSelected.7 <- select( naffSelected.7, -nawashw0 )

### Remove dummy variable nawashw6 - other ###
naffSelected.7 <- select( naffSelected.7, -nawashw6 )

#nawashv - was the vernix removed at the time of washing?
naffSelected.7$nawashvRC <- rep( NA, length( naff$nawashv ) )

# Category 1) 
# Baby washed and vernix removed
naffSelected.7 <- 
        within( naffSelected.7,
                nawashvRC[ !is.na( nawash ) & nawash != 100000 & nawashv == 1 ] <- 3 )
# Category 2) 
# Baby washed and vernix NOT removed
naffSelected.7 <- 
        within( naffSelected.7,
                nawashvRC[ !is.na( nawash ) & nawash != 100000 & nawashv == 0 ] <- 2 )

# Category 3) 
# Baby not washed (i.e vernix not removed implied)
naffSelected.7 <- 
        within( naffSelected.7,
                nawashvRC[ nawash == 100000 ] <- 1 )


naffSelected.7$nawashvRC <- factor( naffSelected.7$nawashvRC, ordered = T )
naffSelected.7 <- select( naffSelected.7, -nawashv )

naffSelected.7 <- select(naffSelected.7, 1:nawashw5, nawashvRC, naoil:nacc )

### OLD VERNIX CODE ###
# where the baby is not washed (nawash==1000) and nawashv is na (is.na(nawashv) ), 
# ... recode the missing value as "0" i.e <- 0
# if baby is not bathed, the vernix is not removed 
# It's no longer "if the baby was washed, was the vernix removed?"
# ...it's "Was the vernix removed yes or no?" 
# ... I don't think there's a way to remove the vernix without washing the baby. 
#naffSelected.7 <- within( naffSelected.7, nawashv[nawash==1000 & is.na(nawashv)] <- 0 )



## Bathing variables recoded 
#####

#3) Oil rubbed on babies body .8
#####

naffSelected.8 <- naffSelected.7

# 
#naoil  - was any oil rubbed on the baby's body?
#naoilk - what kind of oil was used? 

#where naoil == 0, recode naoilk = 5 (another category indicating "no oil")
levels( naffSelected.8$naoilk ) <- c("1","2","4","5")
naffSelected.8 <- within( naffSelected.8, naoilk[ naoil == 0 & !is.na( naoil ) ] <- 5 )
naffSelected.8 <- replCat2bin( feature = "naoilk", DF = naffSelected.8 )
naffSelected.8 <- select( naffSelected.8, -naoilk5 )

# #virtually all used mustard oil
# # 1        2     4 
# # 17509    31    28
#####

#4) Cloth .9
#####

naffSelected.9 <- naffSelected.8

# nacloth  - was clothed after birth?
# naclotht - how soon after birth? 

### where nacloth == 0, naclotht = 1000, a big number (supposed to be infinity, i.e never clothed)
naffSelected.9 <- within( naffSelected.9, naclotht[nacloth==0] <- 100000  ) 
#####

#5) Breastfeeding 
#nabfed   - was the newborn breastfed? y/n?
#nabfedt  - when was it breastfed? 
#nabfedw  - why was it not breastfed? 
#nabfclst - was the baby fed colostrum? 
#####

naffSelected.10 <- naffSelected.9

#nabfedt
# recode the "not breastfed" times to 10080 i.e a large number (time to breastfeeding = inf)
# where nabfed == 0, recode nabfedt = 10080
naffSelected.10 <- within( naffSelected.10, nabfedt[nabfed == 0 & is.na(nabfedt)] <- 10080 ) #10080 minutes, 1 week

#nabfedw - why was the baby not breastfed after birth? 
#1. mother ill or sick
#2. infant ill or weak
#3. nipple/breast problem
#4. insufficient milk
#5. child refuses to breastfeed 
#6. other

#recode the missing NA in "why not breastfed" where "baby was breasted" into "0", a new category
# i.e where nabfed==0 and is.na(nabfedw), recode nabfedw as "0"
# "there's already a 0" in there for somereason?

naffSelected.10 <- within( naffSelected.10, nabfedw[nabfed==1 & is.na(nabfedw) ] <- 0 )
naffSelected.10 <- replCat2bin( feature = "nabfedw", naffSelected.10 )
naffSelected.10 <- select(naffSelected.10, -nabfedw0)

### Remove kth dummy variable nabfedw6 - other ###
naffSelected.10 <- select( naffSelected.10, -nabfedw6 )

#nabfclst 
#where baby is not breastfed, recode "NA" in colostrum feeding to "not fed colostrum"
# nabfed==0 & is.na(nabfclst), recode nabfclst to "0"
naffSelected.10 <- within( naffSelected.10, nabfclst[ nabfed==0 & is.na(nabfclst) ] <- 0 )

#nacord, umbilicus, umb
### Turn the categories of "what was used to cut the cord?" into binary variables 
naffSelected.10 <- replCat2bin(feature = "nacord", DF = naffSelected.10)

### Remove kth dummy variable nacord5 - other ###
naffSelected.10 <- select( naffSelected.10, -nacord5 )

### naumb and naumb1,2,3
### where naumbl == 0, replace NAs in naumbl,2,3 with "0" ...
### indicating a new category where nothing was applied to the umbilicus
naffSelected.10 <- within( naffSelected.10, naumbl1[ naumbl==0  & is.na( naumbl1 ) ] <- 0 )
### 
naffSelected.10 <- select( naffSelected.10, -naumbl ) #no longer needed

### Tally up the naumbl1,2,3 responses into binary variables 
naffSelected.10 <- optionTallyBin( DF = naffSelected.10, 
                                   feature = "naumbl", 
                                   n_vars = 3, 
                                   opts_range = c(0,11) )

### Remove kth dummy variable naumbl11 - other ###
naffSelected.10 <- select(naffSelected.10, -naumbl11)

#sum( with( naffSelected.10, naumbl == 0 ), na.rm=T )
#table( naffSelected.10$naumbl )

### RECODE MISSING NAUMBL AS MISSING IN ALL NAUMBL*###
missingUmblIndices <- which( is.na(naffSelected.9$naumbl) ) 

naffSelected.10[ missingUmblIndices, 
                 c("naumbl0", "naumbl1","naumbl2","naumbl3","naumbl4","naumbl5",
                   "naumbl6","naumbl7","naumbl8","naumbl9","naumbl10")] <- NA

#####

#6) Crying
#####
#Nacry1, nacry2, nacry3
# What was done to make the baby cry? 8 options given and three possible total answers
#8 categories (there is "0" category and no "7" category)

naffSelected.11 <- naffSelected.10
naffSelected.11 <- optionTallyBin(DF = naffSelected.11, 
                                  feature = "nacry", 
                                  n_vars = 3, 
                                  opts_range = c(0,8) 
)

#Option 7 doesn't exist
naffSelected.11 <- select( naffSelected.11, -nacry7)

### Remove kth dummy variable nacry8 - other ###
naffSelected.11 <- select( naffSelected.11, -nacry8 )

### RECODE MISSING NACRY1 AS MISSING IN ALL NACRY*###
missingCryIndices <- which( is.na(naffSelected.10$nacry1) ) 

naffSelected.11[ missingCryIndices, 
                 c("nacry0","nacry1","nacry2","nacry3",
                   "nacry4","nacry5","nacry6") ] <- NA  
#interesting:  
# missing cry --> missing naumbbd, maumbpd, and nabruisd


#####

### 7) Bruising ###
#####
# nabruis  - did the baby
# nabruisd -

naffSelected.12 <- naffSelected.11
# filename <- 
#         "C:/Users/Bhavna/Google Drive/NMR Study/Data Work/JiVitA Datasets/naffSelected.11.rds"
# naffSelected.11 <- readRDS( filename )
# View(naffSelected.11)

### Bruising 
# where no bruising, recode NAs in "day of bruising onset" to 29 
naffSelected.12 <- within( naffSelected.12, nabruisd[ nabruis == 0 ] <- 1000 )

### Bleeding Umbilicus
#where no discharge/pus from umbilicus, recode NAs in "day of discharging/pus umbilicus onset" to 29
naffSelected.12 <- within( naffSelected.12, naumbbd[ naumbb == 0 ] <- 1000 )

### Discharging or Pus in umbilicus 
#where no discharge/pus from umbilicus, recode NAs in "day of discharging/pus umbilicus onset" to 29
naffSelected.12 <- within( naffSelected.12, naumbpd[ naumbp == 0 ] <- 1000 )


#####


### 8) Baby's Placement After Birth ###
#####
### naplaced
naffSelected.13 <- naffSelected.12
naffSelected.13 <- replCat2bin( feature = "naplaced", 
                                DF = naffSelected.13 )

### Remove kth dummy variable naplaced8 - other ###
naffSelected.13 <- select( naffSelected.13, -naplaced8 )

#####

### 9) Birth Place 
naffSelected.14 <- naffSelected.13 
naffSelected.14 <- replCat2bin( feature = "nahome", 
                                DF = naffSelected.13 )

### Remove kth dummy variable nahome7 - other ###
naffSelected.14 <- select( naffSelected.14, -nahome7 )

IDs       <- select( naff, compid, childid)
naffFinal <- cbind( IDs, naffSelected.14) 


###########################################################################
###########################################################################
############################### FINAL MERGE ###############################
###########################################################################
###########################################################################

naff_ivi3 <- merge( naffFinal, merged_i3iv_final, by = c("compid", "childid"), all = FALSE )
finalDF.1 <- merge( naff_ivi3, ktptFinal, by = c( "compid", "childid" ), all = FALSE )

finalDF.2 <- select( finalDF.1, -compid, -childid )
IDs       <- select( finalDF.1, compid, childid )
### Convert everything into a numeric vector ###
finalDF.3 <-
        as.data.frame( 
                sapply(finalDF.2, function(x) {
                        x <- as.numeric( x )
                })
        )
finalDF.4 <- cbind( IDs, finalDF.3 )
finalDF   <- select( finalDF.4, -nawashvRC ) 

saveRDS(   object = finalDF, file = "finalDFDummyCorrected.rds" )
write.csv(      x = finalDF, file = "finalDFDummyCorrected.csv" )



######################################################################
#################### IMPUTATION OF MISSING DATA ######################
######################################################################

library(mice)

setwd("C:/Users/Bhavna/Google Drive/NMR Study/Data Work/JiVitA Datasets")
filename <- "finalDFDummyCorrected.rds"
miceDF.1 <- readRDS( file = filename )
miceDF   <- select( miceDF.1, -survStat_28Day, -childid, -compid)

##############################
### Impute Single Data Set ###
##############################
mice_output <- mice( miceDF, m = 1, method = "pmm" )
saveRDS(file = "mice_output.rds", object = mice_output)


single_imputation  <- complete( mice_output, action = 1, include = FALSE )
saveRDS(file = "finalDF.rds", object = single_imputation )
write.csv( file = "finalDF.csv", x = single_imputation )
