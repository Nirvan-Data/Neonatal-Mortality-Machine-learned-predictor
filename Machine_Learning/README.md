# Machine_learning_function_library

###For Neonatal mortality prediction with structured JiVitA datasets (JHSPH)###

The functions take an abject "mydata" as an argument. "mydata" is the final pre-processed data frame created by the script "complete_PreProcessing_workflow.r". The data frame "mydata" must have the outcome variable "survStat_28day". The user can redfine the data frame to select subsets of predictor variables.

The library contains R code to create a classfier on a data frame and produce all relevant performance metrics (AUC, PPV, NPV, Sens, Spec) at varying levels of sensitivity. Models used are Logistic Regression, Random Forest, and SVMs (linear kernel). 

Source/run the script for the function library and you will have a number of functions defined in your environment. 
LRModel_Results( mydata, r = 3 )
RFModel_Results( mydata, r = 3 )
SVML_Model_Results( mydata, r = 3 ) 
