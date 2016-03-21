# Machine_learning_function_library

###For Neonatal mortality prediction with structured JiVitA datasets (JHSPH)###

Object "mydata" is a data frame created by the script "complete_PreProcessing_workflow.r"

Library contains R code to run analysis on a data frame and produce all relevant metrics (AUC, PPV, NPV, Sens, Spec) at varying levels of sensitivity. Models used are Logistic Regression, Random Forest, and SVMs (linear kernel). 

Source/run the script for the function library and you will have a number of functions defined in your environment. 
LRModel_Results( mydata, r = 3 )
RFModel_Results( mydata, r = 3 )
SVML_Model_Results( mydata, r = 3 ) 
