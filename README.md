## Repository for the Little Rock child maltreatment project. 

This repository will be used to store and distribute the reports and methods for the different analyses performed for child maltreatment project in LR, along with some of the source codes. 


## Reports and R codes. 

The main HTML report with the full predictive analyses can be accessed here: [LR child maltreatment predictive analyses](https://dattahub.github.io/crime-analysis/LR_PAP_Draft_0923.html). Some exploratory analyses to look for patterns are in [this preliminary report](http://dattahub.github.io/crime-analysis/LR-child-mt-expl.html).


R codes for reproducing this report are can be found at this location: [https://github.com/DattaHub/PAP-child/R-Rmd](https://github.com/DattaHub/PAP-child/R-Rmd). The main files are: 

1. replicate_pap_XY_pred.R: replicates the entire pipeline subject to availability of shapefiles in a physical folder. The directory names must be changed for R to find these files. 
2. FEA_CREATE_VARIABLES_LR_2.R: creates variables (predictors and responses) that are used by the replicate_* file. Again, subject to shapefiles and need to change the directories.
3. FUNCTIONS_VAPAP_LR.R. : a host of add-in functions that are used in the replicate_* code. 

Please direct your questions to: 

1. Grant Drawve : drawve@uark.edu 
2. Shaun Thomas : shaun@uark.edu 
3. Jyotishka Datta: jd033@uark.edu. (JD maintains this repository)





