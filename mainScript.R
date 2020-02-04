#### 0. environment ####

rm(list=ls()) 
gc()
cat("\014") 

#### 1. packages ####
library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#### 1. read data and parameters ####
source("functions/readInputData.R")
source("functions/loadParameters.R")
source("functions/mainFunctions.R")

location <- "Junín"
waterCont <- "mid"
soil_series <- 'Santa Isabel'
soil_family <- 'Hapludol típico'

locationData <- list(location=location,
                     soil_series=soil_series,
                     soil_family=soil_family,
                     water=waterCont)

#### structure de decision into strategies and scenarios and evaluate combination with: 
#### FWNM, FC, ROI, and Rotation
decision <- structureDecision(inputData,locationData)

#plot pure strategies
plotPureStrategies(decision)

#### add to evalDf Utility and Regrets
decision <- calculateMetrics(decision,parametersEU)

#### calculate objectives 
decision <- calculateObjectivesByStrategy(decision,parametersList)

#### obtain the strategies optimal front
decision <- getMOFront(decision, parametersList) 

#### plot MO
plotMOChart(decision)

#### explore scenarios by tree
classTree <- getTreeByObjective(decision,"E_ROI")

#### split the scenario set
decisionSplit <- splitByClassTree(decision, classTree)

#### explore vulnerabilities
plotTradeOff(decisionSplit,classTree)

#### compare two strategies by objective into Good and Bad
compare2strategies(decision,decisionSplit,classTree,66,1001)
