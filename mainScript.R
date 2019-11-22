#### 0. environment ####

rm(list=ls()) 
gc()
cat("\014") 

#### 1. packages ####
library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#### 1. read data and parameters ####

source("readInputData.R")
source("loadParameters.R")
source("functions/mainFunctions.R")
#source("functions/plotFunctions.R")

location <- "Junín"

soilTypes = list (list (soil_series='Santa Isabel',	
                        soil_family='Hapludol típico'),
                  list (soil_series='Saforcada',	
                        soil_family='Hapludol éntico'),
                  list (soil_series='Delgado',	
                        soil_family='Argiudol típico'),
                  list (soil_series='Ortiz de Rosas',	
                        soil_family='Hapludol thapto árgico')
                  )

waterLoop <- c("low","mid","high")

#single execution
soil <- soilTypes[[1]]
waterCont <- waterLoop[2]
locationData <- list(location=location,
                     soil_series=soil$soil_series,
                     soil_family=soil$soil_family,
                     water=waterCont)

decisionStructured <- structureDecision(inputData,locationData)
decisionEvaluated <- calculateUtility(decisionStructured,parametersEU)
decisionEvaluated <- calculateRegrets(decisionEvaluated)

MOSolList <- getMOFront(decisionEvaluated, parametersList) 

#plot MO
grid.arrange(plotMOLines(MOSolList),plotMOVerticalBars(MOSolList), nrow = 2)

#plot pure strategies
#plotPureStrategies2(MOSolList)

#explore vulnerabilities

#### 3. tree parameters ####
control.rpart <- rpart.control(minbucket = 80,minsplit = 80, cp = 0.015, 
                               xval = 10,
                               maxdepth = 3)

loss.matrix <-  matrix(c(0, 1,2, 0), nrow=2, byrow=TRUE)

classTree <- getTreeByObjective(MOSolList,"E_ROI")

#classTree <- getTreeByObjectiveStr(MOSolList,"E_ROI", str=881)

SolSplited <- splitSolByClassTree(MOSolList, classTree)

## customized trade off plots
plotCustTradeOff(SolSplited$MOSolListGood,SolSplited$MOSolListBad, classTree, objBad="E_ROI", 
                 annotateId=NA,getBestStrategies(MOSolList))

grid.arrange(linePlotAternatives(MOSolList,SolSplited,classTree, 66, 1001),
             verticalBarsPlotAlternatives(MOSolList,66,1001), nrow = 2)

plotTradeOff(SolSplited$MOSolListGood,SolSplited$MOSolListBad, classTree, objBad="E_ROI", annotateId=c(66,896,1001,881))

plotTradeOff(SolSplited$MOSolListGood,SolSplited$MOSolListBad, classTree, objBad="E_ROI", annotateId=getBestStrategies(MOSolList))
grid.arrange(linePlotAternatives(MOSolList,SolSplited,classTree, 66, 1001),
             verticalBarsPlotAlternatives(MOSolList,66,1001), nrow = 2)



#explore vulnerabilities
classTree <- getTreeByObjective(MOSolList,"E_ROI")

SolSplited <- splitSolByClassTree(MOSolList, classTree)

plotTradeOff(SolSplited$MOSolListGood,SolSplited$MOSolListBad, classTree, objBad="ES_U", annotateId=c(890,999))

#plot tree and trade-off
grid.arrange(linePlotAternatives(MOSolList,SolSplited,classTree, 999, 890),
             verticalBarsPlotAlternatives(MOSolList,999,890), nrow = 2)







getBestStrategies(MOSolList)




#loop execution

#single execution
for (soil in soilTypes ) {
  
for ( waterCont in waterLoop) {

locationData <- list(location=location,
                     soil_series=soil$soil_series,
                     soil_family=soil$soil_family,
                     water=waterCont)

decisionStructured <- structureDecision(inputData,locationData)
decisionEvaluated <- calculateUtility(decisionStructured,parametersEU)
decisionEvaluated <- calculateRegrets(decisionEvaluated)

MOSolList <- getMOFront(decisionEvaluated, parametersList) 

#plot MO
grid.arrange(plotMOLines(MOSolList),plotMOVerticalBars(MOSolList), nrow = 2)

#explore vulnerabilities
classTree <- getTreeByObjective(MOSolList,"R3Q_ROI")
classTree

}}

##
plotSimLocs(simulatedLocs, "Junín, Buenos Aires")

##
#correlaation between variable.names
tempDf <- unique(decisionEvaluated$scenarioDf[,c("y2.1","ensoVar","PP","p_dic2marNY")])
rownames(tempDf) <- tempDf$y2.1
tempDf <- tempDf[rownames(inputData$pricesDf),]
tempDf2 <- cbind(inputData$pricesDf, tempDf)
round(cor(tempDf2[,c("S","W","Mt","MT","ensoVar","PP","p_dic2marNY")]),2)


