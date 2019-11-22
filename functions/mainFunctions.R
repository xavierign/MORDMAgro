library(emoa)
library(ggplot2)
library(caret)
library(ggtern)
library(reshape2)
library(rpart.plot)
library(gridBase)
library(cvar)


convertToPricesKey <- function (crop) {
  outKey <- substr(crop, 1, 1)
  if (crop=="MZ-E") {
    outKey <- 'Mt'    
  } else if(crop=="MZ-L") {
    outKey <- 'MT'    
  }
  return(outKey)
}

simulateCropsVarInScenarios <- function(inputData,locationData) {
  
  #build scenarios
  yieldsByLocList <- inputData$yieldsList[[locationData$location]][[locationData$soil_series]][[locationData$soil_family]][[locationData$water]]
  
  cropVarMarginsDf <- expand.grid(rownames(yieldsByLocList[[1]][[1]]),rownames(inputData$pricesDf))
  colnames(cropVarMarginsDf) <- c('climaticYear','priceYear')
  cropVarDirectCostsDf <- cropVarMarginsDf
  
  for (crop in names(yieldsByLocList)) {
    for (management in names(yieldsByLocList[[crop]])) {
      
      df <- yieldsByLocList[[crop]][[management]]
      
      pricesVec <- inputData$pricesDf[cropVarMarginsDf[,'priceYear'],convertToPricesKey(crop)] 
      
      yieldsVec <- df[cropVarMarginsDf[,'climaticYear'],'yield']
      
      income <- pricesVec*yieldsVec/1000
      
      variableCosts <- sum(inputData$costsDf[management,c('Harvest.percent','Transportation.cost.percent','Commercialization.cost.percent')])
      
      netIncome <- income*(1-variableCosts)
      #netIncome <- income - variableCosts
      
      fixedCosts <- sum(inputData$costsDf[management,c('Seeds','Agrochemicals','Others','Land.rental','Labors')]) 
      
      netMargin <- netIncome - fixedCosts
      
      cropVarMarginsDf[,management] <- netMargin
      
      cropVarDirectCostsDf[,management] <- fixedCosts
      
    }
  }
  cropVarList <- list(cropVarMarginsDf=cropVarMarginsDf,cropVarDirectCostsDf=cropVarDirectCostsDf)
  return(cropVarList)
  
}

aggregateDoubleCrops <- function(inputData, cropVarList) {
  cropAltMarginsDf <- cropVarList$cropVarMarginsDf
  cropAltDirectCostsDf <- cropVarList$cropVarDirectCostsDf
  managements <- colnames(cropAltMarginsDf)[3:length(colnames(cropAltMarginsDf))]
  
  
  firstSeqCrops <- managements[which(inputData$costsDf[managements,"Seq"]==1)]
  secondSeqCrops <- managements[which(inputData$costsDf[managements,"Seq"]==2)]
  
  for (fCrop in firstSeqCrops) {
    for (sCrop in secondSeqCrops) {
      #sum of margins
      cropAltMarginsDf[,paste(fCrop, sCrop, sep="->")] <- cropAltMarginsDf[,fCrop] + cropAltMarginsDf[,sCrop]
      
      #just consider the first direct cost
      cropAltDirectCostsDf[,paste(fCrop, sCrop, sep="->")] <- cropAltDirectCostsDf[,fCrop] + cropAltDirectCostsDf[,sCrop] 
                                                                #inputData$costsDf[fCrop,'Land.rental'] +
                                                                #inputData$costsDf[sCrop,'Land.rental']
    }
  }
  
  #delete columns of f and s crops 
  cropAltDirectCostsDf[,c(firstSeqCrops,secondSeqCrops)] <- NULL
  cropAltMarginsDf[,c(firstSeqCrops,secondSeqCrops)] <- NULL
  
  cropAltList <- list(cropAltMarginsDf=cropAltMarginsDf,cropAltDirectCostsDf=cropAltDirectCostsDf)
  return(cropAltList)
}

simulateCropsAltInScenarios <- function(inputData,locationData) {
  cropVarList <- simulateCropsVarInScenarios(inputData,locationData)
  cropAltList <- aggregateDoubleCrops(inputData,cropVarList)
  return(cropAltList)
}

calculateClimateParameters <- function(tempDf,periods){
  tempDf$date <- as.Date(tempDf$date, format="%Y-%m-%d")
  #tempDf$PP[is.na(tempDf$PP)]<- 0
  
  #yearsCompleted <- names(table(format(tempDf$date,format="%Y")))[table(format(tempDf$date,format="%Y"))>360]
  yearsCompleted <- names(table(format(tempDf$date,format="%Y")))
  
  ppList <- list()
  for (y in yearsCompleted) {
    ppList[[y]] <- list()
    ref <- as.Date(paste(y,"-01-01", sep=""), format="%Y-%m-%d")
    for (p in names(periods)) {
      limInf <- ref + periods[[p]]$limInf
      limSup <- ref + periods[[p]]$limSup
      
      ids <- tempDf$date >= limInf & tempDf$date <= limSup
      tempVec <- tempDf$rain[ids]
      
      if(sum(is.na(tempVec))>10) {
        ppList[[y]][[p]] <- NA
      } else {
        tempVec[is.na(tempVec)] <- 0
        ppList[[y]][[p]] <-  sum(tempVec)
      }
    }
  }
  outMat <- matrix(unlist(ppList),ncol = length(periods), byrow = T)
  rownames(outMat) <- yearsCompleted
  colnames(outMat) <- names(periods)
  outMat[outMat==0] <- NA
  outMat[!complete.cases(outMat),'PP'] <- NA
  return(outMat)
}

structureDecision <- function(inputData,locationData) {
  
    ##### create strategies: strategyDf
  
    cropAltList <- simulateCropsAltInScenarios(inputData,locationData)
    strategyDf <- expand.grid(seq(0,1,by=0.1),seq(0,1,by=0.1),seq(0,1,by=0.1),seq(0,1,by=0.1),seq(0,1,by=0.1),seq(0,1,by=0.1))

    #remove one column if strategies are 5
    colMax <- ncol(cropAltList$cropAltMarginsDf)-2
    strategyDf = strategyDf[,1:colMax]
    
    ids <- round(rowSums(strategyDf),2)==1

    strategyDf <- strategyDf[ids,]
    
    rownames(strategyDf) <- NULL
    
    colnames(strategyDf) <- colnames(cropAltList$cropAltMarginsDf[3:(2+colMax)])
  
    #### create evaluation Df: evalDf
    
    #write metrics
    evalDf <- expand.grid(sc=1:nrow(cropAltList$cropAltMarginsDf),st=1:nrow(strategyDf))
    
    evalDf$FWNM <- rowSums(as.matrix(cropAltList$cropAltMarginsDf[evalDf$sc,colnames(strategyDf)]) * as.matrix(strategyDf[evalDf$st,]))

    # include costs
    evalDf$fixedCosts <- rowSums(as.matrix(cropAltList$cropAltDirectCostsDf[evalDf$sc,colnames(strategyDf)]) * as.matrix(strategyDf[evalDf$st,]))
    
    # include ROI
    evalDf$ROI <- evalDf$FWNM/evalDf$fixedCosts

    # include rotation
    cropCols <- inputData$costsDf[names(strategyDf),'crop']
    cropCols[is.na(cropCols)] <- "W"
    patterns <- unique(cropCols)
    tempDf <- sapply(patterns, function(xx) rowSums(strategyDf[,cropCols==xx, drop=FALSE]))
    tempVec <- apply(tempDf,MARGIN=1,FUN=max)
    tempVec[tempVec < 0.5] <- 0.5 #0.5 is the maximum
    evalDf$rotation <- tempVec[evalDf$st]
    #### create scenario Df
    #build scenarioDf based on scnearioYieldPricesDf
    
    cropAltList$cropAltMarginsDf$priceYear <- as.character(cropAltList$cropAltMarginsDf$priceYear)
    cropAltList$cropAltMarginsDf$climaticYear <- as.character(cropAltList$cropAltMarginsDf$climaticYear)

    scenarioDf <- data.frame(W= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'W'],
                             S= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'S'],
                             Mt= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'Mt'],
                             MT= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'MT'],
                             S2W= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'S']/inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'W'],
                             W2M= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'W']/inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'Mt'],
                             M2S= inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'Mt']/inputData$pricesDf[cropAltList$cropAltMarginsDf$priceYear,'S'],
                             row.names = rownames(cropAltList$cropAltMarginsDf))

    #append rains data
    
    #get the location id from locationDf
    loc_id <- as.character(inputData$locationsDf[locationData$location,'id_num'])
    
    #calculate climate aggregated data
    climateAggregatedDf <- data.frame(calculateClimateParameters(inputData$climateList[[loc_id]],
                                                                 periods))
    scenarioDf[,colnames(climateAggregatedDf)] <- NA
    scenarioDf[,colnames(climateAggregatedDf)] <- climateAggregatedDf[cropAltList$cropAltMarginsDf$climaticYear,]

    for (col in colnames(climateAggregatedDf)) {
      scenarioDf[,paste("Q",col, sep="")] <- quintile(scenarioDf[,col])
    }
    
    #append ENSO
    scenarioDf[,colnames(ensoDf)] <- NA
    scenarioDf[,colnames(ensoDf)] <-ensoDf[cropAltList$cropAltMarginsDf$climaticYear,]

    #convert ENSO to quintiles
    #scenarioDf$ensoVar <- ceiling(scenarioDf$ensoVar)
    
    strategyDf <- cbind(strategyDf, data.frame(totalSoy=tempDf[,"SB"],totalMaize=tempDf[,"MZ-E"] +tempDf[,"MZ-L"] ,
                                                totalWheat=tempDf[,"W"] ,
                                                col=rgb(tempDf[,"MZ-E"] +tempDf[,"MZ-L"],
                                                        tempDf[,"SB"],tempDf[,"W"])),
                                                col2 = rgb(
                                                      #red
                                                      round((tempDf[,"MZ-E"] +tempDf[,"MZ-L"])*255 + 
                                                              tempDf[,"SB"]*0 +
                                                              tempDf[,"W"]*0,0),
                                                      #green
                                                      round((tempDf[,"MZ-E"] +tempDf[,"MZ-L"])*0 + 
                                                              tempDf[,"SB"]*171 +
                                                              tempDf[,"W"]*64,0),
                                                      #blue
                                                      round((tempDf[,"MZ-E"] +tempDf[,"MZ-L"])*0 + 
                                                              tempDf[,"SB"]*0 +
                                                              tempDf[,"W"]*255,0)
                                                      ,maxColorValue = 255)
                      )

    return(list(strategyDf=strategyDf,scenarioDf=scenarioDf,evalDf=evalDf))
}  

calculateUtility <- function (decisionStructured,parametersEU) {
  R <- parametersEU$R
  w0 <- parametersEU$w0
  
  if (R==1) {
    decisionStructured$evalDf$utility <- log(decisionStructured$evalDf$FWNM+w0)
  } else {
    decisionStructured$evalDf$utility <- (decisionStructured$evalDf$FWNM+w0)^(1-R)/(1-R)
  }
  return(decisionStructured)
}

calculateRegret <- function(decisionStructured) {
  
  maxFWNMByScenario <- as.numeric(tapply(decisionStructured$evalDf$FWNM,decisionStructured$evalDf$sc,max))
  tempMaxFWNMByScenario <- maxFWNMByScenario[decisionStructured$evalDf$sc]
  decisionStructured$evalDf$regret <- tempMaxFWNMByScenario -  decisionStructured$evalDf$FWNM
  return(decisionStructured)
}

calculateRegrets <- function(decisionStructured, metrics = c('ROI','utility')) {
  
  for (metric in metrics) {
    maxMetricByScenario <- as.numeric(tapply(decisionStructured$evalDf[,metric],decisionStructured$evalDf$sc,max))
    
    tempMaxMetricByScenario <- maxMetricByScenario[decisionStructured$evalDf$sc]
    
    decisionStructured$evalDf[,paste('Reg_',metric,sep="")] <- tempMaxMetricByScenario -  decisionStructured$evalDf[,metric]
  }
  return(decisionStructured)
}


calculateRegretROI <- function(decisionStructured) {
  
  maxROIbyScenario <- as.numeric(tapply(decisionStructured$evalDf$ROI,decisionStructured$evalDf$sc,max))
  tempMaxROIbyScenario <- maxROIbyScenario[decisionStructured$evalDf$sc]
  decisionStructured$evalDf$regretROI <- tempMaxROIbyScenario -  decisionStructured$evalDf$ROI
  return(decisionStructured)
}

calculateEquiWEU <- function (utility,parametersEU) {
  R <- parametersEU$R
  w0 <- parametersEU$w0
  if (R==1) {
    out <- exp(utility)-w0
  } else {
    out <- (utility*(1-R))^(1/(1-R)) - w0
  }
  return(out)
}

calculateES <- function(x,parametersEU){
  thres <- quantile(x, probs=parametersEU$q, type=8)
  x <- x[x<thres]
  
  #return(calculateEquiWEU(mean(x), parametersEU))
  #return(-calculateEquiWEU(mean(x),parametersEU))
  return(mean(x))
}

calculatePerformaceByStrategy <- function(decisionEvaluated,parametersList) {
  
  parametersEU <- parametersList$parametersEU
  parametersRegret <- parametersList$parametersRegret
  
  #return a matrix rows: strategies, cols: multiple performance metrics
  #EUM
  E_U <- calculateEquiWEU(tapply(decisionEvaluated$evalDf$utility, decisionEvaluated$evalDf$st, FUN = mean), parametersEU)
  
  #ES (aka CVaR)
  ES_U <- tapply(decisionEvaluated$evalDf$utility, decisionEvaluated$evalDf$st, FUN = calculateES, parametersEU=parametersEU)
  ES_U <- calculateEquiWEU(ES_U,parametersEU)
  
  R3Q_U <- tapply(decisionEvaluated$evalDf$Reg_utility, decisionEvaluated$evalDf$st, FUN = quantile,prob=parametersRegret$q, type=qType)
  if (parametersEU$R==1) {
    R3Q_U <- exp(R3Q_U + log(parametersEU$w0)) - parametersEU$w0
  } else {
    R3Q_U <- (R3Q_U*(1-parametersEU$R)+(parametersEU$w0)^(1-parametersEU$R))^(1/(1-parametersEU$R)) - parametersEU$w0
  }
  #rotation
  rotation <- tapply(decisionEvaluated$evalDf$rotation, decisionEvaluated$evalDf$st, FUN = mean)
  
  #fixedCosts
  fixedCost <- tapply(decisionEvaluated$evalDf$fixedCosts, decisionEvaluated$evalDf$st, FUN = mean)
  
  E_ROI <- tapply(decisionEvaluated$evalDf$ROI, decisionEvaluated$evalDf$st,FUN = mean)
  ES_ROI <- tapply(decisionEvaluated$evalDf$ROI, decisionEvaluated$evalDf$st, FUN = calculateES, parametersEU=parametersEU)
  
  R3Q_ROI <- tapply(decisionEvaluated$evalDf$Reg_ROI, decisionEvaluated$evalDf$st, FUN = quantile,prob=parametersRegret$q, type=qType)
  
  outDf <- data.frame(row.names = names(E_U),
                      rotation=rotation,
                      fixedCost=fixedCost,
                      ES_U=ES_U,
                      E_U=E_U,
                      R3Q_U=R3Q_U,
                      
                      ES_ROI=ES_ROI,
                      E_ROI=E_ROI,
                      R3Q_ROI=R3Q_ROI)
  
  return(outDf)
}

calculatePerformaceMetricsByStrategy <- function(decisionEvaluated,parametersList, 
                                                   metrics=c('EU','ES','regret', 'rotation','fixedCost','ROI', 'regretROI')) {
    
  parametersEU <- parametersList$parametersEU
  parametersRegret <- parametersList$parametersRegret
  
  #return a matrix rows: strategies, cols: multiple performance metrics
  #EUM
  EU <- -calculateEquiWEU(tapply(decisionEvaluated$evalDf$utility, decisionEvaluated$evalDf$st, FUN = mean), parametersEU)
  
  #ES (aka CVaR)
  ES <- tapply(decisionEvaluated$evalDf$utility, decisionEvaluated$evalDf$st, FUN = calculateES, parametersEU=parametersEU)
  
  #regret 
  regret <- tapply(decisionEvaluated$evalDf$regret, decisionEvaluated$evalDf$st, FUN = quantile,prob=parametersRegret$q, type=qType)
  
  #rotation
  rotation <- tapply(decisionEvaluated$evalDf$rotation, decisionEvaluated$evalDf$st, FUN = mean)
  
  #fixedCosts
  fixedCost <- tapply(decisionEvaluated$evalDf$fixedCosts, decisionEvaluated$evalDf$st, FUN = mean)
  
  #ROI <- tapply(decisionEvaluated$evalDf$ROI, decisionEvaluated$evalDf$st,FUN = quantile,prob=parametersRegret$q, type=qType)
  
  ROI <- -tapply(decisionEvaluated$evalDf$ROI, decisionEvaluated$evalDf$st,FUN = mean)
  
  regretROI <- tapply(decisionEvaluated$evalDf$regretROI, decisionEvaluated$evalDf$st, FUN = quantile,prob=parametersRegret$q, type=qType)
  
  outDf <- data.frame(row.names = names(EU),EU=EU,regret=regret,ES = ES,
                      fixedCost=fixedCost,
                      rotation=rotation, ROI= ROI,regretROI=regretROI)
  
  return(outDf[,metrics])
}


getMOFront <- function(decisionEvaluated, parametersList, metricsMaxBool=c(F,F,T,T,F,T,T,F)) {
  
  performanceMetrics <- calculatePerformaceByStrategy(decisionEvaluated,parametersList)
  
  for (col in which(metricsMaxBool)) {
    performanceMetrics[,col] <- -performanceMetrics[,col]
  }
  
  #exclude rotation in MOcalculation
  performanceMetrics <- performanceMetrics[,-1]
  
  #reorder columns 
  performanceMetrics <- performanceMetrics[,c(2:7,1)]
  
  MOfrontDf <- t(nondominated_points(t(as.matrix(performanceMetrics))))
  
  #mark if it is in front
  decisionEvaluated$strategyDf$MOfront <- F
  decisionEvaluated$strategyDf[rownames(MOfrontDf),"MOfront"] <- T
  
  return(list(decisionEvaluated=decisionEvaluated,performanceMetrics=performanceMetrics[rownames(MOfrontDf),]))
}

getBestStrategies <- function(MOSolList) {
  tempDf <-  MOSolList$performanceMetrics[order(MOSolList$performanceMetrics$E_U),]
  
  bestVect <- rownames(tempDf)[apply(tempDf, MARGIN=2,FUN=which.min)]
  names(bestVect) <- colnames(tempDf)
  return(bestVect)
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

plotMOLines <- function(MOSolList) {
  
  #extract the best strategies by objective
  bestVect <- getBestStrategies(MOSolList)
  
  #MO front by objective
  frontScaled <- apply(MOSolList$performanceMetrics,MARGIN=2,FUN=range01)
  
  tempStDf <- MOSolList$decisionEvaluated$strategyDf[MOSolList$decisionEvaluated$strategyDf$MOfront,]
  
  df <- NULL
  for (column in colnames(frontScaled)) {
    dfTemp <- data.frame(id=rownames(frontScaled),value =frontScaled[,column],
                         objective=column,
                         color = tempStDf$col,
                         stringsAsFactors = F)
    df <- rbind(df,dfTemp)
  }
  
  maxVec <- apply(MOSolList$performanceMetrics,MARGIN=2,FUN=max)
  minVec <- apply(MOSolList$performanceMetrics,MARGIN=2,FUN=min)
  yRange <- seq(0,1,by=0.25)
  
  rangeList <- list()
  for(c in names(maxVec)) {
    rangeList[[c]] <- yRange*(maxVec[c]-minVec[c]) + minVec[c]
  }
  
  df$objective <- factor(df$objective, levels =colnames(MOSolList$performanceMetrics))
  pal3 <- as.character(decisionEvaluated$strategyDf$col2)
  names(pal3) <- rownames(decisionEvaluated$strategyDf)
  g2<- ggplot(df, aes(x = objective, y = value,group=id, color=id, alpha=0.3)) + 
    geom_line() + scale_color_manual(values=pal3) + 
    ggtitle("Performance of strategies by objective") +  
    theme(plot.title = element_text(hjust = 0.5),
          #plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm"),
          text = element_text(family="Times"),
          legend.position="none",axis.title.y = element_blank(), axis.text.y=element_blank()) +
    scale_y_continuous(trans = "reverse") +
    scale_x_discrete(labels=paste0(names(bestVect),c("[$]","[$]","[$]","[%]","[%]","[%]","[$]"),sep=""))

  sign_ <- c(-1,-1,1,-1,-1,1,1)
  decimPlaces <- c(0,0,0,2,2,2,0)
  for (obj in names(bestVect)){
    ind <- which(obj==names(bestVect))
    
    g2 <- g2 + annotate('text', x=ind,y=yRange, label=sign_[ind]*round(rangeList[[obj]],decimPlaces[ind]),size=3,  hjust = 1,family="Times")
  }
  return(g2)
}

plotMOVerticalBars <- function(MOSolList) {
  #extract the best strategies by objective
  bestVect <- getBestStrategies(MOSolList)
  
  tempVec <- decisionEvaluated$strategyDf[bestVect,]
  rownames(tempVec) <- names(bestVect)
  
  tempDf <- tempVec #[rev(c('fixedCost','rotation','ES','EU','regret')),]
  tempDf<- t(tempDf[,colnames(tempVec) %in% rownames(colorsPal)])
  
  tempDf <- tempDf[order(colorsPal[rownames(tempDf),'orders'],decreasing = T),]
  
  dfmelt <- melt(tempDf, id.vars = "crop")
  colnames(dfmelt) <- c('crop','id','value')
  dfmelt$id <- as.factor(dfmelt$id)
  
  #order factors to plot
  lev <- unique(dfmelt$crop)
  
  #if .1 in names replace by =""
  #dfmelt$crop <- gsub(".1", "", dfmelt$Var1)
  dfmelt$crop <- factor(dfmelt$crop, levels = lev[order(colorsPal[lev,'orders'])])
  
  #pallete
  pal2 <- as.character(colorsPal[,2])
  names(pal2) <- rownames(colorsPal)
  
  #barplot
  levels(dfmelt$id) <- names(bestVect)
  g3 <- ggplot(dfmelt, aes(x=id, y=value,fill=crop)) +
    geom_bar(stat="identity") + 
    #coord_flip() + 
    scale_fill_manual(values = pal2, labels =rev(CAsNames)) +
    theme( text=element_text(family="Times"),  #,
          #plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
          axis.title.y = element_blank(), axis.text.y=element_blank()) +
    #ylab("proportion of land") +
    xlab("objective") +
    ggtitle("Best strategies by objective") + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
  return(g3)
  
}


plotMOfront <- function(MOSolList) {
  
  #extract the best strategies by objective
  bestVect <- getBestStrategies(MOSolList)

  ##tri plot
  tempStDf <- MOSolList$decisionEvaluated$strategyDf[MOSolList$decisionEvaluated$strategyDf$MOfront,]
  
  triplotDf <- data.frame(Soy= tempStDf$totalSoy,
                          WheatSoy= tempStDf$totalWheat,
                          Maize=tempStDf$totalMaize)
  
  df2 <- MOSolList$decisionEvaluated$strategyDf[c(bestES,bestEU,bestRegret,bestFixedCost),c('totalSoy', 'totalMaize', 'totalWheat')]
  
  s <- seq(0,1, by=0.2)
  
  g1 <- ggtern(triplotDf,aes(Soy,WheatSoy,Maize)) +
    
    geom_Tline(Tintercept = 0.5, size = 0.7, color = "gray30", linetype = "dashed")+
    geom_Lline(Lintercept = 0.5, size = 0.7, color = "gray30", linetype = "dashed")+
    geom_Rline(Rintercept = 0.5, size = 0.7, color = "gray30", linetype = "dashed")+
    geom_point(color=rgb(as.matrix(triplotDf[,c(3,1,2)])), size =6) +
    #theme_classic() +
    theme_nomask()+
    theme_arrowdefault() +
    #limit_tern(breaks = seq(0,1,by=0.5)) +
    ggtitle("Strategies in the Multi-Objective frontier") +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,
                            family="Times")) +
    annotate(geom = 'text',
             x = df2$totalSoy,
             y = df2$totalWheat+0.02,
             z = df2$totalMaize,
             #angle = c(0,30,60),
             vjust = c(-1,-1,-1,-1),
             hjust = c(0,0,1.1,0),
             label = c('bES','bEU','bReg','bFC'),family="Times", size=3.5) +
    
    annotate(geom = 'text',
             x = 0,
             y = 0.5,
             z = 0.5,
             #angle = c(0,30,60),
             vjust = -1,
             hjust = 0,
             label = 'bRot', family="Times", size=3.5) +
    annotate(geom='point',
             x = df2$totalSoy,
             y = df2$totalWheat,
             z = df2$totalMaize, size=7, shape=c(0,3,4,5))  +
    scale_T_continuous(breaks=s,labels=paste("   ",s,"   ", sep=""))+ 
    scale_L_continuous(breaks=s,labels=paste("   ",s,"   ", sep=""))+ 
    scale_R_continuous(breaks=s,labels=paste("   ",s,"   ", sep=""))
  
  #MO front by objective
  frontScaled <- apply(MOSolList$performanceMetrics,MARGIN=2,FUN=range01)
  
  df <- NULL
  for (column in colnames(frontScaled)) {
    dfTemp <- data.frame(id=rownames(frontScaled),value =frontScaled[,column],
                         objective=column,
                         color = tempStDf$col,
                         stringsAsFactors = F)
    df <- rbind(df,dfTemp)
  }
  
  maxVec <- apply(MOSolList$performanceMetrics,MARGIN=2,FUN=max)
  minVec <- apply(MOSolList$performanceMetrics,MARGIN=2,FUN=min)
  yRange <- seq(0,1,by=0.25)
  
  rangeList <- list()
  for(c in names(maxVec)) {
    rangeList[[c]] <- yRange*(maxVec[c]-minVec[c]) + minVec[c]
  }
  
  df$objective <- factor(df$objective, levels =c('fixedCost','rotation','ES','EU','regret','ROI'))
  pal3 <- as.character(decisionEvaluated$strategyDf$col)
  names(pal3) <- rownames(decisionEvaluated$strategyDf)
  g2<- ggplot(df, aes(x = objective, y = value,group=id, color=id)) + 
    geom_line() + scale_color_manual(values=pal3) + 
    ggtitle("Performance of strategies by objective") +  
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family="Times"),
          legend.position="none",axis.title.y = element_blank(), axis.text.y=element_blank()) +
    #scale_y_continuous(breaks = c(0,0.3,.7,1), labels=c('a','b','c','d')) +
    annotate('text', x=1,y=yRange, label=round(rangeList[['fixedCost']],0),size=3,  hjust = 1,family="Times") +
    annotate('text', x=2,y=yRange, label=round(rangeList[['rotation']],3),size=3,  hjust = 1,family="Times") +
    annotate('text', x=3,y=yRange, label=round(-rangeList[['ES']],0),size=3,  hjust = 1,family="Times") +
    annotate('text', x=4,y=yRange, label=round(-rangeList[['EU']],0),size=3,  hjust = 1,family="Times") +
    annotate('text', x=5,y=yRange, label=round(rangeList[['regret']],0),size=3,  hjust = 1,family="Times") +
    #scale_x_discrete(labels = c("fixed cost [$]", "rotation", "ES eq. [$]", "EU eq. [$]","regret [$]"))
    scale_x_discrete(labels = c("FC [$]", "Rot", "ES [$]", "EU [$]","Reg [$]", "ROI [%]", "regretROI [%]"))
  
  #strategies in the MO front
  #identify candidates
  tempVec <- decisionEvaluated$strategyDf[bestVect,]
  rownames(tempVec) <- names(bestVect)
  
  tempDf <- tempVec #[rev(c('fixedCost','rotation','ES','EU','regret')),]
  tempDf<- t(tempDf[,colnames(tempVec) %in% rownames(colorsPal)])
  
  tempDf <- tempDf[order(colorsPal[rownames(tempDf),'orders'],decreasing = T),]
  
  dfmelt <- melt(tempDf, id.vars = "crop")
  colnames(dfmelt) <- c('crop','id','value')
  dfmelt$id <- as.factor(dfmelt$id)
  
  #order factors to plot
  lev <- unique(dfmelt$crop)
  
  #if .1 in names replace by =""
  #dfmelt$crop <- gsub(".1", "", dfmelt$Var1)
  dfmelt$crop <- factor(dfmelt$crop, levels = lev[order(colorsPal[lev,'orders'])])
  
  #pallete
  pal2 <- as.character(colorsPal[,2])
  names(pal2) <- rownames(colorsPal)
  
  #barplot
  levels(dfmelt$id) <- c("Reg", "EU","ES","Rot",  "FC")
  g3 <- ggplot(dfmelt, aes(x=id, y=value,fill=crop)) +
    geom_bar(stat="identity") + 
    #coord_flip() + 
    scale_fill_manual(values = pal2) +
    theme(axis.title.y = element_blank(), text=element_text(family="Times")) +
    ylab("proportion of land") +
    ggtitle("Best strategies by objective") + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
  
  
  return(list(triplot=g1, MOplot=g2, strategiesPlot=g3, bestStrategies=bestVect))
}

quintile = function(y) {
  
  qn = quantile(y, probs = (0:5)/5,na.rm = T)
  result = as.numeric(cut(y, qn, include.lowest = T))
  return(result)
}

getTreeByObjective <- function(MOSolList,obj='E_ROI') {
  
  if(obj=='E_ROI') {
    objToSplit <- 'ROI'
    q <- parametersEU$q
    minimization=F
  }

  if(obj=='R3Q_ROI') {
    objToSplit <- "Reg_ROI" 
    q <- parametersRegret$q
    minimization=T
  }
  
  if(obj=='E_U') {
    objToSplit <- "utility"
    q <- parametersEU$q  
    minimization=F
  }
  
  if(obj=='R3Q_U') {
    objToSplit <- "Reg_utility" 
    q <- parametersRegret$q
    minimization=T
  }
  
  
  bestVect <- getBestStrategies(MOSolList)
  #define object for simplicity
  evalDf <- MOSolList$decisionEvaluated$evalDf
  
  candidateId <- as.numeric(bestVect[obj])
  tempDf <- evalDf[evalDf$st==candidateId,]
  
  thres <- as.numeric(quantile(tempDf[,objToSplit],prob=q))
  
  if (minimization) {
    tempDf$target <- as.factor(tempDf[,objToSplit]>thres)
  } else {
    tempDf$target <- as.factor(tempDf[,objToSplit]<thres)
  }
  
  tempDf <- cbind(tempDf, decisionEvaluated$scenarioDf[tempDf$sc,])
  rownames(tempDf) <-tempDf$sc  
  
  cols <- c('target','W','S','Mt','MT','S2W','W2M','M2S',#'ppQuintile',
          paste("Q",names(periods),sep=""), 'ensoVar') #, 
    # 'DJF') #,'JFM','NDJ')
  tempDfML <- tempDf[,cols[cols %in% colnames(tempDf)]]

  #tree
  #print(summary(tempDfML))
  #loss.matrix <- matrix(c(0, 1, 2, 0), nrow=2, byrow=TRUE)
  levels(tempDfML$target) <- c("GOOD","BAD")
  fit <- rpart(target ~ ., data = tempDfML,
               control=control.rpart,
               parms=list(loss = loss.matrix)
  )

  #performance
  pred.tree <- predict(fit, newdata=tempDf, type="class")
  levels(pred.tree) <- c(F,T)
  actual.class <- tempDf$target
  confMat <- as.matrix(confusionMatrix(data=pred.tree, reference=actual.class))
  coverage <- confMat[2,2]/(confMat[1,2]+confMat[2,2])
  
  #pdf(paste0('plots/',soil,'-',waterCont,'-',obj,'-explorartion.pdf',sep=""),width=12,height=6) 
  #par(mfrow=c(1,3))
  y <- 1- fit$frame$yval2[,5]
  cols2 <- rgb(1,y,y)
  
  prp(fit, type=0, extra=1, under=TRUE, uniform=TRUE, 
    branch.col=cols2, box.col=cols2, 
    
    branch.type=5, yesno=T, faclen=0 ,split.family = "Times",
    nn.family ="Times",#cex=0.5
    fam.main="Times",
    family="Times",
    split.font=3,
    under.font=1,
    nn.font = 1,
    tweak = 1,
    varlen = 0,
    main = paste("Decision tree to identify vulnerabilities\nof optimal strategy according to objective",obj)
  )
  return(list(tree=fit,confMat = confMat, coverage=coverage, obj=obj, candidate=candidateId))
}

getTreeByObjectiveStr <- function(MOSolList,obj='E_ROI', str) {
  
  if(obj=='E_ROI') {
    objToSplit <- 'ROI'
    q <- parametersEU$q
    minimization=F
  }
  
  if(obj=='R3Q_ROI') {
    objToSplit <- "Reg_ROI" 
    q <- parametersRegret$q
    minimization=T
  }
  
  if(obj=='E_U') {
    objToSplit <- "utility"
    q <- parametersEU$q  
    minimization=F
  }
  
  if(obj=='R3Q_U') {
    objToSplit <- "Reg_utility" 
    q <- parametersRegret$q
    minimization=T
  }
  
  
  bestVect <- getBestStrategies(MOSolList)
  #define object for simplicity
  evalDf <- MOSolList$decisionEvaluated$evalDf
  
  #candidateId <- as.numeric(bestVect[obj])
  candidateId <- str
  tempDf <- evalDf[evalDf$st==candidateId,]
  
  thres <- as.numeric(quantile(tempDf[,objToSplit],prob=q))
  
  if (minimization) {
    tempDf$target <- as.factor(tempDf[,objToSplit]>thres)
  } else {
    tempDf$target <- as.factor(tempDf[,objToSplit]<thres)
  }
  
  tempDf <- cbind(tempDf, decisionEvaluated$scenarioDf[tempDf$sc,])
  rownames(tempDf) <-tempDf$sc  
  
  cols <- c('target','W','S','Mt','MT','S2W','W2M','M2S',#'ppQuintile',
            paste("Q",names(periods),sep=""), 'ensoVar') #, 
  # 'DJF') #,'JFM','NDJ')
  tempDfML <- tempDf[,cols[cols %in% colnames(tempDf)]]
  
  #tree
  #print(summary(tempDfML))
  #loss.matrix <- matrix(c(0, 1, 2, 0), nrow=2, byrow=TRUE)
  #levels(tempDfML$target) <- c("GOOD","BAD")
  fit <- rpart(target ~ ., data = tempDfML,
               control=control.rpart,
               parms=list(loss = loss.matrix)
  )
  
  #performance
  pred.tree <- predict(fit, newdata=tempDf, type="class")
  actual.class <- tempDf$target
  confMat <- as.matrix(confusionMatrix(data=pred.tree, reference=actual.class))
  coverage <- confMat[2,2]/(confMat[1,2]+confMat[2,2])
  
  #pdf(paste0('plots/',soil,'-',waterCont,'-',obj,'-explorartion.pdf',sep=""),width=12,height=6) 
  #par(mfrow=c(1,3))
  y <- 1- fit$frame$yval2[,5]
  cols2 <- rgb(1,y,y)
  
  prp(fit, type=0, extra=1, under=TRUE, uniform=TRUE, 
      branch.col=cols2, box.col=cols2, 
      
      branch.type=5, yesno=T, faclen=0 ,split.family = "Times",
      nn.family ="Times",#cex=0.5
      fam.main="Times",
      family="Times",
      split.font=3,
      under.font=1,
      nn.font = 1,
      tweak = 1,
      varlen = 0,
      main = paste("Decision tree to identify vulnerabilities\nof optimal strategy according to objective",obj)
  )
  return(list(tree=fit,confMat = confMat, coverage=coverage, obj=obj, candidate=candidateId))
}

splitSolByClassTree <- function(MOSolList, classTree) {
  
  pred.tree <- predict(classTree$tree, 
                       newdata=MOSolList$decisionEvaluated$scenarioDf, type="class")=="BAD"
                       #,type="class"))
  
  #filter GOOD
  MOSolListGood <- MOSolList
  MOSolListGood$decisionEvaluated$scenarioDf <- MOSolListGood$decisionEvaluated$scenarioDf[!pred.tree,]
  indexTemp <- rep(!pred.tree,nrow(MOSolList$decisionEvaluated$strategyDf))
  MOSolListGood$decisionEvaluated$evalDf <- MOSolListGood$decisionEvaluated$evalDf[indexTemp,]
  MOSolListGood$performanceMetrics <- calculatePerformaceByStrategy(MOSolListGood$decisionEvaluated, parametersList)
  
  #filter BAD  
  MOSolListBad <- MOSolList
  MOSolListBad$decisionEvaluated$scenarioDf <- MOSolListBad$decisionEvaluated$scenarioDf[pred.tree,]
  indexTemp <- rep(pred.tree,nrow(MOSolList$decisionEvaluated$strategyDf))
  MOSolListBad$decisionEvaluated$evalDf <- MOSolListBad$decisionEvaluated$evalDf[indexTemp,]
  MOSolListBad$performanceMetrics <- calculatePerformaceByStrategy(MOSolListBad$decisionEvaluated, parametersList)
  
  
  
  
  return(list(MOSolListGood=MOSolListGood,MOSolListBad=MOSolListBad))
}

plotTradeOff <- function(MOSolListGood,MOSolListBad, classTree, objBad=classTree$obj, annotateId=1001) {
  pointsInGood <- MOSolListGood$performanceMetrics[,classTree$obj]
  pointsInBad <- MOSolListBad$performanceMetrics[,objBad]
  
  ## filter only points in the frontier
  pointsInGood2 <- pointsInGood[as.numeric(rownames(MOSolList$performanceMetrics))]
  pointsInBad2 <- pointsInBad[as.numeric(rownames(MOSolList$performanceMetrics))]
  
  dfToPlot <- data.frame(good=pointsInGood, 
                        bad=pointsInBad,
                        totalMaize=MOSolListGood$decisionEvaluated$strategyDf$totalMaize, 
                        totalSoy=MOSolListGood$decisionEvaluated$strategyDf$totalSoy,
                        totalWheat=MOSolListGood$decisionEvaluated$strategyDf$totalWheat)

    margin <- 0.05
    maxX <- max(dfToPlot$good)+margin
    idMaxX <- as.numeric(which.max(dfToPlot$good))
    maxY <- max(dfToPlot$bad)+margin

    idMaxY <- which.max(dfToPlot$bad)
    minX <- dfToPlot$good[idMaxY]- margin
    minY <- dfToPlot$bad[idMaxX]- margin

    #maxY1 <- max(dfToPlot$bad1)+margin
    #minY1 <- dfToPlot$bad1[idMaxX]- margin
    #title2 <- paste("EU -", locDf[loc,1], "\n",
    #                paste(locDf[loc,c(2,3)], collapse = "-"))

    title2 <- "GOOD scenarios vs.\nBAD scenarios\nclassified by decision tree"
    title3 <- "GOOD scenarios vs.\nBAD scenarios\nclassified by decision tree"
    #par(mfrow=c(1,2))
    #pdf(paste0('plots/',soil,'-',waterCont,'-tradeOff.pdf',sep=""),width=6,height=6)
    
    plot(dfToPlot$good,
         dfToPlot$bad,
         type="p",
         xlab=paste(classTree$obj," in GOOD Scenarios",sep=""),
         ylab=paste(objBad," in BAD Scenarios",sep=""),
         pch=16, cex=0.9,
         #xlim=c(-maxX,-minX),
         #ylim=c(-maxY,-minY),


         
         bg=2,
         col=rgb(dfToPlot[,"totalMaize"],dfToPlot[,"totalSoy"],dfToPlot[,"totalWheat"],alpha=0.4),
         main=title2,family="Times", font.main=1)

    if(!objectiveMinimization[objBad]==objectiveMinimization[classTree$obj]) {
      abline(a=(-dfToPlot$good[classTree$candidate]+dfToPlot$bad[classTree$candidate]), b= 1,lwd=0.5)
    } else {
      abline(a=(dfToPlot$good[classTree$candidate]+dfToPlot$bad[classTree$candidate]), b= -1,lwd=0.5)
    }    
  
    #plot MO front
    points(pointsInGood2,pointsInBad2, type="p",
           pch=16, cex=0.6)
    
    points(dfToPlot$good[classTree$candidate],dfToPlot$bad[classTree$candidate])
    points(dfToPlot$good[annotateId],dfToPlot$bad[annotateId])

    text(dfToPlot$good[annotateId],dfToPlot$bad[annotateId]+0.01,annotateId,family = "Times",adj=0)
}

linePlotAternatives <- function(MOSolList,SolSplited,classTree, alt1, alt2) {
  #extract the best strategies by objective
  alternatives <- c(alt1,alt2)
  
  #rbind the MOSolList$performanceMetrics
  tempAll <- MOSolList$performanceMetrics[as.character(alternatives),]
  
  #this metrics need to change sign: ES_U, E_U, ES_ROI, E_ROI
  maxMetrics <- c('E_U','E_ROI','R3Q_U','ES_U','ES_ROI','R3Q_ROI')
  tempAll[,maxMetrics] <- -tempAll[,maxMetrics]

  tempGood <- SolSplited$MOSolListGood$performanceMetrics[as.character(alternatives),]
  tempBad <- SolSplited$MOSolListBad$performanceMetrics[as.character(alternatives),]
  
  tempGood[,c('R3Q_U','R3Q_ROI')] <-  -tempGood[,c('R3Q_U','R3Q_ROI')]
  tempBad[,c('R3Q_U','R3Q_ROI')] <-  -tempBad[,c('R3Q_U','R3Q_ROI')]
  
  perfMetricsCombined <- rbind(tempAll,tempGood[,c(3:8,2)], tempBad[,c(3:8,2)])
  
  
  #MO front by objective
  #frontScaledAll <- apply(perfMetricsCombined,MARGIN=2,FUN=range01)
  frontScaledAll <- perfMetricsCombined
  cols <- rep(MOSolList$decisionEvaluated$strategyDf[as.character(alternatives),'col2'],3)
  cols <- as.character(cols)
  #cols[c(2,4,6)] <- "#c133cc"
  df <- NULL
  for (column in colnames(frontScaledAll)) {
    dfTemp <- data.frame(id=rownames(frontScaledAll),value =frontScaledAll[,column],
                         objective=column,
                         color = cols,
                         stringsAsFactors = F)
    df <- rbind(df,dfTemp)
  }
  
  scenarioSet <- c("all","all","GOOD","GOOD","BAD","BAD")

  df$scSet<- rep(scenarioSet,7)
  
  #maxVec <- apply(perfMetricsCombined,MARGIN=2,FUN=max)
  
  #minVec <- apply(perfMetricsCombined,MARGIN=2,FUN=min)
  
  df$objective <- factor(df$objective, levels =colnames(perfMetricsCombined))
  pal3 <- as.character(cols)
  names(pal3) <- paste0(rep(alternatives,3), c("","",1,1,2,2))
  
  fcMean <- mean(df[df$objective=='fixedCost','value'])
  
  #adjust ROIs
  df[grepl("_ROI",df$objective),'value'] <- df[grepl("_ROI",df$objective),'value']*fcMean
  
  #adjust Rotation
  rg <- range(df$value)
  #df[df$objective=='rotation','value'] <- (1-(df[df$objective=='rotation','value']-0.5)*2)*(rg[2]-rg[1])+rg[1]
  
  #adjust Fixed Costs
  df[df$objective=='fixedCost','value'] <- rg[2] - df[df$objective=='fixedCost','value'] 
  
  df$id <- as.factor(rep(alternatives,21))
  
  g2<- ggplot(df, aes(x = objective, y = value,group=interaction(id,scSet), color=id)) +
    geom_line(aes(linetype=scSet),size=0.5) + scale_linetype_manual(values=c("solid","longdash","dotted")) +
                scale_color_manual(values=pal3) + 
    ggtitle("Performance of strategies by objective") +  
    theme(plot.title = element_text(hjust = 0.5),
          #plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm"),
          text = element_text(family="Times"),
          legend.position="bottom",
          legend.title = element_text(family="Times"),
          axis.title.y = element_blank(), axis.text.y=element_blank()) +
    #guides(scSet = "legend", colour = "none",linetype=guide_legend(title="Scenario Sub-set:")) +
    guides(scSet = "legend",linetype=guide_legend(title="Scenario Sub-set:"),
           colour=guide_legend(title="Strategy:")) +
    scale_x_discrete(labels=paste0(colnames(frontScaledAll),c("[$]","[$]","[$]","[%]","[%]","[%]","[$]"),sep=""))
  
  #add rotation labels
  # g2 <- g2 + annotate('text', 
  #                     x=1,
  #                     y=(rg[2]-rg[1])*0.25*(4:0) + rg[1], 
  #                     label=as.character(seq(.5,1,by=1/8)),
  #                     size=3,  hjust = 1,family="Times")
  # 
  #add fixedCost labels
  g2 <- g2 + annotate('text', 
                      x=7,
                      y=(rg[2]-rg[1])*0.25*(4:0) + rg[1], 
                      label=round(seq(0,rg[2]-rg[1], by = (rg[2]-rg[1])/4),0),
                      size=3,  hjust = 1,family="Times")
  
  #add ES_U labels
  g2 <- g2 + annotate('text', 
                      x=1,
                      y=(rg[2]-rg[1])*0.25*(0:4) + rg[1], 
                      label=round(seq(rg[1],rg[2], by = (rg[2]-rg[1])/4),0),
                      size=3,  hjust = 1,family="Times")
  
  #add ES_U labels
  g2 <- g2 + annotate('text', 
                      x=2,
                      y=(rg[2]-rg[1])*0.25*(0:4) + rg[1], 
                      label=round(seq(rg[1],rg[2], by = (rg[2]-rg[1])/4),0),
                      size=3,  hjust = 1,family="Times")

  #add R3Q_U labels
  g2 <- g2 + annotate('text', 
                      x=3,
                      y=(rg[2]-rg[1])*0.25*(0:1) + rg[1], 
                      label=-round(seq(rg[1],0, by = (rg[2]-rg[1])/4),0),
                      size=3,  hjust = 1,family="Times")  
  
  
  #add ES_ROI labels
  g2 <- g2 + annotate('text', 
                      x=4,
                      y=(rg[2]-rg[1])*0.25*(0:4) + rg[1], 
                      label=round(seq(rg[1],rg[2], by = (rg[2]-rg[1])/4)/fcMean,2),
                      size=3,  hjust = 1,family="Times")
  
  #add E_ROI labels
  g2 <- g2 + annotate('text', 
                      x=5,
                      y=(rg[2]-rg[1])*0.25*(0:4) + rg[1], 
                      label=round(seq(rg[1],rg[2], by = (rg[2]-rg[1])/4)/fcMean,2),
                      size=3,  hjust = 1,family="Times")
  
  
  #add R3Q_ROI labels
  g2 <- g2 + annotate('text', 
                      x=6,
                      y=(rg[2]-rg[1])*0.25*(0:1) + rg[1], 
                      label=-round(seq(rg[1],0, by = (rg[2]-rg[1])/4)/fcMean,2),
                      size=3,  hjust = 1,family="Times")  
  
  
  return(g2)
}

verticalBarsPlotAlternatives <- function(MOSolList, alt1, alt2) {
  #extract the best strategies by objective
  bestVect <- c(alt1,alt2)
  
  tempVec <- decisionEvaluated$strategyDf[bestVect,]
  rownames(tempVec) <- names(bestVect)
  
  tempDf <- tempVec #[rev(c('fixedCost','rotation','ES','EU','regret')),]
  tempDf<- t(tempDf[,colnames(tempVec) %in% rownames(colorsPal)])
  
  tempDf <- tempDf[order(colorsPal[rownames(tempDf),'orders'],decreasing = T),]
  
  dfmelt <- melt(tempDf, id.vars = "crop")
  colnames(dfmelt) <- c('crop','id','value')
  dfmelt$id <- as.factor(dfmelt$id)
  
  #order factors to plot
  lev <- unique(dfmelt$crop)
  
  #if .1 in names replace by =""
  #dfmelt$crop <- gsub(".1", "", dfmelt$Var1)
  dfmelt$crop <- factor(dfmelt$crop, levels = lev[order(colorsPal[lev,'orders'])])
  
  #pallete
  pal2 <- as.character(colorsPal[,2])
  names(pal2) <- rownames(colorsPal)
  
  #barplot
  #levels(dfmelt$id) <- names(bestVect)
  g3 <- ggplot(dfmelt, aes(x=id, y=value,fill=crop)) +
    geom_bar(stat="identity") + 
    #coord_flip() + 
    scale_fill_manual(values = pal2, labels =rev(CAsNames)) +
    #scale_fill_manual(values = pal2) +
    scale_x_discrete(labels=as.character(c(alt1, alt2)))+ 
    #                   limits=as.character(c(alt1, alt2))) +
    theme( text=element_text(family="Times"),
           plot.margin = unit(c(0.5,8,0.5,8), "cm"),
           axis.title.y = element_blank(),
           axis.text.y=element_blank()
           ) +
    xlab("Strategies") +
    ggtitle("Alternative Strategies") + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
  return(g3)
  
}

plotPureStrategies <- function(MOSolList) {
  
  decisionEvaluated <- MOSolList$decisionEvaluated
  pureStrategies <- which(apply(decisionEvaluated$strategyDf[,1:6],1,max)==1)
  names(pureStrategies) <- colnames(decisionEvaluated$strategyDf[,1:6])
  plotDf <- as.data.frame(t(t(pureStrategies)))
  colnames(plotDf) <- 'id'
  plotDf$mean <- 0
  plotDf$sd <- 0
  
  for (crop in rownames(plotDf)) {
    cropId <- plotDf[crop,'id']
    plotDf[crop,'mean'] <- mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==cropId])
    plotDf[crop,'sd'] <- sd(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==cropId])
    
  }
  
  p1 <- ggplot(plotDf, aes(x=mean, y=sd)) +
    geom_point(size=5, shape=16, color=colorsPal[rownames(plotDf),'colorsPal']) + 
    labs(title=paste0("FWNM of crop alternatives in Junín\nwater: ",waterCont),
         x ="Mean ($)", y = "Standard Deviation ($)") +
    annotate(geom = 'text',
             x =plotDf$mean,
             y = plotDf$sd+1,
             hjust = 'right',
             label = rownames(plotDf),
             family="Times", size=3) +
    geom_vline(xintercept = 0) + 
    xlim(0, 600) +
    ylim(270,415) +
    theme(plot.title = element_text(hjust = 0.5),text=element_text(size=12,  
                                                                   family="Times"))
  
  bestEU <- rownames(MOSolList$performanceMetrics)[which.min(MOSolList$performanceMetrics$EU)]
  bestES <- rownames(MOSolList$performanceMetrics)[which.min(MOSolList$performanceMetrics$ES_U)]
  colorsPureStrategies <- colorsPal[rownames(plotDf),'colorsPal']
  
  tempDf <- subset(decisionEvaluated$evalDf,st %in% c('11','1001',bestES))
  tempDf$st <- as.factor(tempDf$st)
  p2 <- ggplot(tempDf,aes(x=FWNM)) + 
    labs(title="FWNM histogram for selected crop alternatives\nand their combination",
         y ="Scenario Count", x = "FWNM ($)") +
    
    geom_histogram(data=subset(tempDf,st=='11'),aes(fill = st), alpha = 0.5 ) + 
    geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st=='11']), 
               color=colorsPureStrategies[1], alpha=0.4) +
    
    geom_histogram(data=subset(tempDf,st == '1001'),aes(fill = st), alpha = 0.5) +
    geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==bestEU]), 
               color=colorsPureStrategies[5], alpha=0.4) +
    
    geom_histogram(data=subset(tempDf,st == bestES),aes(fill = st), alpha = 0.5) +
    geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==bestES]), color="gray30", alpha=0.4) +
    xlim(-500,2300) +
    ylim(0,300) +
    scale_fill_manual(name="Strategies",values=c(as.character(colorsPureStrategies[5]),as.character(colorsPureStrategies[2]),"gray30"),
                      labels=c("100% W->S", "100% Mt","BestES")) +
    theme(plot.title = element_text(hjust = 0.5),text=element_text(size=12,  
                                                                   family="Times"),
          legend.position="bottom")
  
  pdf(paste0('plots/',soil,'-',waterCont,'.pdf',sep=""),width=12,height=6) 
  grid.arrange(p1,  
               p2,  
               nrow = 1)
  dev.off()
  return(grid.arrange(p1,  
                      p2,  
                      nrow = 1))
}

plotPureStrategies2 <- function(MOSolList) {
  
  decisionEvaluated <- MOSolList$decisionEvaluated
  pureStrategies <- which(apply(decisionEvaluated$strategyDf[,1:6],1,max)==1)
  names(pureStrategies) <- colnames(decisionEvaluated$strategyDf[,1:6])
  plotDf <- as.data.frame(t(t(pureStrategies)))
  colnames(plotDf) <- 'id'
  plotDf$mean <- 0
  plotDf$sd <- 0
  
  for (crop in rownames(plotDf)) {
    cropId <- plotDf[crop,'id']
    plotDf[crop,'mean'] <- mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==cropId])
    plotDf[crop,'sd'] <- sd(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==cropId])
    
  }
  
  p1 <- ggplot(plotDf, aes(x=mean, y=sd)) +
    geom_point(size=5, shape=16, color=colorsPal[rownames(plotDf),'colorsPal']) + 
    labs(title=paste0("FWNM of crop alternatives in ",soil$soil_series,"\nwater: ",waterCont),
         x ="Mean ($)", y = "Standard Deviation ($)") +
    annotate(geom = 'text',
             x =plotDf$mean+1,
             y = plotDf$sd+3,
             hjust = 'left',
             #label = rownames(plotDf),
             label = CAsNames,
             family="Times", size=3) +
    geom_vline(xintercept = 0) + 
    xlim(0, 600) +
    ylim(270,415) +
    theme(plot.title = element_text(hjust = 0.5),text=element_text(size=12,  
                                                                   family="Times"))
  
  bestEU <- rownames(MOSolList$performanceMetrics)[which.min(MOSolList$performanceMetrics$EU)]
  bestES <- rownames(MOSolList$performanceMetrics)[which.min(MOSolList$performanceMetrics$ES_U)]
  colorsPureStrategies <- colorsPal[rownames(plotDf),'colorsPal']
  
  tempDf <- subset(decisionEvaluated$evalDf,st %in% c('11','1001'))
  tempDf$st <- as.factor(tempDf$st)
  

  p2 <- ggplot(tempDf, aes(FWNM, fill = st)) + geom_density(alpha = 0.7,lty="blank") +
            labs(title="FWNM histogram for selected CAs", 
                 x = "FWNM ($)") +
    ## add vlines
    geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st=='11']), 
                           color=colorsPureStrategies[2], alpha=0.7) +
    geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st=='1001']), 
                           color=colorsPureStrategies[5], alpha=0.8) +
    #limits
       xlim(-800,2500) +
       ylim(0,0.002) +
    #colors
    scale_fill_manual(name="Strategies",values=c(as.character(colorsPureStrategies[2]),
                                                 as.character(colorsPureStrategies[5])),
                                         labels=c("MT","Ws-S")) +
    #theme
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(size=12,    family="Times"),
          legend.position="bottom",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
                                                                 
  # p2 <- ggplot(tempDf,aes(x=FWNM)) + 
  #   labs(title="FWNM histogram for selected crop alternatives\nand their combination",
  #        y ="Scenario Count", x = "FWNM ($)") +
  #   
  #   geom_histogram(data=subset(tempDf,st=='11'),aes(fill = st), alpha = 0.5 ) + 
  #   geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st=='11']), 
  #              color=colorsPureStrategies[1], alpha=0.4) +
  #   
  #   geom_histogram(data=subset(tempDf,st == '1001'),aes(fill = st), alpha = 0.5) +
  #   geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==bestEU]), 
  #              color=colorsPureStrategies[5], alpha=0.4) +
  #   
  #   geom_histogram(data=subset(tempDf,st == bestES),aes(fill = st), alpha = 0.5) +
  #   geom_vline(xintercept=mean(decisionEvaluated$evalDf$FWNM[decisionEvaluated$evalDf$st==bestES]), color="gray30", alpha=0.4) +
  #   xlim(-700,2300) +
  #   ylim(0,300) +
  #   scale_fill_manual(name="Strategies",values=c(as.character(colorsPureStrategies[5]),as.character(colorsPureStrategies[2]),"gray30"),
  #                     labels=c("100% W->S", "100% Mt","BestES")) +
  #   theme(plot.title = element_text(hjust = 0.5),text=element_text(size=12,  
  #                                                                  family="Times"),
  #         legend.position="bottom")
  
  pdf(paste0('plots/',soil,'-',waterCont,'.pdf',sep=""),width=12,height=6) 
  grid.arrange(p1,  
               p2,  
               nrow = 1)
  dev.off()
  return(grid.arrange(p1,  
                      p2,  
                      nrow = 1))
}

plotCustTradeOff <- function(MOSolListGood,MOSolListBad, classTree, 
                             objBad=classTree$obj, annotateId=1001,bestStrategies) {
  pointsInGood <- MOSolListGood$performanceMetrics[,classTree$obj]
  pointsInBad <- MOSolListBad$performanceMetrics[,objBad]
  
  ## filter only points in the frontier
  pointsInGood2 <- pointsInGood[as.numeric(rownames(MOSolList$performanceMetrics))]
  pointsInBad2 <- pointsInBad[as.numeric(rownames(MOSolList$performanceMetrics))]
  
  dfToPlot <- data.frame(good=pointsInGood, 
                         bad=pointsInBad,
                         totalMaize=MOSolListGood$decisionEvaluated$strategyDf$totalMaize, 
                         totalSoy=MOSolListGood$decisionEvaluated$strategyDf$totalSoy,
                         totalWheat=MOSolListGood$decisionEvaluated$strategyDf$totalWheat)
  
  margin <- 0.05
  maxX <- max(dfToPlot$good)+margin
  idMaxX <- as.numeric(which.max(dfToPlot$good))
  maxY <- max(dfToPlot$bad)+margin
  
  idMaxY <- which.max(dfToPlot$bad)
  minX <- dfToPlot$good[idMaxY]- margin
  minY <- dfToPlot$bad[idMaxX]- margin
  
  #maxY1 <- max(dfToPlot$bad1)+margin
  #minY1 <- dfToPlot$bad1[idMaxX]- margin
  #title2 <- paste("EU -", locDf[loc,1], "\n",
  #                paste(locDf[loc,c(2,3)], collapse = "-"))
  
  title2 <- "GOOD scenarios vs.\nBAD scenarios\nclassified by decision tree"
  title3 <- "GOOD scenarios vs.\nBAD scenarios\nclassified by decision tree"
  #par(mfrow=c(1,2))
  #pdf(paste0('plots/',soil,'-',waterCont,'-tradeOff.pdf',sep=""),width=6,height=6)
  
  plot(dfToPlot$good,
       dfToPlot$bad,
       type="p",
       xlab=paste(classTree$obj," in GOOD Scenarios",sep=""),
       ylab=paste(objBad," in BAD Scenarios",sep=""),
       pch=16, cex=0.9,
       xlim=c(0.31,1.02),
       #ylim=c(-maxY,-minY),
       
       
       
       bg=2,
       col=rgb(dfToPlot[,"totalMaize"],dfToPlot[,"totalSoy"],dfToPlot[,"totalWheat"],alpha=0.4),
       main=title2,family="Times", font.main=1)
  
  if(!objectiveMinimization[objBad]==objectiveMinimization[classTree$obj]) {
    abline(a=(-dfToPlot$good[classTree$candidate]+dfToPlot$bad[classTree$candidate]), b= 1,lwd=0.5)
  } else {
    abline(a=(dfToPlot$good[classTree$candidate]+dfToPlot$bad[classTree$candidate]), b= -1,lwd=0.5)
  }    
  
  #plot MO front
  points(pointsInGood2,pointsInBad2, type="p",
         pch=16, cex=0.6)
  
  points(dfToPlot$good[classTree$candidate],dfToPlot$bad[classTree$candidate], cex=1.2)
  points(dfToPlot$good[annotateId],dfToPlot$bad[annotateId], cex=1.2)
  
  text(dfToPlot$good[annotateId],dfToPlot$bad[annotateId]+0.01,annotateId,family = "Times",adj=0)
  
  
  points(dfToPlot$good[as.numeric(bestStrategies)],dfToPlot$bad[as.numeric(bestStrategies)])
  
  text(dfToPlot$good[as.numeric(bestStrategies)],
       dfToPlot$bad[as.numeric(bestStrategies)]+0.01+c(0,0,0,-0.015,0,0,-0.015),
       names(bestStrategies),family = "Times",adj=0)
  
  
}


# exploreByObjective <- function(MOSolList,obj = "EU") {
#   
#   bestVect <- getBestStrategies(MOSolList)
#   #define object for simplicity
#   evalDf <- MOSolList$decisionEvaluated$evalDf
#   
#   #get candidate
#   if(obj=="EU") {
#     candidateId <- which.min(MOSolList$performanceMetrics$EU)
#     
#     candidate <- as.numeric(rownames(MOSolList$performanceMetrics)[candidateId])
#     q <- parametersEU$q
#     tempDf <- evalDf[evalDf$st==candidate,]
#     thres <- as.numeric(quantile(tempDf$utility,prob=q))
#     tempDf$target <- as.factor(tempDf$utility<thres)
#     tempDf <- cbind(tempDf, decisionEvaluated$scenarioDf[tempDf$sc,])
#     rownames(tempDf) <-tempDf$sc
#   } 
#   
#   if(obj=="regret") {
#     candidateId <- which.min(MOSolList$performanceMetrics$regret)
#     
#     candidate <- as.numeric(rownames(MOSolList$performanceMetrics)[candidateId])
#     
#     q <- parametersRegret$q
#     tempDf <- evalDf[evalDf$st==candidate,]
#     thres <- as.numeric(quantile(tempDf$regret,prob=q))
#     tempDf$target <- as.factor(tempDf$regret>thres)
#     tempDf <- cbind(tempDf, decisionEvaluated$scenarioDf[tempDf$sc,])
#     rownames(tempDf) <-tempDf$sc
#   }
#   
#   if(obj=="regretROI") {
#     candidateId <- which.min(MOSolList$performanceMetrics$regretROI)
#     
#     candidate <- as.numeric(rownames(MOSolList$performanceMetrics)[candidateId])
#     
#     q <- parametersRegret$q
#     tempDf <- evalDf[evalDf$st==candidate,]
#     thres <- as.numeric(quantile(tempDf$regret,prob=q))
#     tempDf$target <- as.factor(tempDf$regret>thres)
#     tempDf <- cbind(tempDf, decisionEvaluated$scenarioDf[tempDf$sc,])
#     rownames(tempDf) <-tempDf$sc
#   }
#   
#   
#   cols <- c('target','W','S','Mt','MT','S2W','W2M','M2S',#'ppQuintile',
#             paste("Q",names(periods),sep=""), 'ensoVar') #, 
#   # 'DJF') #,'JFM','NDJ')
#   tempDfML <- tempDf[,cols[cols %in% colnames(tempDf)]]
#   
#   #tree
#   #print(summary(tempDfML))
#   #loss.matrix <- matrix(c(0, 1, 2, 0), nrow=2, byrow=TRUE)
#   #levels(tempDfML$target) <- c("GOOD","BAD")
#   fit <- rpart(target ~ ., data = tempDfML,
#                control=control.rpart,
#                parms=list(loss = loss.matrix)
#   )
#   
#   #performance
#   pred.tree <- predict(fit, newdata=tempDf, type="class")
#   actual.class <- tempDf$target
#   confMat <- as.matrix(confusionMatrix(data=pred.tree, reference=actual.class))
#   print(confMat)
#   print(confMat[2,2]/(confMat[1,2]+confMat[2,2]))
#   
#   #dev.off()
#   #layout(matrix(c(1,2,1,3), 2,2, byrow = TRUE))
#   #, widths=c(3,1), heights=c(1,2))
#   
#   #title= paste('\n\n','acc:', round((confMat[1,1] + confMat[2,2])/sum(confMat),2),"\n",
#   #            'rec:',round((confMat[2,2]/(confMat[2,2]+confMat[1,2])),2))
#   
#   pdf(paste0('plots/',soil,'-',waterCont,'-',obj,'-explorartion.pdf',sep=""),width=12,height=6) 
#   par(mfrow=c(1,3))
#   prp(fit, type=0, extra=1, under=TRUE, uniform=TRUE, 
#       #branch.col=cols, box.col=cols, 
#       
#       branch.type=5, yesno=T, faclen=0 ,split.family = "Times",
#       nn.family ="Times",#cex=0.5
#       fam.main="Times",
#       family="Times",
#       split.font=3,
#       under.font=1,
#       nn.font = 1,
#       main=""
#       
#       
#   )
#   
#   title(main = "Decision tree to identify\nvulnerabilities of bReg", 
#         family="Times",
#         cex.main = 1,   font.main= 1 #col.main= "red",
#         
#         
#   )
#   
#   #plot points
#   tempDf2 <- evalDf
#   
#   
#   if(obj=="EU") {
#     pointsInGood <- tapply(tempDf2$utility[pred.tree[tempDf2$sc]=='FALSE'],
#                            INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='FALSE'],
#                            FUN=mean, simplify=TRUE)
#     
#     pointsInBad <- tapply(tempDf2$utility[pred.tree[tempDf2$sc]=='TRUE'],
#                           INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#                           FUN=mean, simplify=TRUE)
#     
#     pointsInBadP1 <- -tapply(tempDf2$utility[pred.tree[tempDf2$sc]=='TRUE'],
#                              INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#                              #FUN=quantile,probs=parametersEU$q, simplify=TRUE)
#                              FUN=calculateES,parametersEU=parametersEU)
#     
#     dfToPlot <- data.frame(good=calculateEquiWEU(pointsInGood,parametersEU), 
#                            bad=calculateEquiWEU(pointsInBad,parametersEU),
#                            #bad1=calculateEquiWEU(pointsInBadP1,parametersEU),
#                            bad1=pointsInBadP1,
#                            totalMaize=decisionEvaluated$strategyDf$totalMaize, totalSoy=decisionEvaluated$strategyDf$totalSoy,totalWheat=decisionEvaluated$strategyDf$totalWheat)
#     
#     xlab="EU in scenarios w/ GOOD performance [$]"
#     ylab="EU in scenarios w/ BAD performance [$]"
#     ylab1="ES in scenarios w/ BAD performance [$]"
#   }  
#   
#   if(obj=="regret") {
#     pointsInGood <- -tapply(tempDf2$regret[pred.tree[tempDf2$sc]=='FALSE'],
#                             INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='FALSE'],
#                             FUN=quantile, simplify=TRUE, probs=parametersRegret$q, type=qType)
#     
#     pointsInBad <- -tapply(tempDf2$regret[pred.tree[tempDf2$sc]=='TRUE'],
#                            INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#                            FUN=quantile, simplify=TRUE, probs=parametersRegret$q, type=qType)
#     
#     # pointsInBadP1 <- tapply(tempDf2$utility[pred.tree[tempDf2$sc]=='TRUE'],
#     #                         INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#     #                         FUN=mean, simplify=TRUE)
#     
#     pointsInBadP1 <- -tapply(tempDf2$utility[pred.tree[tempDf2$sc]=='TRUE'],
#                              INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#                              #FUN=quantile,probs=parametersEU$q, simplify=TRUE)
#                              FUN=calculateES,parametersEU=parametersEU)
#     
#     dfToPlot <- data.frame(good=pointsInGood, 
#                            bad=pointsInBad,
#                            #bad1=calculateEquiWEU(pointsInBadP1,parametersEU),
#                            bad1=pointsInBadP1,
#                            totalMaize=decisionEvaluated$strategyDf$totalMaize, totalSoy=decisionEvaluated$strategyDf$totalSoy,totalWheat=decisionEvaluated$strategyDf$totalWheat)
#     
#     xlab="Regret in scenarios w/ GOOD performance [$]"
#     ylab="Regret in scenarios w/ BAD performance [$]"
#     ylab1="ES in scenarios w/ BAD performance [$]"
#     
#     
#   }  
#   
#   
#   if(obj=="regretROI") {
#     pointsInGood <- -tapply(tempDf2$regretROI[pred.tree[tempDf2$sc]=='FALSE'],
#                             INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='FALSE'],
#                             FUN=quantile, simplify=TRUE, probs=parametersRegret$q, type=qType)
#     
#     pointsInBad <- -tapply(tempDf2$regretROI[pred.tree[tempDf2$sc]=='TRUE'],
#                            INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#                            FUN=quantile, simplify=TRUE, probs=parametersRegret$q, type=qType)
#     
#     # pointsInBadP1 <- tapply(tempDf2$utility[pred.tree[tempDf2$sc]=='TRUE'],
#     #                         INDEX=tempDf2$st[pred.tree[tempDf2$sc]=='TRUE'],
#     #                         FUN=mean, simplify=TRUE)
#     
#     dfToPlot <- data.frame(good=pointsInGood, 
#                            bad=pointsInBad,
#                            #bad1=calculateEquiWEU(pointsInBadP1,parametersEU),
#                            #bad1=pointsInBadP1,
#                            totalMaize=decisionEvaluated$strategyDf$totalMaize, totalSoy=decisionEvaluated$strategyDf$totalSoy,totalWheat=decisionEvaluated$strategyDf$totalWheat)
#     
#     xlab="Regret ROI in scenarios w/ GOOD performance [$]"
#     ylab="Regret ROI in scenarios w/ BAD performance [$]"
#     #ylab1="ES in scenarios w/ BAD performance [$]"
#     
#     
#   } 
#   
#   
#   margin <- 0.05
#   maxX <- max(dfToPlot$good)+margin
#   idMaxX <- as.numeric(which.max(dfToPlot$good))
#   maxY <- max(dfToPlot$bad)+margin
#   
#   idMaxY <- which.max(dfToPlot$bad)
#   minX <- dfToPlot$good[idMaxY]- margin
#   minY <- dfToPlot$bad[idMaxX]- margin
#   
#   #maxY1 <- max(dfToPlot$bad1)+margin
#   #minY1 <- dfToPlot$bad1[idMaxX]- margin
#   #title2 <- paste("EU -", locDf[loc,1], "\n",
#   #                paste(locDf[loc,c(2,3)], collapse = "-"))
#   
#   title2 <- "GOOD scenarios vs.\nBAD scenarios\nclassified by candidate performance"
#   title3 <- "GOOD scenarios vs.\nBAD scenarios\nclassified by candidate performance"
#   #par(mfrow=c(1,2))
#   #pdf(paste0('plots/',soil,'-',waterCont,'-tradeOff.pdf',sep=""),width=6,height=6) 
#   plot(dfToPlot$good*-1,
#        dfToPlot$bad*-1,
#        type="p",
#        #xlim=range(pretty(uu10, n=20)),
#        #ylim=range(pretty(uu11, n=20)),
#        xlab=xlab,
#        ylab=ylab,
#        pch=0, cex=0.9,
#        #xlim=c(-maxX,-minX),
#        #ylim=c(-maxY,-minY),
#        
#        #xlim=c(100,375),
#        #ylim=c(250,450),
#        
#        bg=2,
#        col=rgb(dfToPlot[,"totalMaize"],dfToPlot[,"totalSoy"],dfToPlot[,"totalWheat"]),
#        main=title2,family="Times", font.main=1)
#   
#   abline(a=(dfToPlot$good[candidate]+dfToPlot$bad[candidate])*-1, b= -1,lwd=0.5)
#   
#   points(dfToPlot$good[MOSolList$decisionEvaluated$strategyDf$MOfront]*-1,
#          dfToPlot$bad[MOSolList$decisionEvaluated$strategyDf$MOfront]*-1,
#          type="p",
#          pch=15, cex=0.9,
#          xlim=c(minX,maxX),
#          ylim=c(minY,maxY),
#          #bg=2,
#          col=rgb(dfToPlot[MOSolList$decisionEvaluated$strategyDf$MOfront,"totalMaize"],
#                  dfToPlot[MOSolList$decisionEvaluated$strategyDf$MOfront,"totalSoy"],
#                  dfToPlot[MOSolList$decisionEvaluated$strategyDf$MOfront,"totalWheat"]),
#          main=title2)
#   
#   #print(dfToPlot$good[c(1001)]*-1)
#   #print(bestVect)
#   #points (dfToPlot$good[999]*-1,dfToPlot$bad[999]*-1,cex=1)
#   #points (dfToPlot$good[999]*-1,dfToPlot$bad[999]*-1,cex=2)
#   idsToMark <- c(1001,999,998,991,989,973,970,940,886)
#   
#   points (dfToPlot$good[idsToMark]*-1,dfToPlot$bad[idsToMark]*-1,cex=2)
#   text(dfToPlot$good[idsToMark]*-1,dfToPlot$bad[idsToMark]*-1,as.character(idsToMark), 
#        family="Times",cex=0.6, adj = c(1.5, 1.5))
#   
#   plot.new()              ## suggested by @Josh
#   vps <- baseViewports()
#   grid::pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
#   vp1 <-grid::plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
#   p <- plotStrategies(rev(idsToMark))
#   print(p,vp = vp1) 
#   #text(dfToPlot$good[bestVect]-gap,dfToPlot$bad[bestVect]-gap, names(bestVect), family="Times")
#   #text(dfToPlot$good[c('854','844','677','1433','1976')],dfToPlot$bad[c('854','844','677','1433','1976')]+(maxY-minY)/40, c('854','844','677','1433','1976'))
#   
#   
#   dev.off()
# }

