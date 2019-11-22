#### plot location ####
plotLocation <- function(loc, locDf, zoneMap) {
  title <- paste(locDf[loc,'location'],", series name: ", locDf[loc,'specificLocation'],sep="")
  subtitle <- paste("soil type: ", locDf[loc,'soilType']," - water content: ",locDf[loc,'waterContent'], sep="")
  
  
  g <- ggmap(zoneMap) + # ggtitle(paste(i,locDf[i,3])) +  
    labs(title=title,
         subtitle=subtitle)+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
    
    geom_point(data = locDf[locDf$waterContent=="low",c("lon","lat")], color = "gray20", size = 2) +
    geom_point(data = locDf[loc,c("lon","lat")], color = "red", size = 3) 
  
  return(g)
}

#### plot location ####
plotSimLocs <- function(simulatedLocs, zoneMap) {
  title <- "Location of simulated farms"
  subtitle <- paste0("Near ",zoneMap)
  
  register_google(key = "AIzaSyBiJw6yRkb4si_vWP1VUbPmUke4NXv8Baw",  # your Static Maps API key
                  account_type = "standard")
  
  m <- get_googlemap(center = zoneMap, zoom = 7,
                     style = 'element:labels|visibility:off')
  
  g <- ggmap(m) + # ggtitle(paste(i,locDf[i,3])) +  
    labs(title=title) +

         #subtitle=subtitle)+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
          text=element_text(size=12,family="Times")) +
    xlab("longitude") + ylab("latitude") +
    geom_point(data = data.frame(lon=simulatedLocs$lon,
                                 lat=simulatedLocs$lat), 
               color = "red", size = 2,shape=18) +
    #geom_point(data = locDf[loc,c("lon","lat")], color = "red", size = 3) 
    annotate("text",x =simulatedLocs$lon, y = simulatedLocs$lat+0.1, 
             label = simulatedLocs$name,
             family="Times",hjust = 0, size=3)
  
  m2 <- get_googlemap(center = zoneMap, zoom = 5)
  
  g2 <- ggmap(m2) + # ggtitle(paste(i,locDf[i,3])) +  
    labs(title="Region of Interest")+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
          text=element_text(size=12,family="Times")) +
    xlab("longitude") + ylab("latitude") +
    annotate("rect",
             xmin = -64.2, xmax = -58.2, 
             ymin = -37.4, ymax = -32.4, alpha = .3 )
  
  
  gOut <- grid.arrange(g2,  
               g,  
               nrow = 1)
  
  gOut
  
  return(gOut)
}


#### plot location ####
plotLocations <- function(loc, locDf, zoneMap) {
  title <- "Map of Argentinas Pampas with modeled locations"
  #subtitle <-  "Modeled Locations"
  
  
  g <- ggmap(zoneMap) + # ggtitle(paste(i,locDf[i,3])) +  
    labs(title=title)+#,
         #subtitle=subtitle)+
    theme(plot.title = element_text(hjust = 0.5),
          #plot.subtitle = element_text(hjust = 0.5),
                                    text=element_text(size=12,family="Times")) +
    xlab("longitude") + ylab("latitude") +
    geom_point(data = locDf[locDf$waterContent=="low",c("lon","lat")], color = "gray20", size = 1) +
    geom_point(data = locDf[loc,c("lon","lat")], color = "red", size = 1) +
    annotate("text",x =locDf[loc,"lon"], y = locDf[loc,"lat"]+0.1, label = locDf[loc,"specificLocation"],family="Times",hjust = 1, size=3)
  return(g)
}

getBestStrategies <- function(MOSolList) {
  tempDf <-  MOSolList$performanceMetrics[order(MOSolList$performanceMetrics$EU),]
  
  bestVect <- rownames(tempDf)[apply(tempDf, MARGIN=2,FUN=which.min)]
  names(bestVect) <- colnames(tempDf)
  return(bestVect)
}




plotStrategies <- function(strategies) {
  
  tempVec <- MOSolList$decisionEvaluated$strategyDf[strategies,]
  
  tempDf <- tempVec
  tempDf<- t(tempDf[names(tempVec) %in% rownames(colorsPal)])
  
  tempDf <- tempDf[order(colorsPal[rownames(tempDf),'orders'],decreasing = T),]
  
  library(reshape2)
  
  dfmelt <- melt(tempDf, id.vars = "crop")
  colnames(dfmelt) <- c('crop','id','value')
  dfmelt$id <- factor(dfmelt$id, levels=strategies)
  
  #order factors to plot
  lev <- unique(dfmelt$crop)
  
  #if .1 in names replace by =""
  #dfmelt$crop <- gsub(".1", "", dfmelt$crop)
  dfmelt$crop <- factor(dfmelt$crop, levels = lev[order(colorsPal[lev,'orders'])])
  
  #pallete
  pal2 <- as.character(colorsPal[,2])
  names(pal2) <- rownames(colorsPal)
  
  ### change this line #####
  levels(dfmelt$id) <- as.character(strategies)
  
  #dfmelt$crop <- as.character(dfmelt$crop)
  
  #barplot
  g3 <- ggplot(dfmelt, aes(x=id, y=value,fill=crop)) +
    geom_bar(stat="identity") + coord_flip() + scale_fill_manual(values = pal2) +
    theme(axis.title.y = element_blank(),text=element_text(family="Times")) +
    ylab("proportion of land") +
    ggtitle("Hedging strategies") + 
    theme(plot.title = element_text(hjust = 0.5,size=10),axis.title=element_text(size=8), 
            legend.position='none', legend.box = "horizontal")
  
  return(g3)
}

plotBestStrategiesByObjective <- function(solutionList, objective="EU") {
  
  ##tri plot
  triplotDf <- data.frame(Soy= numeric(length(solutionList)),
                          Wheat= numeric(length(solutionList)),
                          Maize=numeric(length(solutionList)))
  
  for (i in 1:length(solutionList)) {
    id <- names(which.min(solutionList[[i]]$performanceMetrics[,objective]))
    triplotDf[i,c('Soy','Maize','Wheat')] <- solutionList[[i]]$decisionEvaluated$strategyDf[id,c('totalSoy', 'totalMaize', 'totalWheat')]
  }
  
  
  #df2 <- MOSolList$decisionEvaluated$strategyDf[c(bestES,bestEU,bestRegret,bestFixedCost),c('totalSoy', 'totalMaize', 'totalWheat')]
  
  s <- seq(0,1, by=0.2)
  
  g1 <- ggtern(triplotDf,aes(Soy,Wheat,Maize)) +
    
    geom_Tline(Tintercept = 0.5, size = 0.7, color = "gray30", linetype = "dashed")+
    geom_Lline(Lintercept = 0.5, size = 0.7, color = "gray30", linetype = "dashed")+
    geom_Rline(Rintercept = 0.5, size = 0.7, color = "gray30", linetype = "dashed")+
    geom_point(color=rgb(as.matrix(triplotDf[,c(3,1,2)])), size =8, alpha=0.2) +
    #theme_classic() +
    theme_nomask()+
    theme_arrowdefault() +
    #limit_tern(breaks = seq(0,1,by=0.5)) +
    ggtitle("Strategies in the Multi-Objective frontier") +
    theme(plot.title = element_text(hjust = 0.5)) +
    # annotate(geom = 'text',
    #          x = df2$totalSoy,
    #          y = df2$totalWheat,
    #          z = df2$totalMaize,
    #          #angle = c(0,30,60),
    #          vjust = c(-1,-1,-1,-1),
    #          hjust = c(0,0,1.3,0),
    #          label = c('ES','EU','Reg','FC')) +
    # 
    annotate(geom = 'text',
             x = 0,
             y = 0.5,
             z = 0.5,
             #angle = c(0,30,60),
             vjust = -1,
             hjust = 0,
             label = 'Rot') +
    # annotate(geom='point',
    #          x = df2$totalSoy,
    #          y = df2$totalWheat,
    #          z = df2$totalMaize, size=7, shape=c(0,3,4,5))  +
    scale_T_continuous(breaks=s,labels=paste("   ",s,"   ", sep=""))+ 
    scale_L_continuous(breaks=s,labels=paste("   ",s,"   ", sep=""))+ 
    scale_R_continuous(breaks=s,labels=paste("   ",s,"   ", sep=""))
    return(g1)
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



