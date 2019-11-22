#### 1. read data ####
locationsDf <- read.csv("data/locations.csv", stringsAsFactors = F)
rownames(locationsDf) <- locationsDf$ciudad

climateDf <- read.csv("data/climate.csv", stringsAsFactors = F)
climateList <- split(climateDf,climateDf$id)

yieldsDf <- read.csv("data/yields.csv", stringsAsFactors = F)

ensoDf <- read.csv("data/enso.csv",stringsAsFactors = F)
rownames(ensoDf) <- ensoDf$y2

#convert df to list to simplify processing
yieldsList <- lapply(
              split(yieldsDf,yieldsDf$location, drop=T), 
                  FUN = function(x) lapply(
                          split(x,x$soil_series, drop=T), 
                          FUN= function (y) lapply(
                                split(y,y$soil_family, drop=T),
                                FUN=function (w) lapply (
                                      split(w,w$water,drop=T), 
                                      FUN= function(z) lapply(
                                            split(z,z$crop,drop=T),
                                            FUN= function (q) lapply(
                                                  split(q,q$management,drop=T),
                                                  FUN= function (t) {
                                                  rownames(t) <- t$year
                                                                  return(t[,c("soil_prop",
                                                                              "yield")])})
                                     )))))

costsDf <- read.csv("data/costs.csv", stringsAsFactors = F)
rownames(costsDf) <- costsDf$management

pricesDf <- read.csv("data/prices.csv", stringsAsFactors = F)
rownames(pricesDf) <- pricesDf$Year

inputData <- list(locationsDf=locationsDf,
                  climateList=climateList,
                  yieldsList=yieldsList,
                  costsDf=costsDf,
                  pricesDf=pricesDf)

simulatedLocs <- read.csv("data/simulatedLocs.csv",stringsAsFactors = F)
rm(climateDf,pricesDf,costsDf,yieldsDf,climateList,locationsDf,yieldsList)





