#### 1. read data ####

#yields
yieldsDf <- read.csv("data/yields.csv", stringsAsFactors = F)

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
          split(z,z$cropVar,drop=T),
          FUN= function (q) lapply(
            split(q,q$management,drop=T),
            FUN= function (t) {
              rownames(t) <- t$year
              return(t[,c("soil_prop",
                          "yield")])})
        )))))

#climate
climateDf <- read.csv("data/climate.csv", stringsAsFactors = F)
climateList <- split(climateDf,climateDf$weatherStationCode)

#enso
ensoDf <- read.csv("data/enso.csv",stringsAsFactors = F)
rownames(ensoDf) <- ensoDf$year

#prices
pricesDf <- read.csv("data/prices.csv", stringsAsFactors = F)
rownames(pricesDf) <- pricesDf$year

#costs
costsDf <- read.csv("data/costs.csv", stringsAsFactors = F)

#convert df to list to simplify processing
costList <- split(costsDf,costsDf$location, drop=T)
for (loc in names(costList)) {
  rownames(costList[[loc]]) <- costList[[loc]]$management
}


rownames(costsDf) <- costsDf$management

#locations
locationsDf <- read.csv("data/locations.csv", stringsAsFactors = F)
rownames(locationsDf) <- locationsDf$location

inputData <- list(yieldsList=yieldsList,
                  climateList=climateList,
                  ensoDf=ensoDf,
                  pricesDf=pricesDf,
                  costList=costList,
                  locationsDf=locationsDf
              )

rm(climateDf,pricesDf,costsDf,yieldsDf,climateList,locationsDf,yieldsList, ensoDf,costList)


