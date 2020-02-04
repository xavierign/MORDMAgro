#### load parameters ####
library(rpart)

#### 1. scenario parameters ####
f1 <- "2018-06-01"
f2 <- "2018-09-25"
f3 <- "2018-12-01"
f4 <- "2018-03-30"

periods <- list(PP= list(limInf=as.numeric(strftime(as.Date(f1, format="%Y-%m-%d"), format = "%j")),
                         limSup=365+as.numeric(strftime(as.Date(f1, format="%Y-%m-%d"), format = "%j"))),
                p_jun2sep=list(limInf=as.numeric(strftime(as.Date(f1, format="%Y-%m-%d"), format = "%j")),
                           limSup=as.numeric(strftime(as.Date(f2, format="%Y-%m-%d"), format = "%j"))),
                p_sep2dic=list(limInf=as.numeric(strftime(as.Date(f2, format="%Y-%m-%d"), format = "%j"))+1,
                         limSup=as.numeric(strftime(as.Date(f3, format="%Y-%m-%d"), format = "%j"))),
                p_dic2marNY=list(limInf= as.numeric(strftime(as.Date(f3, format="%Y-%m-%d"), format = "%j"))+1 ,
                        limSup=365 + as.numeric(strftime(as.Date(f4, format="%Y-%m-%d"), format = "%j"))),
                p_marNY2junNY= list(limInf=365+ as.numeric(strftime(as.Date(f4, format="%Y-%m-%d"), format = "%j"))+1 ,
                                    limSup=365 + as.numeric(strftime(as.Date(f1, format="%Y-%m-%d"), format = "%j"))))


#### 1. statistical parameters ####
qType <- 9 #parameter to calculate percentile of a discrete function.

#### 2. decisions parameters ####
parametersEU <- list(R = 1.5,w0 = 800,q=0.25)

parametersRegret <- list(q=0.75)

parametersList <- list(parametersEU=parametersEU,parametersRegret=parametersRegret)

#### 3. tree parameters ####
control.rpart <- rpart.control(minbucket = 80,minsplit = 80, cp = 0.015, 
                               xval = 10,
                               maxdepth = 3)

loss.matrix <-  matrix(c(0, 1,2, 0), nrow=2, byrow=TRUE)


#colors visualization
colorsPal <- read.csv (file="functions/pallete.csv")
rownames(colorsPal) <- colorsPal[,1]

#control parameters
objectiveMinimization <- c(T,T,F,T,F,F,F)
names(objectiveMinimization) <- c('fixedCost','R3Q_ROI','E_ROI','R3Q_U','ES_ROI',"ES_U","E_U")

CAsNames <-c("Mt","MT","Siii","Siv","Ws-S","Wl-S")