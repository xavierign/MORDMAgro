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
qType <- 9

#### 2. decisions parameters ####
parametersEU <- list(R = 1.5,w0 = 800,q=0.25)

parametersRegret <- list(q=0.75)

parametersList <- list(parametersEU=parametersEU,parametersRegret=parametersRegret)

#### 3. tree parameters ####
control.rpart <- rpart.control(minbucket = 80,minsplit = 80, cp = 0.04, 
                               xval = 10,
                               maxdepth = 3)

loss.matrix <-  matrix(c(0, 1,1.5, 0), nrow=2, byrow=TRUE)

#### 4. colors ####
# colorsPal <- data.frame(row.names= c('MZ_E','MZ_L', 'WH_IB','WH_IL', 'SB_III',	'SB_IV', 'SB_V','SB_VI'),
#                         colorsPal = c('#bd0026',	'#f03b20','#237cbb','#43a2ca','#005a32','#238443','#41ab5d','#78c679'),
#                         orders = 1:8, stringsAsFactors = F)

colorsPal <- read.csv (file="data/pallete.csv")
rownames(colorsPal) <- colorsPal[,1]

objectiveMinimization <- c(F,T,F,T,F,F,F)
names(objectiveMinimization) <- c('E_ROI','R3Q_ROI','E_ROI','R3Q_U','ES_ROI',"ES_U","E_U")

CAsNames <-c("Mt","MT","Siii","Siv","Ws-S","Wl-S")









