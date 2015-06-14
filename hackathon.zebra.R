library('gbm')
library("lubridate")
library(car)
library(plyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(Hmisc)


roundByN <- function(x,n){
  n*round(x/n)
}

#setwd("/net/mw-0775-nas22p/isg_prodops_work/mmal4/Hackathon")
collision <- read.csv("C:/Insurance Industry Code/hackathon//hackdata1.csv", colClasses=c("CLAIM_NUMBER"="character"))
collision$Snapshot_Date <- gsub("-","/",collision$Snapshot_Date)
collision$Date_Of_Loss <- gsub("-","/",collision$Date_Of_Loss)
collision$Snapshot_Date <- as.Date(collision$Snapshot_Date, "%Y/%m/%d")
collision$Date_Of_Loss <- as.Date(collision$Date_Of_Loss, "%Y/%m/%d")
collision$SnapshotDateNumeric <- as.numeric(collision$Snapshot_Date)
collision$DateofLossNumeric <- as.numeric(collision$Date_Of_Loss)
collision$SnapshotDaysinYear <- collision$SnapshotDateNumeric %% 365 - 9
collision$DateOfLossDaysinYear <- collision$DateofLossNumeric %% 365 - 9
collision$Snapshot_Year <- (collision$SnapshotDateNumeric - 10) / 365 + 1970
collision$vehicleAge <- collision$Snapshot_Year - collision$Model_Year
collision$Time_Of_Loss <- round(collision$Time_Of_Loss,1)
#collision <- subset(collision,!is.na(collision$LOSS))
collision$Policy_ZIP <- as.numeric(as.character(collision$Policy_ZIP))
collision$Policy_ZIP <- floor(collision$Policy_ZIP / 100)
collision$Policy_ZIP <- as.factor(collision$Policy_ZIP)
collision$logLoss <- log(collision$LOSS)
collision$logLoss <- pmax(0.1,collision$logLoss)
factor.vars <- c(35:41,49:110,138)
#factor.vars <- c(3,7,12:17,22,23)
for(i in factor.vars){
  collision[,i] <- as.factor(collision[,i])
}


collision$ReportLag <- apply(collision[, c('SnapshotDateNumeric', 'Snapshot_Time', 'DateofLossNumeric', 'Time_Of_Loss')], 1, function(x){
  ifelse((x[1]*24 - x[3]*24) + (x[2] - x[4]) < 0, 0, (x[1]*24 - x[3]*24) + (x[2] - x[4]))
})

#hack11 <- subset(collision,(collision$Snapshot_Year >= 2011  & collision$Snapshot_Year < 2012))
#hack12 <- subset(collision,(collision$Snapshot_Year >= 2012  & collision$Snapshot_Year < 2013))
#hack13 <- subset(collision,(collision$Snapshot_Year >= 2013))
#sum11 <- sum(hack11$LOSS)
#mean11 <- mean(hack11$LOSS)
#sum12 <- sum(hack12$LOSS)
#mean12 <- mean(hack12$LOSS)


collision$drive <- as.character(collision$drive)
collision$drive <- gsub('.*(4X4).*', '\\1', collision[, 'Series_Name'])
collision$drive <- gsub('.*(4X2).*', '\\1', collision$drive)
collision$drive <- gsub('.*(4WD).*', '\\1', collision$drive)
collision$drive <- gsub('.*(2WD).*', '\\1', collision$drive)
collision$drive <- gsub('.*(2WD).*', '\\1', collision$drive)
collision$drive <- gsub('.*(FWD).*', '\\1', collision$drive)
collision$drive <- gsub('.*(RWD).*', '\\1', collision$drive)
collision$drive <- gsub('.*(4D).*', '\\1', collision$drive)
collision$drive <- gsub('.*(2D).*', '\\1', collision$drive)
collision$drive <- gsub('.*(CONV).*', '\\1', collision$drive)
collision$drive <- gsub('.*(VAN).*', '\\1', collision$drive)
collision$drive <- gsub('.*(SW).*', '\\1', collision$drive)
collision$drive <- gsub('.*(SEDAN).*', '\\1', collision$drive)
collision$drive <- gsub('.*(3D).*', '\\1', collision$drive)
collision$drive <- gsub('.*(5D).*', '\\1', collision$drive)
collision$drive <- gsub('.*(WGN|WAGON).*', 'WGN', collision$drive)
collision$drive <- gsub('.*(COUPE).*', '\\1', collision$drive)
collision$drive <- gsub('.*(LW).*', '\\1', collision$drive)
collision$drive <- gsub('2-.*|4-.*|MONDIAL.*|P30.*|SCI.*|911.*|F-.*|TORONA.*|575.*|MED.*|CUT.*|MR2.*', 'OTHER', collision$drive)
collision$drive <- as.factor(collision$drive)

collision$Front_HOOD <- as.factor(collision$Front_HOOD)
collision$Rear_TRUNK_TAILGATE_HATCH <- as.factor(collision$Rear_TRUNK_TAILGATE_HATCH)
collision$Interior_AIR_BAGS <- as.factor(collision$Interior_AIR_BAGS)
collision$Passenger_Side_Exterior_IND <- as.factor(collision$Passenger_Side_Exterior_IND)
collision$Driver_Side_Exterior_IND <- as.factor(collision$Driver_Side_Exterior_IND)
collision$Passenger_Side_Exterior_REAR_DOOR <- as.factor(collision$Passenger_Side_Exterior_REAR_DOOR)
collision$Driver_Side_Exterior_REAR_DOOR <- as.factor(collision$Driver_Side_Exterior_REAR_DOOR)
collision$Front_IND <- as.factor(collision$Front_IND)
collision$Passenger_Side_Exterior_FRONT_DOOR <- as.factor(collision$Passenger_Side_Exterior_FRONT_DOOR)
collision$vehicleAge <- round(collision$vehicleAge)
collision$vehicleAge <- pmax(0,collision$vehicleAge)
luxury <- c('BENTLEY','FERRARI','JAGUAR','LAMBORGHINI','LOTUS','MASERATI','MERCEDES BENZ','PORSCHE',
            'ROLLS ROYCE','TESLA')
collision$Make_Name <- as.character(collision$Make_Name)
collision$Make_Name <- gsub(' ','',collision$Make_Name)
collision$luxury.car <- ifelse(collision$Make_Name %in% luxury,1,0)
collision$luxury.car <- as.factor(collision$luxury.car)
rm(luxury)

collision$Front_Frac <- pmin(collision$Front_Frac,0.25)
collision$Rear_Frac <- pmin(collision$Rear_Frac,0.2)
collision$Driver_Side_Exterior_Frac <- pmin(collision$Driver_Side_Exterior_Frac,0.3)
collision$Passenger_Side_Exterior_Frac <- pmin(collision$Passenger_Side_Exterior_Frac,0.3)
collision$old.cars <- collision$vehicleAge - 20
collision$old.cars[collision$old.cars < 0] <- 0

collision$Asset_is_Safely_operable <- as.character(collision$Asset_is_Safely_operable)
collision$Asset_Towed_From_Scene <- as.character(collision$Asset_Towed_From_Scene)
collision$Doors_open_freely <- as.character(collision$Doors_open_freely)
collision$Asset_is_Safely_operable <- ifelse((is.na(collision$Asset_is_Safely_operable)|collision$Asset_is_Safely_operable=='Missing')
                                              ,'Unknown',collision$Asset_is_Safely_operable)
collision$Asset_Towed_From_Scene <- ifelse((is.na(collision$Asset_Towed_From_Scene)|collision$Asset_Towed_From_Scene=='Missing')
                                             ,'Unknown',collision$Asset_Towed_From_Scene)
collision$Doors_open_freely <- ifelse((is.na(collision$Doors_open_freely)|collision$Doors_open_freely=='Missing')
                                             ,'Unknown',collision$Doors_open_freely)
collision$Asset_is_Safely_operable <- as.factor(collision$Asset_is_Safely_operable)
collision$Asset_Towed_From_Scene <- as.factor(collision$Asset_Towed_From_Scene)
collision$Doors_open_freely <- as.factor(collision$Doors_open_freely)
collision$Make_Name <- as.factor(collision$Make_Name)
collision$Base_Price <- pmax(100,collision$Base_Price)
collision$logBasePrice <- log(collision$Base_Price)
collision$Curb_Weight <- pmax(1000,collision$Curb_Weight)
collision$logCurbWeight <- log(collision$Curb_Weight)

sapply(collision[,c(3,143,128)],mean,na.rm=TRUE)
sapply(collision[,c(3,143,128)],sd,na.rm=TRUE)
describe(collision[,c(3,143,128)],na.rm=TRUE)
collision <- subset(collision,!is.na(collision$LOSS))


set.seed(100)
smp_size <- floor(0.70 * nrow(collision))
train_ind <- sample(seq_len(nrow(collision)), size = smp_size)
train <- collision[train_ind, ]
test <- collision[-train_ind,]
#all <- rbind(test, train)
rm(smp_size)
rm(train_ind)

#fithd <- glm(logLoss ~.,data=hd,family=gaussian())
#step <- stepAIC(fithd, direction="backward")
#summary(fithd)

### I'm using the glm function with a binomial family link in order to output the log-odds coefficients
fit.glm <- glm(LOSS ~ Asset_is_Safely_operable + Detailed_Loss_Type + Asset_Towed_From_Scene + Front_Frac +                                                              
                  Front_HOOD + Doors_open_freely + Interior_AIR_BAGS + vehicleAge + Base_Price +
                  Rear_TRUNK_TAILGATE_HATCH + Passenger_Side_Exterior_Frac + Rear_Frac + 
                  Driver_Side_Exterior_Frac + Driver_Side_Exterior_IND + Passenger_Side_Exterior_IND + 
                  Did_this_loss_occur_in_a_parking_lot + Passenger_Side_Exterior_REAR_DOOR + 
                  Was_a_police_report_filed + Driver_Side_Exterior_REAR_DOOR+old.cars + luxury.car+
                  logBasePrice + Curb_Weight,
                  data=train,family=gaussian(link='log'))
summary(fit.glm)

train$LOSS.pred <- predict(fit.glm,newdata=train,type='response')
test$LOSS.pred <- predict(fit.glm,newdata=test,type='response')



#test$gbm.resid <- test$logLoss.pred.gbm-test$logLoss
#train$gbm.resid <- train$logLoss.pred.gbm-train$logLoss
test$resid <- test$LOSS.pred - test$LOSS
train$resid <- train$LOSS.pred - train$LOSS

test.lowDollar <- test[which(test$Base_Price <= 25000),]
test.medDollar <- test[which(test$Base_Price > 25000 & test$Base_Price < 50000),]
test.highDollar <- test[which(test$Base_Price >= 50000),]

mean.resid.all <- round(mean(test$resid,rm.na=TRUE))
mean.resid.low <- round(mean(test.lowDollar$resid,rm.na=TRUE))
mean.resid.med <- round(mean(test.medDollar$resid,rm.na=TRUE))
mean.resid.high <- round(mean(test.highDollar$resid,rm.na=TRUE))


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



p1 <- ggplot(test,aes(x=Base_Price,y=resid))+geom_point(alpha=0.002) + 
  xlim(c(10000,100000)) + ylim(c(-25000,25000)) + ggtitle(paste('Loss Residuals: mean = ',
                            mean.resid.all,' dollars',sep=''))

p2 <- ggplot(test,aes(Base_Price)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Base Price Histogram") + 
  xlab("Base Price") + ylab("Count") + xlim(c(10000,100000))

multiplot(p1,p2,cols=1)

p35 <- ggplot(test,aes(x=LOSS.pred,y=resid))+geom_point(alpha=0.02) + 
  xlim(c(0,25000)) + ylim(c(-25000,25000)) + 
  ggtitle(paste('Loss Residuals: mean = ',
      mean.resid.all,' dollars',sep=''))

p36 <- ggplot(test,aes(LOSS.pred)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Base Price Histogram") + 
  xlab("Base Price") + ylab("Count") + xlim(c(0,25000))

multiplot(p35,p36,cols=1)

p37 <- ggplot(test,aes(x=LOSS.pred,y=resid))+geom_point(alpha=0.02) + 
  xlim(c(0,5000)) + ylim(c(-10000,10000)) + 
  ggtitle(paste('Loss Residuals: mean = ',
                mean.resid.all,' dollars',sep=''))

p38 <- ggplot(test,aes(LOSS.pred)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Base Price Histogram") + 
  xlab("Base Price") + ylab("Count") + xlim(c(0,5000))

multiplot(p37,p38,cols=1)

layout(matrix(1:4,2,2)) 
plot40 <- plot(fit.glm)

p3 <- ggplot(test.lowDollar,aes(x=Base_Price,y=resid))+geom_point(alpha=0.2) + 
  xlim(c(10000,25000)) +ylim(c(-15000,15000)) + ggtitle(paste('Loss Residuals Low Dollar: mean = ',
                            mean.resid.low,' dollars',sep=''))
p4 <- ggplot(test.lowDollar,aes(Base_Price)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Base Price Low Dollar Histogram") + 
  xlab("Base Price") + ylab("Count") + xlim(c(0,25000))
multiplot(p3,p4,cols=1)

p5 <- ggplot(test.medDollar,aes(x=Base_Price,y=resid))+geom_point(alpha=0.2) + 
  xlim(c(25000,50000)) +ylim(c(-15000,15000)) + ggtitle(paste('Loss Residuals Medium Dollar: mean = ',
                      mean.resid.med,' dollars',sep=''))
p6 <- ggplot(test.medDollar,aes(Base_Price)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Base Price Medium Dollar Histogram") + 
  xlab("Base Price Medium Dollar") + ylab("Count") + xlim(c(25000,50000))
multiplot(p5,p6,cols=1)

p7 <- ggplot(test.highDollar,aes(x=Base_Price,y=resid))+geom_point(alpha=0.2) + 
  xlim(c(50000,100000)) +ylim(c(-15000,15000)) + ggtitle(paste('Loss Residuals High Dollar: mean = ',
                  mean.resid.high,' dollars',sep=''))
p8 <- ggplot(test.highDollar,aes(Base_Price)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Base Price High Dollar Histogram") + 
  xlab("Base Price") + ylab("Count") + xlim(c(50000,100000))
multiplot(p7,p8,cols=1)



rm(test.lowDollar)
rm(test.medDollar)
rm(test.highDollar)


ggplot(collision,aes(x=LOSS,col='blue')) + geom_histogram(binwidth=100) +xlim(c(0,15000))+
  theme(legend.position='none') + ggtitle ('Loss Histogram') + xlab('Dollar Loss')
ggplot(collision,aes(x=logLoss,col='red')) + geom_histogram(binwidth=0.1) +
  theme(legend.position='none') + xlim(c(5,11)) + ggtitle('log Loss Histogram') 

a <- outlierTest(fit.glm)
cutoff <- 4/((nrow(train)-length(fit.glm$coefficients)-2)) 
plot(fit.glm, which=4, cook.levels=cutoff)


# Evaluate Collinearity
vif(fit.glm) # variance inflation factors 
vif(fit.glm) > 10 # problem?

qqPlot(train$resid) 
qqPlot(test$resid) 





collision$Front_Frac <- roundByN(100*collision$Front_Frac,5)/100
collision$Rear_Frac <- roundByN(100*collision$Rear_Frac,5)/100
collision$Driver_Side_Exterior_Frac <- roundByN(100*collision$Driver_Side_Exterior_Frac,10)/100
collision$Passenger_Side_Exterior_Frac <- roundByN(100*collision$Passenger_Side_Exterior_Frac,10)/100
test$Front_Frac <- roundByN(100*test$Front_Frac,5)/100
test$Rear_Frac <- roundByN(100*test$Rear_Frac,5)/100
test$Driver_Side_Exterior_Frac <- roundByN(100*test$Driver_Side_Exterior_Frac,10)/100
test$Passenger_Side_Exterior_Frac <- roundByN(100*test$Passenger_Side_Exterior_Frac,10)/100
train$Front_Frac <- roundByN(100*train$Front_Frac,5)/100
train$Rear_Frac <- roundByN(100*train$Rear_Frac,5)/100
train$Driver_Side_Exterior_Frac <- roundByN(100*train$Driver_Side_Exterior_Frac,10)/100
train$Passenger_Side_Exterior_Frac <- roundByN(100*train$Passenger_Side_Exterior_Frac,10)/100
collision$Base_Price <- pmin(collision$Base_Price,100000)
collision$Base_Price <- roundByN(collision$Base_Price,10000)
train$Base_Price <- pmin(train$Base_Price,100000)
train$Base_Price <- roundByN(train$Base_Price,10000)
test$Base_Price <- pmin(test$Base_Price,100000)
test$Base_Price <- roundByN(test$Base_Price,10000)
collision$Base_Price <- roundByN(collision$Base_Price,10000)
train$Base_Price <- pmin(train$Base_Price,100000)
train$Base_Price <- roundByN(train$Base_Price,10000)
test$Base_Price <- pmin(test$Base_Price,100000)
test$Base_Price <- roundByN(test$Base_Price,10000)
test$Curb_Weight <- roundByN(test$Curb_Weight,500)
train$Curb_Weight <- roundByN(train$Curb_Weight,500)
collision$Curb_Weight <- roundByN(collision$Curb_Weight,500)

age.actual <- ddply(test,c("vehicleAge"),summarise,mloss=mean(LOSS))
age.pred <- ddply(test,c("vehicleAge"),summarise,mloss=mean(LOSS.pred))
dlt.actual <- ddply(test,c("Detailed_Loss_Type"),summarise,mloss=mean(LOSS))
dlt.pred <- ddply(test,c("Detailed_Loss_Type"),summarise,mloss=mean(LOSS.pred))
atfs.actual <- ddply(test,c("Asset_Towed_From_Scene"),summarise,mloss=mean(LOSS))
atfs.pred <- ddply(test,c("Asset_Towed_From_Scene"),summarise,mloss=mean(LOSS.pred))
fh.actual <- ddply(test,c("Front_HOOD"),summarise,mloss=mean(LOSS))
fh.pred <- ddply(test,c("Front_HOOD"),summarise,mloss=mean(LOSS.pred))
dof.actual <- ddply(test,c("Doors_open_freely"),summarise,mloss=mean(LOSS))
dof.pred <- ddply(test,c("Doors_open_freely"),summarise,mloss=mean(LOSS.pred))
iab.actual <- ddply(test,c("Interior_AIR_BAGS"),summarise,mloss=mean(LOSS))
iab.pred <- ddply(test,c("Interior_AIR_BAGS"),summarise,mloss=mean(LOSS.pred))
rtth.actual <- ddply(test,c("Rear_TRUNK_TAILGATE_HATCH"),summarise,mloss=mean(LOSS))
rtth.pred <- ddply(test,c("Rear_TRUNK_TAILGATE_HATCH"),summarise,mloss=mean(LOSS.pred))
ff.actual <- ddply(test,c("Front_Frac"),summarise,mloss=mean(LOSS))
ff.pred <- ddply(test,c("Front_Frac"),summarise,mloss=mean(LOSS.pred))
rf.actual <- ddply(test,c("Rear_Frac"),summarise,mloss=mean(LOSS))
rf.pred <- ddply(test,c("Rear_Frac"),summarise,mloss=mean(LOSS.pred))
psef.actual <- ddply(test,c("Passenger_Side_Exterior_Frac"),summarise,mloss=mean(LOSS))
psef.pred <- ddply(test,c("Passenger_Side_Exterior_Frac"),summarise,mloss=mean(LOSS.pred))
dsef.actual <- ddply(test,c("Driver_Side_Exterior_Frac"),summarise,mloss=mean(LOSS))
dsef.pred <- ddply(test,c("Driver_Side_Exterior_Frac"),summarise,mloss=mean(LOSS.pred))
lc.actual <- ddply(test,c("luxury.car"),summarise,mloss=mean(LOSS))
lc.pred <- ddply(test,c("luxury.car"),summarise,mloss=mean(LOSS.pred))
bp.actual <- ddply(test,c("Base_Price"),summarise,mloss=mean(LOSS))
bp.pred <- ddply(test,c("Base_Price"),summarise,mloss=mean(LOSS.pred))
cw.actual <- ddply(test,c("Curb_Weight"),summarise,mloss=mean(LOSS))
cw.pred <- ddply(test,c("Curb_Weight"),summarise,mloss=mean(LOSS.pred))

p9 <- ggplot() +
  geom_line(data=age.actual, aes(x=vehicleAge, y=mloss, color='red')) +
  geom_line(data=age.pred, aes(x=vehicleAge, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Count",
                                "Actual Count")) +
  ggtitle("Univariate Plot for Vehicle Age") + theme(legend.position='bottom') +
  xlab("Vehicle Age (years)") +
  ylab("Mean Loss")

p10 <- ggplot(test,aes(vehicleAge)) + geom_histogram(col='blue',fill='red') + 
  xlab("Vehicle Age (years)") + ylab("Count") 
multiplot(p9,p10,cols=1)




p11 <- ggplot() +
  geom_point(data=dlt.actual, aes(x=Detailed_Loss_Type, y=mloss, color='red')) +
  geom_point(data=dlt.pred, aes(x=Detailed_Loss_Type, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Detailed_Loss_Type") + 
  xlab("Detailed_Loss_Type") +
  ylab("Mean Loss") + theme(axis.text.x = element_text(angle = 60,hjust=1))

p12 <- ggplot(test,aes(Detailed_Loss_Type)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Detailed Loss Type") + 
  xlab("Detailed Loss Type") + ylab("Loss") 
multiplot(p11,p12,cols=1)
p11
p12 + theme(axis.text.x = element_text(angle = 60,hjust=1))



p13 <- ggplot() +
  geom_point(data=atfs.actual, aes(x=Asset_Towed_From_Scene, y=mloss, color='red')) +
  geom_point(data=atfs.pred, aes(x=Asset_Towed_From_Scene, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Asset_Towed_From_Scene") + 
  xlab("Asset_Towed_From_Scene") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p14 <- ggplot(test,aes(Asset_Towed_From_Scene)) + geom_histogram(col='blue',fill='red') +  
  xlab("Asset Towed?") + ylab("Loss") 
multiplot(p13,p14,cols=1)



p15 <- ggplot() +
  geom_point(data=fh.actual, aes(x=Front_HOOD, y=mloss, color='red')) +
  geom_point(data=fh.pred, aes(x=Front_HOOD, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Front Hood Damage") + 
  xlab("Front Hood") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p16 <- ggplot(test,aes(Front_HOOD)) + geom_histogram(col='blue',fill='red') + 
  xlab("Front Hood?") + ylab("Loss") 
multiplot(p15,p16,cols=1)



p17 <- ggplot() +
  geom_point(data=dof.actual, aes(x=Doors_open_freely, y=mloss, color='red')) +
  geom_point(data=dof.pred, aes(x=Doors_open_freely, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Doors Open Freely") + 
  xlab("Doors Open Freely?") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p18 <- ggplot(test,aes(Doors_open_freely)) + geom_histogram(col='blue',fill='red') + 
  xlab("Doors Open Freely?") + ylab("Loss") 
multiplot(p17,p18,cols=1)



p19 <- ggplot() +
  geom_point(data=iab.actual, aes(x=Interior_AIR_BAGS, y=mloss, color='red')) +
  geom_point(data=iab.pred, aes(x=Interior_AIR_BAGS, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Interior Air Bags Deployed") + 
  xlab("Interior Air Bags?") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p20 <- ggplot(test,aes(Interior_AIR_BAGS)) + geom_histogram(col='blue',fill='red') + 
  xlab("Interior Air Bags") + ylab("Loss") 
multiplot(p19,p20,cols=1)



p21 <- ggplot() +
  geom_point(data=rtth.actual, aes(x=Rear_TRUNK_TAILGATE_HATCH, y=mloss, color='red')) +
  geom_point(data=rtth.pred, aes(x=Rear_TRUNK_TAILGATE_HATCH, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Rear Trunk Tailgate Hatch") + 
  xlab("Rear Trunk Tailgate Hatch?") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p22 <- ggplot(test,aes(Rear_TRUNK_TAILGATE_HATCH)) + geom_histogram(col='blue',fill='red') + 
  xlab("Rear Trunk Tailgate Hatch") + ylab("Loss") 
multiplot(p21,p22,cols=1)


p23 <- ggplot() +
  geom_line(data=ff.actual, aes(x=Front_Frac, y=mloss, color='red')) +
  geom_line(data=ff.pred, aes(x=Front_Frac, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Front Frac") + 
  xlab("Front Frac") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p24 <- ggplot(test,aes(Front_Frac)) + geom_histogram(col='blue',fill='red') + 
  xlab("Front Frac") + ylab("Loss") 
multiplot(p23,p24,cols=1)


p25 <- ggplot() +
  geom_line(data=rf.actual, aes(x=Rear_Frac, y=mloss, color='red')) +
  geom_line(data=rf.pred, aes(x=Rear_Frac, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Rear Frac") + 
  xlab("Rear Frac") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p26 <- ggplot(test,aes(Rear_Frac)) + geom_histogram(col='blue',fill='red') + 
  xlab("Rear Frac") + ylab("Loss") 
multiplot(p25,p26,cols=1)

p27 <- ggplot() +
  geom_line(data=psef.actual, aes(x=Passenger_Side_Exterior_Frac, y=mloss, color='red')) +
  geom_line(data=psef.pred, aes(x=Passenger_Side_Exterior_Frac, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Passenger Side Exterior Frac") + 
  xlab("Passenger Side Exterior Frac") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p28 <- ggplot(test,aes(Passenger_Side_Exterior_Frac)) + geom_histogram(col='blue',fill='red') + 
  xlab("Passenger Side Exterior Frac") + ylab("Loss") 
multiplot(p27,p28,cols=1)



p29 <- ggplot() +
  geom_line(data=dsef.actual, aes(x=Driver_Side_Exterior_Frac, y=mloss, color='red')) +
  geom_line(data=dsef.pred, aes(x=Driver_Side_Exterior_Frac, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Driver Side Exterior Frac") + 
  xlab("Driver Side Exterior Frac") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p30 <- ggplot(test,aes(Driver_Side_Exterior_Frac)) + geom_histogram(col='blue',fill='red') + 
  xlab("Driver Side Exterior Frac") + ylab("Loss") 
multiplot(p29,p30,cols=1)


p31 <- ggplot() +
  geom_point(data=lc.actual, aes(x=luxury.car, y=mloss, color='red')) +
  geom_point(data=lc.pred, aes(x=luxury.car, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Luxury Car") + 
  xlab("luxury car?") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p32 <- ggplot(test,aes(luxury.car)) + geom_histogram(col='blue',fill='red') + 
  xlab("Luxury Car") + ylab("Loss") 
multiplot(p31,p32,cols=1)

p33 <- ggplot() +
  geom_line(data=bp.actual, aes(x=Base_Price, y=mloss, color='red')) +
  geom_line(data=bp.pred, aes(x=Base_Price, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Base Price") + 
  xlab("Base Price") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p34 <- ggplot(test,aes(Base_Price)) + geom_histogram(col='blue',fill='red') + 
  xlab("Base Price") + ylab("Loss") 
multiplot(p33,p34,cols=1)


p39 <- ggplot() +
  geom_line(data=cw.actual, aes(x=Curb_Weight, y=mloss, color='red')) +
  geom_line(data=cw.pred, aes(x=Curb_Weight, y=mloss, color='blue')) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Predicted Loss",
                                "Actual Loss")) +
  ggtitle("Univariate Plot for Curb Weight") + 
  xlab("Curb Weight") +
  ylab("Mean Loss") + theme(legend.position='bottom')

p40 <- ggplot(test,aes(Curb_Weight)) + geom_histogram(col='blue',fill='red') + 
  ggtitle("Curb Weight") + 
  xlab("Curb Weight") + ylab("Loss") 
multiplot(p39,p40,cols=1)


train$res <- residuals(fit.glm)

ff <- fitted(fit.glm)
res <- residuals(fit.glm) # residuals
anov <- anova(fit.glm) # anova table 
vcovar <- vcov(fit.glm) # covariance matrix for model parameters 
inf <- influence(fit.glm) 
library(car)
ci <- confint(fit.glm, level=0.95)


test$resid.sq <- test$resid^2
train$resid.sq <- train$resid^2
sqrt(sum(test$resid.sq))/nrow(test)
sqrt(sum(train$resid.sq))/nrow(train)
