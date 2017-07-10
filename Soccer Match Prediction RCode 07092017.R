#Soccer Games Prediction Project
#Start Date: 06/22/2017
#Created  by Ray Anthony Roderos
#
#
#f
##-----------------------------------------------------------------------------##
#Set-Up

ptm <- proc.time() #start time
set.seed(1) #set seed for reproduction

#Getting working drive and setting the working drive

getwd()
setwd('C:\\Users\\Knight Roderos\\Google Drive\\Personal Projects\\01 Soccer Match Prediction Project')
#F:\\01 Soccer Match Prediction Project

#Create function "packages" that installs the package or require if already installed
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x)
    require(x,character.only=TRUE)
  }
}

packages(plyr)
packages(dplyr)
packages(h2o)


#Create function that gets only relevant stats

stats.only <- function(x,y){
  x <- x[,c("Div",
            "Date",
            "HomeTeam",
            "AwayTeam",
            "FTHG",
            "FTAG",
            "FTR",
            "HTHG",
            "HTAG",
            "HTR",
            "HS",
            "AS",
            "HST",
            "AST",
            "HF",
            "AF",
            "HC",
            "AC",
            "HY",
            "AY",
            "HR",
            "AR")]

}

#Create function that transforms the data

transform.data <- function(x){
  x$Key <- seq(1,nrow(x),by=1)
  x$Date <- as.Date(x$Date,format = "%d/%m/%y")
  x$FTRV <- as.numeric(x$FTR)
  
  x.date <- x[,c("Key","Date")]
  x.home <- x[,c("Div","HomeTeam","FTHG","HTHG","HS","HST","HF","HC","HY","HR","Key","FTR","FTRV")]
  
  x.home <- within(x.home,{ Game <- ave(as.character(HomeTeam), HomeTeam, FUN = seq_along)})
  x.home$Game <- as.numeric(x.home$Game) - 1
  
  
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.FTHG = cumsum(FTHG)-FTHG)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.FTHG = round(Cum.FTHG/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HTHG = cumsum(HTHG)-HTHG)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HTHG = round(Cum.HTHG/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HS = cumsum(HS)-HS)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HS = round(Cum.HS/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HST = cumsum(HST)-HST)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HST = round(Cum.HST/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HF = cumsum(HF)-HF)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HF = round(Cum.HF/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HC = cumsum(HC)-HC)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HC = round(Cum.HC/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HY = cumsum(HY)-HY)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HY = round(Cum.HY/Game,2))
  x.home <- ddply(x.home,'HomeTeam',transform,Cum.HR = cumsum(HR)-HR)
  x.home <- ddply(x.home,'HomeTeam',transform,Mean.HR = round(Cum.HR/Game,2))
  
  x.home[is.na(x.home)] <- 0
  
  x.away <- x[,c("AwayTeam","FTAG","HTAG","AS","AST","AF","AC","AY","AR","Key")]
  
  x.away <- within(x.away,{ Game <- ave(as.character(AwayTeam), AwayTeam, FUN = seq_along)})
  x.away$Game <- as.numeric(x.away$Game) - 1
  
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.FTAG = cumsum(FTAG)-FTAG)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.FTAG = round(Cum.FTAG/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.HTAG = cumsum(HTAG)-HTAG)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.HTAG = round(Cum.HTAG/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.AS = cumsum(AS)-AS)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.AS = round(Cum.AS/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.AST = cumsum(AST)-AST)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.AST = round(Cum.AST/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.AF = cumsum(AF)-AF)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.AF = round(Cum.AF/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.AC = cumsum(AC)-AC)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.AC = round(Cum.AC/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.AY = cumsum(AY)-AY)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.AY = round(Cum.AY/Game,2))
  x.away <- ddply(x.away,'AwayTeam',transform,Cum.AR = cumsum(AR)-AR)
  x.away <- ddply(x.away,'AwayTeam',transform,Mean.AR = round(Cum.AR/Game,2))
  
  x.away[is.na(x.away)] <- 0
  
  x <- merge(x.home,x.away, by ="Key", ALL=TRUE)
  x <- merge(x,x.date,by="Key",ALL=TRUE)
  
}

#Create function that gets only relevant data ready and scaled for ML - with result as FTR

data.FTR <- function(x){
  x <- x[,c("FTR",
            "Cum.FTHG",
            "Mean.FTHG",
            "Cum.HTHG",
            "Mean.HTHG",
            "Cum.HS",
            "Mean.HS",
            "Cum.HST",
            "Mean.HST",
            "Cum.HF",
            "Mean.HF",
            "Cum.HC",
            "Mean.HC",
            "Cum.HY",
            "Mean.HY",
            "Cum.HR",
            "Mean.HR",
            "Cum.FTAG",
            "Mean.FTAG",
            "Cum.HTAG",
            "Mean.HTAG",
            "Cum.AS",
            "Mean.AS",
            "Cum.AST",
            "Mean.AST",
            "Cum.AF",
            "Mean.AF",
            "Cum.AC",
            "Mean.AC",
            "Cum.AY",
            "Mean.AY",
            "Cum.AR",
            "Mean.AR",
            "h2h.Mean.FTHG",
            "h2h.Mean.HTHG",
            "h2h.Mean.HS",
            "h2h.Mean.HST",
            "h2h.Mean.HF",
            "h2h.Mean.HC",
            "h2h.Mean.HY",
            "h2h.Mean.HR",
            "h2h.Mean.FTAG",
            "h2h.Mean.HTAG",
            "h2h.Mean.AS",
            "h2h.Mean.AST",
            "h2h.Mean.AF",
            "h2hMean.AC",
            "h2h.Mean.AY",
            "h2h.Mean.AR")]  
}



#Get CSV Data


  english.1617.pl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/E0.csv')))
  english.1617.cl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/E1.csv')))
  english.1617.l1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/E2.csv')))
  english.1617.l2 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/E3.csv')))
  
  english.1516.pl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/E0.csv')))
  english.1516.cl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/E1.csv')))
  english.1516.l1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/E2.csv')))
  english.1516.l2 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/E3.csv')))
  
  english.1415.pl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/E0.csv')))
  english.1415.cl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/E1.csv')))
  english.1415.l1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/E2.csv')))
  english.1415.l2 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/E3.csv')))
  
  english.1314.pl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/E0.csv')))
  english.1314.cl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/E1.csv')))
  english.1314.l1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/E2.csv')))
  english.1314.l2 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/E3.csv')))
  
  english.1213.pl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/E0.csv')))
  english.1213.cl <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/E1.csv')))
  english.1213.l1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/E2.csv')))
  english.1213.l2 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/E3.csv')))
  
  germany.1617.d1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/D1.csv')))
  germany.1516.d1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/D1.csv')))
  germany.1415.d1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/D1.csv')))
  germany.1314.d1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/D1.csv')))
  germany.1213.d1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/D1.csv')))
  
  italy.1617.i1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/I1.csv')))
  italy.1516.i1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/I1.csv')))
  italy.1415.i1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/I1.csv')))
  italy.1314.i1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/I1.csv')))
  italy.1213.i1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/I1.csv')))
  
  spain.1617.sp1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/SP1.csv')))
  spain.1516.sp1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/SP1.csv')))
  spain.1415.sp1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/SP1.csv')))
  spain.1314.sp1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/SP1.csv')))
  spain.1213.sp1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/SP1.csv')))
  
  france.1617.f1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/F1.csv')))
  france.1516.f1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/F1.csv')))
  france.1415.f1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/F1.csv')))
  france.1314.f1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/F1.csv')))
  france.1213.f1 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/F1.csv')))
  
  scotland.1617.sc0 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1617/SC0.csv')))
  scotland.1516.sc0 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1516/SC0.csv')))
  scotland.1415.sc0 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1415/SC0.csv')))
  scotland.1314.sc0 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1314/SC0.csv')))
  scotland.1213.sc0 <- transform.data(stats.only(read.csv('http://www.football-data.co.uk/mmz4281/1213/SC0.csv')))  

#create function that combines all leagues into one table
bind.all <- function(){
    rbind(english.1617.pl,
        english.1617.cl,
        english.1617.l1,
        english.1617.l2,
        english.1516.pl,
        english.1516.cl,
        english.1516.l1,
        english.1516.l2,
        english.1415.pl,
        english.1415.cl,
        english.1415.l1,
        english.1415.l2,
        english.1314.pl,
        english.1314.cl,
        english.1314.l1,
        english.1314.l2,
        english.1213.pl,
        english.1213.cl,
        english.1213.l1,
        english.1213.l2,
        germany.1617.d1,
        germany.1516.d1,
        germany.1415.d1,
        germany.1314.d1,
        germany.1213.d1,
        italy.1617.i1,
        italy.1516.i1,
        italy.1415.i1,
        italy.1314.i1,
        italy.1213.i1,
        spain.1617.sp1,
        spain.1516.sp1,
        spain.1415.sp1,
        spain.1314.sp1,
        spain.1213.sp1,
        france.1617.f1,
        france.1516.f1,
        france.1415.f1,
        france.1314.f1,
        france.1213.f1,
        scotland.1617.sc0,
        scotland.1516.sc0,
        scotland.1415.sc0,
        scotland.1314.sc0,
        scotland.1213.sc0)
}  

full.data2 <- bind.all()

#Create h2h mean performance columns

h2h <- function(a){
  a$Match <- paste(a$HomeTeam,a$AwayTeam)
  
  a <- a[order(a$Date),]
  
  a <- within(a,{ Game <- ave(as.character(Match), Match, FUN = seq_along)})
  a$Game <- as.numeric(a$Game) - 1
  
  a <- ddply(a,'Match',transform,h2h.Cum.FTHG = cumsum(FTHG)-FTHG)
  a <- ddply(a,'Match',transform,h2h.Mean.FTHG = round(Cum.FTHG/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HTHG = cumsum(HTHG)-HTHG)
  a <- ddply(a,'Match',transform,h2h.Mean.HTHG = round(Cum.HTHG/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HS = cumsum(HS)-HS)
  a <- ddply(a,'Match',transform,h2h.Mean.HS = round(Cum.HS/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HST = cumsum(HST)-HST)
  a <- ddply(a,'Match',transform,h2h.Mean.HST = round(Cum.HST/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HF = cumsum(HF)-HF)
  a <- ddply(a,'Match',transform,h2h.Mean.HF = round(Cum.HF/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HC = cumsum(HC)-HC)
  a <- ddply(a,'Match',transform,h2h.Mean.HC = round(Cum.HC/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HY = cumsum(HY)-HY)
  a <- ddply(a,'Match',transform,h2h.Mean.HY = round(Cum.HY/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HR = cumsum(HR)-HR)
  a <- ddply(a,'Match',transform,h2h.Mean.HR = round(Cum.HR/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.FTAG = cumsum(FTAG)-FTAG)
  a <- ddply(a,'Match',transform,h2h.Mean.FTAG = round(Cum.FTAG/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.HTAG = cumsum(HTAG)-HTAG)
  a <- ddply(a,'Match',transform,h2h.Mean.HTAG = round(Cum.HTAG/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.AS = cumsum(AS)-AS)
  a <- ddply(a,'Match',transform,h2h.Mean.AS = round(Cum.AS/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.AST = cumsum(AST)-AST)
  a <- ddply(a,'Match',transform,h2h.Mean.AST = round(Cum.AST/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.AF = cumsum(AF)-AF)
  a <- ddply(a,'Match',transform,h2h.Mean.AF = round(Cum.AF/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.AC = cumsum(AC)-AC)
  a <- ddply(a,'Match',transform,h2h.Mean.AC = round(Cum.AC/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.AY = cumsum(AY)-AY)
  a <- ddply(a,'Match',transform,h2h.Mean.AY = round(Cum.AY/Game,2))
  a <- ddply(a,'Match',transform,h2h.Cum.AR = cumsum(AR)-AR)
  a <- ddply(a,'Match',transform,h2h.Mean.AR = round(Cum.AR/Game,2))

  a[mapply(is.infinite, a)] <- 0
  a[mapply(is.nan, a)] <- 0
  empty.rows  <- which(is.na(a$Date))
  a <- a[-empty.rows,]
      
}

full.data <- h2h(full.data2)


#----------------------------------------------------#

#Prepare data for machine learning

x1 <- data.FTR(full.data)

#Make a training and test set

sample.size <- floor(0.8 * nrow(x1))

train <- sample(seq_len(nrow(x1)), size = sample.size)

x1.train <- x1[train, ]
x1.test <- x1[-train, ]


##-----------------------------------------------------------------------------##

#h20 model
#Deep Learning 1

number_of_col <- ncol(x1.train)

h2o.init()
Train.h2o<-as.h2o(x1.train)
Test.h2o<-as.h2o(x1.test)

local.h2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads=-1)

fit.h2o.x1 <- h2o.deeplearning(x = 2:number_of_col, 
                               y = 1,Train.h2o, 
                               activation = "Maxout", 
                               hidden=rep(30,20,10),
                               standardize = TRUE,
                               nfolds=5,
                               epochs=20)
predict.h2o.x1<-h2o.predict(object=fit.h2o.x1, newdata=Test.h2o[,-1])
test <- as.data.frame(predict.h2o.x1)
test

table(test$predict,x1.test$FTR)

test1 <- chartr("123", "ADH", test$predict)
x1.test1 <- chartr("123", "ADH", x1.test$FTR)

#Check the accuracy rate
table(test1==x1.test1)
result1 <- table(test1==x1.test1)[[2]]/(table(test1==x1.test1)[[1]]+table(test1==x1.test1)[[2]])
result1

##-----------------------------------------------------------------------------##
#Deep Learning 2

fit.h2o.x3 <- h2o.deeplearning(x = 2:number_of_col, 
                               y = 1,Train.h2o, 
                               activation = "Maxout", 
                               hidden=rep(30,20),
                               standardize = TRUE,
                               nfolds=5,
                               epochs=20)

predict.h2o.x3<-h2o.predict(object=fit.h2o.x3, newdata=Test.h2o[,-1])
test3 <- as.data.frame(predict.h2o.x3)
test3

table(test3$predict,x1.test$FTR)


test3.1 <- chartr("123", "ADH", test3$predict)
x1.test1 <- chartr("123", "ADH", x1.test$FTR)

#Check the accuracy rate
table(test3.1==x1.test1)
result3 <- table(test3.1==x1.test1)[[2]]/(table(test3.1==x1.test1)[[1]]+table(test3.1==x1.test1)[[2]])
result3



##-----------------------------------------------------------------------------##
#Create filter

# pred.tab <- cbind( as.data.frame(test$predict),predict2 = test3, actual = x1.test$FTR)
# colnames(pred.tab)[1] <- "predict1"
# pred.tab$actual2 <- ifelse(pred.tab$actual=="","D",x1.test$FTR)
# pred.tab$actual2 <- chartr("123", "ADH", pred.tab$actual2)
# pred.tab$actual <-NULL
# 
# pred.tab$compute1 <- ifelse(pred.tab$predict1==pred.tab$predict2,1,0)
# pred.tab$compute2 <- ifelse(pred.tab$predict1==pred.tab$actual2,1,0)
# pred.tab$compute3 <- ifelse(pred.tab$predict2==pred.tab$actual2,1,0)
# 
# pred.tab$result <- pred.tab$compute1+pred.tab$compute2+pred.tab$compute3
# table(pred.tab$result)
# 
# View(pred.tab)

##-----------------------------------------------------------------------------##
#For production

prod.data <- full.data
prod.data$year <- format(as.Date(prod.data$Date, format="%y/%m/%d"),"%Y")


#Create function that uses any team thad had a history of a match and outputs the prediction
predict.outcome <- function(HomeTeam1,AwayTeam1){

  Match1 <- paste(HomeTeam1,AwayTeam1)
  z.home <- subset(prod.data,
                   prod.data$HomeTeam==HomeTeam1 &
                     which.max(prod.data$Key))
  z.home <- z.home[order(z.home$Date,decreasing=TRUE),]
  z.home <- z.home[1,1:30]
  
  
  z.away <- subset(prod.data,
                   prod.data$AwayTeam==AwayTeam1 &
                     which.max(prod.data$Key))
  z.away <- z.away[order(z.away$Date,decreasing=TRUE),]
  z.away <- z.away[1,c(1,31:56)]
  
  z.match <- subset(prod.data,
                    prod.data$Match==Match1 &
                      which.max(prod.data$Key))
  z.match <- z.match[order(z.match$Date,decreasing=TRUE),]
  z.result <- z.match$FTR[1]
  z.match <- z.match[1,c(1,57:92)]
  
  z.data <- cbind(z.home,z.away,z.match)
  
  z1 <- data.FTR(z.data)
  h2o.init()
  z1.h2o<-as.h2o(z1)
  
  
  predict.h2o.z1<-h2o.predict(object=fit.h2o.x1, newdata=z1.h2o[,-1])
  predict.h2o.z1.2<-h2o.predict(object=fit.h2o.x3, newdata=z1.h2o[,-1])
  test.z <- as.data.frame(predict.h2o.z1)
  test.z3 <- as.data.frame(predict.h2o.z1.2)
  
  
  pred.tab <- cbind(HomeTeam1,AwayTeam1, as.data.frame(test.z$predict),predict2 = as.data.frame(test.z3$predict), actual = z.result)
  colnames(pred.tab) <- c("Home","Away","predict1","predict2","actual")
  pred.tab
    
  
}

g1 <- predict.outcome("Aberdeen","Hamilton")
g1

g2 <- predict.outcome("Motherwell","Rangers")
g2






##-----------------------------------------------------------------------------##

time <- proc.time() - ptm #check the computation time
time
##-----------------------------------------------------------------------------##

