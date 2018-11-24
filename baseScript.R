library(XML)
library(ggplot2)
library(stats)

LargestRemainders <- function(n,v) {
  floats <- n*v
  wholes <- floor(floats)
  remainders <- floats - wholes
  nwhole <- sum(wholes)
  nremaining <- n-nwhole
  remseat <- rank(remainders,ties.method="random")>length(v)-nremaining
  wholes + remseat  
}

pollParser <- function(pollNode,institute) {
  poll <- list()
  poll$Institute <- institute
  poll$Date <- substring(xmlValue(pollNode[[1]]),1,10)
  poll$PubDate <- substring(xmlValue(pollNode[[2]]),1,10)
  poll$PubTime <- substring(xmlValue(pollNode[[2]]),12,19)
  poll$Desc <- xmlValue(pollNode[[3]])
  poll$DoubtRate <- as.numeric(xmlValue(pollNode[[5]]))/100

  if (substring(poll$Date,1,4)=="2011")  {
    poll$Respondents <- 1300
    poll$Result <- xmlSApply(pollNode[[6]],function(n) as.numeric(xmlValue(n[[2]]))/100)
    names(poll$Result) <- xmlSApply(pollNode[[6]],function(n) xmlValue(n[[1]][[4]]))
  } else {
    poll$Respondents <- as.numeric(xmlValue(pollNode[[6]]))
    poll$Result <- xmlSApply(pollNode[[7]],function(n) as.numeric(xmlValue(n[[2]]))/100)
    names(poll$Result) <- xmlSApply(pollNode[[7]],function(n) xmlValue(n[[1]][[4]]))
  }
  poll$Count <- round(poll$Respondents*poll$Result)
  poll
}

pollParser11 <- function(pollNode,institute) {
  poll <- list()
  poll$Institute <- institute
  poll$Date <- substring(xmlValue(pollNode[[1]]),1,10)
  poll$PubDate <- substring(xmlValue(pollNode[[2]]),1,10)
  poll$PubTime <- substring(xmlValue(pollNode[[2]]),12,19)
  poll$Desc <- xmlValue(pollNode[[3]])
  poll$DoubtRate <- as.numeric(xmlValue(pollNode[[5]]))/100
  poll$Respondents <- 1300
  poll$Result <- xmlSApply(pollNode[[6]],function(n) as.numeric(xmlValue(n[[2]]))/100)
  names(poll$Result) <- xmlSApply(pollNode[[6]],function(n) xmlValue(n[[1]][[4]]))
  poll$Count <- round(poll$Respondents*poll$Result)
  poll
}

resultFileParser <- function(doc) {
  tryCatch({
  root <- xmlRoot(doc)
  inst <- xmlValue(root[[1]][[2]])
  xmlApply(root[[2]],pollParser,institute=inst)
  }, error = function(x){NULL})
}

pollByDate <- function(date, pollSet) {
  pollSet[sapply(pollSet,function(p) p$Date == date)]
}

GetCount <- function(poll,letter) {
  c <- poll$Count[letter]
  if (is.na(c)) c <- 0
  c
}

GetResult <- function(poll,letter) {
  c <- poll$Result[letter]
  if (is.na(c)) c <- 0
  c
}

CountMatrix <- function(pollSet) {
  partyLetters <- c('A','B','C','F','I','K','O','V','Ø','Å')
  counts <- sapply(partyLetters,function(l) sapply(pollSet,function(p) GetCount(p,l)))
  dimnames(counts)[[1]] <- sapply(pollSet, function(p) p$Institute)
  counts
}

ResultMatrix <- function(pollSet) {
  partyLetters <- c('A','B','C','F','I','K','O','V','Ø','Å')
  counts <- sapply(partyLetters,function(l) sapply(pollSet,function(p) GetResult(p,l)))
  if (is.matrix(counts)) dimnames(counts)[[1]] <- sapply(pollSet, function(p) p$Institute)
  counts
}


# fileNames <- rep("http://www.berlingske.dk/upload/webred/bmsandbox/opinion_poll",7*11)
# fileNames <- paste(fileNames,rep(2010:2016,each=11),sep="/")
# fileNames <- paste(fileNames,rep(c(1:11),times=7),sep="/")
# fileNames <- paste(fileNames,"xml",sep=".")
# 
# 
# pollSet <- unlist(lapply(fileNames, function(x) resultFileParser(xmlInternalTreeParse(x))), recursive = FALSE)


#save(pollSet,file="C:\\Users\\nybruger\\Google Drive\\Opinion Polls\\pollSet.RData")

load(file="C:\\Users\\nybruger\\Google Drive\\Opinion Polls\\pollSet.RData")

#Get election dates
sapply(pollSet[sapply(pollSet,function(x) x$Institute=="Valgresultater")],function(y) y$Date)

#Institutes to use
sapply(pollSet[sapply(pollSet,function(x) x$Date=="2011-09-15")],function(y) y$Institute)
sapply(pollSet[sapply(pollSet,function(x) x$Date=="2011-09-14")],function(y) y$Institute)[1:5]

sapply(pollSet[sapply(pollSet,function(x) x$Date=="2015-06-18")],function(y) y$Institute)[3]
sapply(pollSet[sapply(pollSet,function(x) x$Date=="2015-06-17")],function(y) y$Institute)[1:4]

elec2 <- ResultMatrix(pollByDate("2011-09-15",pollSet))[-10]
poll2 <- ResultMatrix(pollByDate("2011-09-14",pollSet))[1:5,-10]
pollAvg2 <- apply(poll2,2,mean)


elec1 <- ResultMatrix(pollByDate("2015-06-18",pollSet))[3,]
poll1 <- ResultMatrix(pollByDate("2015-06-17",pollSet))[1:4,]
pollAvg1 <- apply(poll1,2,mean)

MultiLogit <- function(p) {
  log(p) - mean(log(p))
}

MultiLogitInv <- function(y) {
  exp(y)/sum(exp(y))
}

logitElec1 <- MultiLogit(elec1)
logitElec2 <- MultiLogit(elec2)
logitPoll1 <- t(apply(poll1,1,MultiLogit))
logitPoll2 <- t(apply(poll2,1,MultiLogit))
logitPollAvg1 <- MultiLogit(pollAvg1)
logitPollAvg2 <- MultiLogit(pollAvg2)

plot(logitPollAvg1 - logitElec1,rep(.3,10),ylim = c(0,1), xlim = c(-.5,.5))
points(logitPollAvg2-logitElec2,rep(.7,9))

round(logitPollAvg1 - logitElec1,2)
round(logitPollAvg2 - logitElec2,2)

sd(logitPollAvg1 - logitElec1)
sd(logitPollAvg2 - logitElec2)
mad(logitPollAvg1 - logitElec1)
mad(logitPollAvg2 - logitElec2)

allErrors <- c(logitPollAvg1 - logitElec1,logitPollAvg2 - logitElec2)

plot(pt(allErrors*10,2,lower.tail = FALSE),rep(.5,19),xlim=c(0,1))

plot(sort(allErrors),qt((1:19)/20,1),type="l")
lines(sort(allErrors),qt((1:19)/20,2),col=2)
lines(sort(allErrors),qt((1:19)/20,3),col=3)
lines(sort(allErrors),qt((1:19)/20,Inf),col=4)



qscale <- function(x,df) {
  s <- sort(x)
  v <- qt((1:length(x))/(length(x)+1),df,lower.tail = FALSE)
  s*sum(s*v)/sum(s*s)
}

plot(pt(qscale(allErrors,2),2,lower.tail = FALSE),(1:19)/20,type="l")
lines(pt(qscale(allErrors,1),1,lower.tail = FALSE),(1:19)/20,col=2)

dferror <- function(df) sum((pt(qscale(allErrors,df),df,lower.tail = FALSE)-(1:19)/20)^2)
plot(10:50/10,sapply(10:50/10,dferror))

plot(pt(qscale(allErrors,2),2,lower.tail = FALSE),rep(.5,19),xlim=c(0,1))
abline(v=0)
abline(v=1)


MCResults2 <- function(p,std, nrep){
  set.seed(123)
  p <- p/sum(p)
  phat <- p*(1+p*std^2)
  phat <- phat/sum(phat)
  z <- std*matrix(rnorm(length(p)*nrep),ncol = nrep)+log(phat)
  res <- apply(z,2,MultiLogitInv)
  rownames(res) <- names(p)
  res
}




LargestRemaindersThres <- function(v,n,thres) {
  vthres <- v*(v>=thres)
  LargestRemainders(n, vthres/sum(vthres))
}

x <- apply(MCResults2(pollAvg1, sd(logitPollAvg1 - logitElec1), 100000),2,LargestRemaindersThres, n=175, thres=.02)

mean(apply(x,2,function(s) sum(s[govLetters])>=89))


shapiro.test((pollAvg1-elec1)/sqrt(elec1*(1-elec1)))
shapiro.test(logitPollAvg1-logitElec1)

shapiro.test((pollAvg2-elec2)/sqrt(elec2*(1-elec2)))
shapiro.test(logitPollAvg2-logitElec2)

shapiro.test(c((pollAvg1-elec1)/sqrt(elec1*(1-elec1)),(pollAvg2-elec2)/sqrt(elec2*(1-elec2))))
shapiro.test(c(logitPollAvg1-logitElec1,logitPollAvg2-logitElec2))

plot(pnorm((pollAvg1-elec1)/sqrt(elec1*(1-elec1))/sd((pollAvg1-elec1)/sqrt(elec1*(1-elec1)))),rep(.3,10),xlim=c(0,1),ylim=c(0,1))
points(pnorm((logitPollAvg1-logitElec1)/sd(logitPollAvg1-logitElec1)),rep(.7,10),col = 2)
abline(v=0)
abline(v=1)

plot(pnorm((pollAvg2-elec2)/sqrt(elec2*(1-elec2))/sd((pollAvg2-elec2)/sqrt(elec2*(1-elec2)))),rep(.3,9),xlim=c(0,1),ylim=c(0,1))
points(pnorm((logitPollAvg2-logitElec2)/sd(logitPollAvg2-logitElec2)),rep(.7,9),col = 2)
abline(v=0)
abline(v=1)



allDates <- sapply(pollSet, function(x) x$Date)
uniqueDates <- unique(allDates)
plot(1:544,sapply(uniqueDates,function(x) sum(allDates==x))[order(uniqueDates)],type="l", xlim=c(400,450))
#80#430
uniqueDates[535:544]
order(uniqueDates[535:544])

MCResults <- function(counts) {

  nrep <- 25000
  set.seed(123)
  
  randgamma <- apply(counts,c(1,2),function(x) rgamma(nrep,x))
  dim(randgamma) <- c(4*nrep,10)
  dimnames(randgamma)[[2]] <- c('A','B','C','F','I','K','O','V','Ø','Å')
  
  randdir <- randgamma / apply(randgamma,1,sum)
  
  thresdist <- t(apply(randdir,1,function(v) v*(v>=.02) / sum(v[v>=.02])))
  
  seatdist <- t(apply(thresdist,1,function(v) LargestRemainders(175,v)))
  
  list(seats=seatdist, votes=randdir)
  
}

SimulateDate <- function(date) {
  
  counts <- CountMatrix(pollByDate(date,pollSet))
  print(rownames(counts))
  
  MCResults(counts)
    
}



load(file="C:\\Users\\kaspe_000\\Documents\\Opinion Polls\\FV15Polls2.RData")

# FV15Polls2[["2015-05-28"]] <- SimulateDate("2015-05-28")
# FV15Polls2[["2015-05-29"]] <- SimulateDate("2015-05-29")
# FV15Polls2[["2015-05-30"]] <- SimulateDate("2015-05-30")
# FV15Polls2[["2015-05-31"]] <- SimulateDate("2015-05-31")
# FV15Polls2[["2015-06-01"]] <- SimulateDate("2015-06-01")
# FV15Polls2[["2015-06-02"]] <- SimulateDate("2015-06-02")
# FV15Polls2[["2015-06-03"]] <- SimulateDate("2015-06-03")
# FV15Polls2[["2015-06-04"]] <- SimulateDate("2015-06-04")
# FV15Polls2[["2015-06-05"]] <- SimulateDate("2015-06-05")
# FV15Polls2[["2015-06-06"]] <- SimulateDate("2015-06-06")
# FV15Polls2[["2015-06-07"]] <- SimulateDate("2015-06-07")
# FV15Polls2[["2015-06-08"]] <- SimulateDate("2015-06-08")
# FV15Polls2[["2015-06-09"]] <- SimulateDate("2015-06-09")
# FV15Polls2[["2015-06-10"]] <- SimulateDate("2015-06-10")
# FV15Polls2[["2015-06-11"]] <- SimulateDate("2015-06-11")
# FV15Polls2[["2015-06-12"]] <- SimulateDate("2015-06-12")
# FV15Polls2[["2015-06-13"]] <- SimulateDate("2015-06-13")
# FV15Polls2[["2015-06-14"]] <- SimulateDate("2015-06-14")
# FV15Polls2[["2015-06-15"]] <- SimulateDate("2015-06-15")
# FV15Polls2[["2015-06-16"]] <- SimulateDate("2015-06-16")
FV15Polls2[["2015-06-17"]] <- SimulateDate("2015-06-17")

save(FV15Polls2,file="C:\\Users\\kaspe_000\\Documents\\Opinion Polls\\FV15Polls2.RData")

#-----------------------------
#  Analytics functions
#-----------------------------

govLetters <- c('A','B','F','Ø','Å')
oppLetters <- c('C','I','K','O','V')

SeatHistogramData <- function(MCRes, letters) {
  if (length(letters)>1) return(apply(MCRes$seats[,letters],1,sum))
  else return(MCRes$seats[,letters])
}

RoedRegering <- function(MCRes) {
  mean(SeatHistogramData(MCRes,govLetters)>=87)  
}

CrossesThreshold <- function(MCRes, letter) {
  mean(SeatHistogramData(MCRes,letter)>0)
}

TulleTallet <- function(MCRes) {
  oppWins <- SeatHistogramData(MCRes,oppLetters) >= 89
  OgtV    <- SeatHistogramData(MCRes,'O') > SeatHistogramData(MCRes,'V')
  mean(oppWins & OgtV)
}



SamiraTallet <- function(MCRes) {
  mean(SeatHistogramData(MCRes,'B')>=12)
}

AlternativetInd <- function(MCRes) {
  CrossesThreshold(MCRes,'Å')
}

BankUnionTilFolkeafstemning <- function(MCRes) {
  mean(SeatHistogramData(MCRes,c('I','O','Ø'))>=60)
}

NordatlantenAfgoer <- function(MCRes) {
  govWinsWithoutNA <- mean(SeatHistogramData(MCRes,govLetters) >= 90)
  oppWinsWithoutNA <- mean(SeatHistogramData(MCRes,oppLetters) >= 90)
  1-govWinsWithoutNA-oppWinsWithoutNA
}

IStoerreEndB <- function(MCRes) {
  mean(SeatHistogramData(MCRes,'B')<SeatHistogramData(MCRes,'I'))
}

AAStoerreEndB <- function(MCRes) {
  mean(SeatHistogramData(MCRes,'B')<SeatHistogramData(MCRes,'Å'))
}

DFKongemager <- function(MCRes) {
  mean(SeatHistogramData(MCRes,c('A','O','F'))>=87 & SeatHistogramData(MCRes,oppLetters)>=89)
}
DFEdderkop <- function(MCRes) {
  mean(SeatHistogramData(MCRes,c('A','O','F'))>=87 & SeatHistogramData(MCRes,oppLetters)>=89 & SeatHistogramData(MCRes,c('I','O','Ø'))>=60)
}

KonservativeUd <- function(MCRes) {
  mean(SeatHistogramData(MCRes,'C')==0)
}

GovSeats <- function(MCRes, n) {
  mean(SeatHistogramData(MCRes,govLetters)>=n)
}

ByInstitute <- function(MCRes, func) {
  nrep <- nrow(MCRes$seats)/4
  res  <- func(list(seats=MCRes$seats[1:nrep,]))
  res  <- c(res,func(list(seats=MCRes$seats[(1+nrep):(2*nrep),])))
  res  <- c(res,func(list(seats=MCRes$seats[(1+2*nrep):(3*nrep),])))
  c(res,func(list(seats=MCRes$seats[(1+3*nrep):(4*nrep),])))
}

# picData <- rbind(
#   sapply(FV15Polls,RoedRegering),
#   sapply(FV15Polls,TulleTallet),
#   sapply(FV15Polls,SamiraTallet),
#   sapply(FV15Polls,AlternativetInd),
#   sapply(FV15Polls,NordatlantenAfgoer),
#   sapply(FV15Polls,BankUnionTilFolkeafstemning),
#   sapply(FV15Polls,IStoerreEndB)
# )
# rownames(picData) <- c("Rød sejr", "Tulle-tallet", "Samira-tallet", "Alternativet", "Nordatlanten", "Bankunion til folkeafstemning" ,"I>B?")

# picData <- rbind(
#   sapply(FV15Polls2,RoedRegering),
#   sapply(FV15Polls2,DFKongemager),
#   sapply(FV15Polls2,NordatlantenAfgoer),
#   sapply(FV15Polls2,BankUnionTilFolkeafstemning),
#   sapply(FV15Polls2,IStoerreEndB),
#   sapply(FV15Polls2,SamiraTallet)
# )
# rownames(picData) <- c("Rød sejr", "DF som kongemager", "Nordatlanten bestemmer", "Bankunion til folkeafstemning" ,"I større end B","Samira-tallet")

picData <- rbind(
  sapply(FV15Polls2,RoedRegering),
  sapply(FV15Polls2,DFKongemager),
  sapply(FV15Polls2,NordatlantenAfgoer),
  sapply(FV15Polls2,BankUnionTilFolkeafstemning),
  sapply(FV15Polls2,AAStoerreEndB),
  sapply(FV15Polls2,TulleTallet)
)
rownames(picData) <- c("Rød sejr", "DF som kongemager", "Nordatlanten bestemmer", "Bankunion til folkeafstemning" ,"Å større end B","Tulle-tallet")

picData

picData <- cbind(picData,c(0,1,0,1,1,1))

colnames(picData)[22] <- "2015-06-18"

save(picData,file="C:\\Users\\kaspe_000\\Documents\\Opinion Polls\\picData.RData")

farver <- c("red","blue","purple","magenta","green","pink","brown", "cyan")
par(mar=c(5,5,3,4), xpd=FALSE)
plot(NA, xlab="dag i valgkamen", 
     ylab="sandsynlighed i procent", 
     main="Udviklingen i valgets hovedtal til dags dato",  type="n", 
     xlim=c(-28,0), ylim=c(0,100))
lapply(1:6,function(i) {
  lines((1:(length(picData[i,])))-22,100*picData[i,],lwd=2,col=farver[i])
})
abline(h=0,lty=1)
abline(h=50,lty=2)
abline(h=100,lty=1)
abline(v=0, lty=4,lwd=2)
legend(-28,99, rownames(picData), col=farver, lwd=2, bty = "n", cex=.8)

picData3 <- rbind(
  sapply(FV15Polls2,RoedRegering),
  sapply(FV15Polls2,DFKongemager),
  sapply(FV15Polls2,NordatlantenAfgoer),
  sapply(FV15Polls2,BankUnionTilFolkeafstemning),
  sapply(FV15Polls2,AAStoerreEndB),
  sapply(FV15Polls2,KonservativeUd),
  sapply(FV15Polls2,TulleTallet),
  sapply(FV15Polls2,SamiraTallet),
  sapply(FV15Polls2,IStoerreEndB),
  sapply(FV15Polls2,AlternativetInd)
)
rownames(picData3) <- c("Rød sejr"
                        , "DF som kongemager"
                        , "Nordatlanten bestemmer"
                        , "Bankunion til folkeafstemning" 
                        ,"Å større end B"
                        ,"Konservative ryger ud"
                        , "Tulle-tallet"
                        , "Samira-tallet"
                        , "I større end B"
                        , "Alternativet kommer ind")

picData3


farver <- c("red","blue","purple","magenta","green","pink","brown", "cyan","orange","gray")
par(mar=c(5,5,3,4), xpd=FALSE)
plot(NA, xlab="dag i valgkamen", 
     ylab="sandsynlighed i procent", 
     main="Udviklingen i valgets hovedtal til dags dato",  type="n", 
     xlim=c(-28,0), ylim=c(0,100))
abline(h=0,lty=1)
abline(h=50,lty=2)
abline(h=100,lty=1)
lapply(1:nrow(picData3),function(i) {
  lines((1:(length(picData3[i,])))-22,100*picData3[i,],lwd=2,col=farver[i])
})
abline(v=0, lty=4,lwd=2)
legend(-28,102, rownames(picData3), col=farver, lwd=2, bty = "n", cex=.8)









pic2Data <- rbind(
  sapply(FV15Polls2,function(p) GovSeats(p,84)),
  sapply(FV15Polls2,function(p) GovSeats(p,85)),
  sapply(FV15Polls2,function(p) GovSeats(p,86)),
  sapply(FV15Polls2,function(p) GovSeats(p,87)),
  sapply(FV15Polls2,function(p) GovSeats(p,88)),
  sapply(FV15Polls2,function(p) GovSeats(p,89)),
  sapply(FV15Polls2,function(p) GovSeats(p,90))
)
rownames(pic2Data) <- 84:90

pic2Data

save(pic2Data,file="C:\\Users\\kaspe_000\\Documents\\Opinion Polls\\pic2Data.RData")

par(mar=c(5,5,3,4), xpd=FALSE)
plot(NA, xlab="dag i valgkamen", 
     ylab="sandsynlighed i procent", 
     main="Sandsynligheden for at rød blok får x mandater",  type="n", 
     xlim=c(-23,0), ylim=c(0,100))
lapply(1:7,function(i) {
  lines((1:(length(pic2Data[i,])-1))-21,100*pic2Data[i,-1],lwd=2,col=farver[i])
})
abline(h=0,lty=1)
abline(h=50,lty=2)
abline(h=100,lty=1)
abline(v=0, lty=4,lwd=2)
legend(-23,99, rownames(pic2Data), col=farver, lwd=2, bty = "n", cex=.8)

pic3Data <- sapply(FV15Polls2, function(p) ByInstitute(p,RoedRegering))
rownames(pic3Data) <- sapply(pollByDate("2015-06-14",pollSet), function(p) p$Institute)
par(mar=c(5,5,3,4), xpd=FALSE)
plot(NA, xlab="dag i valgkamen", 
     ylab="sandsynlighed i procent", 
     main="Sandsynligheden for at rød blok får 87 mandater",  type="n", 
     xlim=c(-25,0), ylim=c(0,100))
lapply(1:4,function(i) {
  lines((1:(length(pic3Data[i,])))-22,100*pic3Data[i,],lwd=2,col=farver[i])
})
abline(h=0,lty=1)
abline(h=50,lty=2)
abline(h=100,lty=1)
abline(v=0, lty=4,lwd=2)
legend(-25,99, rownames(pic3Data), col=farver, lwd=2, bty = "n", cex=.8)
