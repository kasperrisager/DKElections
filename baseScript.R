library(tidyverse)
library(stats)

source("electionTools.R")
source("cleanData.R")


# My own chi2 test with overdispersion
my.chisq.test <- function(x,p, alpha=0) {
  
  rv <- list()
  sumx <- sum(x)
  var <- p/sumx+alpha*p^2
  stdres <- (x/sumx-p)/sqrt(var)
  chi2 <- sum(stdres^2)
  df <- length(x) - 1
  rv$p.value <- pchisq(chi2,df=df, lower.tail = F)
  rv$chi2 <- chi2
  rv$stdres <- stdres
  rv
}

# Pull cleaned data and clear up

cpolls_2015 <- polls_campaign2015() %>%
  unnest(Results) %>%
  mutate(SupportN = Respondents * Support) %>%
  select(Id, Date, PartyLetter, Support, SupportN) 
  
cpolls_2011 <- polls_campaign2011() %>%
  unnest(Results) %>%
  mutate(SupportN = Respondents * Support) %>%
  select(Id, Date, PartyLetter, Support, SupportN) 

# Get the mean poll and the squashed poll
# + some additional statistics
avgpolls_2015 <- cpolls_2015 %>% 
  group_by(Date, Id) %>%
  mutate(MLogitSupport = mlogit(Support)) %>%
  ungroup() %>%
  group_by(Date, PartyLetter) %>%
  summarise(AvgSupport = mean(Support), 
            StdSupport = sd(Support),
            AvgMLogitSupport = mean(MLogitSupport),
            StdMLogitSupport = sd(MLogitSupport), 
            SupportN = sum(SupportN)
          ) %>%
  ungroup()

avgpolls_2011 <- cpolls_2011 %>% 
  group_by(Date, Id) %>%
  mutate(MLogitSupport = mlogit(Support)) %>%
  ungroup() %>%
  group_by(Date, PartyLetter) %>%
  summarise(AvgSupport = mean(Support), 
            StdSupport = sd(Support),
            AvgMLogitSupport = mean(MLogitSupport),
            StdMLogitSupport = sd(MLogitSupport), 
            SupportN = sum(SupportN)
  ) %>%
  ungroup()

# Check how the squashed polls compare to election results

results15 <- election_results() %>% filter(Year==2015) %>% 
  unnest(Results) %>%
  select(PartyLetter, Support)

results11 <- election_results() %>% filter(Year==2011) %>% 
  unnest(Results) %>%
  select(PartyLetter, Support)

# Squashed polls are _way_ off. Best p value ~10^(-6)
print(  avgpolls_2015 %>% inner_join(results15) %>%
    nest(-Date) %>%
    mutate(pvalue = map_dbl(data, function(tbl) chisq.test(x=tbl$SupportN,p=tbl$Support)$p.value)) %>%
    mutate(my.pvalue = map_dbl(data, function(tbl) my.chisq.test(x=tbl$SupportN,p=tbl$Support,1.3*(tbl$StdSupport/tbl$AvgSupport)^2+.004)$p.value)) %>%
    mutate(chi2 = map_dbl(data, function(tbl) my.chisq.test(x=tbl$SupportN,p=tbl$Support,1.3*(tbl$StdSupport/tbl$AvgSupport)^2+.004)$chi2)) 
  , n= 22) 
  
# Squashed polls are _way_ off. Best p value ~10^(-3)
print(  avgpolls_2011 %>% inner_join(results11) %>%
          nest(-Date) %>%
          mutate(pvalue = map_dbl(data, function(tbl) chisq.test(x=tbl$SupportN,p=tbl$Support)$p.value)) %>%
          mutate(my.pvalue = map_dbl(data, function(tbl) my.chisq.test(x=tbl$SupportN,p=tbl$Support,1.3*(tbl$StdSupport/tbl$AvgSupport)^2+.004)$p.value)) %>%
          mutate(chi2 = map_dbl(data, function(tbl) my.chisq.test(x=tbl$SupportN,p=tbl$Support,1.3*(tbl$StdSupport/tbl$AvgSupport)^2+.004)$chi2)) 
        , n= 22) 


# Individial polls not good either, but some of them get p > 0.05
print(
  cpolls_2015 %>% inner_join(results15, by="PartyLetter") %>%
    nest(-Date, -Id) %>%
    mutate(pvalue = map_dbl(data, function(tbl) (chisq.test(x=tbl$SupportN,p=tbl$Support.y)$p.value))) %>%
    #mutate(mypvalue = map_dbl(data, function(tbl) (my.chisq.test(x=tbl$SupportN,p=tbl$Support.y,(tbl$StdSupport/tbl$Support.y)^2)$p.value))) %>%
select(Date, Id, pvalue) %>%
    spread(Id, pvalue) %>%
    arrange(Date)
  , n= 220)




# How are the differences linked to size?

diffs <- avgpolls_2015 %>% 
  filter(Date == "2015-06-16") %>%
  inner_join(results, by="PartyLetter") %>%
  mutate(MLogitSupport = mlogit(Support)) %>%
  mutate(DiffMLogit = AvgMLogitSupport - MLogitSupport)

diffs %>% 
  mutate(RelDiffMLogit = (DiffMLogit/StdMLogitSupport-0.08784191)/ 1.373621) %>%
  select(PartyLetter, DiffMLogit, StdMLogitSupport, RelDiffMLogit)

addvar <- -0.00^2
scd <- diffs$DiffMLogit/sqrt(diffs$StdMLogitSupport^2+addvar)
#scd <- diffs$DiffMLogit
qqnorm(scd)
qqline(scd)
shapiro.test(scd)


qqplot(qt(ppoints(10),df=100),scd, xlim=c(-3,3))
points(qt(ppoints(10),df=10),sort(scd), col=2)
points(qt(ppoints(10),df=3),sort(scd), col=3)

diffs

mean(diffs$DiffMLogit/diffs$StdMLogitSupport)
sd(diffs$DiffMLogit/diffs$StdMLogitSupport)
pchisq(sum((diffs$DiffMLogit/diffs$StdMLogitSupport)^2 ),df=9)

x <- rnorm(10)
qqnorm(x-mean(x), ylim=c(-3,3))
qqline(x-mean(x))

poll_dates <- avgpolls_2015 %>% distinct(Date) %>%
  arrange(Date) %>%
  pull()


FV15Polls2 <- lapply(poll_dates, function(d) cpolls_2015 %>% filter(Date == d) %>% simulate(nrep = 2500))
names(FV15Polls2) <- poll_dates


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

colnames(picData)[23] <- "2015-06-18"

#save(picData,file="C:\\Users\\kaspe_000\\Documents\\Opinion Polls\\picData.RData")

farver <- c("red","blue","purple","magenta","green","pink","brown", "cyan")
par(mar=c(5,5,3,4), xpd=FALSE)
plot(NA, xlab="dag i valgkamen", 
     ylab="sandsynlighed i procent", 
     main="Udviklingen i valgets hovedtal til dags dato",  type="n", 
     xlim=c(-28,0), ylim=c(0,100))
lapply(1:6,function(i) {
  lines((1:(length(picData[i,])))-23,100*picData[i,],lwd=2,col=farver[i])
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



