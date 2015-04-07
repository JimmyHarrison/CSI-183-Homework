# CSI-183-Homework
https://github.com/JimmyHarrison/CSI-183-Homework.git

data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

#categorize
head(data1)
data1$agecat <- cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

#view
summary(data1)

#brackets
#install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x),mean(x),max(x))}
summaryBy(Age~agecat, data=data1, FUN=siterange)

#so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data=data1)

#plot
#install.packages("ggplot2")
library("ggplot2")
ggplot(data1, aes(x=Impressions, fill=agecat))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat))+geom_boxplot()

#create clickthru rate
#we don't care about clicks if there are no impressions
#if there are clicks with no imps my assumptions about this data are wrong
data1$hasimps <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps,data=data1,FUN=siterange)
ggplot(subset(data1,Impressions>0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1,Clicks>0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1,Clicks>0),aes(x=agecat,y=Clicks,fill=agecat))+geom_boxplot()
ggplot(subset(data1,Clicks>0),aes(x=Clicks,colour=agecat))+geom_density()

#create categories
data1$scode[data1$Impressions==0] <- "NoIMps"
data1$scode[data1$Impressions>0] <- "Imps"
data1$scode[data1$Clicks>0] <- "Clicks"

#Convert the column to a factor
data1$scode <- factor(data1$scode)
head(data1)

#look at levels
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat,data=data1,FUN=clen)

#-----------------------------------------------------------------------
#Homework!
totalSummary <- data.frame()
totalClickSummary <- data.frame()

for(i in 1:31){
  file="http://stat.columbia.edu/~rachel/datasets/nyt"
  data1 <- read.csv(url(paste0(file,i,".csv")))
  data1$agecat <- cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
  data1$gendercat <- cut(data1$Gender,c(-Inf,0,1))
  data1$day <- i
  data1$CTR <- data1$Clicks/data1$Impressions
  data1$Clicks <- data1$Clicks
  totalSummary<-rbind(totalSummary,summaryBy(CTR~agecat + Gender + day, FUN=mean, data=data1, na.rm=TRUE))
  #totalSummary<-rbind(totalSummary,summaryBy(CTR~gendercat + Clicks + day, FUN=mean, data=data1, na.rm=TRUE))
  totalClickSummary<-rbind(totalClickSummary,summaryBy(Clicks~agecat + Clicks + day, FUN=mean, data=data1, na.rm=TRUE))
  #totalClickSummary<-rbind(totalClickSummary,summaryBy(Clicks~gendercat + Clicks + day, FUN=mean, data=data1, na.rm=TRUE))
  print(i)
}

totalSummary$day = rep(1:31, each=15)
View(totalSummary)
View(totalClickSummary)

totalSummary$Gender <- factor(totalSummary$Gender)

#ggplot(totalSummary,aes(x=CTR.mean,colour=gendercat))+geom_density()
ggplot(totalSummary,aes(x=CTR.mean,fill=agecat))+geom_density()
#ggplot(totalClickSummary,aes(x=Clicks.mean, colour=gendercat))+geom_density()
ggplot(totalClickSummary,aes(x=Clicks.mean, colour=agecat))+geom_density()
