#Get the working directory
rm(list = ls())
getwd()

#Read data as is 
startup <- read.csv(file="CAX_Startup_Data.csv", header=TRUE, as.is=T)
str(startup)
# Change "" and "No Info" to NA
startup[startup==""] <- NA
startup[startup=="No Info"] <- NA

# R code for converting column as date
startup$Est..Founding.Date <- as.Date(startup$Est..Founding.Date, "%m/%d/%Y")
startup$Last.Funding.Date <- as.Date(startup$Last.Funding.Date, "%m/%d/%Y")

# R code for converting character vector to numeric
# noting columns that needs to be converted to numeric
col<- c(3:5,10,11,18:23,25,61,66,68:70,72,74,88,92,94:96,98,99,102:116)
# using for loop for converting column as numeric
for(i in col)
{
  startup[,i]<-as.numeric(startup[,i])
}

str(startup)

####### Univariate analysis ########

table(startup$Cloud.or.platform.based.serive.product., exclude = NULL)
#Converting to upper case
startup$Cloud.or.platform.based.serive.product.<- toupper(startup$Cloud.or.platform.based.serive.product.)

table(startup$Has.the.team.size.grown, exclude = NULL)
#Converting to upper case
startup$Has.the.team.size.grown<- toupper(startup$Has.the.team.size.grown)

table(startup$Local.or.global.player, exclude = NULL)
#Converting to upper case
startup$Local.or.global.player<- toupper(startup$Local.or.global.player)
#trim the white space
startup$Local.or.global.player<- trimws(startup$Local.or.global.player)

table(startup$Number.of..Sales.Support.material, exclude = NULL)
#Converting to upper case
startup$Number.of..Sales.Support.material<- toupper(startup$Number.of..Sales.Support.material)

table(startup$Time.to.maturity.of.technology..in.years., exclude = NULL)
#converting "0 to 5" to "0 to 2"
startup[which(startup$Time.to.maturity.of.technology..in.years. == "0 to 5"),]$Time.to.maturity.of.technology..in.years.<-"0 to 2"


#Find the percentage missing values in each column
percent_misssing<- as.data.frame((colSums(is.na(startup))/nrow(startup))*100)
names<- row.names(percent_misssing)
pcnt_mis_var<-cbind(names,percent_misssing)
row.names(pcnt_mis_var)<-NULL
colnames(pcnt_mis_var)<-c("variable","Percent.Missing")
new_var<-as.character(pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing<=40)])
new_startup<-startup[new_var]
other_var<-
as.character(pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing>40)])
other_data<-startup[other_var]

#Divide the data into Character and Numeric

cnt_df<-new_startup[,c(3:5,10,12:14,17:22,24,60,65,67:69,71,73,85,89,91:93,
                       95,96,99:113)]
cnt_var<-colnames(cnt_df)
var <- colnames(new_startup) %in% cnt_var
char_df <- new_startup[!var]


#Remove dates from the numeric data frame
cnt_dates <- cnt_df[,c(5:6)]
cnt_wd <- cnt_df[,-c(5:6)]

#Total of percentage skill total should not be 0, convert to NA
str(cnt_wd[,27:40])
sumzero<- which(rowSums(cnt_wd[,27:40])==0)
for(i in 27:40)
{
  cnt_wd[,i][sumzero] <- NA
}

#Find pair of columns with correlation greater than .6

corv<- cor(na.omit(cnt_wd))
d<- which(abs(corv)>.6, arr.ind = TRUE)
row.names(d)<-NULL
write.csv(d, "Correlation.csv")

#Employees.per.year.of.company.existence, Team.size.all.employees, Time.to.1st.investment..in.months., 
#year.of.founding, Age.of.company.in.years, Employee.count, Year are related
cnt_wd$Employees.per.year.of.company.existence[is.na(cnt_wd$Employees.per.year.of.company.existence)] <- cnt_wd$Team.size.all.employees[is.na(cnt_wd$Employees.per.year.of.company.existence)]/cnt_wd$Age.of.company.in.years[is.na(cnt_wd$Employees.per.year.of.company.existence)]

quantile(cnt_wd$Employees.per.year.of.company.existence, probs = seq(0, 1, by= 0.05),na.rm=T)
quantile(cnt_wd$Employees.per.year.of.company.existence, probs = seq(0.9, 1, by= 0.001),na.rm=T)
quantile(cnt_wd$Employees.per.year.of.company.existence, probs = seq(0, .1, by= 0.001),na.rm=T)
#capping above 118.297
cnt_wd$Employees.per.year.of.company.existence[cnt_wd$Employees.per.year.of.company.existence>118.29767]<- 118.29767
#capping below .169
cnt_wd$Employees.per.year.of.company.existence[cnt_wd$Employees.per.year.of.company.existence<.169]<- .169

cnt_wd$Years<- cnt_wd$Team.size.all.employees/cnt_wd$Employees.per.year.of.company.existence
cnt_wd$Age.of.company.in.years[is.na(cnt_wd$Age.of.company.in.years)] <- cnt_wd$Years[is.na(cnt_wd$Age.of.company.in.years)]

#Creating addtional features
cnt_wd$year.of.founding <- 2014 - cnt_wd$Age.of.company.in.years

#Last round of Funding amounts are equal
cnt_wd$Last.Funding.Amount[(is.na(cnt_wd$Last.Funding.Amount))]<-
  cnt_wd$Last.round.of.funding.received..in.milionUSD.[(is.na(cnt_wd$Last.Funding.Amount))]*1000000
cnt_wd$Last.round.of.funding.received..in.milionUSD.<- cnt_wd$Last.Funding.Amount/1000000

#Experience in top 100 implies experience in top 500 and top 1000 
cnt_wd$Experience.in.Fortune.500.organizations[which(cnt_wd$Experience.in.Fortune.100.organizations==1)]<- 1
cnt_wd$Experience.in.Fortune.1000.organizations[which(cnt_wd$Experience.in.Fortune.500.organizations==1)]<- 1

#boxplot of all continous variables

for(i in 1:length(colnames(cnt_wd)))
{
  boxplot(cnt_wd[,i], ylab = names(cnt_wd)[i])
}

#Majority of the data is skewed, use histograms
for(i in 1:length(colnames(cnt_wd)))
{
  hist(cnt_wd[,i], xlab = names(cnt_wd)[i])
}

#Using quantile for caping 

quantile(cnt_wd$Years, probs = seq(0, 1, by= 0.05),na.rm=T)
#Jump from 95% to 100%
quantile(cnt_wd$Years, probs = seq(0.9, 1, by= 0.005),na.rm=T)
#Capping above 15.525
cnt_wd$Years[cnt_wd$Years > 15.525] <- 15.525

#Year of founding less than 1997, capped to 1997
cnt_wd$year.of.founding[cnt_wd$year.of.founding < 1997] <- 1997

#Age of company in years capped to 17
cnt_wd$Age.of.company.in.years[cnt_wd$Age.of.company.in.years > 17] <- 17

quantile(cnt_wd$Internet.Activity.Score, probs = seq(0, 1, by= 0.05),na.rm=T)
#Jump from 95% to 100%
quantile(cnt_wd$Internet.Activity.Score, probs = seq(0.9, 1, by= 0.005),na.rm=T)
cnt_wd$Internet.Activity.Score[cnt_wd$Internet.Activity.Score>802.48]<- 802.48
#Jump from 5% to 0%
quantile(cnt_wd$Internet.Activity.Score, probs = seq(0, .1, by= 0.005),na.rm=T)
cnt_wd$Internet.Activity.Score[cnt_wd$Internet.Activity.Score<(-305.88)]<- (-305.88)

quantile(cnt_wd$Employee.Count, probs = seq(0, 1, by= 0.05),na.rm=T)
#Jump from 95% to 100%
quantile(cnt_wd$Employee.Count, probs = seq(0.9, 1, by= 0.005),na.rm=T)
#Jump from 10% to 5%
cnt_wd$Employee.Count[cnt_wd$Employee.Count>295.5]<- 295.5
quantile(cnt_wd$Employee.Count, probs = seq(0, .1, by= 0.005),na.rm=T)
cnt_wd$Employee.Count[cnt_wd$Employee.Count<.875]<- .875

quantile(cnt_wd$Number.of.Investors.in.Seed, probs = seq(0.95, 1, by= 0.001),na.rm=T)
#capping above 16.78
cnt_wd$Number.of.Investors.in.Seed[cnt_wd$Number.of.Investors.in.Seed>16.78]<-16.78

quantile(cnt_wd$Team.size.Senior.leadership, probs = seq(0.99, 1, by= 0.001),na.rm=T)
#Jump after 99.6%
cnt_wd$Team.size.Senior.leadership[cnt_wd$Team.size.Senior.leadership>12.232]<-12.232

quantile(cnt_wd$Team.size.all.employees, probs = seq(.9, 1, by= 0.01),na.rm=T)
#capping above 103.8
cnt_wd$Team.size.all.employees[cnt_wd$Team.size.all.employees>103.8]<-103.8

quantile(cnt_wd$Number.of.of.repeat.investors, probs = seq(.9, 1, by= 0.005),na.rm=T)
#capping above 6
cnt_wd$Number.of.of.repeat.investors[cnt_wd$Number.of.of.repeat.investors>6]<-6

quantile(cnt_wd$Skills.score, probs = seq(.9, 1, by= 0.001),na.rm=T)
#capping above 53.45
cnt_wd$Skills.score[cnt_wd$Skills.score>53.45]<- 53.45

quantile(cnt_wd$Time.to.1st.investment..in.months., probs = seq(.9, 1, by= 0.001),na.rm=T)
#Capping above 103.740
cnt_wd$Time.to.1st.investment..in.months.[cnt_wd$Time.to.1st.investment..in.months.<103.74] <- 103.74

quantile(cnt_wd$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment, probs = seq(0.995, 1, by= 0.001),na.rm=T)
#capping above 93.5
cnt_wd$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment[cnt_wd$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment>93.5]<-93.5
#hist(cnt_wd$Time.to.1st.investment..in.months.)

# Create additional features like counting number of investors for company
char_df$Investor.count<-length(strsplit(char_df$Investors, "|",fixed=T))
for (i in (1:length(char_df$Investors)))
{
  if(is.na(char_df$Investors[i])==T){
    char_df$Investor.count[i]<- NA}
  else{
    lst<-strsplit(char_df$Investors[i], "|", fixed=T)
    char_df$Investor.count[i]<-length(lst[[1]])
  } }

cnt_wd$Investor.count<- char_df$Investor.count
#Removing Investor count from charater data set
char_df <- subset(char_df,select = -c(Investor.count))

cnt_wd$Recession <- as.numeric(cnt_wd$year.of.founding <= 2008)

cnt_wd$Time.since.last.funding <- 2014 - as.numeric(format(cnt_df$Last.Funding.Date, "%Y"))

 

quantile(cnt_wd$Investor.count, probs = seq(0, 1, by= 0.05),na.rm=T)
quantile(cnt_wd$Investor.count, probs = seq(0.9, 1, by= 0.005),na.rm=T)
#capping values above 23.415
cnt_wd$Investor.count[cnt_wd$Investor.count > 23.415] <- 23.415

#Bivariate analysis

#Experience in top organizations to be removed from continous data set and added to categorial data set

char_df$Experience.in.Fortune.1000.organizations <- as.character(cnt_wd$Experience.in.Fortune.1000.organizations)
char_df$Experience.in.Fortune.500.organizations <- as.character(cnt_wd$Experience.in.Fortune.500.organizations)
char_df$Experience.in.Fortune.100.organizations <- as.character(cnt_wd$Experience.in.Fortune.100.organizations)
cnt_wd <- subset(cnt_wd,select = -c(Experience.in.Fortune.1000.organizations, Experience.in.Fortune.500.organizations, Experience.in.Fortune.100.organizations))

str(char_df)

# Recession removed from contious data set and added to categorial data set
char_df$Recession <- as.character(cnt_wd$Recession)
cnt_wd <- subset(cnt_wd,select = -c(Recession))

#Removing years from data as it is nearly the same as Age of company in years
cnt_wd <- subset(cnt_wd,select = -c(Years))

# Anova to check difference in means based on categorial variable for levels less than 3 for filling missing values

anvar <- list()
for(i in 1:length(colnames(cnt_wd)))
{
  for(j in 1:length(colnames(char_df)))
  {
    if(length(levels(as.factor(char_df[,j])))<3)
    {
      if(j!=2)
      {
        aovtest<- aov(cnt_wd[,i]~char_df[,j])
        if(unlist(summary(aovtest))["Pr(>F)1"]<.0001)
        {
          andata<- as.data.frame(cbind(char_df[,j],cnt_wd[,i]))
          andata$V2<-as.numeric(andata$V2)
          group_mean<- tapply(cnt_wd[,i],char_df[,j],mean, na.rm = TRUE)
          #boxplot(V2~V1, data = andata, xlab = names(char_df)[j], ylab = names(cnt_wd)[i], na.exclude = TRUE)
          anvar<- append(anvar, list(c(i,j,group_mean[1],group_mean[2],
                                       unlist(summary(aovtest))["Pr(>F)1"],names(cnt_wd)[i],
                                       names(char_df)[j],sum(is.na(cnt_wd[,i])),sum(is.na(char_df[,j])))))
        }
      }
    }
  }
}

#Saving to text file
sink("anvar.txt")
print(anvar)
sink()

tapply(cnt_wd$Internet.Activity.Score,char_df$B2C.or.B2B.venture.,mean, na.rm = TRUE)
cnt_wd$Internet.Activity.Score[char_df$B2C.or.B2B.venture. == "B2B" & is.na(cnt_wd$Internet.Activity.Score)] <- 145.01411
cnt_wd$Internet.Activity.Score[char_df$B2C.or.B2B.venture. == "B2C" & is.na(cnt_wd$Internet.Activity.Score)] <- 51.911627

tapply(cnt_wd$Employee.Count,char_df$Has.the.team.size.grown,mean, na.rm = TRUE)
cnt_wd$Employee.Count[char_df$Has.the.team.size.grown == "YES" & is.na(cnt_wd$Employee.Count)] <- 53.34504
cnt_wd$Employee.Count[char_df$Has.the.team.size.grown == "NO" & is.na(cnt_wd$Employee.Count)] <- 14.84070

tapply(cnt_wd$Last.Funding.Amount,char_df$Has.the.team.size.grown,mean, na.rm = TRUE)
cnt_wd$Last.Funding.Amount[char_df$Has.the.team.size.grown == "YES" & is.na(cnt_wd$Last.Funding.Amount)] <- 8771627
cnt_wd$Last.Funding.Amount[char_df$Has.the.team.size.grown == "NO" & is.na(cnt_wd$Last.Funding.Amount)] <- 3734329

tapply(cnt_wd$Team.size.all.employees,char_df$Has.the.team.size.grown,mean, na.rm = TRUE)
cnt_wd$Team.size.all.employees[char_df$Has.the.team.size.grown == "YES" & is.na(cnt_wd$Team.size.all.employees)] <- 48.90458
cnt_wd$Team.size.all.employees[char_df$Has.the.team.size.grown == "NO" & is.na(cnt_wd$Team.size.all.employees)] <- 21.02092
  
tapply(cnt_wd$Number.of.Recognitions.for.Founders.and.Co.founders,char_df$Catering.to.product.service.across.verticals,mean, na.rm = TRUE)
cnt_wd$Number.of.Recognitions.for.Founders.and.Co.founders[char_df$Catering.to.product.service.across.verticals == "Yes" & is.na(cnt_wd$Number.of.Recognitions.for.Founders.and.Co.founders)] <- 100.73367
cnt_wd$Number.of.Recognitions.for.Founders.and.Co.founders[char_df$Catering.to.product.service.across.verticals == "No" & is.na(cnt_wd$Number.of.Recognitions.for.Founders.and.Co.founders)] <- 42.96859

tapply(cnt_wd$Employees.per.year.of.company.existence,char_df$Has.the.team.size.grown,mean, na.rm = TRUE)
cnt_wd$Employees.per.year.of.company.existence[char_df$Has.the.team.size.grown == "YES" & is.na(cnt_wd$Employees.per.year.of.company.existence)] <- 21.025317
cnt_wd$Employees.per.year.of.company.existence[char_df$Has.the.team.size.grown == "NO" & is.na(cnt_wd$Employees.per.year.of.company.existence)] <- 7.657971

tapply(cnt_wd$Last.round.of.funding.received..in.milionUSD.,char_df$Has.the.team.size.grown,mean, na.rm = TRUE)
cnt_wd$Last.round.of.funding.received..in.milionUSD.[char_df$Has.the.team.size.grown == "YES" & is.na(cnt_wd$Last.round.of.funding.received..in.milionUSD.)] <- 8.771627
cnt_wd$Last.round.of.funding.received..in.milionUSD.[char_df$Has.the.team.size.grown == "NO" & is.na(cnt_wd$Last.round.of.funding.received..in.milionUSD.)] <- 3.734329

tapply(cnt_wd$Percent_skill_Data.Science,char_df$Predictive.Analytics.business,mean, na.rm = TRUE)
cnt_wd$Percent_skill_Data.Science[char_df$Predictive.Analytics.business == "Yes" & is.na(cnt_wd$Percent_skill_Data.Science)] <- 11.594262
cnt_wd$Percent_skill_Data.Science[char_df$Predictive.Analytics.business == "No" & is.na(cnt_wd$Percent_skill_Data.Science)] <- 5.178986

tapply(cnt_wd$Percent_skill_Consulting,char_df$Was.he.or.she.partner.in.Big.5.consulting.,mean, na.rm = TRUE)
cnt_wd$Percent_skill_Consulting[char_df$Was.he.or.she.partner.in.Big.5.consulting. == "Yes" & is.na(cnt_wd$Percent_skill_Consulting)] <- 3.343554
cnt_wd$Percent_skill_Consulting[char_df$Was.he.or.she.partner.in.Big.5.consulting. == "No" & is.na(cnt_wd$Percent_skill_Consulting)] <- .3702775

tapply(cnt_wd$Percent_skill_Finance,char_df$Was.he.or.she.partner.in.Big.5.consulting.,mean, na.rm = TRUE)
cnt_wd$Percent_skill_Finance[char_df$Was.he.or.she.partner.in.Big.5.consulting. == "Yes" & is.na(cnt_wd$Percent_skill_Finance)] <- 9.428596  
cnt_wd$Percent_skill_Finance[char_df$Was.he.or.she.partner.in.Big.5.consulting. == "No" & is.na(cnt_wd$Percent_skill_Finance)] <- 1.486428

# Function to calculate mode
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

#Companies starting after 2008 are not applicable for survival through recession variable
#Imputing the companies starting before 2008 by "Yes"
table(char_df$Survival.through.recession..based.on.existence.of.the.company.through.recession.times, exclude = NULL)
char_df$Survival.through.recession..based.on.existence.of.the.company.through.recession.times[cnt_wd$year.of.founding < 2009 & is.na(char_df$Survival.through.recession..based.on.existence.of.the.company.through.recession.times)] <- "Yes"

str(char_df)
#Imputing with mode for categorial variables
for(i in 1:length(colnames(char_df)))
{
  if(i != 8 && i != 9 && length(levels(as.factor(char_df[,i])))<10)
  char_df[,i][is.na(char_df[,i])]<-Mode(char_df[,i])
}

table(as.factor(char_df$Gartner.hype.cycle.stage), exclude = NULL)
char_df$Gartner.hype.cycle.stage[is.na(char_df$Gartner.hype.cycle.stage)]<-"Plateau"

#Imputing with median for continous variable as the distribution of variables is skewed
for(i in 1:length(colnames(cnt_wd)))
{
  cnt_wd[,i][is.na(cnt_wd[,i])]<-median(cnt_wd[,i],na.rm=T) 
}

#Sum of percentage skills should be 100
sumrow<-rowSums(cnt_wd[,27:40])
ratio<- 100/sumrow
sum_not_hundred<- which(rowSums(cnt_wd[,27:40])<99)
cnt_wd$Percent_skill_Business.Strategy[sum_not_hundred]<- cnt_wd$Percent_skill_Business.Strategy[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Consulting[sum_not_hundred]<- cnt_wd$Percent_skill_Consulting[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Data.Science[sum_not_hundred]<- cnt_wd$Percent_skill_Data.Science[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Domain[sum_not_hundred]<- cnt_wd$Percent_skill_Domain[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Engineering[sum_not_hundred]<- cnt_wd$Percent_skill_Engineering[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Entrepreneurship[sum_not_hundred]<- cnt_wd$Percent_skill_Entrepreneurship[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Finance[sum_not_hundred]<- cnt_wd$Percent_skill_Finance[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Investment[sum_not_hundred]<- cnt_wd$Percent_skill_Investment[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Law[sum_not_hundred]<- cnt_wd$Percent_skill_Law[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Leadership[sum_not_hundred]<- cnt_wd$Percent_skill_Leadership[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Marketing[sum_not_hundred]<- cnt_wd$Percent_skill_Marketing[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Operations[sum_not_hundred]<- cnt_wd$Percent_skill_Operations[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Product.Management[sum_not_hundred]<- cnt_wd$Percent_skill_Product.Management[sum_not_hundred]*ratio[sum_not_hundred]
cnt_wd$Percent_skill_Sales[sum_not_hundred]<- cnt_wd$Percent_skill_Sales[sum_not_hundred]*ratio[sum_not_hundred]

# Using chisquare to find list of categorial related variables to Dependent company status
chisquare <- list()
for(i in 1:(length(colnames(char_df))))
{
        tbl<-table(char_df[,i],char_df$Dependent.Company.Status)
        h<-which(tbl<5)
        if(length(h)==0)
        {
          chitest<- chisq.test(tbl)
          if(!is.nan(chitest$p.value))
          {
            if(chitest$p.value<.05)
            {
              chisquare<- append(chisquare, list(c(i, chitest$p.value)))
            }
          }
        }
      }

#Saving to text file
sink("chi.txt")
print(chisquare)
sink()

anvar <- list()
for(i in 1:length(colnames(cnt_wd)))
{  anvar<- aov(cnt_wd[,i]~char_df$Dependent.Company.Status)
        if(unlist(summary(aovtest))["Pr(>F)1"]<.001)
        {
          andata<- as.data.frame(cbind(char_df$Dependent.Company.Status,cnt_wd[,i]))
          andata$V2<-as.numeric(andata$V2)
          group_mean<- tapply(cnt_wd[,i],char_df$Dependent.Company.Status,mean, na.rm = TRUE)
          boxplot(V2~V1, data = andata, xlab = "Dependent Company Status", ylab = names(cnt_wd)[i], na.exclude = TRUE)
          anvar<- append(anvar, list(c(i,j,group_mean[1],group_mean[2],
                                       unlist(summary(aovtest))["Pr(>F)1"])))
        }
}

#Saving to text file
sink("anvardepend.txt")
print(anvar)
sink()



