#Data Assessment Script
library(dplyr)
library(ggplot2)
library(gridExtra)


table(prosperData$ListingType)
ggplot(data=prosperData,aes(x=ListingType))+
  geom_bar()


#--------------------------------------------------->
#First C: Character
#-------------------------------------------------->
#Credit Score Range Lower/Upper
#Conclusion: no transofrmation needed, can use either since distributions look the same (just shifted)
#Very robust
summary(prosperData$CreditScoreRangeLower)
summary(prosperData$CreditScoreRangeUpper)

p1_CreditScoreLower<-ggplot(data=prosperData,aes(x=CreditScoreRangeLower))+
  geom_density()+
  scale_x_continuous(limits=c(375,900),breaks=seq(375,900,25))+
  scale_fill_manual(values ="green") 

p2_CreditScoreRangeUpper<-ggplot(data=prosperData,aes(x=CreditScoreRangeUpper))+
  geom_density()+
  scale_x_continuous(limits=c(375,900),breaks=seq(375,900,25))+
  scale_fill_manual(values ="red") 


grid.arrange(p1_CreditScoreLower,p2_CreditScoreRangeUpper) 

#------------------------------------------------------------------>
#Prosper Principal Borrowed
#Conclusion: WOuld be interesting to see if there is a track record with new users vs returning users
# ~ 91,000 hadn't used Prosper before
# 22,000 have used prosper before with some percentae of them having paid out the loan while others are outstanding

summary(prosperData$ProsperPrincipalOutstanding)
NROW(na.omit(prosperData$ProsperPrincipalOutstanding))
ggplot(data=prosperData,aes(x=ProsperPrincipalOutstanding))+
  geom_density()+
  scale_y_continuous()


#--------------------------------------------------------
#Employment Status in Months
#Conclusion: Useable, good distribution
#http://www.r-bloggers.com/density-plot-with-ggplot/
summary(prosperData$EmploymentStatusDuration)
ggplot(data=prosperData,aes(x=EmploymentStatusDuration))+
  geom_density(aes(group=ListingType,colour=ListingType,fill=ListingType,alpha=0.01))

summary(prosperData$EmploymentStatus)


#this is also interesting in that the listing types follow a similar distribution across employment status duration but would the answer change if we drilled down to the employment status & duartion by listing type? 

#-------------------------------------------------------------
#Employment Status
summary(prosperData$EmploymentStatus)

prosperData.Ppl.EmploymentStatus<-prosperData %>%
  group_by(EmploymentStatus) %>%
  summarise(n=n()) %>%
  mutate(freq=(n/sum(n))*100)

#-----------------------------------------------------------
#Occupation
prosperData.Ppl.Occupation<-prosperData %>%
  group_by(Occupation) %>%
  summarise(n=n()) %>%
  mutate(freq=(n/sum(n))*100)
prosperData.Ppl.Occupation
View(prosperData.Ppl.Occupation)

#------------------------------------------------------------
#Recommendations: 
#Conslusion: most people don't have a recommendation, so wouldn't be interesting 
ggplot(prosperData,aes(x=Recommendations))+
  geom_freqpoly()
summary(prosperData$Recommendations)


#-----------------------------------------------------------------
#ProsperScore
summary(prosperData$ProsperScore)



#--------------------------------------------------->
#Second C: Capacity
#-------------------------------------------------->
#Debt to income ratio
#Resources (1): http://www.consumerfinance.gov/askcfpb/1791/what-debt-income-ratio-why-43-debt-income-ratio-important.html
#"A debt-to-income ratio is one way lenders measure your ability to manage the payments you make every month to repay the money you have borrowed. "
#https://en.wikipedia.org/wiki/Debt-to-income_ratio
#http://www.investopedia.com/terms/d/dti.asp
#Important takeaway-- the lower a person's debt-to-income ratio, more likely they are to make the monthly payments. Is this true?
summary(prosperData$DebtToIncomeRatio)
#Very long tail to the right
ggplot(data=prosperData,aes(x=prosperData$DebtToIncomeRatio))+
  geom_histogram()
#Zoom in by cutting out the counts past 1000
ggplot(data=prosperData,aes(x=prosperData$DebtToIncomeRatio))+
  geom_histogram()+
  scale_y_continuous(limits = c(0,1000))
#a very small subset that has a fairly high debt to income ratio
table(prosperData$DebtToIncomeRatio)
ggplot(data=prosperData,aes(x=prosperData$DebtToIncomeRatio))+
  geom_freqpoly(binwidth = 0.01)
#Jesus there are people who have a debt-to-income ratio of over 9.5 ~280
#Might be a super interesting group to see what their background looks like

#-------------------------------------------------------------
#Stated Monthly Income
#Conclusion: Might be interesting except people can lie. Could be useful in a different type of analysis.
#Another possibility is VC's using it to test the service before funding


#Overall-- Need to zoom in because of high positive skew
ggplot(data=prosperData,aes(x=prosperData$StatedMonthlyIncome))+
  geom_freqpoly(binwidth = 1000)


summary(prosperData$StatedMonthlyIncome)
ggplot(data=prosperData,aes(x=prosperData$StatedMonthlyIncome))+
  geom_freqpoly(binwidth = 1000)+
  scale_x_continuous(limits=c(0,6826))
#limited the view to people who ave between 0 and 6826 stated monthly income
#Annual would be 81,900

ggplot(data=prosperData,aes(x=prosperData$StatedMonthlyIncome))+
  geom_freqpoly(binwidth = 50000)+
  scale_x_continuous(limits=c(6826,1750001))+
  scale_y_continuous(limits=c(0,100))
#This is a very select group. Might not be worth analyzing them at all. Should definitely
#also keep in mind when analyzing

#------------------------------------------------------------------------------------
#Monthly Loan Payment:
#Conclusion: We couldl utilize this to use income, flagging records to use that were verified
summary(prosperData$MonthlyLoanPayment)
ggplot(data=prosperData,aes(x=MonthlyLoanPayment))+
  geom_freqpoly()

ggplot(data=prosperData,aes(x=MonthlyLoanPayment))+
  geom_histogram()
#Also a very nice spread but not sure if interesting from an investor perspective. 
#Debt-to-income ratio would be more interesting because it culd include all types of debt, not just mortgages

#-----------------------------------------------------------------------------------
#Income verifiable
#Note: Borrower indicated they had supporting statements
#Conclusion: Use this to segment out the population to be analyzed

summary(prosperData$IncomeVerifiable)
#-----------------------------------------------------------------------
#Income Range
summary(prosperData$IncomeRange)
ggplot(data=prosperData, aes(x=IncomeRange))+
  geom_bar()
#Is not employed the same as 0? 
#So there are people who are making 100,000 plus but are they verified? And why would
#they need a loan?

#-----------------------------------------------------------------------------------
#--------------------------------------------------->
#Third C: Collateral
#-------------------------------------------------->
#Open Credit Lines/Current Credit Lines
#Conclusion: Good data here, nice spread, and we can look at whether open credit lines were correlated with a type of loan
#Would prefer for simplicity sake to use one or the other
summary(prosperData$OpenCreditLines)
summary(prosperData$CurrentCreditLines)

plot1.OpenCLines<-ggplot(data=prosperData, aes(x=OpenCreditLines))+
  geom_density()+
  scale_x_discrete(breaks=seq(0,70,5))+
  #1st Quartile
  geom_vline(xintercept=9.0,colour='red')+
  #Median
  geom_vline(xintercept=6.0,colour='blue')+
  #3rd Quartile
  geom_vline(xintercept=12.0,colour='blue')


plot2.OpenCLines<-ggplot(data=prosperData, aes(x=CurrentCreditLines))+
  geom_density()+
  scale_x_discrete(breaks=seq(0,70,5))+
  #1st quartile
  geom_vline(xintercept=7.0,colour='blue')+
  #Median
  geom_vline(xintercept=10.0,colour='red')+
  #3rd quertile
  geom_vline(xintercept=13.0,colour='blue')
  


grid.arrange(plot1.OpenCLines, plot2.OpenCLines)


#--------------------------------------------------->
#Fourth C: Loan Conditions
#-------------------------------------------------->

#Listing Category
#Conclusion: Fun to look at, most loans were for debt consolidation
prosperData$ListingType<-as.factor(prosperData$ListingType)
prosperData.Loan.ListingType<-prosperData %>%
  group_by(ListingType) %>%
  summarise(n=n()) %>%
  mutate(freq=(n/sum(n))*100)
prosperData.Loan.ListingType<-prosperData.Loan.ListingType[order(-prosperData.Loan.ListingType[,2]),]
View(prosperData.Loan.ListingType)

#Split out in bunches of 5
#Top 5
Top5_ListingType<-prosperData.Loan.ListingType[1:5,]
Top10_ListingType<-prosperData.Loan.ListingType[6:10,]
Top15_ListingType<-prosperData.Loan.ListingType[11:15,]



p1<-ggplot(data=Top5_ListingType,aes(x=reorder(ListingType,-freq),y=freq,fill=ListingType)) +
  geom_bar(colour="black",stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks=seq(0,max(Top5_ListingType$freq)+5,5))+
  xlab('Listing Type')+
  ylab('Frequency')+
  ggtitle("Ranked 1-5")+
  geom_hline(yintercept=2.25738785,colour='red')

  #The frequency level of listing category ranked #6
  
  

p2<-ggplot(data=Top10_ListingType,aes(x=reorder(ListingType,-freq),y=freq,fill=ListingType)) +
  geom_bar(colour="black",stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks=seq(0,max(Top10_ListingType$freq),0.5))+
  xlab('Listing Type')+
  ylab('Frequency')+
  ggtitle("Ranked 6-10")+
  geom_hline(yintercept=0.76884594,colour='red')
  #The frequency level of listing category ranked #11
  


p3<-ggplot(data=Top15_ListingType,aes(x=reorder(ListingType,-freq),y=freq,fill=ListingType)) +
  geom_bar(colour="black",stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks=seq(0,max(Top15_ListingType$freq),0.1))+
  xlab('Listing Type')+
  ylab('Frequency')+
  ggtitle("Ranked 11-15")+
  geom_hline(yintercept=0.19045613,colour='red')
  #The frequency level of listing category ranked #16
  

#http://docs.ggplot2.org/0.9.3.1/geom_bar.html



#---------------------------------------------------------->
#Most important non-C, the Returns
#---------------------------------------------------------->

#Loan Status
#----------------------------------------
#Conclusion: Nice segmentation
#So this is fairly interesting in that we can see that no to many people have actually defaulted on loans

ggplot(prosperData[prosperData$ListingType!="NA",],aes(x=LoanStatus_Type,fill=ListingType))+
  geom_bar(color='black') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#I'd like to do a stacked bar chart that shows percentage
#http://stackoverflow.com/questions/22231124/how-to-draw-stacked-bars-in-ggplot2-that-show-percentages-based-on-group
#http://stackoverflow.com/questions/12386005/r-stacked-percentage-bar-plot-with-percentage-of-binary-factor-and-labels-with
#http://rpubs.com/nikedenise/3256

ggplot(prosperData[prosperData$ListingType!="NA",],aes(x=LoanStatus_Type,fill=ListingType))+
  geom_bar(color='black') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()


#Estimated Effective Yield
#---------------------------------------------------------
summary(prosperData$EstimatedEffectiveYield)


##------------------------------------------->
#Exploring who are these people
#--------------------------------------------->
prosperData.Ppl.EmploymentStatus<-prosperData %>%
  group_by(EmploymentStatus) %>%
  summarise(n=n()) %>%
  mutate(freq=(n/sum(n))*100)

prosperData.Ppl.EmploymentStatus
#Need to recode these values


#----------------------------------------------------------------------------------->
library(stringr)
prosperData$LoanYear<-substring(prosperData$LoanOriginationQuarter,4,7)
