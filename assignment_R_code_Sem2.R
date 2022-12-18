#Required packages
library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(data.table)
library(curl)
library(stargazer)


#Reading csv files
if(file.exists("ccm.csv")) {
  mydata<-fread("ccm.csv")
} else {
  mydata<-fread("https://www.dropbox.com/s/pzryml6ble0jyvk/crsp_compustatannual_merged.csv?dl=1")
  fwrite(mydata, "ccm.csv")
}

#Reading csv files
if(file.exists("ff5.csv")) {
  ff5<-fread("ff5.csv")
} else {
  ff5<-fread("https://www.dropbox.com/s/owbpkdns6aog2qs/FF5.csv?dl=1")
  fwrite(ff5, "ff5.csv")
}

#Convert the date string to date. We can use the $ syntax to access the column (variable) of the data frame by name (mydata$date) 
format(Sys.time(), "%a %b %d %X %Y %Z")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
mydata$date<-as.Date(mydata$date,"%d-%b-%y")
Sys.setlocale("LC_TIME", lct)

# Sort the input data frame by permno and date
mydata<-mydata[with(mydata,order(permno,date)),]

# Create a few anomalies - "xreturns" is the new dataframe, set up by adding new variables (columns) to "mydata"
# Remember the anomalies only use data from the previous month(s), not the current month - lag() function
xreturns<-
  mydata%>%
  group_by(permno)%>%
  mutate(
    quick = (lag(act, 1) - lag(invt, 1))/lag(lct, 1)
    )

# Select the predictor - (for example, select from the list of anomalies created in the previous step)
predictor<-xreturns$quick

# Create a new data frame of predictor, variables used to construct predictor, and current returns 
sortdat<-data.table(permno=xreturns$permno,date=xreturns$date,eret=xreturns$eret,predictor, act=xreturns$act, invt=xreturns$invt, lct=xreturns$lct)
sortdat$predictor[which(abs(sortdat$predictor)==Inf)] = NA

# Keep if not missing lagged momentum and current returns
sortdat<-sortdat[!is.na(sortdat$predictor) & !is.na(sortdat$eret),]

# Summary statistics of predictor, variables used to construct predictor, and current returns
sumstat<-data.table(predictor=sortdat$predictor,eret=sortdat$eret, act=sortdat$act, invt=sortdat$invt, lct=sortdat$lct)
sink("summary.txt")
sumtab<-stargazer(sumstat, type="text")
sink()

# OLS regression of next month stock returns on the predictor
library(foreign)
library(sandwich)
library(lmtest)
model<-lm(eret~predictor,data=sortdat)
panel<-coeftest(model,vcov=NeweyWest(model,lag=6))
sink("panel_regression.txt")
print(panel)
sink()

# Create factor portfolio
sortdat<- sortdat[with(sortdat,order(date,predictor)),]
sortdat<- sortdat %>% group_by(date) %>% 
          mutate(quantilegroup = ntile(predictor,10)) 
group1<-aggregate(sortdat$eret,list(date=sortdat$date,decile=sortdat$quantilegroup),mean) # Group sortdat by quantilegroup and calculate the average return within each decile-month combination
decileports<-data.table(date=group1$date,decile=group1$decile,eret=group1$x)

# Bar chart
decileports<- decileports[with(decileports,order(decile)),]
group2<- aggregate(decileports$eret,list(decile=decileports$decile),mean) # Group data by deciles, and calculate average return of each decile portfolio over time
barchart<-data.table(decile=group2$decile,eret=group2$x)
pdf(file = "deciles.pdf")
ggplot(data=barchart, aes(x=decile,y=eret)) + geom_bar(stat="identity",width=0.5, fill="steelblue")+ theme_minimal()+xlab("Decile portfolios") + ylab("Mean returns")+ scale_y_continuous(breaks = seq(0,0.05, by = 0.005),labels = scales::percent)
dev.off()

# reshape decileports from long to wide format
decileports_wide<-dcast(decileports, decile~date, value.var = "eret")
HP<-decileports_wide[10,]-decileports_wide[2,] # returns of the long-short decile hedge portfolio over time
HP<-subset(HP,select=-1)

# Time-series graph
timeseries<-aggregate(decileports$eret,list(time=decileports$date),mean) 
timeseries<-subset(timeseries,select=-x)
timeseries$eret<-t(HP) # t(HP) is the transpose of HP
pdf(file = "hp_timeseries.pdf")
ggplot(data = timeseries, aes(x=time,y=eret,group=1))+ geom_line(color="#00AFBB",size=0.25)+xlab("Time")+ylab("Returns of the decile hedge portfolio") + scale_x_date(breaks = date_breaks("10 year"), labels = date_format("%Y")) + scale_y_continuous(breaks = seq(-0.5,0.25, by = 0.05),labels = scales::percent) + geom_hline(yintercept=0, linetype="dashed", color = "red")
dev.off()

# Merge timeseries with FF5 factors 
ff5$Date<-as.character(ff5$Date,"%Y%m")
timeseries$time<-as.character(timeseries$time,"%Y%m")
mergedat<-merge(ff5,timeseries,by.x="Date",by.y="time")

model1<-lm(eret~1,data=mergedat)
panel1<-coeftest(model1,vcov=NeweyWest(model1,lag=6))

model2<-lm(eret~MktRF,data=mergedat)
panel2<-coeftest(model2,vcov=NeweyWest(model2,lag=6))

model3<-lm(eret~MktRF+SMB+HML,data=mergedat)
panel3<-coeftest(model3,vcov=NeweyWest(model3,lag=6))

model4<-lm(eret~MktRF+SMB+HML+RMW+CMA,data=mergedat)
panel4<-coeftest(model4,vcov=NeweyWest(model4,lag=6))

sink("timeseries_alpha.txt")
summodel<-stargazer(panel1,panel2,panel3,panel4,title="Results",type="text", align = TRUE,report='vc*t')
sink()