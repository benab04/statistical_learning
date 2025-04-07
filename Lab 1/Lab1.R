data("airquality")
View(airquality)
dim(airquality)
head(airquality)
tail(airquality)
colnames(airquality)
mean(airquality$Ozone) #gives error since NA values present
colSums(is.na(airquality)) #number of missing values in each column
str(airquality) #str is structure
summary(airquality)
summary(airquality$Ozone)

#Handling missing values
mean(airquality$Ozone) # $ is for calling particular column in a dataset
is.na(airquality$Ozone)
mean(airquality$Ozone,na.rm=TRUE) # .rm is to remove na values
airquality$Ozone[is.na(airquality$Ozone)]<-mean(airquality$Ozone,na.rm=TRUE)
install.packages("mice") #installing mice package
library(mice)
md.pattern(airquality)
methods(mice)
install.packages("VIM")
library(VIM)
aggr_plot<-aggr(airquality,col=c('navyblue','red'),numbers=TRUE,sortVars=TRUE,labels=names(data),cex.axis=.7,gap=3,ylab=c("Histogram of missing data","Pattern"))
impute_object<-mice(airquality,method="pmm",m=1)
imputed_data<-complete(impute_object,action="long")
head(imputed_data)
summary(impute_object)

#Descriptive Statsitics
duplicated(airquality)
mean(airquality$Ozone)
median(airquality$Ozone)
var(airquality$Ozone)
quantile(airquality$Ozone)
range(airquality$Ozone)
sd(airquality$Ozone)
IQR(airquality$Ozone)
cor(airquality,airquality,method="pearson",use="complete.obs")
cor.test(airquality$Ozone,airquality$Wind,method="pearson",use="complete.obs")

#Visualise the data
install.packages("ggplot2")
library(ggplot2)
plot(airquality)
plot(Ozone~Temp,airquality)
plot(airquality$Temp,xlab="observation number",ylab="temperature value",pch=15,col"Red",main="Scatter plot")
boxplot(airquality)
boxplot(airquality$Ozone)
boxplot(Ozone~Month,airquality,xlab="Month",ylab="Ozone (ppb)")
palette()
boxplot(airquality,Temp~Month,col=c(1,2,4,6,8))
barplot(airquality$Ozone)
par(mfrow=c(1,2)) #divides plotting frame into two parts, to show multiple charts side by side
hist(airquality$Temp)
with(airquality,hist(Temp))
par(mfrow=c(1,1))

#Join two datasets
df1=data.frame(StudentID=c(101:106),
               Product=c("Hindi","English","Maths","Science","Political Science","Physics"))
df1
df2=data.frame(StudentID=c(102,104,106,107,108),
               State=c("Mangalore","Mysore","Pune","Dehradun","Delhi"))
df2
dfi=merge(x=df1,y=df2,by="StudentID",all=FALSE) #dont take all students from datasets, only students with same stuednt ids
dfi
dfl=merge(x=df1,y=df2,by="StudentID",all.x=TRUE) #making sure all students from dataset x are present
dfl
dfr=merge(x=df1,y=df2,by="StudentID",all.y=TRUE)
dfr
df=merge(x=df1,y=df2,by="StudentID",all=TRUE)
df
#Join two datasets
