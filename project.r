#Installing Needed Packages and Importing it
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
library("dplyr")
library("ggplot2")
library("corrplot")

#Reading our Dataset
dataset = read.csv("D:/data science/csv/RProject/Attrition.csv",stringsAsFactors = FALSE)

#Basic Analysis
View(head(dataset,50))
str(dataset)
dim(dataset)
glimpse(dataset)
unique(dataset$Department)
summary(dataset)

#Preprocessing
#1. Checking for Null Values

col_names <- names(dataset)
i = 1
while(i<=35)
{
  cat(col_names[i]," : ",sum(is.na(dataset[i])),"\n")
  i = i+1
}

summary(dataset$JobSatisfaction)
str(dataset$JobSatisfaction)
unique(dataset$Age)

#From above function we conclude that there is no any null value in dataset

#2. Feature Reducution / Removal
#There are 3 columns in our dataset has only single value so they are of no use so drop it
#Columns : Over18,EmployeeCount,StandardHours

dataset <- subset(dataset,select=-c(Over18,EmployeeCount,StandardHours))
dim(dataset)

#3. Feature Transformation

dataset$JobSatisfaction=replace(dataset$JobSatisfaction,dataset$JobSatisfaction==1,"Low")
dataset$JobSatisfaction=replace(dataset$JobSatisfaction,dataset$JobSatisfaction==2,"Medium")
dataset$JobSatisfaction=replace(dataset$JobSatisfaction,dataset$JobSatisfaction==3,"High")
dataset$JobSatisfaction=replace(dataset$JobSatisfaction,dataset$JobSatisfaction==4,"Very High")

dataset$WorkLifeBalance=replace(dataset$WorkLifeBalance,dataset$WorkLifeBalance==1,"Bad")
dataset$WorkLifeBalance=replace(dataset$WorkLifeBalance,dataset$WorkLifeBalance==2,"Good")
dataset$WorkLifeBalance=replace(dataset$WorkLifeBalance,dataset$WorkLifeBalance==3,"Better")
dataset$WorkLifeBalance=replace(dataset$WorkLifeBalance,dataset$WorkLifeBalance==4,"Best")

#4. Visualization

#1. Univariate Analysis
dataset%>%
  ggplot(aes(x=MonthlyIncome)) +
  geom_density(fill="#69b3a2")

ggplot(dataset,aes(x=MonthlyRate))+
  geom_boxplot()+
  coord_flip()

summary(dataset$PerformanceRating)

#2. Bivariate Analysis

ggplot(dataset,aes(x=Gender,fill=Attrition)) +
  geom_bar()

"From above graph it clearly shows that there is no major 
reason for attrition related to gender But still attrition rate for female is more"

ggplot(dataset,aes(x=JobSatisfaction,fill=Attrition)) +
  geom_bar()

#But Persons with Low Job Satisfaction are most likely to leave

ggplot(dataset,aes(x=Education,fill=Attrition)) +
  geom_bar()

ggplot(dataset,aes(x=BusinessTravel,fill=Attrition)) +
  geom_bar()

"From above it clarifies that people who get chance to travel rarely are also
leaving more"

ggplot(dataset,aes(x=MaritalStatus,fill=Attrition)) +
  geom_bar()
#Attrition rate for single persons are more

ggplot(dataset,aes(x=Department,fill=Attrition)) +
  geom_bar()

#From above visualization it is clear that Sales department has high attrition rate

ggplot(dataset,aes(x=Age,fill=Attrition)) +
  geom_bar() +
  facet_wrap(~ Attrition)

"From above visualization we can say that there is no any age problem for attrition
But it tells us that people who are between 30-35 are mostlye leaving and people 
between 35-40 are still working"


ggplot(dataset,aes(x=DistanceFromHome,fill=Attrition)) +
  geom_bar() +
  facet_wrap(~ Attrition)

"From above graph it shows that people who has distance utop 24-25 km's
from home there attrition rate is more"

ggplot(dataset,mapping = aes(TotalWorkingYears, MonthlyIncome)) +
  geom_point() +
  coord_cartesian()

#Dosen't have any strong relation

ggplot(dataset,aes(x=YearsAtCompany)) +
  geom_histogram()+
  facet_wrap(~Attrition)

#Multivariate Analysis

n <- select_if(dataset,is.numeric) #Extracting all Numeric Columns from dataset
Corr_matrix <- cor(n) #Coorelation Matrix
corrplot(Corr_matrix,method='number')
??attrition
