SWEDISH MOTOR INSURANCE

setwd("C:/Users/dell/OneDrive")
getwd()

###DATA STRUCTURE AND SUMMARY
#load the dataset
insurance_data<-read.csv("insurance.csv")

#get the structure of the dataset
str(insurance_data)
dim(insurance_data)

#summary statistics
summary(insurance_data)

#visualizing missing values
library(ggplot2)
install.packages("ggplot2")
library(DataExplorer)
plot_str(insurance_data)

sapply(insurance_data,function(x) sum(is.na(x)))
plot_missing(insurance_data)

prop.table(table(insurance_data$Zones))
table(insurance_data$Zones)

prop.table(table(insurance_data$Claims))
table(insurance_data$Claims)

###DATA CLEANING AND TRANSFORMATION
insurance_data$Make <- as.factor(insurance_data$Make)
insurance_data$Make <- as.factor(insurance_data$Make)
insurance_data$Zone <- as.factor(insurance_data$Zone)
insurance_data$Bonus <- as.factor(insurance_data$Bonus)

###DATA VISUALIZATION
#histogram of the number of claims
hist(insurance_data$Claims, main="Histogram of Claims",xlab="Number of Claims")

#barplot of claims by insurance zone
boxplot(Claims~Zone, data=insurance_data,main="Boxplot of Claims by Zone",xlab="Zone",ylab="Number of Claims")

#Scatter plot of total payment vs. No of claims
plot(insurance_data$Claims,insurance_data$Payment,main="Total Payment vs. Number of Claims",xlab="Number of Claims", ylab="Total Payment")


### Descriptive statistics 
# total number of claims per zone 
aggregate(Claims~Zone,data=insurance_data,sum)

# average payment per claim per zone
insurance_data$PaymentPerClaim<-insurance_data$Payment/insurance_data$Claims
aggregate(PaymentPerClaim~Zone,data=insurance_data,mean)

#Zone with highest average payment per claim
zone_avg_payment<-aggregate(PaymentPerClaim~Zone,data=insurance_data,mean)
zone_avg_payment[which.max(zone_avg_payment$PaymentPerClaim),]

### REGGRESSION ANALYSIS
#linear regression model
model<-lm(Payment~Claims+Insured,data=insurance_data)
summary(model)

# Adding zone as a categorical variable 
model_with_zone<-lm(Payment~Claims+Insured+as.factor(Zone),data=insurance_data)
summary(model_with_zone)

##CORRELATION ANALYSIS
correlation <- cor(insurance_data$Kilometres, insurance_data$Claims)
print(correlation)

###EXPLORARTORY DATA ANALYSIS
#Descriptive statistics
# Frequency table for categorical columns (e.g., Zone)
table(insurance_data$Zone)

#Distribution of Numerical Variables
# Histogram for Claims
hist(insurance_data$Claims, main = "Distribution of Claims", xlab = "Number of Claims", col = "blue")

# Histogram for Insured
hist(insurance_data$Insured, main = "Distribution of Insured Vehicles", xlab = "Number of Insured Vehicles", col = "green")

# Histogram for Kilometres
hist(insurance_data$Kilometres, main = "Distribution of Kilometres", xlab = "Kilometres", col = "red")

#Box Plots for Outlier Detection
# Box plot for Claims by Zone
boxplot(Claims ~ Zone, data = insurance_data, main = "Claims by Zone", xlab = "Zone", ylab = "Number of Claims")

# Box plot for Claims by Bonus
boxplot(Claims ~ Bonus, data = insurance_data, main = "Claims by Bonus Class", xlab = "Bonus Class", ylab = "Number of Claims")

#Scatter Plots to Identify Relationships
# Scatter plot of Claims vs. Insured
plot(insurance_data$Insured, insurance_data$Claims, main = "Claims vs. Insured", xlab = "Number of Insured Vehicles", ylab = "Number of Claims")

# Scatter plot of Claims vs. Kilometres
plot(insurance_data$Kilometres, insurance_data$Claims, main = "Claims vs. Kilometres", xlab = "Kilometres", ylab = "Number of Claims")



# Calculate correlation matrix
correlation_matrix <- cor(insurance_data[, c("Claims", "Insured", "Kilometres")])
print(correlation_matrix)

# Visualization of the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "circle")






