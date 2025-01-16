x <- c(4, 8, 23, 2, 16, 7) 
x = c(4, 8, 23, 2, 16, 7) 
mean(x) # arithmetic mean 
median(x) # middle value 
length(x) # number of elements in a vector or list 
range(x) # largest and smallest value 
sd(x) # standard deviation 
var(x) # variance 

help(max) 
?max 

summary(x)

#Ceiling function
ceiling(1) 
ceiling(1.4) 
ceiling(1.5) 
ceiling(1.6) 
ceiling(1.9) 
ceiling(-1)  
ceiling(-1.4) 
ceiling(-1.5) 
ceiling(-1.6) 
ceiling(-1.9)

#Floor function
floor(1) 
floor(1.4) 
floor(1.5) 
floor(1.6) 
floor(1.9) 
floor(-1)  
floor(-1.4) 
floor(-1.5) 
floor(-1.6) 
floor(-1.9)

#Trunc function
trunc(1) 
trunc(1.4) 
trunc(1.5) 
trunc(1.6) 
trunc(1.9) 
trunc(-1)  
trunc(-1.4) 
trunc(-1.5) 
trunc(-1.6) 
trunc(-1.9)

#Round function
round(1) 
round(1.4) 
round(1.5) 
round(1.6) 
round(1.9) 
round(-1)  
round(-1.4) 
round(-1.5) 
round(-1.6) 
round(-1.9) 
round(1.949,digits = 0) 
round(1.949,digits = 1) 
round(1.949,digits = 2) 
round(-1.949,digits = 0) 
round(-1.949,digits = 1) 
round(-1.949,digits = 2) 

#Signif Function
signif(1.949,digits = 0) 
signif(1.949,digits = 1)  
signif(1.949,digits = 2) 
signif(-1.949,digits = 0) 
signif(-1.949,digits = 1) 
signif(-1.949,digits = 2)

#Exploring data graphically 
#We can see the outliers
#Get a pattern about the data set
data()

cars
summary(cars)
plot(cars)
hist(cars$speed)
hist(cars$dist)
plot(density(cars$speed))
plot(density(cars$dist))

#Data Visualization with R
Thai_tourist <- read.csv("Thaitourism1.csv", header= TRUE) #Import the Data

#Inspect the data set in R
names(Thai_tourist)  
head(Thai_tourist)  
tail(Thai_tourist)  
str(Thai_tourist) 
summary(Thai_tourist) 

#Import the second file.
Thai_tourist_full <- read.csv("Thaitourism2.csv", header= TRUE)

#Inspect the data set in R
names(Thai_tourist_full)  
head(Thai_tourist_full) 
tail(Thai_tourist_full)  
str(Thai_tourist_full) 
summary(Thai_tourist_full)  

#Filter the Thai_tourist 
Thai_2016<-Thai_tourist[Thai_tourist$Year==2016,] 
Thai_2016

#create a new data frame called Thai_UK to contain only UK tourists data.
Thai_UK<-Thai_tourist_full[Thai_tourist_full$nationality=="UnitedKingdom",] 
Thai_UK 

#Plot function

plot(factor(Thai_2016$Region),Thai_2016$Tourists_1000s,
     type = "p",
     main = "Tourists by Region",
     xlab = "Region (Numeric)",
     ylab = "Tourists (in thousands)")

plot(factor(Thai_2016$Region),Thai_2016$Tourists_1000s,
     type="l",
     lwd=5)

plot(factor(Thai_2016$Region),Thai_2016$Tourists_1000s, 
     type = "p",
     lwd=5,
     lty=3)

#Plotting character
plot(Thai_tourist$Year,Thai_tourist$Tourists_1000s,pch = 19)

plot(Thai_tourist$Year,Thai_tourist$Tourists_1000s,pch = 19, cex=3)

#Specifying colors
plot(Thai_tourist$Year,Thai_tourist$Tourists_1000s,pch = 19,cex=2,col="blue")

plot(Thai_tourist$Year,Thai_tourist$Tourists_1000s,
     pch = 19,cex=2,col=factor(Thai_tourist$Region))

#8.Bar plot function
barplot(Thai_2016$Tourists_1000s)

barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region)

barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region,horiz=TRUE)

barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region,col = c("red","blue"))

barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region,col = rainbow(8))

#Hist() function
hist(Thai_UK$tourists)
hist(Thai_UK$tourists,breaks=16)
hist(Thai_UK$tourists,freq = FALSE)
hist(Thai_UK$tourists, labels = TRUE)
hist(Thai_UK$tourists, xlab = "No. of UK Tourists per month", col = rainbow(8))

#Box plot

#below code crates a new data frame called Thai_Europe to hold only data from Europe Region. 
#subset() function return subsets of vectors, matrices or data frames which meet conditions. The subset( ) function is the easiest way to select variables and observations from large data set. 
#drop levels() function is used to drop unused levels from factors in a data frame.

Thai_Europe <- droplevels(subset(Thai_tourist_full, region=="Europe"))

boxplot(Thai_Europe$tourists, data=Thai_Europe)
boxplot(tourists~nationality, data = Thai_Europe,col=rainbow(8))
boxplot(tourists~nationality, data = Thai_Europe,col=rainbow(8),horizontal = TRUE)
boxplot(tourists~nationality, data = Thai_Europe,col=rainbow(8),
        subset = nationality %in% c("France","Russia","Germany"))

#Pie() function
pie(Thai_2016$Tourists_1000s)

pie(Thai_2016$Tourists_1000s,labels = Thai_2016$Region,col = rainbow(8))

percent <- round(100*Thai_2016$Tourists_1000s/sum(Thai_2016$Tourists_1000s), 1) 
percent <- paste(Thai_2016$Region, "-",percent,"%") # add percents to labels  
# paste0 function is simply concatenates the vector with space separator. 
pie(Thai_2016$Tourists_1000s, labels = percent, col = rainbow(8))

#load libraries into R session 
library(ggplot2)
qplot(Region,Tourists_1000s,data=Thai_tourist)  
qplot(Region,Tourists_1000s,data=Thai_tourist, color = Year)
qplot(Region,Tourists_1000s,data=Thai_tourist, color = Year, size = I(10))

#Box plot with q plot() function 
qplot(Region,Tourists_1000s,data=Thai_tourist, geom = "boxplot",fill="red")

#Bar plot with q plot() function 
qplot(tourists,data=Thai_UK, geom = "histogram",fill="red") 

#Density plot with qplot() function

qplot(tourists,data=Thai_UK, geom = "density",fill="red")

qplot(occupancy_data$Temperature) 
qplot(occupancy_data$Temperature, occupancy_data$Humidity)


