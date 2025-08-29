setwd("C:\\Users\\User\\Desktop\\IT24102806Lab5")
data <- read.table("Data.txt", header=TRUE, sep=",")


fix(data)
attach(data)

##Rename
names(data) <- c("X1", "X2")
attach(data)

# Obtain histogram for number of shareholders
##Main(We can give name for our histogram)
hist(X2, main="Histogram for Number of Shareholders")

##Part2
##Breake(We can define number of classes we need in histogram)
##Right(We can define classes have closed intervals or open intervals )
##False (Intervals are closed)
histogram<-hist(X2,main = "Histogram for Number of Shareholders",breaks = seq(130,270,length=8),right = FALSE)
?hist

# Part 3
# Assign class limits of the frequency distribution into a variable called "breaks"
##Break(To breaks points)
##round(Round the value)
##$(To acces the data)
breaks <- round(histogram$breaks)



# Assign class frequencies of the histogram into a variable called "freq"
##Frequency mean values in each class
##Count(To get  frequencies in each colum )
freq <- histogram$counts

# Assign mid-point of each class into a variable called "mids"
##Mids(To get the mids points)
##Histogram is variable name
mids <- histogram$mids

# Creating the variable called "Classes" for the frequency distribution
##You can create empty vector for it
Classes <- c()

# Creating a "for" loop to assign classes of the frequency distribution into "Classes" variable created above.
##Create it lower limit to upper limit
##paste we are goingto cotatinate with out space
##Paste0 we are goingto cotatinate with  space
for (i in 1:length(breaks)-1) {
  Classes[i] <- paste0("(", breaks[i], "-", breaks[i+1], ")", sep="")
}

# Obtaining frequency distribution by combining the values of "Classes" & "freq" variables
# The "cbind" command used to merge the columns with same length
##Classe(vector name)
cbind(Classes = Classes, Frequency = freq)

# Part 4
# Portray the distribution in the form of a frequency polygon
plot(mids, freq, type = "l", main = "Frequency Polygon for Shareholders", xlab = "Shareholders", ylab = "Frequency")


# Portray the distribution in a cumulative frequency polygon (ogive).
# Adding a cumulative(the running total of the frequency) frequency line to the same plot.
lines(mids, cumsum(freq), type = "l", col = "red")

##Part 5
##"Cumsum"-We can get cumulative frequencies
cum.freq <- cumsum(freq)

##Creating a null variable called "new"
new<-c()

##Using "for"
for( i in 1: length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}

##Draw cumlative frequency polygon in a plot
plot(breaks,new,type ='1')


getwd()

#Q1
Delivery_Times<-read.table("Exercise - Lab 05.txt",header=TRUE)
attach(Delivery_Times)
#Q2
breaks<-seq(20,70,by=(70-20)/9)
hist(Delivery_Time_.minutes.,main = "Delivery Time(Minutes)",breaks = breaks,right=TRUE)

#Q3
#The curve shows a binomial distributions and appears approximately symmetrical.The data spans between 20 to 70 minutes


#Q4
hist_data <- hist(Delivery_Times$Delivery_Time_.minutes,
                  breaks = seq(20, 70, length.out = 10),
                  right = TRUE,
                  plot = FALSE)

cum_freq <- cumsum(hist_data$counts)
upper_bounds <- hist_data$breaks[-1]

plot(upper_bounds, cum_freq, type = "o",
     main = "Cumulative Frequency Polygon (Ogive)",
     xlab = "Delivery Time (minutes)",
     ylab = "Cumulative Frequency",
     col = "red")

