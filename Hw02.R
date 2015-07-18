setwd("/Users//homw/Documents/MSDS16//STAT_ Jeff/Hw02/")
# load the grades data
gr <- read.csv("samplegrades.csv", header = T)
str(gr)
# a ##
#It is generally thought that students who score well on homework will also do well 
#in the course. Compare the course averages of students who had the maximum 
#possible homework score with those whose homework score was in the bottom 10%.
Homework_max <- gr[(gr$Homework..200.0. == max(gr$Homework..200.0.)),]
  # 123 students got max possible score in the Homework
Homework_bot10 <- gr[(gr$Homework..200.0. <= quantile(gr$Homework..200.0.,.1)),]
  # bottom 10% in Homework scores

# function to find confidence interval when a data frame is passed..
confInt <- function(x) {
  n = nrow(x)
  Int_lower = mean(x$Course.Average..100.0.)-2*(sd(x$Course.Average..100.0.)/sqrt(n))
  Int_upper = mean(x$Course.Average..100.0.)+2*(sd(x$Course.Average..100.0.)/sqrt(n))
  return(c(Int_lower,Int_upper))
}
# confidence interval of Course Average for top scorers in Homework module
confInt(Homework_max) 
# > confInt(Homework_max)
# [1] 85.95184 87.97987
 
confInt(Homework_bot10)
# > confInt(Homework_bot10)
# [1] 58.59651 67.52072

#Observation a #Top scorers in homework are most likely to have a course average of 45% higher than 
# bottom 10% students of homework

# b ##
# It is speculated that consistent students do better in courses. 
# Compare the course averages of those students who had midterm and final scores 
# within 2% of each other with those students whose exams scores were at least 10% apart. 
# Find approximate confidence intervals for each using the above.
  #Consistent students
Con <- gr[((gr$Midterm..100.0. < 1.02*gr$Final.Exam..100.0. & 
             gr$Midterm..100.0. > .98*gr$Final.Exam..100.0.) |
             (gr$Final.Exam..100.0.< 1.02*gr$Midterm..100.0. & 
                gr$Final.Exam..100.0. > .98*gr$Midterm..100.0.)),]
  # Not consistent students
NotCon <- gr[((gr$Midterm..100.0. < .9*gr$Final.Exam..100.0.  |
                 gr$Midterm..100.0. > 1.1*gr$Final.Exam..100.0.) |
                (gr$Final.Exam..100.0.< .9*gr$Midterm..100.0. | 
                   gr$Final.Exam..100.0. > 1.1*gr$Midterm..100.0.)),]
# Course Average intervals for Con and NotCon
confInt(Con)
# > confInt(Con)
# [1] 81.28736 84.67797
confInt(NotCon)
# > confInt(NotCon)
# [1] 76.89382 79.53094
#Obs b# Though consistent students course average interval is slightly more than 
# Non-consistent set, there is no conceivable difference as such

# Determine the new course average for each student after adding 5% class participation
gr$New.Course.Avearge <- 0.95*gr$Course.Average..100.0.+5

# Function to determine letter grade when the course average is passed
letterGrade <- function(x){
  LG <- NULL
  for (i in 1:length(x)) {
    if (x[i] >= 90) LG[i] <- "A"
    if (x[i] >= 80 & x[i] < 90) LG[i] <- "B"
    if (x[i] >= 70 & x[i] < 80) LG[i] <- "C"
    if (x[i] >= 60 & x[i] < 70) LG[i] <- "D"
    if (x[i] < 60) LG[i] <- "F"
  }
  return(LG)
  rm(i)
}
gr$NewGrade <- letterGrade(gr$New.Course.Avearge) #call function to find new grade
gr$OldGrade <- letterGrade(gr$Course.Average..100.0.) #To find old grade

# count the number of grade changes
sum(gr$NewGrade != gr$OldGrade) #57 grade change requests to be raised
# > sum(gr$NewGrade != gr$OldGrade)
# [1] 57

##2Q##
# function that claculates min, median and max values
MMM <- function(x){
  return(quantile(x,c(0,.5,1)))
}
MMM(gr$Midterm..100.0.)
# > MMM(gr$Midterm..100.0.)
# 0%  50% 100% 
# 0   76  100 
# > min(gr$Midterm..100.0.)
# [1] 0
# > median(gr$Midterm..100.0.)
# [1] 76
# > max(gr$Midterm..100.0.)
# [1] 100

#Q3## Function to generate nearetst even integer
nearestEven <- function(x){
  n = trunc(x)
  if (n %% 2 == 0) return(n)
  else
    ifelse(n<0,return(n-1),return(n+1)) 
}
nearestEven(2.8)
nearestEven(1.3)
nearestEven(-5.2)
# Output
# > nearestEven(2.8)
# [1] 2
# > nearestEven(1.3)
# [1] 2
# > nearestEven(-5.2)
# [1] -6


#Q4## Function for random histogram
my.hist1 <- function(x){
  #runif() is used to generate random real numbers in an interval
  hist(runif(x,0,10), xlab = "Random Numbers", main = "Histogram of Random")
  return()
}
my.hist1(30)

#Q5## Function - histogram of maximums of 'm' random sequences of length 'n'
my.hist2 <- function(m,n){
  maxR = 0
  for (i in 1:m) {
    maxR[i] <- max(runif(n,0,10))
  }
  hist(maxR)
  return()
}
my.hist2(100,10) #100 sequences of length 10
my.hist2(10,100) #10 sequences of length 100
  
#Q6## Function that returns median, mode and mean
my.centers <- function(x){
  med = median(x)
  # to calculate mode 
  if (length(x) != length(unique(x)))
    mod = names(sort(-table(x)))[1]
  else
    mod = NA
  # 3 std deviations almost certainly contains all the values :)
  if (max(abs((x-mean(x))/sd(x)) > 3)) #just for checking sake!
    mea = NA #Not returning anything
  else
    mea = mean(x)
  
  return(c(med,mod,mea)) 
}
my.centers(c(0,1,2,3,4))
# > my.centers(c(0,1,2,3,4))
# [1]  2 NA  2
my.centers(c(0,1,2,3,4,4,4,5,5,5,5))
# > my.centers(c(0,1,2,3,4,4,4,5,5,5,5))
# [1] "4"                "5"                "3.45454545454545"
my.centers(c(1,2,100000))
# > my.centers(c(1,2,100000))
# [1]     2.00       NA 33334.33
