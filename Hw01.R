setwd("~/Documents/MSDS 16/STAT - Jeff/day 02/")
# load the samplegrades.csv file
gr <- read.csv("samplegrades.csv", header = T)
str(gr) # to understand the data types of columns

#a#Determine the percentage of students who had a course average of less than 60%, 
#the minimum to pass the course
nrow(gr[which(gr$Course.Average..100.0. < 60),])*100 /nrow(gr) #4.24 %

#b# How many students did not take the final exam?
sum(is.na(gr$Final.Exam..100.0.)) # 0 students
sum(gr$Final.Exam..100.0. == 0) # 5 students: If zero marks are considered as "Test not taken"

#c# Which was higher, the average for quizzes, the midterm, or the final?
x <- summary(gr[,c(5,4,2)]) # Average is highest for Midterm
  ## alternate approach after normalizing the Quiz scores to 100 scale
x <- matrix(c(mean(gr$Quiz..45.0.*100/45), mean(gr$Midterm..100.0.), mean(gr$Final.Exam..100.0.)))
means <- data.frame(cbind(c("Quiz", "Mid", "Final"),x))
names(means) <- c("test type", "Average")
means$Average <- as.numeric(means$Average)
means[which(means$Average == max(means$Average)),"test type"] 
  #Average is highest in case of Quiz if the Quiz scores are scaled to 100

#d# How many students had a midterm score of at least 80% 
# and also a quiz score of no more than 70%?
nrow(gr[which(gr$Midterm..100.0. >= 80 &
                (gr$Quiz..45.0.*100)/45 <= 70),]) #8 students

#e# Find the percentage of students in the entire class that had a course average lower 
# than their final exam score.
(nrow(gr[which(gr$Course.Average..100.0. < gr$Final.Exam..100.0.),]) / nrow(gr))*100
         ## 8.12 % of total students or 46 students

#f# Repeat the previous part, but this time for students in the top 20% 
# of the class, and then for students in the bottom 20% of the class.
gr_order <- gr[order(-gr$Course.Average..100.0.),]
gr_top20 <- gr_order[(1:(0.2*nrow(gr_order))),]
gr_bot20 <- gr_order[(0.8*nrow(gr)):nrow(gr)+1,]
 ## course average lower than final score for the Top 20%
(nrow(gr[which(gr_top20$Course.Average..100.0. < gr_top20$Final.Exam..100.0.),]) / nrow(gr))*100
## 1.76 % of total students or 11 students

## course average lower than final score for the Bottom 20%
(nrow(gr[which(gr_bot20$Course.Average..100.0. < gr_bot20$Final.Exam..100.0.),]) / nrow(gr))*100
## 2.65 % of total students or 15 students

#g# Find the number of students who either had a quiz average between 70% and 80% or 
# had homework average between 90% and 95%.
nrow(gr[which((gr$Quiz..45.0.*100/45 > 70 & gr$Quiz..45.0.*100/45 < 80) |
                (gr$Homework..200.0.*100/200 > 90 & gr$Homework..200.0.*100/200 < 95)),])
  ## 111 students

## 2. Give the R code required to add a new column to grade.data that contains a letter grade, 
#  assigned based on the table below.
for (i in 1:nrow(gr)) {
  if (gr$Course.Average..100.0.[i] >= 90) gr$LetterGrade[i] <- "A"
  if (gr$Course.Average..100.0.[i] >= 80 & gr$Course.Average..100.0.[i] < 90) gr$LetterGrade[i] <- "B"
  if (gr$Course.Average..100.0.[i] >= 70 & gr$Course.Average..100.0.[i] < 80) gr$LetterGrade[i] <- "C"
  if (gr$Course.Average..100.0.[i] >= 60 & gr$Course.Average..100.0.[i] < 70) gr$LetterGrade[i] <- "D"
  if (gr$Course.Average..100.0.[i] < 60) gr$LetterGrade[i] <- "F"
}
rm(i)

#3# Export grade.data (with course grades) to a CSV file mygradefile.csv.
write.csv(gr, "mygradefile.csv")

