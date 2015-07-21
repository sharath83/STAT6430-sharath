# ---
# Name: Sharath Chand PV
# Homework3
# Title: Hw03.R
# ID: vp4pa
# ---
  
  #Q1
# Reading the transactions.txt line by line using readLines() 
setwd("~/Documents/MSDS16/STAT_ Jeff/Day2/homework03")

library(stringr) # To perform acrobats on strings

TransLines <- readLines("transactions.txt", n=-1) # to Read all the lines of a file 

#To filter out only those line that are of interest to us
#The filtering criterion I have chosen is to check for "]" in a line
TransLines <- TransLines[(str_detect(TransLines,"]") == T)]

# Function to extract required information from a given string
extract_required <- function(x){
  item1 <- str_sub(x, end = str_locate(x, "]")[1]) #item1 to Extract Date/Time string 
  # item2 to Extracts the file path starting with Library
  item2 <- str_sub(x, 
                   start = str_locate(x,"Library")[1], 
                   end = str_locate(x,".pg")[2]) 
  return(cat(item1,item2)) #return the combined string date/time and file path
}

#To write extracted information into a file as required
for (i in 1:length(TransLines)){
  write(capture.output(extract_required(TransLines[i])),"output.txt",append=T)
}



#Q2:
#Download a copy of supremecourtclerks.html

#install.packages("XML")
library(XML)
# to Read all the lines of a file 
#Source: http://www.r-bloggers.com/reading-html-pages-in-r-for-text-processing/
rm(list = ls(all=T))
SCourt <- htmlTreeParse("supremecourtclerks.html",useInternalNodes = T)#Creates HTML parsed tree 
Clerks <- xpathSApply(SCourt, "//*/table", xmlValue) # Parse by tables
Clerks <- unlist(strsplit(Clerks,split = "\n")) #create a list by splitting in to lines

#strip the all tables  other than the TABLE that has CJ, clerks etc. information
start <- match("Seat",Clerks) #Get the start position of Clerks table
Clerks <- Clerks[start:length(Clerks)] #It starts from the Clerks table

#Now let us create a Data Frame with 8 columns with names as in Clerks[1:8]
Names <- Clerks[1:8] #Header of the table 
Clerks.df <- data.frame(matrix(Clerks[9:length(Clerks)],ncol = 8, byrow = TRUE)) #Data
#The dataframe will contain some unwanted data in the end rows
tail(Clerks.df) # It needs to be chopped off
colnames(Clerks.df) <- Names #With header

#Spaces in between column names will cause errors, rename them!
colnames(Clerks.df)[c(4,7,8)] <- c("ClerkName", "LawSchool_Year", "PrevClerkship")
str(Clerks.df) # All the fields are factors: Num, Started, Finished fields should be numeric

#Conevert to numeric fields
Clerks.df$Num <- as.numeric(as.character(Clerks.df$Num)) #Num field to numeric
#Clerks.df$Started<- as.numeric(as.character(Clerks.df$Started)) #Start year field to numeric
#Clerks.df$Finished<- as.numeric(as.character(Clerks.df$Finished)) #Finished year field to numeric

Clerks.df <- Clerks.df[complete.cases(Clerks.df),] #all the unwanted rows at the end are gone

#Justice, Law Clerk names are double printed. a bit of clean up required
#Cleanup function for Justice Names
J.clean <- function(x){
  y <- str_sub(x, start = str_locate(x,",")[1] + 3, end = -1)
  return(y)
}

#Cleanup function for Clerk Names
C.clean <- function(x){
  comma_start <- str_locate(x,",")[1]
  red <- str_sub(x, start=1, end = comma_start -1)
  x <- str_sub(x, start = comma_start+1, end = -1)
  red_start <- str_locate(x,red)[1]
  y <- str_sub(x, start = 1+(red_start-1)/2, end = -1)
  return(y)
}

#for every record in Clerks data, process Justice name and Clerk name by calling their 
# respective cleanup function
###IMP## The following loop should be run only once. The cleanup functions are written
## based on original raw form
J = ''
C = ''
for(i in 1:nrow(Clerks.df)){
  J[i] <- J.clean(Clerks.df$Justice[i])
  C[i] <- C.clean(Clerks.df$ClerkName[i])
}
Clerks.df$Justice <- J #Processed Names are written back to the data
Clerks.df$ClerkName <- C #Processed names of Clerks

write.csv(Clerks.df,"clerks.csv") #Export as a CSV file #for Q2f



####### Clerks.df data - to answer the following questions #####:
# 2a) For which justice did David Kravitz clerk?

data <- read.csv("clerks.csv", header = TRUE) #read the saved data

#data$ClerkName <- tolower(data$ClerkName)
sum(is.na(data$ClerkName)) #14 clerk names missing in the original data

# Check for david kravitz in clerk names. Some times middle name can decieve us
# So check separately
data[grepl(pattern = "kravitz", x = data$ClerkName, ignore.case = T) &
       grepl(pattern = "david", x = data$ClerkName, ignore.case = T),
     "Justice"]
#Answer# [1] Sandra Day O'Connor
# 74 Levels: Abe Fortas Anthony Kennedy Antonin Scalia Arthur Goldberg Benjamin N. Cardozo ... Willis Van Devanter


##2b) How many clerks went to school at the University of Texas?

#Considering Texas as University of Texas
nrow(data[grepl(pattern = "texas", x = data$LawSchool_Year, ignore.case = T),])
# > nrow(data[grepl(pattern = "texas", x = data$LawSchool_Year, ignore.case = T),])
# [1] 33       

##2c) How many clerks did Hugo Black have in his career? What are their names?

# Check for hugo black in justice names. Some times middle name can decieve us
# So check separately
x <- data[grepl(pattern = "hugo", x = data$Justice, ignore.case = T) &
            grepl(pattern = "black", x = data$Justice, ignore.case = T),"ClerkName"]
length(x)
x
# > length(x)
# [1] 33
# > x
#  [1] Jerome A. Cooper         Marx Leva                Max Isenbergh            John Paul Frank         
#  [5] Charles F. Luce          Louis F. Oberdorfer      William Joslin           Truman M. Hobbs         
#  [9] Frank M. Wozencraft      George Treister          Luther L. Hill, Jr.      Neal P. Rutledge        
# [13] Charles A. Reich         avid J. Vann             Daniel J. Meador         George C. Freeman, Jr.  
# [17] David M. Clark           Guido Calabresi          Nicholas Johnson         John K. McNulty         
# [21] Floyd F. Feeney          A. E. Dick Howard        John G. Kester           Drayton Nabers, Jr.     
# [25] John W. Vardaman         Margaret J. Corcoran     Stephen D. Susman        Joseph Price            
# [29] Walter E. Dellinger, III Kenneth C. Bass, III     ohn M. Harmon            Larry A. Hammond        
# [33] Covert E. Parnell III   
```

##2d) Which law school has produced the most clerks? How many?

x <- data$LawSchool_Year

#Get only the name after removing other characters and numbers
x <- str_replace_all(x,"[1234567890(),]","") 
topSchools_Clerks <- sort(table(x), decreasing = TRUE)[1:10]
topSchools_Clerks[1] 
# > topSchools_Clerks[1] - Harvard produced maximum clerks, 461 numbers as per the data
# Harvard  
#      461 
#122 fields in law school data are missing
```

##2e) Which justice had there most clerks?

J_MostClerks <- sort(table(data$Justice), decreasing = TRUE)[1:10]
J_MostClerks[1]
# Antonin Scalia 
#            117 

#There are some CJs who had same clerk twice
J_SameClerk <- paste(data$Justice,data$ClerkName)
J_SameClerk <- sort(table(J_SameClerk), decreasing = TRUE)
length(J_SameClerk[J_SameClerk > 1]) # 21 CJs had same person as clerk twice in their tenure
# Antonin Scalia is not one of them

##2f Clerks table -> write in to a csv
# Please refere to line no.101
