# The associate dean of a business school was looking for ways to improve the quality of the
# applicants to its MBA program. In particular she wanted to know whether the undergraduate
# degree of applicants differed among her school and the three nearby universities with MBA
# programs. She sampled 100 applicants of her program and an equal number from each of the
# other universities. She recorded their undergraduate degree (1 = BA, 2 = B.Eng, 3 = BBA, and 4
#                                                              =Other) as well as the university (codes 1, 2, 3, and 4).
# Refer to the MBA.rds dataset and answer the below questions.
# a. What type of data is this?
#   b. Using R, tabulate and graph the relationship between the applicant’s undergraduate degree
# and the university that applied to.
# c. Is the undergraduate degree and the university each person applied to appear to be related?

# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

#comment out if not mac and point to directory
setwd("~/Documents/Adv Stat/Assignments")

#Load up MBA.rds data into the R data frame
MBA<- readRDS("MBA.rds")

#Add two new features to the data frame:MBA
MBA$Degreelabel <- factor(MBA$Degree,
                                  levels=c(1,2,3,4),
                                  labels=c("BA",
                                           "B.Eng",
                                           "BBA",
                                           "Other"))
levels(MBA$Degreelabel)

# MBA$Universitylabel<- factor(MBA$University,
#                                  levels=c(1,2,3,4),
#                                  labels=c("G&M",
#                                           "Post",
#                                           "Star",
#                                           "Sun")) 
# levels(newsread$Newspaperlabel)

##Table of Frequencies
n=nrow(MBA); n #Total number of observations
addmargins(with(MBA,table(Degreelabel,University)))

#Row-relative Frequencies
with(MBA,table(Degreelabel,University)) %>% 
  prop.table(margin=1) %>% #value of each cell is divided by the sum of the row cells
  round(2)

#Column-relative Frequencies
with(MBA,table(Degreelabel,University)) %>% 
  prop.table(margin=2) %>% #each cell is divided by the sum of the column cells.
  round(2)

#Overall-relative Frequencies
with(MBA,table(Degreelabel,University)) %>% 
  prop.table(margin=) %>% #default is "margin=", which calculates proportions over the entire table.
  round(2)

#----------Graphing the Relationship b/w two Nominal Variables------------------
#Comparison Bar Graph
count<- table(MBA$Degreelabel,MBA$University); count
barplot(count, 
        main="Two-dimentional Bar Chart",
        xlab="Occupation", 
        ylab="Frequency", 
        col=c("darkblue","red","green", "yellow"),
        cex=1.2, cex.axis=1.2,cex.lab=1.2,
        legend=rownames(count), beside=TRUE)





