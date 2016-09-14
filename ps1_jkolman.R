## Joseff Kolman
## September 14, 2016


setwd("C:/Users/Joseff/Dropbox (MIT)/Documents/2016-2017/Fall/17.800/PSET 1")
library(ggplot2)
library(tidyr)
library(dplyr)
library(foreign)
library(xtable)
#library(Matching)

#This is a standard set of packages I use to manipulate data and make graphics. In the first class that I learned R, 
#I picked up tdyr and dplyr, o I'll try to use them as much as possible.

#Problem 1
vote <- read.csv(file="vote.csv") %>% tbl_df() #import file and change to tbl class

dim(vote)
#1500x7

#Problem 2
vote %>% select(vote) %>% table() #Steps: load tbl, select just the vote column, provide frequency table.
#1283

1283/1500
#0.86

#Problem 3
vote %>% select(state) %>% table() #select state column, provide frequency table for that vector
#AR: 500, SC:1000

#Problem 4
vote %>% select(c(state,vote)) %>% table() #Same as Problem 2, but adds the state variable.
#AR Vote: 409, AR Non-Vote: 91
#SC Vote: 874, SC Non-Vote: 126

vote %>% select(c(state,vote)) %>% table() %>% as.data.frame() %>% tbl_df() %>% xtable() #Created a table for LaTex

#Problem 5
by_state <- vote %>% select(c(state,vote)) %>% table() #Save the frequency table from Problem 4
by_state[1,2]/(sum(by_state[1,])) #Voted over the total for AR
#0.82
by_state[2,2]/(sum(by_state[2,])) #Voted over the total for SC
#0.87 
#This SC had a higher turnout in 2000

#Problem 6
vote %>% select(age) %>% summary() #Select age column and find basic descriptive statistics
#Range is from 18 to 85

vote %>% group_by(state,vote) %>% summarise(mean(age))
#AR Vote: 51, AR Non-Vote: 43
#SC Vote: 49, SC Non-Vote: 50

vote %>% group_by(state,vote) %>% summarise(mean(age)) %>% xtable #Table for LaTex

#Problem 7
vote_7 <- with(vote,order(state,age)) %>% vote[.,] #First, "within the vote environment, order the data first by state,
#then by age". That produces a vector of ranks for each row. Second, place those ranks into the row call argument of vote
#and that calls them in the correct order.

vote_7 %>% group_by(state) %>% summarise(min(age))
#18 is the youngest for both states

#Problem 8
vote %>% filter(age == 18) %>% group_by(state) %>% summarise(mean(income)) #Select 18 yr olds, summarise income by state
#AR: 14, SC:16

#Problem 9
vote_9 <- vote_7 %>% mutate(low_inc = as.numeric(income < 13)) #add a column with 1's and 0's corresponding to answers YES 
#or NO to the logical question of vote_7[x,y = income] < 13 for each x.

vote_9 %>% select(low_inc) %>% table()
#676 are low income

#Problem 10
par(mfrow=c(1,3))
ggplot(vote_9,aes(education)) + geom_bar() + xlab("Education Level") + ggtitle("All Individuals") + theme_light()
ggsave("AllIn.jpg")
vote_9 %>% filter(low_inc == 1) %>% ggplot(.,aes(education)) + geom_bar() + xlab("Education Level") + 
ggtitle("Low Income Individuals") + theme_light()
ggsave("LowIn.jpg")
vote_9 %>% filter(low_inc == 0) %>% ggplot(.,aes(education)) + geom_bar() + xlab("Education Level") + 
ggtitle("High Income Individuals") + theme_light()
ggsave("HighIn.jpg")

#Problem 11
by_low_inc <- vote_9 %>% select(c(low_inc,vote)) %>% table() #Frequency table of low_inc by vote
by_low_inc[1,2]/sum(by_low_inc[1,])
#High Income: 0.91
by_low_inc[2,2]/sum(by_low_inc[2,])
#Low Income: 0.79 
#This shows that in this particular election, a larger percentage of high income voters elected to vote in SC and AR

#Problem 12
set.seed(17800)
vote_9 %>% sample_n(100) %>% summary() #Sample the table vote_9 100 times without replacement, find vote summary statistic from 
#summary() and the mean row shows the proportion of individuals who voted
#0.92

#Problem 13
set.seed(17800)
vote_9 %>% sample_n(1000) %>% summary()
#0.82 voted in this sample which is less than problem 12. As we sample a larger number of rows without replacement, the sampled 
#dataset approaches the dataset sampled.

#Problem 14
#part a)
my.mean <- function(x) {
  sum(x)/length(x)
}

vote_9 %>% select(-c(state,year)) %>% summarise_each(funs(my.mean)) #Deselect the state and year columns, apply the summary 
#function my.mean to each column
#vote=0.86, income=12, ed=2.6,age=49, female=.56, low_inc=.45
vote_9 %>% select(-c(state,year)) %>% summarise_each(funs(mean))
#vote=0.86, income=12, ed=2.6,age=49, female=.56, low_inc=.45

#part b)
sanders <- function(x) {
  x <-  x %>% mutate(rank = row_number(income)) #Adds a column with the ranking by income
  x %>% filter(rank < .99*length(rank)) %>% summarise(sanders = mean(income)) #Filters out those rows who's place is higher 
  #than 99% of the total length of the vector. The mean is then found of the resulting table's income.
}
sanders(vote_9)
#12.42

#Problem 15
load("matrixA.RData")
matrixA2 <- matrixA %>% as.data.frame() %>% tbl_df #conversion
#part a)
matrixA2 %>% select(c(1:3)) %>% summarise_each(funs(sum)) #Selected the first three columns, summarized each with the mean() 
#function
#115.9,159.9,95.4

#part b)
a <- 0
for (i in 1:10) {
  a[i] <-  matrixA[,i] %>% sum()
}
matrixA %>% colSums()
