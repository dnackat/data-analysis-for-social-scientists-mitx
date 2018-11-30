# Preliminaries
#-------------------------------------------------
#install.packages('perm')
library("Matrix")
library("lfe")

rm(list = ls())
setwd("/Users/raz/Dropbox/14.31 edX Building the Course/Final Exam")

qiandata <- read.csv('qian.csv')
qiandata$post <- (qiandata$biryr >= 1979)

qiandata$teapost <- qiandata$teasown * qiandata$post
summary(qiandata)

model1 <- lm(sex ~ teasown + post + teapost, data = qiandata)
summary(model1)

model2 <- felm(sex ~ teasown + post + teapost + G(factor(admin)) , data = qiandata)
summary(model2)