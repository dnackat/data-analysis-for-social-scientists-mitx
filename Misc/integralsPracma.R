# Integrals with pracma
library(pracma)

#1
f1 <- function(x,y) x^2*y
xmin <- -1
xmax <- 1
ymin <- function(x) x^2
ymax <- 1

integral2(f1, xmin, xmax, ymin, ymax)$Q

#2
f2 <- function(x,y) x*y
xmin <- 1
xmax <- 2
ymin <- 0
ymax <- 1

integral2(f2, xmin, xmax, ymin, ymax)$Q

#3
f3 <- function(x,y) (21/4)*x^2*y
xmin <- 0
xmax <- 1
ymin <- function(x) x^2
ymax <- function(x) x

integral2(f3, xmin, xmax, ymin, ymax)$Q

#4
f4 <- function(x,y) (1/9)*x*y
xmin <- 2
xmax <- 3
ymin <- 1
ymax <- 2

integral2(f4, xmin, xmax, ymin, ymax)$Q

#5
f5 <- function(x,y) (x + y^2)
xmin <- 0
xmax <- 1
ymin <- 0
ymax <- 1

integral2(f5, xmin, xmax, ymin, ymax)$Q

#6
f6 <- function(x) (6/5)*(x + (1/3))
xmin <- 0.9
xmax <- 1

integral(f6, xmin, xmax)

#7
f7 <- function(y) (6/5)*((1/2) + y^2)
ymin <- 0.9
ymax <- 1

integral(f7, ymin, ymax)

#8
f8 <- function(y) (y)*(exp(-4*y))
ymin <- 0
ymax <- Inf

integral(f8, ymin, ymax)

#9
f9 <- function(y) 2*(1/(sqrt(2)*sqrt(2*pi)))*(y)*(exp(-(y^2)/4))
ymin <- 0
ymax <- Inf

integral(f9, ymin, ymax)

#10
a = 2
f10 <- function(y) exp(a)*(a/(y^2))*(exp(-a/y))
ymin <- 0
ymax <- 1

integral(f10, ymin, ymax)

#11
f11 <- function(y) (1/(y^2))*(y-log(2))^2
ymin <- 1/2
ymax <- 1

integral(f11, ymin, ymax)

#12
b = 2
f12 <- function(x) (1/(2*b))*x*exp(-abs(x)/b)
xmin <- -Inf
xmax <- 0

integral(f12, xmin, xmax)

#13
theta = 1
f13 <- function(x) (x^2/theta^2)*exp(-x^2/(2*theta^2))
xmin <- 0
xmax <- Inf

integral(f13, xmin, xmax)

#14
f14 <- function(x) x*((0.5/sqrt(2*pi))*exp(-(x-2)^2/2) - (0.5/sqrt(2*pi))*exp(-(x+2)^2/2))
xmin <- -Inf
xmax <- Inf

integral(f14, xmin, xmax)

# 3D plot
library(plot3D)
library(utils)

M <- mesh(seq(0,1, length=100), seq(0,1, length=100))
x <- M$x
y <- M$y
z <- (6/5)*(x + y^2)

persp3D(x,y,z,xlab="X variable",ylab="Y variable",xlim=c(0,1),main="Plotting joint pdf")

# CFD plot
library(ggplot2)
library(dplyr)
library(tidyr)

x <- seq(0, 1, length=100)
fx <- (6/5)*((x^2/2) + (x/3))
fy <- (6/5)*((x^3/3) + (x/2))

data_x <- data.frame(cbind(x, f=fx))
data_y <- data.frame(cbind(x, f=fy))

ggplot(data_x, aes(x=x, y=f)) +
  stat_ecdf(data=data_x, aes(x=x, y=f), color="darkblue") +
  stat_ecdf(data=data_y, aes(x=x, y=f), color="darkred")