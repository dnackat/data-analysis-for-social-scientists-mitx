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

sp_volume <- seq(0, 1, length=100)
fx <- (6/5)*(sp_volume + (1/3))
fy <- (6/5)*((1/2) + sp_volume^2)

data_x <- data.frame(cbind(sp_volume, f))
data_y <- data.frame(cbind(sp_volume, f))

ggplot(data_x, aes("f")) +
  stat_ecdf(data_x, aes("f"), color="darkblue") +
  stat_ecdf(data_y, aes("f"), color="darkred")