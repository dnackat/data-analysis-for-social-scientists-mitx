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
