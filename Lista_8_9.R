# Zadanie 1
library(DescTools)
library(psych)

data <- matrix(c(35, 22, 15, 0, 18, 0, 22, 15, 40, 3, 0, 0, 15, 10, 5), nrow = 5)

#1
goodman <- function(data) {
  n <- sum(data)
  
  R <- dim(data)[1]
  C <- dim(data)[2]
  
  sum1 <- 0
  
  for (i in 1:R) {
    for (j in 1:C) {
      sum1 <- sum1 + data[i, j]^2 / (n * sum(data[i, ]))
    }
  }
  
  sum2 <- 0
  for (j in 1:C) {
    sum2 <- sum2 + (sum(data[ , j])/n)^2
  }
  
  (sum1 - sum2) / (1 - sum2)
}



goodman(data)
GoodmanKruskalTau(data, direction = "column")
data[2, ]


#2
crammer <- function(data) {
  chi2 <- as.numeric(chisq.test(data, simulate.p.value = TRUE)$statistic)
  C <- dim(data)[2]
  R <- dim(data)[1]
  sqrt(chi2/(sum(data)*min(R-1, C-1)))
  
}

CramerV(data)
coefficient(data, 2)
crammer(data)

#3
t.czuprow <- function(data) {
  chi2 <- as.numeric(chisq.test(data, simulate.p.value = TRUE)$statistic)
  C <- dim(data)[2]
  R <- dim(data)[1]
  sqrt(chi2/(sum(data)*sqrt((R-1)*(C-1))))
}
t.czuprow(data)
coefficient(data, 3)
TschuprowT(data)


#4
phi <- function(data) {
  chi2 <- as.numeric(chisq.test(data, simulate.p.value = TRUE)$statistic)
  sqrt(chi2/sum(data))
}
phi(data)
coefficient(data, 4)
phi(data)

# 5
pearson <- function(data) {
  chi2 <- as.numeric(chisq.test(data, simulate.p.value = TRUE)$statistic)
  sqrt(chi2/(chi2 + sum(data)))
  
}

pearson(data)
coefficient(data, 5)
ContCoef(data)

coefficient <- function(data, coef) {
  if (coef == 1) {
    goodman(data) 
  } else if (coef == 2) {
    crammer(data)
  } else if (coef == 3) {
    t.czuprow(data)
  } else if (coef == 4) {
    phi(data)
  } else if (coef == 5) {
    pearson(data)
  }
}
coefficient(data, 3)


