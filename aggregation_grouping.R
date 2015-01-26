
install.packages("plyr")
set.seed(42)   # fix seed so that you get the same results
dat <- data.frame(assetclass=sample(LETTERS[1:5], 20, replace=TRUE), 
                   return=rnorm(20), assets=1e7+1e7*runif(20))
library(plyr)
ddply(dat, .(assetclass),   # so by asset class invoke following function
       function(x) data.frame(wret=weighted.mean(x$return, x$assets)))
dat

# Summarize a dataset by two variables
require(plyr)
dfx <- data.frame(
  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
  sex = sample(c("M", "F"), size = 29, replace = TRUE),
  age = runif(n = 29, min = 18, max = 54)
)

# Note the use of the '.' function to allow
# group and sex to be used without quoting
ddply(dfx, .(group, sex), summarize,
      mean = round(mean(age), 2),
      sd = round(sd(age), 2))

findInterval

x <- 2:18
v <- c(5, 10, 15) # create two bins [5,10) and [10,15)
cbind(x, findInterval(x, v))

N <- 100; N
X <- sort(round(stats::rt(N, df = 2), 2)); X
tt <- c(-100, seq(-2, 2, len = 201), +100); tt
it <- findInterval(tt, X); it
tt[it < 1 | it >= N] # only first and last are outside range(X)
