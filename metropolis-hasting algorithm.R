source("C:\\Users\\xwj93\\Documents\\GitHub\\assignment-2-wendy182\\all the functions.R")

### Create test data
trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
y <- trueA*x + trueB + rnorm(n = sampleSize, mean = 0, sd = trueSd)
plot(x, y, main = "Test Data")

startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000
acceptance = 1- mean(duplicated(chain[-(1:burnIn), ]))

plot_chain(chain, burnIn, c(5,0,10),c("a","b","sd"))

# for comparison:
summary(lm(y~x))