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

# compare_outcomes:
compare_outcomes <- function(iterations){
  mean_sd_of_a = array(dim = c(10, 2))
  for (j in 1:10){
    startvalue = c()
    startvalue[1] = runif(1, min=0, max=10)
    startvalue[2] = rnorm(1, sd=5)
    startvalue[3] = runif(1, min=0, max=30)
    chain = array(dim = c(iterations+1, 3))
    chain[1, ] = startvalue
    for (i in 1:iterations){
      proposal = proposalfunction(chain[i, ])
      probab = exp(posterior(proposal) - posterior(chain[i, ]))
      if (runif(1) < probab){
        chain[i+1, ] = proposal 
      } else{
        chain[i+1, ] = chain[i, ]
      }
    }
    mean_sd_of_a[j,] = c(mean(chain[, 1]), sd(chain[, 1]))
  }
  return (mean_sd_of_a)
}

compare_outcomes(1000)
compare_outcomes(10000)
compare_outcomes(100000)



