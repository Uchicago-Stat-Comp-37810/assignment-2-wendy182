### Create test data
# assign value 5 as the true value of A
trueA <- 5
# assign value 0 as the true value of B
trueB <- 0
# assign value 10 as the true value of Sd
trueSd <- 10
# make the sample size as 31
sampleSize <- 31
# create independent x-values, they are -15:15
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0, sd)
y <- trueA*x + trueB + rnorm(n = sampleSize, mean = 0, sd = trueSd)
# plot (x,y) to have a look at the test data
plot(x, y, main = "Test Data")

### Derive the likelihood function
# likelihood is a function with parameters including 3 elements.
likelihood <- function(param) {
# assign the 1st, 2nd, and 3rd element of param to a, b, and sd
  a = param[1]
  b = param[2]
  sd = param[3]
# use these a,b,sd value to calculate predicted value of y  
  pred = a*x + b
# gives the log of probability of observing y
# given the normal distibution of y under a, b, and sd values given by param)
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
# given sum of all log probabilities of observing each y. 
# this is equivalent to the joint probability of observing each y in our test data. 
  sumll = sum(singlelikelihoods)
  return(sumll)
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x,trueB,trueSd)))}
slopelikelihoods <- lapply(seq(3,7, by =.05),slopevalues)
plot(seq(3,7,by = .05),slopelikelihoods,type="l",xlab = "value of slope parameter a")

### Defining the prior
# prior is a function returning the probability of noticing a, b, sd with value shown in param
# based on their original distribution that we assumed. 
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
# calculate log(probability of a = param[1], given its uniform distribution)
  aprior = dunif(a, min = 0, max = 10, log = T)
# calculate log(probability of b = param[2], given its normal distribution)
  bprior = dnorm(b, sd = 5, log = T)
# calculate log(probability of sd = param[3], given its uniform distribution)
  sdprior = dunif(sd, min = 0, max = 30, log = T)
# return the joint probability of observing a, b, sd as the value given by param
  return (aprior+bprior+sdprior)
}

### The posterior
# posterior is function with param as input paramters. 
# it returns the product of prior and likelihood
# which is the target function used in following MCMC algorithm. 
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

### The Metropolis-Hastings algorithm
# draw random samples of a, b, sd from a normal dist
proposalfunction <- function(param) {
  return(rnorm(3, mean = param, sd = c(0.1, 0.5, 0.3)))
}

# run_metropolis_MCMC is a function returns a list of sampling values of a, b, and sd.
run_metropolis_MCMC <- function(startvalue, iterations) {
# create an empty array with (iterations+1) rows, and 3 columns (for a, b, and sd)
  chain = array(dim = c(iterations+1, 3))
# the first row of this array is just the starvlue we give. 
  chain[1, ] = startvalue
# do iterations 
  for (i in 1:iterations){
# call proposalfunction to work on each row of chain
    proposal = proposalfunction(chain[i, ])
# the probab of jumping to the new point with a probability p(proposal)/p(chain[i,])
# since we use log(probability) here, it can be calculated as exp(diff)   
    probab = exp(posterior(proposal) - posterior(chain[i, ]))
# generate a uniform random variable on [0,1], if it is smaller than probab
# assign proposal to the next row of chain
    if (runif(1) < probab){
      chain[i+1, ] = proposal
    }else {
# otherwise, keep the value of next row of chain the same as this row. 
      chain[i+1, ] = chain[i, ]
    }
  }
  return(chain)
}

# test the MCMC using start value of a = 4, b = 0, sd = 10
startvalue = c(4,0,10)
# call the run_metropolis_MCMC function to work on this startvalue, and interate 10000 times. 
chain = run_metropolis_MCMC(startvalue, 10000)

# create a new variable called burnIn with value of 5000 (half of iterations) 
burnIn = 5000
# the acceptance rate is calculated as the 
# 1-(# of repated rows after 5000th row compared with previous rows of chain)
acceptance = 1- mean(duplicated(chain[-(1:burnIn), ]))

### Plot the result
# divide the whole frame into 2*3 subframe. 
par(mfrow = c(2,3))
# plot the histogram of the value of a got from MCMC (start at the 5001th iteration)
hist(chain[-(1:burnIn), 1], nclass=30, main="Posterior of a", xlab="True value = red line")
# add a verical line to show the mean value of posterior of a 
abline(v=mean(chain[-(1:burnIn), 1]))
# add a vertical value with color of red to show the true value of a (=5)
abline(v=trueA,col="red")
# Similar to comments above for a 
hist(chain[-(1:burnIn),2], nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v=mean(chain[-(1:burnIn), 2]))
abline(v=trueB, col="red")
hist(chain[-(1:burnIn),3], nclass=30, main="Posterior of sd", xlab="True value =red line")
abline(v=mean(chain[-(1:burnIn),3]))
abline(v=trueSd, col="red")
# plot each point of sampled a got from MCMC (start at the 5001th iteration)
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a")
# add a horizontal line to show the true value of a (=5)
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b")
# similar to comments above for a
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd")
abline(h = trueSd, col="red" )

# for comparison:
# returns the results of linear regression with y as the dependent variable and x as the independent variable. 
summary(lm(y~x))