
### likelihood function
likelihood <- function(param) {
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

### prior function
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min = 0, max = 10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min = 0, max = 30, log = T)
  return (aprior+bprior+sdprior)
}


### posterior function
posterior <- function(param){
  return (likelihood(param) + prior(param))
}


### The Metropolis-Hastings algorithm
proposalfunction <- function(param) {
  return(rnorm(3, mean = param, sd = c(0.1, 0.5, 0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations) {
  chain = array(dim = c(iterations+1, 3))
  chain[1, ] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i, ])
    probab = exp(posterior(proposal) - posterior(chain[i, ]))
    if (runif(1) < probab){
      chain[i+1, ] = proposal
    }else {
      chain[i+1, ] = chain[i, ]
    }
  }
  return(chain)
}


### Plot the result
plot_chain<- function(chain, burnIn, truevalue, paramname){
  par(mfrow=c(2,3))
  for (i in 1:3){
    hist(chain[-(1:burnIn),i],nclass=30, main=paste("Posterior of", paramname[i]), xlab="True value = red line")
    abline(v=mean(chain[-(1:burnIn), i]))
    abline(v=truevalue[i],col="red")
  }
  for (j in 1:3){
    plot(chain[-(1:burnIn),j], type="l", xlab="True value = red line", main=paste("Chain value of", paramname[j]))
    abline(h = truevalue[j], col="red")
  }
}