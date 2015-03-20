# file: test_monteCarlo.R

test_monteCarlo.data.frame <- function(a=2,b=3){
  # First test with single numeric variable --------------------------------------------------------------
  # Sample size
  n=1000
  # Generate a (0,1) normally distributed random variable
  x <- data.frame(rnorm(n))
  # Define a function f(x) = ax + b:
  f <- function(x,a,b){
    x*a+b
  }
  # Run Monte Carlo simulation
  myMonteCarlo <- monteCarlo.data.frame(x,f,a,b)
  # Print variance of transformed random variable
  print(myMonteCarlo)
  # Plot original and transformed random variable
  h1<-hist(x[,1])
  h2<-hist(myMonteCarlo)
  plot(h2,col='red')
  plot(h1,col=rgb(0, 1, 0, 0.5),add=TRUE)
  
  # Second test with named column vector --------------------------------------------------------------
  # Generate a (0,1) normally distributed random variable
  x1<-data.frame(firstComponent=vector(length=n),secondComponent=vector(length=n))
  x1$firstComponent <- rnorm(n)
  # Create column vector
  
  # Define a function g(x) = ax + b:
  g <- function(x,a,b){
    x["firstComponent"]*a+b
  }
  # Run Monte Carlo simulation
  myMonteCarlo1 <- monteCarlo.data.frame(x=x1,f=g,a,b)
  # Print variance of transformed random variable
  print(myMonteCarlo1)
  # Plot original and transformed random variable
  h1<-hist(x1$firstComponent)
  h2<-hist(myMonteCarlo1)
  plot(h2,col='red')
  plot(h1,col=rgb(0, 1, 0, 0.5),add=TRUE)  
  summary(myMonteCarlo)
  summary(myMonteCarlo1)
}



