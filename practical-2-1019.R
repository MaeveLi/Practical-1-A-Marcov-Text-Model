##### Practical 2 #####

Zoesimu <- function(n=5.5e6,ne=10,nt=150,lambda=0.4/n,gamma=1/3,delta=1/5){
## Zoesimu stochastic simulation model.
## n = population size; ne = initially exposed; nt = number of days;
## lambda is the overall viral infectivity parameter
## S stands for susceptible; E for exposed; I for infected;
## L for leaving the state of infected;
## gamma = daily prob E -> I; delta = daily prob I -> L;
  x <- rep(0,n) ## initialize to susceptible state
  beta <- rlnorm(n,0,0.5); beta<-beta/mean(beta)
  ilowest <- order(beta)[1:5.5e5]
  isample <- sample(length(x),5500)
  x[1:ne] <- 1
  DNew <- DNew_lowest <- DNew_sample <- S <- E <- I <- L <- rep(0,nt)
  S[1] <- n-ne; E[1] <- ne
  for (i in 2:nt) {
    u <- runif(n)
    x[x==2&u<delta] <- 3 ## I -> L with prob delta
    
    xlow <- x[ilowest]
    xsample <- x[isample]
    dnew <- length(x[x==1&u<gamma])
    dnew_lowest <- length(xlow[xlow==1&u[ilowest]<gamma])
    dnew_sample <- length(xsample[xsample==1&u[isample]<gamma])
    x[x==1&u<gamma] <- 2 ## E -> I with prob gamma
    
    iinfected <- which(x==2)
    x[x==0&u<lambda*sum(beta[iinfected])*beta] <- 1 ## S -> E with prob
     
    
    S[i] <- sum(x==0); E[i] <- sum(x==1)
    I[i] <- sum(x==2); L[i] <- sum(x==3)
    DNew[i] <- dnew; DNew_lowest[i] <- dnew_lowest; DNew_sample[i] <- dnew_sample
  }
  list(DNew=DNew,DNew_lowest=DNew_lowest,DNew_sample=DNew_sample)
}

epi <- Zoesimu() ## run simulation

