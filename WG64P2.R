#### Practical 2 Group 64: Xinyu HOU(s2145863), Maeve LI(Minqing LI)(s2167017), Di WU(s2176435)
#### Github repo: https://github.com/MaeveLi/SP-Practicals-Group-64

## Overview: the aim is to simulate epidemics and compare the reconstructed daily infection trajectories 
## of the 10% of the population with the lowest individual transmission probabilities (considered 
## to be the more cautious group of people, representing the ZOE app data) to those of the whole population 
## and a randomly generated 0.1% sample. Interpret the results and study the representativeness of such a group. 

Zoesimu <- function(n=5.5e6,ne=10,nt=150,lambda=0.4/n,gamma=1/3,delta=1/5){
  ## Zoesimu stochastic simulation model:
  ## An epidemic model that simulates the whole process in a given population, 
  ## and outputs a list of the daily new infections in the entire population, 10% of the population 
  ## with the lowest contact rate with other people and a random sample of 0.1% of the population respectively.
  
  ## inputs:
  ## n = population size (here set to the population of Scotland)
  ## ne = initially exposed; nt = number of days;
  ## lambda is the overall viral infectivity parameter
  ## S stands for susceptible; E for exposed; I for infectious;
  ## L for leaving the state of infected;
  ## gamma = daily prob E -> I; delta = daily prob I -> L;
  
  x <- rep(0,n) ## initialize to susceptible state
  beta <- rlnorm(n,0,0.5); beta<-beta/mean(beta) ## contact rate beta i values generated
  ilowest <- order(beta)[1:5.5e5] ## 10% people with the lowest contact rate selected
  isample <- sample(length(x),5500) ## 0.1% sample randomly generated
  index <- sample(length(x),ne)
  x[index] <- 1 ## randomly choose ne people initialized to exposed state 
  DNew <- DNew_lowest <- DNew_sample <- rep(0,nt)
  DNew[1] <- ne
  DNew_lowest[1] <- length(index[index %in% ilowest])
  DNew_sample[1] <- length(index[index %in% isample]) ## first day infection in respective groups.
  for (i in 2:nt) { ## loop over days
    u <- runif(n) ## uniform random deviates
    x[x==2&u<delta] <- 3 ## I -> L with prob delta
    x[x==1&u<gamma] <- 2 ## E -> I with prob gamma
    
    iinfectious <- which(x==2) ## index of people in I
    idnew <- which(x==0&u<lambda*sum(beta[iinfectious])*beta) ## index of new infections(S->E) with prob lambda*sum(beta[iinfectious])*beta
    idnew_lowest <- intersect(idnew,ilowest)
    idnew_sample <- intersect(idnew,isample) ## index of new infections in respective groups
    dnew <- length(idnew)
    dnew_lowest <- length(idnew_lowest)
    dnew_sample <- length(idnew_sample) ## the number of new infections in respective groups
    x[idnew] <- 1 ## S -> E
    
    DNew[i] <- dnew; DNew_lowest[i] <- dnew_lowest; DNew_sample[i] <- dnew_sample
  }
  list(DNew=DNew, DNew_lowest=DNew_lowest, DNew_sample=DNew_sample)
}

par(mfcol=c(2,5),mar=c(4,4,1,1)) ## create an output format for multiple plots
for (i in 1:10) { ## A loop then generates 10 simulations and add each plot of the simulation to the above format  
  epi <- Zoesimu() ## run simulation
  ## plot each trajectory
  plot(epi$DNew, ylim=c(0,max(epi$DNew)+25000), xlab="Day", ylab="Daily New Infections") 
  lines((epi$DNew_lowest)*10, type='p', col="royalblue3") ## DNew_lowest(blue)
  lines((epi$DNew_sample)*1000, type='p', col=2) ## DNew_sample(red)
  ## form dashed lines that passes through the peaks respectively
  abline(v=which(epi$DNew==max(epi$DNew)), col = "dimgray", lwd=1, lty=2)
  abline(v=which(epi$DNew_lowest==max(epi$DNew_lowest)), col="lightskyblue", lwd=1, lty=2)
  abline(v=which(epi$DNew_sample==max(epi$DNew_sample)), col="coral4", lwd=1, lty=2)
  ## Mark the day each trajectory peaks
  text(which(epi$DNew==max(epi$DNew)), max(epi$DNew), which(epi$DNew==max(epi$DNew)), pos=4)
  text(which(epi$DNew_lowest==max(epi$DNew_lowest)), max(epi$DNew_lowest)*10, 
       which(epi$DNew_lowest==max(epi$DNew_lowest)), pos=4, col="royalblue3")
  text(which(epi$DNew_sample==max(epi$DNew_sample)), max(epi$DNew_sample)*1000, 
       which(epi$DNew_sample==max(epi$DNew_sample)), pos=4, col=2)
  ## print the legends on the right of every plot
  text(90, 160000, "whole population", pos=4, col=1, cex=.6)
  text(90,150000, "cautious 10% * 10", pos=4, col="royalblue3", cex=.6)
  text(90,140000, "0.1% random sample * 1000", pos=4, col=2, cex=.6)
}

## From the result of simulation on the plot with standardized figures, we can find that the infection 
## trajectory of the 0.1% random sample can almost coincide with that of the whole population. However, there are 
## obvious differences between the infection trajectory of the most cautious 10% of the population which we use to 
## represent the users of ZOE and the trajectories of the whole population. It has a lower peak value 
## of new infections per day and lagging epidemic development than the others, as people takes more precautions in this group.
## It implies that for the whole population, this part of the population is not highly representative, which means 
## that the infection trajectory established by ZOE cannot clearly and accurately represent the infection trajectory of the whole population.


