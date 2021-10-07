###### Pratical 1 Group 64 #####
### Group Member: HOU Xinyu(s2145863), LI Maeve(LI Minqing)(s2167017), WU Di(s2176435) ###

## 3
setwd("E:/Mae/EDIN/Stat Programming (SP)/W1/Practical 1") 
a <- scan("1581-0.txt",what="character",skip=156) ## read file
n <- length(a)
a <- a[-((n-2909):n)] ## strip license (the last 2909 lines).
a ## check out what is in a

## 4
## create a function that separate the punctuation marks
split_punct <- function(inputvec,punct){
  ipunct <- grep(punct,inputvec,fixed=TRUE)  ##find the indices of the word containing the object punctuation mark
  newvec <- rep("",length(inputvec)+length(ipunct)) ##new vector to store the words and the separated punctuation marks
  iipunct <- ipunct + (1:length(ipunct)) ## calculate the new indices of the objective punctuation marks in the new vector
  newvec[iipunct] <- rep(punct,length(ipunct)) ##put the punctuation marks in derived indices
  newvec[-iipunct] <- gsub(punct,"",inputvec,fixed =TRUE) ##remove the punctuation mark of words containing it and put them in the rest of the blank spaces
  return(newvec)
}


## 5
a1 <- split_punct(a,",")
a2 <- split_punct(a1,".")
a3 <- split_punct(a2,";")
a4 <- split_punct(a3,"!")
a5 <- split_punct(a4,":")
anew <- split_punct(a5,"?") ## separate the punctuation marks one by one mentioned in Q5


## 6.a
alower <- tolower(anew) ## replace the capital letters with lower case
uniq <- unique(alower)  ## find a unique list of words
## 6.b
index_vector <- match(alower,uniq) ## vector of indices, each representing which element in uniq it corresponds to
## 6.c
occurences <- tabulate(index_vector) ## find the frequency of each unique word in the text
## 6.d
thres = 100   ## set up the initial threshold value (i.e. collecting unique words that occurred more than 100 times)
#Search for the threshold
for (i in 1:100){
  ii <- which(occurences>thres) ## save the indices of unique words that occurs more than the threshold value
  if (length(ii) <= 995) 
    thres = thres - 1  ## not enough words, lower threshold
  else if (length(ii)>=1005)
    thres = thres + 1  ## too many words, lift up threshold
  else break
}
length(ii) ## 1004, approximately equal to 1000
thres  ## threshold now 89
##6.e
b <- uniq[ii] ## the indices from the last loop are what we want for the most common approximately 1000 words, stored in vec b


##7.a
vec0 <- match(alower,b) ## 'full text -> most common words' index correspondence
##7.b
firstcolumn <- vec0[1:length(alower)-1] ## remove the last entry
secondcolumn <- vec0[2:length(alower)]  ## remove the first entry
tcmatrix <- cbind(firstcolumn, secondcolumn) ## pairs of subsequent words
##7.c
nas = rowSums(is.na(tcmatrix)) ## count the number of NA in each row
iina <- which(nas>0) ## obtain the indices of row containing NA
tcmatrix1 <- tcmatrix[-iina,] ## drop the NA rows
##7.d
A <- matrix(0,length(ii),length(ii)) ## create a zero matrix for later to store the occurrences of word pairs
for (i in 1:nrow(tcmatrix1)){
  i1 <- tcmatrix1[i,1]
  j1 <- tcmatrix1[i,2]
  A[i1,j1] = A[i1,j1] + 1 ## for each time a word pair occurs, add 1 to A[i1,j1]
}
#View(A) (not run)
##7.e
A1 <- matrix(0,length(ii),length(ii)) ## create a zero matrix for later to store the probability of each word pair occurring
for (i in 1:nrow(A)){
  for (j in 1:ncol(A)){
    A1[i,j] = A[i,j] / sum(A[i,]) ## calculate the probability
  }
}


##8
w <- vector(length = 50) ## create a vector for later to store the generated words' indices
for (i in 1:50){
  if (i == 1)
    w[i] <- sample(length(b), size = 1) ## start by generating a random word's index from our common word vector
  else
    w[i] <- sample(length(b), size = 1, prob = A1[w[i-1],]) ## generate the next word following the probability in the matrix
}
cat(b[w])


##9
bnew=b
for(i in 1:length(b)){
  first=toupper(substr(b[i],1,1)) ## Capitalize the first letter in b[i]
  end=substring(b[i],2) ## the rest of b[i]
  bnew[i]=paste(first, end, sep = "") ## combine them and store them in a new vector bnew, which is a vector of the same common words but with their initials capitalized
}

lower <- match(anew,b) ## full text -> most common words' index correspondence
lower_occurences=numeric() ## initialize numeric vector
for(i in 1:length(b)){
  lower_ <- length(which(lower==i)) ## how many times the word with index i occurred
  lower_occurences <- c(lower_occurences,lower_)
}

upper <- match(anew,bnew) ## full text -> most common words initials capitalized' index correspondence
upper_occurences = numeric()
for(i in 1:length(b)){
  upper_ <- length(which(upper==i)) ## how many times the word with index i occurred
  upper_occurences <- c(upper_occurences,upper_)
}

bb=character() ##initialize character vector
## Loop: compare the frequency of each initial-capitalized word and lowercase word 
## and choose the one with the higher frequency, add to the character vector bb
for(i in 1:length(b)){
  if(lower_occurences[i] < upper_occurences[i]){
    bb=c(bb,bnew[i])
  }else{
    bb=c(bb,b[i])
  }
}

##the generation process is same as before
ww <- vector(length = 50) 
for (i in 1:50){
  if (i == 1)
    ww[i] <- sample(length(bb), size = 1) 
  else
    ww[i] <- sample(length(bb), size = 1, prob = A1[ww[i-1],])
}
cat(bb[ww])
