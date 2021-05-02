"%+%" <- function(...){
  paste0(...,sep="")
}


setwd("G:\\§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance data")

apw = read.csv("parsed_words_wt.csv")

apw_counts = apw %>% group_by(o1,n1,c1,t1,o2,n2,c2,t2) %>% summarise(count = n())
apw_counts = cbind.data.frame(apw_counts, probs = apw_counts$count / sum(apw_counts$count))
apw_counts = apw_counts %>% mutate(o1star = factor((ifelse(o1 == "","0","O")))) %>% mutate(o2star = factor((ifelse(o2 == "","0","O"))))
apw_counts = apw_counts %>% mutate(n1star = factor((ifelse(n1 == "","0","N")))) %>% mutate(n2star = factor((ifelse(n2 == "","0","N"))))
apw_counts = apw_counts %>% mutate(c1star = factor((ifelse(c1 == "","0","C")))) %>% mutate(c2star = factor((ifelse(c2 == "","0","C"))))
apw_counts = apw_counts %>% mutate(t1star = factor((ifelse(t1 == "","0","T")))) %>% mutate(t2star = factor((ifelse(t2 == "","0","T"))))

apw_counts = cbind.data.frame(id = 1:nrow(apw_counts),apw_counts)

getEntropy = function(data){
  values = unique(data)
  problgprob = function(x){
    prob = length(data[data==x]) / length(data)
    return(prob * log(prob,2))
  }
  entropy = -sum(sapply(values,problgprob))
  return(entropy)
}


overallEntropy = getEntropy(paste(apw$o1,apw$n1,apw$c1,apw$t1,apw$o2,apw$n2,apw$c2,apw$t2))

#Calculating modified entropies, method 1 (faster, not extensible)
apw_noo = apw %>% mutate(o1 = factor((ifelse(o1 == "","0","O")))) %>% mutate(o2 = factor((ifelse(o2 == "","0","O"))))
apw_non = apw %>% mutate(n1 = factor((ifelse(n1 == "","0","N")))) %>% mutate(n2 = factor((ifelse(n2 == "","0","N"))))
apw_noc = apw %>% mutate(c1 = factor((ifelse(c1 == "","0","C")))) %>% mutate(c2 = factor((ifelse(c2 == "","0","C"))))
apw_not = apw %>% mutate(t1 = factor((ifelse(t1 == "","0","T")))) %>% mutate(t2 = factor((ifelse(t2 == "","0","T"))))

entropyWOO = getEntropy(paste(apw_noo$o1,apw_noo$n1,apw_noo$c1,apw_noo$t1,apw_noo$o2,apw_noo$n2,apw_noo$c2,apw_noo$t2))
entropyWON = getEntropy(paste(apw_non$o1,apw_non$n1,apw_non$c1,apw_non$t1,apw_non$o2,apw_non$n2,apw_non$c2,apw_non$t2))
entropyWOC = getEntropy(paste(apw_noc$o1,apw_noc$n1,apw_noc$c1,apw_noc$t1,apw_noc$o2,apw_noc$n2,apw_noc$c2,apw_noc$t2))
entropyWOT = getEntropy(paste(apw_not$o1,apw_not$n1,apw_not$c1,apw_not$t1,apw_not$o2,apw_not$n2,apw_not$c2,apw_not$t2))

fl_o = (overallEntropy - entropyWOO)/overallEntropy
fl_n = (overallEntropy - entropyWON)/overallEntropy
fl_c = (overallEntropy - entropyWOC)/overallEntropy
fl_t = (overallEntropy - entropyWOT)/overallEntropy
fl = c(fl_o,fl_n,fl_c,fl_t)

#Alternative way of finding modified entropies, without relying so much on dplyr
findModifiedEntropy = function(cols = c("o1star","n1","c1","t1","o2star","n2","c2","t2"), df){
  done = logical(nrow(df))
  probs = vector()
  for(i in 1:nrow(df)){
    if(done[i]) next;
    filteredDF = df;
    for(j in 1:length(cols)) filteredDF = filteredDF %>% filter(get(cols[j])==df[i,cols[j]])
    curr_sumprob = sum(filteredDF$probs)
    probs = probs %>% append(curr_sumprob)
    done[filteredDF$id] = T
  }
  entropy = -sum((probs * log(probs,2)))
  return(entropy)
}

findModifiedEntropy(c("o1star","n1","c1","t1","o2star","n2","c2","t2"),apw_counts)


findModifiedEntropyAndDerivative = function(cols = c("o1star","n1","c1","t1","o2star","n2","c2","t2"), df){
  done = logical(nrow(df))
  probs = vector()
  
  lastRowID = nrow(df)
  lastRowGroup = df
    for(j in 1:length(cols)) lastRowGroup = lastRowGroup %>% filter(get(cols[j])==df[lastRowID,cols[j]])
  lastRowProb = sum(lastRowGroup$probs)
  lastLog = log(lastRowProb,2)
  derivatives = numeric(nrow(df)-1)
  
  for(i in 1:nrow(df)){
    if(done[i]) next;
    filteredDF = df;
    for(j in 1:length(cols)) filteredDF = filteredDF %>% filter(get(cols[j])==df[i,cols[j]])
    curr_sumprob = sum(filteredDF$probs)
    probs = probs %>% append(curr_sumprob)
    
    derivatives[filteredDF$id] = lastLog - log(curr_sumprob,2)
    done[filteredDF$id] = T
  }
  derivatives = derivatives[-length(derivatives)]
  entropy = -sum((probs * log(probs,2)))
  
  return(list(entropy = entropy, derivatives = derivatives))
}



findFisher = function(theta, n){
  pk = 1 - sum(theta)
  diagonal = 1/theta
  fisher =  matrix(numeric(length(theta)^2)+1/pk,nrow=length(theta)) +diag(diagonal)
  return(n * fisher)
}

findAlpha = function(data){
  alpha = sapply(c("o","n","c","t"),findAlphaRow,data)
  return(t(alpha))
}

findAlphaRow = function(element,df){
  components = c("o1","n1","c1","t1","o1","n1","c1","t1")
  components = sub(element %+% "(\\d)",element %+% "\\1star",components)
  
  entropy = findModifiedEntropy(c("o1","n1","c1","t1","o2","n2","c2","t2"),df)
  probs = (df$probs)[-finalID]
  probLast = (df$probs)[finalID]
  finalID = nrow(df)
  entropyPrime = log(probLast,2) - log(probs,2)
  
  entropyAndDerivative= findModifiedEntropyAndDerivative(components, df)
  modifiedEntropy = entropyAndDerivative$entropy
  modifiedEntropyPrime = entropyAndDerivative$derivatives
  
  row = ((entropyPrime - modifiedEntropyPrime) * entropy  - (entropy - modifiedEntropy) * entropyPrime ) / (entropy)^2
  return(row)
}

findAlphaRow("n",apw_counts)

alpha = findAlpha(apw_counts)
info = findFisher(apw_counts$probs[-nrow(apw_counts)], nrow(apw))
var = alpha %*% (solve(info)) %*% t(alpha)
cmatrix = t(matrix(c(1,-1,0,0,
                     1,0,-1,0,
                     1,0,0,-1,
                     0,1,-1,0,
                     0,1,0,-1,
                     0,0,1,-1),nrow=4))
var_diffs = cmatrix %*% var %*% t(cmatrix)

estimates_diffs = cmatrix %*% fl
cv = qnorm(1-.05/6)
round(estimates_diffs - cv * sqrt(diag(var_diffs)),8)
round(estimates_diffs + cv * sqrt(diag(var_diffs)),8)
