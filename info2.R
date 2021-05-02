setwd("G:\\My Drive\\Phonotactics (1)\\judgement-experiment\\distance data")

getEntropy = function(data){
  values = unique(data)
  problgprob = function(x){
    prob = length(data[data==x]) / length(data)
    return(prob * log(prob,2))
  }
  entropy = -sum(sapply(values,problgprob))
  return(entropy)
}

aps = read.csv("all_parsed_syls_wt.csv")

getEntropy(aps$o)
getEntropy(aps$n)
getEntropy(aps$c)
getEntropy(aps$t)

apw = read.csv("parsed_words_wt.csv")

getEntropy(paste(apw$o1,apw$o2,sep=""))
getEntropy(paste(apw$n1,apw$n2,sep=""))
getEntropy(paste(apw$c1,apw$c2,sep=""))
getEntropy(paste(apw$t1,apw$t2,sep=""))

getEntropy(paste(apw$o1,sep=""))
getEntropy(paste(apw$n1,sep=""))
getEntropy(paste(apw$c1,sep=""))
getEntropy(paste(apw$t1,sep=""))

getEntropy(paste(apw$o2,sep=""))
getEntropy(paste(apw$n2,sep=""))
getEntropy(paste(apw$c2,sep=""))
getEntropy(paste(apw$t2,sep=""))

library(dplyr)
aps_aug = aps %>% group_by(o,n,c,t) %>% summarise(count = n())
aps_aug = cbind.data.frame(syl = paste(aps_aug$o, aps_aug$n, aps_aug$c, aps_aug$t, sep=""), aps_aug)
probs = aps_aug$count / sum(aps_aug$count)
theta = probs[-length(probs)]

aps_o = aps %>% group_by(o) %>% summarise(count = n())
aps_n = aps %>% group_by(n) %>% summarise(count = n())
aps_c = aps %>% group_by(c) %>% summarise(count = n())
aps_t = aps %>% group_by(t) %>% summarise(count = n())
aps_sep = list(o=aps_o,n=aps_n,c=aps_c,t=aps_t)

findFisher = function(theta, n){
  pk = 1 - sum(theta)
  diagonal = 1/theta
  fisher =  matrix(numeric(length(theta)^2)+1/pk,nrow=length(theta)) +diag(diagonal)
  return(n * fisher)
}

findAlpha = function(aps_sep, aps_aug){
  components = c("o","n","c","t")
  alpha_prime = sapply(components, function(x) return(findAlphaRow(aps_sep[[x]],aps_aug,x)))
  return(alpha_prime[-nrow(alpha_prime),])
}

findAlphaRow = function(counts, data, element){
  lastSyl = data[nrow(data),]
  lastProb = counts[pull(counts,element)==pull(lastSyl, element),"count"]/sum(counts$count)
  findValueProb = function(value){
    if(value == pull(lastSyl, element)[1]){
      return(0)
    } else{
      prob = counts[pull(counts,element)==value,"count"]/sum(counts$count)
      return(log(lastProb,2)-log(prob,2))
    }
     
  }
  alpha = unlist(sapply(pull(data,element),findValueProb),use.names=F)
  return(alpha)
}

alpha_monosyl = findAlpha(aps_sep, aps_aug)
info_monosyl = findFisher(theta, nrow(aps))
var_monosyl = t(alpha_monosyl) %*% (solve(info_monosyl)) %*% alpha_monosyl
cmatrix = t(matrix(c(1,-1,0,0,
                     1,0,-1,0,
                     1,0,0,-1,
                     0,1,-1,0,
                     0,1,0,-1,
                     0,0,1,-1),nrow=4))
var_diffs_monosyl = cmatrix %*% var_monosyl %*% t(cmatrix)
chisq.cutoff = qchisq(.95,3)
estimates = c(getEntropy(aps$o),getEntropy(aps$n),getEntropy(aps$c),getEntropy(aps$t))
estimates_diffs = cmatrix %*% estimates
xinvsumx = t(estimates_diffs) %*% solve(var_diffs_monosyl) %*% estimates_diffs
invsumx = solve(var_diffs_monosyl) %*% estimates_diffs
xinvsum = t(estimates_diffs) %*% solve(var_diffs_monosyl)

Aprime = var_diffs_monosyl / chisq.cutoff
B = eigen(Aprime)$vectors
axes = eigen(Aprime)$values
#713984.4 - 2 * 507670.0 * x - 2 * 578236.7 * y - 2 * 569337.6 * z + 383655.5 * x^2 + 655305.4 * y^2 + 819617.3 * z^2 + 2 * 352274.0 * x * y + 2 * 322912.5 * x * z + 2 * 624813.7 * y * z = 7.814728
#[x, y, z] = ellipsoid(1.022414, 0.199607, 0.139663, 0.23363782, 0.09330904, 0.01277085);
#hMesh = mesh(x, y, z);
#rotate(hMesh, [0 1 0], 45);
#[X, Y, Z] = meshgrid(-5:5, -5:5, -5:5);
#isosurface(X, Y, Z, 383655.5*X.^2 + 655305.4*Y.^2 + 819617.3*Z.^2  + 2 * 352274.0 * X.*Y + 2 * 322912.5*X.*Z + 2 * 624813.7 *Y.*Z  - 2 * 507670.0*X - 2 * 578236.7 * Y - 2 * 569337.6 *Z, 7.814728-713984.4);

#CIs
cv = qnorm(1-.05/6)
estimates_diffs - cv * sqrt(diag(var_diffs_monosyl))
estimates_diffs + cv * sqrt(diag(var_diffs_monosyl))

apw$c1 = as.factor(sapply(apw$c1,function(x) if(x=="") return("0") else return(as.character(x))))
apw$c2 = as.factor(sapply(apw$c2,function(x) if(x=="") return("0") else return(as.character(x))))
apw$o1 = as.factor(sapply(apw$o1,function(x) if(x=="") return("0") else return(as.character(x))))
apw$o2 = as.factor(sapply(apw$o2,function(x) if(x=="") return("0") else return(as.character(x))))

apw_aug = apw %>% group_by(o1,n1,c1,t1,o2,n2,c2,t2) %>% summarise(count = n())
apw_aug = cbind.data.frame(paste(apw_aug$o1, apw_aug$n1, apw_aug$c1, apw_aug$t1, apw_aug$o2, apw_aug$n2, apw_aug$c2, apw_aug$t2, sep=""), apw_aug)
probs_di = apw_aug$count / sum(apw_aug$count)
theta_di = probs_di[-length(probs_di)]

apw_o = apw %>% group_by(o1, o2) %>% summarise(count = n())
apw_n = apw %>% group_by(n1, n2) %>% summarise(count = n())
apw_c = apw %>% group_by(c1, c2) %>% summarise(count = n())
apw_t = apw %>% group_by(t1, t2) %>% summarise(count = n())
apw_o = cbind.data.frame(o = paste(apw_o$o1, apw_o$o2, sep=""), apw_o)
apw_n = cbind.data.frame(n = paste(apw_n$n1, apw_n$n2, sep=""), apw_n)
apw_c = cbind.data.frame(c = paste(apw_c$c1, apw_c$c2, sep=""), apw_c)
apw_t = cbind.data.frame(t = paste(apw_t$t1, apw_t$t2, sep=""), apw_t)
apw_aug = cbind.data.frame(o = paste(apw_aug$o1, apw_aug$o2, sep=""), apw_aug)
apw_aug = cbind.data.frame(n = paste(apw_aug$n1, apw_aug$n2, sep=""), apw_aug)
apw_aug = cbind.data.frame(c = paste(apw_aug$c1, apw_aug$c2, sep=""), apw_aug)
apw_aug = cbind.data.frame(t = paste(apw_aug$t1, apw_aug$t2, sep=""), apw_aug)



apw_sep = list(o=apw_o,n=apw_n,c=apw_c,t=apw_t)

alpha_disyl = findAlpha(apw_sep, apw_aug)
info_disyl = findFisher(theta_di, nrow(apw))
var_disyl = t(alpha_disyl) %*% (solve(info_disyl)) %*% alpha_disyl
var_diffs_disyl = cmatrix %*% var_disyl %*% t(cmatrix)
chisq.cutoff = qchisq(.95,3)
estimates = c(getEntropy(paste(apw$o1,apw$o2,sep="")),
              getEntropy(paste(apw$n1,apw$n2,sep="")),
              getEntropy(paste(apw$c1,apw$c2,sep="")),
              getEntropy(paste(apw$t1,apw$t2,sep="")))
estimates_diffs = cmatrix %*% estimates
xinvsumx = t(estimates_diffs) %*% solve(var_diffs_disyl) %*% estimates_diffs
invsumx = solve(var_diffs_disyl) %*% estimates_diffs
xinvsum = t(estimates_diffs) %*% solve(var_diffs_disyl)

#Aprime = var_diffs_disyl / chisq.cutoff
#B = eigen(Aprime)$vectors
#axes = eigen(Aprime)$values
#hMesh = mesh(x, y, z);
#rotate(hMesh, [0 1 0], 45);
#[X, Y, Z] = meshgrid(-5:5, -5:5, -5:5);
#isosurface(X, Y, Z, 383655.5*X.^2 + 655305.4*Y.^2 + 819617.3*Z.^2  + 2 * 352274.0 * X.*Y + 2 * 322912.5*X.*Z + 2 * 624813.7 *Y.*Z  - 2 * 507670.0*X - 2 * 578236.7 * Y - 2 * 569337.6 *Z, 7.814728-713984.4);

#CIs
estimates_diffs - cv * sqrt(diag(var_diffs_disyl))
estimates_diffs + cv * sqrt(diag(var_diffs_disyl))
