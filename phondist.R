# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


library(reticulate)
use_python("C:\\Users\\User\\Anaconda3\\python.exe")
fastkde = import("fastkde")
feature.matrix.multivalue = read.csv("cantonese-multifeatures.csv")


fastkde$fastKDE$pdf(array(c(1.1,2.2,3,4,1,1,2,1,2,3,2,4,5)),axes = list(array(c(1,2,3,4,5,6,7,8,9))))

stringdist = function(strings, mode = "default", mode.detailed = c(TRUE, TRUE, TRUE, FALSE),
                      costs = c(1,1,1,2), segment.specific.costs = FALSE, distance.matrix = NULL,
                      scale.to.average.subst.cost = TRUE, algo = "wf", tostrings = NULL,
                      weights = NULL, cross = TRUE) {
  if(!is.list(strings)){
    strings = as.character(strings); #ensure they aren't factors
    strings.list = sapply(strings,strsplit,"")
    strings = strings.list
  }
  if(is.null(tostrings)) tostrings = strings
  if(!is.list(tostrings)){
    tostrings = as.character(tostrings); #ensure they aren't factors
    tostrings.list = sapply(tostrings,strsplit,"")
    tostrings = tostrings.list
  }
  if(length(strings) == 1 & is.null(tostrings)){
    stop("Cannot evaluate distance with only one string.")
  }
  print(tostrings)
  if(!cross & (length(tostrings) != length(strings))){
    stop("strings and tostrings length not equal; please set cross = TRUE.")
  }

  #evaluate and store string lengths.
  string.lengths = integer(length = length(strings))
  tostring.lengths = integer(length = length(strings))
  for(i in 1:length(strings)){
    string.lengths[i] = sum(nchar(strings[[i]]));
  }
  for(i in 1:length(tostrings)){
    print(i)
    tostring.lengths[i] = sum(nchar(tostrings[[i]]));
  }

  #Set up modes
  if(mode == "hamming"){
    mode.detailed = c(FALSE, FALSE, TRUE, FALSE)
  } else if (mode == "levenshtein"){
    mode.detailed = c(TRUE, TRUE, TRUE, FALSE)
  } else if (mode == "damerau" | mode == "damerau-levenshtein"){
    mode.detailed = c(TRUE, TRUE, TRUE, TRUE)
  } else if (mode == "lcd"){
    mode.detailed = c(TRUE, TRUE, FALSE, FALSE)
  } else if (mode == "jaro"){
    mode.detailed = c(FALSE, FALSE, FALSE, TRUE)
  } else {
    if(mode != "default"){
      warning("Unrecognised mode. Will determine metric type using detailed definition.")
    }
  }

  if(any(!mode.detailed[c(1,2)])){
    if(any(string.lengths != tostring.lengths)){
      stop("Some strings are not of the same length. Please allow insertion and deletion.")
    }
  } else {
    if(algo == "hamming"){
      stop("Please don't specify 'hamming' in the algorithm field if you want indel.")
    }
  }

  names(mode.detailed) = c("ins","del","sub","tra");
  names(costs) = c("ins","del","sub","tra");

  #Throw errors if there are problems in input
  if((mode.detailed["ins"] == FALSE | mode.detailed["del"] == FALSE) & mode.detailed["sub"] == FALSE) {
    inventory = sort(strings[[1]])
    if(!all(sapply(strings,function(x) inventory==sort(x)))){
      stop("Under the current mode, all strings must be made of the same characters");
      break;
    }
  }

  if(segment.specific.costs == TRUE){
    if(is.null(distance.matrix)) stop("No distance table provided.")
    if(nrow(distance.matrix) != ncol(distance.matrix)){
      stop("Distance table must be a square matrix.")
    }
    if(is.null(rownames(distance.matrix))) stop("No row labels provided.")
    if(is.null(colnames(distance.matrix))) stop("No column labels provided.")
    if(any(rownames(distance.matrix) != colnames(distance.matrix))){
      stop("Row and column names of the distance table are not identical.")
    }
    if(length(rownames(distance.matrix)) != length(unique(rownames(distance.matrix))) ){
      stop("An element appeared more than once in the table.")
    }
    for(i in 1:nrow(distance.matrix)){
      for(j in 1:i){
        if(i == j){
          if(distance.matrix[i,j] != 0){
            warning(paste("Metric assumption violation: distance from",
                          rownames(distance.matrix)[i],"to itself is nonzero."))
          }
        } else {
          if(distance.matrix[i,j] != distance.matrix[j,i]){
            warning(paste("Metric assumption violation: distance from",
                          rownames(distance.matrix)[i],"to",rownames(distance.matrix)[j],
                          "is not equal to the opposite distance."))
          }
        }
      }
    }
    if(scale.to.average.subst.cost){
      costs[1] = costs[1]/costs[3]*mean(colMeans(distance.matrix))
      costs[2] = costs[2]/costs[3]*mean(colMeans(distance.matrix))
      costs[4] = costs[4]/costs[3]*mean(colMeans(distance.matrix))
    }
  }
  distances = matrix(0, nrow=length(strings), ncol=length(tostrings))
  rownames(distances) = strings; colnames(distances) = tostrings;

  if(algo=="wf"){
    for(s1 in 1:length(strings)){
      for(s2 in 1:length(tostrings)){
        if(!cross & (s1 != s2)) next
        m = length(strings[[s1]])+1
        n = length(tostrings[[s2]])+1
        d = matrix(0, nrow=m, ncol=n)
        rownames(d) = c(" ",strings[[s1]])
        colnames(d) = c(" ",tostrings[[s2]])

        for(i in 1:m) d[i,1] = (i-1) * costs[1]
        for(j in 1:n) d[1,j] = (j-1) * costs[2]

        for(j in 2:n){
          for(i in 2:m){
            #print(paste("ABC:",paste(strings[[s1]][i-1], strings[[s1]][i],
            #                          strings[[s1]][j-1], strings[[s1]][j])
            if(strings[[s1]][i-1] == tostrings[[s2]][j-1]){
              d[i,j] = d[i-1,j-1]
            } else {
              if(segment.specific.costs){
                char1 = rownames(d)[i]
                char2 = colnames(d)[j]
                print(strings[[s1]])
                print(char1)
                print(char2)
                subst.cost = distance.matrix[char1,char2]
              } else {
                subst.cost = costs[3]
              }
              ops = c(d[i,j-1]+costs[1], d[i-1,j]+costs[2], d[i-1,j-1]+subst.cost);
                print(ops)
              if(mode.detailed[4]==TRUE &
                 strings[[s1]][i-1] == tostrings[[s2]][j] &
                 strings[[s1]][i] == tostrings[[s2]][j-1]){
                ops = append(ops,d[i-1,j-1]+costs[4])
              }
              if(!mode.detailed[1] | !mode.detailed[2]) ops = ops[-c(1,2)]
              d[i,j] = min(ops)
            }
          }
          print(d)
        }
        distances[s1,s2] = d[m,n]
      }
    }
  } else if(algo=="hamming"){
    if(is.null(weights)){
      weights = numeric(length(strings[[s1]]))+1
    }
    for(s1 in 1:length(strings)){
      for(s2 in 1:length(tostrings)){
        if(!cross & (s1 != s2)) next
        from.values = strings[[s1]]
        to.values = tostrings[[s2]]
        distances[s1,s2] = sum(as.integer(from.values != to.values) * weights)
      }
    }

  }

  return(distances)
}

stringdist(strings=c("hall","hell"),mode="hamming",tostring=c("hill","hale"),weights=c(1,1,1,2),algo="hamming")

stringToNGram = function(string, n = 2){
  string = paste("-",string,"-",sep="")
  ngrams = sapply(n:nchar(string), function(x) return(substr(string,x-n+1,x)))
}

distance.matrix = read.csv("distances3.csv")
rownames(distance.matrix) = distance.matrix$X
distance.matrix = distance.matrix[,-1]
colnames(distance.matrix)[ncol(distance.matrix)]="0"
for(i in 1:nrow(distance.matrix)) distance.matrix[i,] = as.numeric(distance.matrix[i,])
distance.matrix = as.matrix(distance.matrix)


#stringdist(strings = c("hell","halle","hale","hello","hail"))
stringdist(strings = c("halle","holes"),mode="levenshtein",segment.specific.costs = TRUE,distance.matrix=distance.matrix,costs=c(1,1,1,.5),tostring=c("halle","holes"))
stringdist(strings = c("halle","halel"),mode="damerau",costs=c(1,1,1,1),segment.specific.costs = TRUE,distance.matrix=distances)
#stringdist(strings=c("hlele","helle"),mode="jaro")

getNaturalClassMatrixFromFeatures = function(feature.matrix){
  #'Translated' from SimilarityCalculator.pl
  #Make unitary classes
  unitary.classes = list()
  segments = rownames(feature.matrix)
  features = colnames(feature.matrix)
  for(feature in features){
    unitary.classes[[feature]] = list()
    unitary.classes[[feature]][[1]] = character()
    unitary.classes[[feature]][[2]] = character()
    for(seg in segments){
      if(as.character(feature.matrix[seg,feature]) == "+"){
        unitary.classes[[feature]][[1]] = append(unitary.classes[[feature]][[1]],seg)
      } else if(as.character(feature.matrix[seg,feature]) == "-"){
        unitary.classes[[feature]][[2]] = append(unitary.classes[[feature]][[2]],seg)
      }
      #print(unitary.classes[[feature]])
    }
  }

  #nat.classes.layers = list()
  #nat.classes.layers[[1]] = list()

  getClassIdentity = function(class1,class2){
    return((all(class1 %in% class2) & all(class2 %in% class1)))
  }

  nat.classes = list()
  for(uclass in unitary.classes){
    for(value in c(1,2)){
      print(uclass[[value]])
      #nat.classes.layers[[1]] = append(nat.classes.layers[[1]],list(uclass[[value]]))
      nat.classes = append(nat.classes,list(uclass[[value]]))
    }
  }

  nat.classes = unique(nat.classes)
  base.nat.classes = nat.classes
  nclass = length(base.nat.classes)
  for(i in 1:length(base.nat.classes)){
    j = i+1
    while(j <= nclass){
      cand = intersect(nat.classes[[i]],nat.classes[[j]])
      if(length(cand) > 0){
        repeated = F
        for(class in nat.classes){
          if(getClassIdentity(class,cand)){
            repeated = T
            break
          }
        }
        if(!repeated){
          nat.classes = append(nat.classes,list(cand))
          nclass = nclass + 1
        }
      }
      j = j + 1
    }
  }

  nat.classes = append(nat.classes, list(segments))
  nat.classes = unique(nat.classes)

  classnames = paste("class",seq(1,length(nat.classes),1),sep="")
  natclass.matrix = data.frame(matrix(0,nrow=length(segments),ncol=length(nat.classes)))
  rownames(natclass.matrix) = segments
  colnames(natclass.matrix) = classnames
  for(i in 1:length(nat.classes)){
    for(seg in segments){
      natclass.matrix[seg,i] = c("+","-")[2 - as.integer(seg %in% nat.classes[[i]])]
    }
  }
  natclass.matrix = cbind.data.frame(unlist(sapply(natclass.matrix,as.factor)))
  rownames(natclass.matrix) = segments

  return(natclass.matrix)
}


getClassIdentity = function(class1,class2){
  return((all(class1 %in% class2) & all(class2 %in% class1)))
}
correct.natclasses = read.table("G:\\§Úªº¶³ºÝµwºÐ\Ÿ\\Phonotactics\\celexperl\\similarity\\sample-canto-features.cls",header=F,sep="\t")[,1]

correct.natclasses.list = list()
for(class in correct.natclasses) correct.natclasses.list = append(correct.natclasses.list,list(sort(strsplit(as.character(class),split=NULL)[[1]])))

nat.classes.sorted = list()
for(class in nat.classes) nat.classes.sorted = append(nat.classes.sorted, list(sort(class)))

found = logical(length(correct.natclasses.list))
i=1
for(cclass in correct.natclasses.list){
  for(mclass in nat.classes.sorted){
    if(getClassIdentity(cclass,mclass)){
      found[i] = T
      break;
    }
  }
  i=i+1
}

getDistancesFromFeatures = function(feature.matrix, mode = "hamming", weights = NULL, normalise=FALSE, zeroMultiplier = 1){
  #TODO - Some error-catching with feature matrices go here
  if(is.null(weights)){
    weights = numeric(ncol(feature.matrix))+1
  } else {
    if(length(weights) != ncol(feature.matrix)){
      stop("Number of weights must match number of features")
    }
  }
  segments = rownames(feature.matrix)
  if(length(unique(segments)) < length(segments)){
    stop("One of the segments appeared more than once.")
  }
  if(mode == "hamming"){
    computeDistance = function(seg1_features,seg2_features){
      disequivalences = as.integer(as.character(seg1_features) != as.character(seg2_features))
      print(sum(disequivalences))
      unweighted.dist = disequivalences - (1-zeroMultiplier) * xor(as.character(seg1_features)=="0",as.character(seg2_features)=="0")
      dist = sum(unweighted.dist*weights)
      if(normalise) dist = dist/sum(weights)
      return(dist)
    }
  }
  if(mode == "manhattan"){
    for(i in ncol(feature.matrix)){
      if(!is.numeric(feature.matrix[,i]))  stop("Numeric features are needed for computing Manhattan distance.")
    }
    computeDistance = function(seg1_features,seg2_features){
      dist = sum((abs(seg1_features-seg2_features)*weights))
      return(dist)
    }

  }
  if(mode == "euclidean"){
    for(i in ncol(feature.matrix)){
      if(!is.numeric(feature.matrix[,i]))  stop("Numeric features are needed for computing Euclidean distance.")
    }
    computeDistance = function(seg1_features,seg2_features){
      dist = sqrt(sum(((seg1_features-seg2_features)^2)*weights))
      return(dist)
    }
  }

  if(mode %in% c("natclass","pierrehumbert")){

    computeDistance = function(seg1_features, seg2_features){
      disequivalences = as.integer(as.character(seg1_features) != as.character(seg2_features))
      unweighted.dist = disequivalences - (1-zeroMultiplier) * xor(as.character(seg1_features)=="0",as.character(seg2_features)=="0")
      dist = sum(unweighted.dist*weights)
      if(normalise) dist = dist/sum(weights)
      return(dist)
    }

    old.feature.matrix = feature.matrix
    feature.matrix = natclass.matrix

  }

  distance.matrix = matrix(0,nrow=length(segments),ncol=length(segments))
  rownames(distance.matrix) = segments
  colnames(distance.matrix) = segments
  for(i in 1:nrow(feature.matrix)){
    for(j in 1:nrow(feature.matrix)){
      print(paste(rownames(feature.matrix)[i]," and",rownames(feature.matrix)[j]))
      distance.matrix[i,j] = computeDistance(feature.matrix[i,],feature.matrix[j,])
    }
  }

  if(mode %in% c("euclidean","manhattan") & normalise){
    distance.matrix = distance.matrix / max(distance.matrix)
  }
  return(distance.matrix)
}

feature.matrix= read.table("G:\\§Úªº¶³ºÝµwºÐ\Ÿ\\R packages\\phondist\\sample-canto-features.txt")
getDistancesFromFeatures(feature.matrix,normalise=T,weights=broeweights,zeroMultiplier=.5)
getDistancesFromFeatures(feature.matrix,normalise=T,weights=broeweights)
getDistancesFromFeatures(feature.matrix,normalise=T,weights=naiveweights)

feature.matrix.chaotone = as.matrix(read.csv("cantonese-chao-tl.csv"))
feature.matrix.chaotone[,3] = as.numeric(feature.matrix.chaotone[,3])
feature.matrix.chaotone[,2] = as.numeric(feature.matrix.chaotone[,2])
feature.matrix.chaotone = data.frame(feature.matrix.chaotone)
rownames(feature.matrix.chaotone) = feature.matrix.chaotone$tone
feature.matrix.chaotone = feature.matrix.chaotone[,-1]
getDistancesFromFeatures(feature.matrix.chaotone,mode="euclidean",normalise=T)
getDistancesFromFeatures(feature.matrix.chaotone,mode="manhattan",normalise=T,weights=c(.5,1.5))
getDistancesFromFeatures(feature.matrix.chaotone,mode="euclidean",normalise=T,weights=c(.5,1.5))

feature.matrix.chaotone.combine = paste(feature.matrix.chaotone[,1],feature.matrix.chaotone[,2],sep="")
names(feature.matrix.chaotone.combine) = rownames(feature.matrix.chaotone)
toneletter.differences = matrix(0,nrow=5,ncol=5)
for(i in 1:5) for(j in 1:5) toneletter.differences[i,j] = abs(i-j)
toneletter.differences = data.frame(toneletter.differences)
rownames(toneletter.differences) = seq(1,5,1)
colnames(toneletter.differences) = seq(1,5,1)
getDistancesFromFeatures(feature.matrix.chaotone,mode="manhattan")
stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE
           ,distance.matrix=toneletter.differences,mode="hamming")

getNGramDistances = function(distance.matrix, n = 2, edgeMultiplier = .5){
  #Make sure distance matrix is okay
  checkDistanceMatrix(distance.matrix)
  edgeDist = sum(distance.matrix) / length(distance.matrix) * edgeMultiplier
  distance.matrix = cbind(distance.matrix,numeric(nrow(distance.matrix))+edgeDist)
  distance.matrix = rbind(distance.matrix,numeric(ncol(distance.matrix))+edgeDist)
  distance.matrix[nrow(distance.matrix),ncol(distance.matrix)] = 0
  colnames(distance.matrix)[nrow(distance.matrix)]="-"
  rownames(distance.matrix)[nrow(distance.matrix)]="-"
  print(distance.matrix)
  segments = colnames(distance.matrix)
  recurr = function(gram, n){
    if(n > 0){
      grams = character()
      for(segment in segments){
        grams = append(grams, recurr(paste(gram,segment,sep=""),n-1))
      }
    } else {
      #print(gram)
      grams = gram
    }
    return(grams)
  }
  grams = recurr("",n)
  return(stringdist(grams, costs = c(1,1,.5,2),segment.specific.costs=TRUE,distance.matrix=distance.matrix))
}

getNGramDistances(distance.matrix.two/max(distance.matrix.two))

checkDistanceMatrix = function(distance.matrix){
  if(is.null(distance.matrix)) stop("No distance table provided.")
  if(nrow(distance.matrix) != ncol(distance.matrix)){
    stop("Distance table must be a square matrix.")
  }
  if(is.null(rownames(distance.matrix))) stop("No row labels provided.")
  if(is.null(colnames(distance.matrix))) stop("No column labels provided.")
  if(any(rownames(distance.matrix) != colnames(distance.matrix))){
    stop("Row and column names of the distance table are not identical.")
  }
  if(length(rownames(distance.matrix)) != length(unique(rownames(distance.matrix))) ){
    stop("An element appeared more than once in the table.")
  }
  for(i in 1:nrow(distance.matrix)){
    for(j in 1:i){
      if(i == j){
        if(distance.matrix[i,j] != 0){
          warning(paste("Metric assumption violation: distance from",
                        rownames(distance.matrix)[i],"to itself is nonzero."))
        }
      } else {
        if(distance.matrix[i,j] != distance.matrix[j,i]){
          warning(paste("Metric assumption violation: distance from",
                        rownames(distance.matrix)[i],"to",rownames(distance.matrix)[j],
                        "is not equal to the opposite distance."))
        }
      }
    }
  }
}

compareRowToRows = function(row, rows){
  resultMatrix = matrix(TRUE,nrow=nrow(rows),ncol=ncol(rows))
  for(i in 1:nrow(rows)){
    resultMatrix[i,] = (row == rows[i, ])
  }
  return(resultMatrix)
}

getInfoGainFromWordlist = function(feature.matrix, wordlist, freqs = NULL, mode = "naive", discrete = TRUE){
  if(is.null(wordlist) | is.null(feature.matrix)){
    stop("Information missing!")
  }
  
  #TODO - Some error-catching with feature matrices go here
  #And some error catching with the wordlist, whatever can go wrong with it
  if(!is.null(freqs)) {
    if(length(wordlist) != length(freqs)){
      stop("Worldist and frequencies must be identical in length.")
    }
  } else {
    print("No frequencies provided. Assuming no frequency weighting.")
    freqs = rep(1, length(wordlist))
  }
  #print(freqs)
  #print(rownames(feature.matrix))
  segments = rownames(feature.matrix)
  i=1
  segmentFreqs = numeric(length(segments))
  names(segmentFreqs) = segments
  for(entry in wordlist){
    splitSegments = strsplit(entry,"")[[1]]
    #print(segments)
    #print(splitSegments)
    if(!all(splitSegments %in% segments)){
      unknownSegment = splitSegments[which(!(splitSegments %in% segments))]
      stop(paste("Unknown segment found: ",unknownSegment," in string ",entry))
    }
    segmentFreqs[splitSegments] = segmentFreqs[splitSegments] + freqs[i]
    i = i + 1
  }
  segmentProbs = segmentFreqs / sum(segmentFreqs)
  #print(segmentFreqs)
  #print(segmentProbs)

  if(mode=="naive" & discrete){
    baseEntropy = -sum(segmentProbs[segmentProbs>0] * log2(segmentProbs[segmentProbs>0]))
    #print(paste("base",baseEntropy))
    featureEntropy = numeric(ncol(feature.matrix))
    for(i in 1:ncol(feature.matrix)){
      curr.levels = levels(feature.matrix[,i])
      remainingEntropies = numeric(length(curr.levels))
      currSum = numeric(length(curr.levels))
      print(paste("Reached",curr.levels))
      for(j in 1:length(curr.levels)){
        currFreqs = segmentFreqs[feature.matrix[,i]==curr.levels[j]]
        #print("currFreqs:")
        #print(currFreqs)
        if(all(currFreqs == 0)){
          next;
          stop("Some feature values were never used! You may want to remove unused items from the alphabet.")
        }
        currProbs = currFreqs / sum(currFreqs)
        #print(currProbs)
        remainingEntropies[j] = -sum(currProbs[currProbs>0] * log2(currProbs[currProbs>0]))
        #print(remainingEntropies[j])
        currSum[j] = sum(segmentFreqs[which(feature.matrix[,i]==curr.levels[j])])
        #print(paste("remainingEntropy",remainingEntropies[j]))
        #print(paste("currSum",currSum[j]))
      }
      #print("newit")
      #print(sum(remainingEntropies))
      #print(sum(segmentFreqs))
      #print(sum(currSum))
      featureEntropy[i] = sum(remainingEntropies * currSum / sum(segmentFreqs))
      #print(paste("featureEntropy",featureEntropy[j]))
    }
    #print(featureEntropy)
    infoGain = baseEntropy - featureEntropy
    names(infoGain) = colnames(feature.matrix)
  } else if(mode == "broe" & discrete){
    infoGain = numeric(ncol(feature.matrix))
    names(infoGain) = colnames(feature.matrix)
    pintersect_cum = 0
    for(i in 1:ncol(feature.matrix)){
      #print(colnames(feature.matrix)[i])
      pintersect_cum = 0
      num_cum = 0
      reduced.feature.matrix = unique(feature.matrix)
      for(j in 1:nrow(reduced.feature.matrix)){
        if(as.character(feature.matrix[j,i]) != "0" & !is.na(feature.matrix[j,i])){
          print("start")
          pintersect = sum(segmentProbs[apply(compareRowToRows(as.matrix(reduced.feature.matrix[j,]),as.matrix(feature.matrix[,])),1,all)])
          print(pintersect)
          pcurr = sum(segmentProbs[apply(compareRowToRows(as.matrix(reduced.feature.matrix[j,i]),as.matrix(feature.matrix[,i])),1,all)])
          prest = sum(segmentProbs[apply(compareRowToRows(as.matrix(reduced.feature.matrix[j,-i]),as.matrix(feature.matrix[,-i])),1,all)])
          print(paste("pcurr",pcurr))
          print(prest)
          pintersect_cum = pintersect_cum + pintersect
          print(pintersect_cum)
          print(log2(1 / pcurr))
          num_cum = num_cum + pintersect * log2(1 / pcurr)
          print(paste("numcum",num_cum))
          #print(num_cum)
        }
      }
      infoGain[i] = num_cum / pintersect_cum
    }
  } else if(mode == "naive1" & discrete){ #A slow version of naive
    infoGain = numeric(ncol(feature.matrix))
    names(infoGain) = colnames(feature.matrix)
    pintersect_cum = 0
    for(i in 1:ncol(feature.matrix)){
      num_cum = 0
      reduced.feature.matrix = unique(feature.matrix)
      for(j in 1:nrow(reduced.feature.matrix)){
        pintersect = sum(segmentProbs[apply(compareRowToRows(as.matrix(reduced.feature.matrix[j,]),as.matrix(feature.matrix[,])),1,all)])
        pcurr = sum(segmentProbs[apply(compareRowToRows(as.matrix(reduced.feature.matrix[j,i]),as.matrix(feature.matrix[,i])),1,all)])
        prest = sum(segmentProbs[apply(compareRowToRows(as.matrix(reduced.feature.matrix[j,-i]),as.matrix(feature.matrix[,-i])),1,all)])
        #print("start")
        #print(pintersect)
        #print(pcurr)
        #print(prest)
        #print(pintersect_cum)
        #print(log2(pintersect / pcurr / prest))
        num_cum = num_cum + pintersect * log2(1 / pcurr)
        #print(num_cum)
      } 
      infoGain[i] = num_cum
    }
  } else if(mode == "naive" & !discrete){
    #todo: tokens instead of types
    require(mclust)
    require(mvtnorm)
    wordlist.tokens = Reduce(c, lapply(1:nrow(lexicon), function(x) rep(wordlist[x], freqs[x])))
    wordlist.feat = feature.matrix[Reduce(c, strsplit(wordlist.tokens, "")),]
    feat.gmm = densityMclust(wordlist.feat)
    feat.gmm.means = feat.gmm$parameters$mean
    feat.gmm.vars = feat.gmm$parameters$variance$sigma
    feat.gmm.probs = feat.gmm$parameters$pro
    infoGain =  as.vector(simulateInfoGainFromGMM(feat.gmm.means, feat.gmm.vars, feat.gmm.probs))
    names(infoGain) = names(feature.matrix)
  }
  return(infoGain)
}

simulateInfoGainFromGMM = function(gmm.means, gmm.vars, gmm.probs, nsim = 1000){
  infoGainPerGaussian = sapply(1:ncol(gmm.means), function(x) rowMeans(simulateInfoGainFromGaussian(gmm.means, gmm.vars, gmm.probs, x, nsim)))
  infoGainPerGaussian %*% gmm.probs
}
simulateInfoGainFromGaussian = function(gmm.means, gmm.vars, gmm.probs, i, nsim = 1000){
  sim_data = rmvnorm(nsim, gmm.means[,i], gmm.vars[,,i])
  dnorms = function(y, x) dnorm(sim_data[x,], gmm.means[,y], sqrt(diag(gmm.vars[,,y])))
  pwtInfoGains = sapply(1:nsim, function(x) -log(sapply(1:ncol(gmm.means), dnorms, x) %*% gmm.probs, 2))
  pwtInfoGains
}


lexicon = read.csv("syl_freqs.csv",na.strings = "nincompoop")
naiveweightsprime = getInfoGainFromWordlist(feature.matrix[,1:3],lexicon$klatt,lexicon$freq,mode="naive")
broeweights = getInfoGainFromWordlist(feature.matrix,lexicon$klatt,lexicon$freq,mode="broe")


getInfoGainFromWordlist(feature.matrix.two,wordlist.two,c(2,1,1,1),mode="broe")

getInfoGainFromWordlist(feature.matrix.two,wordlist.two,c(2,1,1),mode="naive1")


naiveweightsprime = getInfoGainFromWordlist(feature.matrix[,1:3],lexicon$klatt,lexicon$freq,mode="naive")
naiveweights = getInfoGainFromWordlist(feature.matrix[,1:3],as.character(lexicon$klatt),lexicon$freq,mode="naive1")



broeweights = getInfoGainFromWordlist(feature.matrix,lexicon$klatt,lexicon$freq,mode="broe")
naiveweights = getInfoGainFromWordlist(feature.matrix,lexicon$klatt,lexicon$freq,mode="naive")
naiveweights2 = getInfoGainFromWordlist(feature.matrix,lexicon$klatt,lexicon$freq,mode="naive")


wordlist = c("nei","lei","nin","man","wat","bAu","biN","pou","Kik","Gok","ziu","ciu","si",
             "hA","dE","lO","zEt","No","fiN","dak","tan","lau","gik","jau","zeu","zYt","but")
freqs = ceiling(runif(27,0,10))
getInfoGainFromWordlist(feature.matrix,wordlist,freqs,mode="broe")

feature.matrix.two = data.frame(matrix(0,nrow=4,ncol=3))
rownames(feature.matrix.two) = c("a","u","i","o")
colnames(feature.matrix.two) = c("hi","bk","rd")
feature.matrix.two[,1] = as.factor(c("-","+","+","-"))
feature.matrix.two[,2] = as.factor(c("+","+","-","+"))
feature.matrix.two[,3] = as.factor(c("-","0","-","+"))
wordlist.two = c("a","u","i","o")
getInfoGainFromWordlist(feature.matrix.two,wordlist.two,c(2,1,1,1),mode="broe")
getInfoGainFromWordlist(feature.matrix.two,wordlist.two,c(2,1,1,1),mode="naive")

feature.matrix.two = data.frame(matrix(0,nrow=4,ncol=3))
rownames(feature.matrix.two) = c("a","u","i","o")
colnames(feature.matrix.two) = c("hi","bk","rd")
feature.matrix.two[,1] = as.factor(c("10","90","100","75"))
feature.matrix.two[,2] = as.factor(c("90","100","20","80"))
feature.matrix.two[,3] = as.factor(c("0","100","0","20"))
wordlist.two = c("a","ue","ie","oe")



distance.matrix.two = getDistancesFromFeatures(feature.matrix.two)

#todo
#getNeighbourhoodModel = function(){
#  neighbourhoodModel = 
#}

hcf = function(num1, num2){
  #Euclidean algorithm
  while(num2){
    dummy = num2
    num2 = num1 %% num2
    num1 = dummy
  }
  return(num1)
}

lcm = function(num1, num2){
  return(num1 * num2 / hcf(num1,num2))
}

spectrumDistance = function(spectrum1, spectrum2){
  k = lcm(ncol(spectrum1),ncol(spectrum2))
  findColDistance = function(i){
    r1 = (i-1) %/% (k/ncol(spectrum1)) + 1
    r2 = (i-1) %/% (k/ncol(spectrum2)) + 1
    return(sum(spectrum1[,r1] - spectrum2[,r2])^2)
  }
  result = sum(sqrt(sapply(1:k, findColDistance)))/k
  return(result)
}

setwd("G:/§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance audio\\sepaudio\\mfcc")
spectrum1 = read.table("normalised_MONO-NEW-1_mfcc.txt",sep="")
spectrum2 = read.table("normalised_MONO-OLD-4_mfcc.txt",sep="")
spectrumDistance(spectrum1,spectrum2)


