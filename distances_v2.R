library(lme4)
library(reshape2)
library(ggplot2)
library(dplyr)
library(MASS)
library(cowplot)
library(brms)
library(readr)
library(rstan)
library(bayesplot)

#Load the data, put judgements and orders in the relevant tables
setwd("G:\\§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance data")
data = read.csv("jul2data.csv",stringsAsFactors = FALSE)
data = data[data$DistributionChannel != "preview",]
data = data[data[,"ResponseId"] != "R_qLrGDBTgQwXKpYR",] #This is a half-done one by mum - IE went crazy halfway and she's done a complete one later
dist_judgements = data[-c(1,2),c(paste("M",seq(1,72,1),"_1",sep=""),paste("D",seq(1,72,1),"_1",sep=""))]
orders = data[-c(1,2),"Mainsession_DO"]

#Remove empty rows, since they're useless
judgement_empty = logical(nrow(dist_judgements))
for(i in 1:nrow(dist_judgements)) judgement_empty[i] = all(dist_judgements[i,]=="")
dist_judgements = dist_judgements[!judgement_empty,]
orders = orders[!judgement_empty]

#Turn the -99 into 0s
for(i in 1:nrow(dist_judgements)){
  for(j in 1:ncol(dist_judgements)) if(dist_judgements[i,j]=="-99") dist_judgements[i,j] = 0
}

#Create a version of dist_judgements with just the reasonably complete results
judgement_nearempty = logical(nrow(dist_judgements))
for(i in 1:nrow(dist_judgements)) judgement_nearempty[i] = (sum(dist_judgements[i,]=="") >= 36)
dist_judgements_completish = dist_judgements[!judgement_nearempty,]

#A version with complete ones only
judgement_someempty = logical(nrow(dist_judgements))
for(i in 1:nrow(dist_judgements)) judgement_someempty[i] = any(dist_judgements[i,]=="")
dist_judgements_complete = dist_judgements[!judgement_someempty,]


#Turn characters into strings and similarities into distances
for(i in 1:ncol(dist_judgements)) dist_judgements[,i] = as.numeric(dist_judgements[,i])
dist_judgements = 1-dist_judgements/100

#Assign participant IDs
participants = paste("P",seq(1,nrow(dist_judgements),1),sep="")
dist_judgements = cbind.data.frame(participants,dist_judgements)
orders = cbind.data.frame(participants,orders)
colnames(dist_judgements)[1] = "participant"
colnames(orders)[1] = "participant"

dist_judgements_completish = cbind.data.frame(participants[!judgement_nearempty],dist_judgements_completish)
colnames(dist_judgements_completish)[1] = "participant"

#Melt judgements frame
dist_judgements_melted = melt(dist_judgements, id.vars = "participant", value.name="distance",na.rm=TRUE)


#Load distance data and put into judgements frame
mono_theory_dists = read.csv("distances-monosyl.csv",stringsAsFactors = FALSE)[1:72,]
di_theory_dists = read.csv("distances-disylitems.csv",stringsAsFactors = FALSE)[1:72,]
item_names = c(paste("M",seq(1,72,1),"_1",sep=""),paste("D",seq(1,72,1),"_1",sep=""))
theory_dists = rbind(mono_theory_dists,di_theory_dists)
theory_dists[,1] = item_names
colnames(theory_dists)[1] = "item"

#Add theoretical distances
dist_judgements_melted = cbind(dist_judgements_melted,theory_dists[dist_judgements_melted$variable,c("segdist","tonedist")])

#Add trial no
orders = orders[["orders"]]
orders = as.character(orders)
trialno = integer(nrow(dist_judgements_melted))
dist_judgements_melted = cbind.data.frame(dist_judgements_melted,trialno)
for(i in 1:length(orders)){
  currOrder = strsplit(orders[i],"\\|")[[1]]
  for(j in 1:length(currOrder)){
    dist_judgements_melted[(dist_judgements_melted$participant == paste("P",i,sep="")) &
                             (dist_judgements_melted$variable == paste(currOrder[j],"_1",sep=""))
                           ,"trialno"] = j
  }
}


#Monosyls & disyls separate
dist_judgements_melted_mono = dist_judgements_melted[grep("M",dist_judgements_melted$variable),]
dist_judgements_melted_mono$distance = dist_judgements_melted_mono$distance * 4
dist_judgements_melted_di = dist_judgements_melted[grep("D",dist_judgements_melted$variable),]
dist_judgements_melted_di$distance = dist_judgements_melted_di$distance * 8

#End data prep part

#Data exploration
dist_judgements_melted_avg = summarise(group_by(dist_judgements_melted,variable), distance = mean(distance),
                                       segdist = mean(segdist), tonedist = mean(tonedist))
ggplot(dist_judgements_melted_avg) + geom_point(aes(x=segdist,y=tonedist,color=distance))

ggplot(dist_judgements_melted_avg) + geom_point(aes(x=segdist,y=distance,color=tonedist))
ggplot(dist_judgements_melted_avg) + geom_point(aes(x=tonedist,y=distance,color=segdist))
ggplot(dist_judgements_melted_mono) + geom_point(aes(x=segdist,y=tonedist,color=distance))

ggplot(dist_judgements_melted_mono) + geom_point(aes(x=tonedist,y=distance,color=segdist))


ggplot(dist_judgements_melted_avg) + geom_point(aes(x=tonedist,y=distance,color=segdist))


#By-participant exploration
plots_by_participant = list()
#paste(paste("plots_by_participant[[",seq(1,20,1),"]],",sep=""),collapse="")

#for(i in 1:nrow(dist_judgements_completish)){
#  plots_by_participant[[i]] = ggplot(dist_judgements_melted_mono[dist_judgements_melted_mono$participant==as.character(dist_judgements_completish[i,"participant"]),]) +
#    geom_point(aes(x=coef(model_notrial_mono)$variable[1,2]*segdist +coef(model_notrial_mono)$variable[1,3]*tonedist,y=distance))+
theme_classic() + xlab("") + ylab("")
#}
#plot_grid(plots_by_participant[[1]],plots_by_participant[[2]],plots_by_participant[[3]],plots_by_participant[[4]],plots_by_participant[[5]],plots_by_participant[[6]],plots_by_participant[[7]],plots_by_participant[[8]],plots_by_participant[[9]],plots_by_participant[[10]],plots_by_participant[[11]],plots_by_participant[[12]],plots_by_participant[[13]],plots_by_participant[[14]],plots_by_participant[[15]],plots_by_participant[[16]],plots_by_participant[[17]],plots_by_participant[[18]],plots_by_participant[[19]],plots_by_participant[[20]],plots_by_participant[[21]],plots_by_participant[[22]],plots_by_participant[[23]],plots_by_participant[[24]],plots_by_participant[[25]],plots_by_participant[[26]],plots_by_participant[[27]],plots_by_participant[[28]],plots_by_participant[[29]],ncol=6)

plots_by_participant = list()
paste(paste("plots_by_participant[[",which(!judgement_nearempty),"]],",sep=""),collapse="")
for(i in 1:nrow(dist_judgements)){
  plots_by_participant[[i]] = ggplot(dist_judgements_melted_mono[dist_judgements_melted_mono$participant==paste("P",i,sep=""),]) +
    geom_point(aes(x=segdist,y=distance,col=tonedist)) + scale_colour_gradient(low = "#0000FF", high = "#FF0000")+
    theme_classic() + xlab("") + ylab("") + guides(col=FALSE) 
}
together_plot = plot_grid(plots_by_participant[[1]],plots_by_participant[[2]],plots_by_participant[[3]],plots_by_participant[[4]],plots_by_participant[[5]],plots_by_participant[[6]],plots_by_participant[[7]],plots_by_participant[[8]],plots_by_participant[[9]],plots_by_participant[[10]],plots_by_participant[[11]],plots_by_participant[[12]],plots_by_participant[[13]],plots_by_participant[[14]],plots_by_participant[[15]],plots_by_participant[[16]],plots_by_participant[[17]],plots_by_participant[[18]],plots_by_participant[[19]],plots_by_participant[[20]],plots_by_participant[[21]],plots_by_participant[[22]],plots_by_participant[[23]],plots_by_participant[[24]],plots_by_participant[[25]],plots_by_participant[[28]],plots_by_participant[[29]],plots_by_participant[[30]],plots_by_participant[[38]],ncol=5)

plots_by_participant = list()
paste(paste("plots_by_participant[[",which(!judgement_nearempty),"]],",sep=""),collapse="")
for(i in 1:nrow(dist_judgements)){
  plots_by_participant[[i]] = ggplot(dist_judgements_melted_mono[dist_judgements_melted_mono$participant==paste("P",i,sep=""),]) +
    geom_point(aes(x=tonedist,y=distance,col=segdist))+ scale_x_continuous(breaks = c(0,0.5,1)) +
    theme_classic() + xlab("") + ylab("") + guides(col=FALSE) + theme(plot.margin=grid::unit(c(3,3,3,3), "mm"))
}
plot_grid(plots_by_participant[[1]],plots_by_participant[[2]],plots_by_participant[[3]],plots_by_participant[[4]],plots_by_participant[[5]],plots_by_participant[[6]],plots_by_participant[[7]],plots_by_participant[[8]],plots_by_participant[[9]],plots_by_participant[[10]],plots_by_participant[[11]],plots_by_participant[[12]],plots_by_participant[[13]],plots_by_participant[[14]],plots_by_participant[[15]],plots_by_participant[[16]],plots_by_participant[[17]],plots_by_participant[[18]],plots_by_participant[[19]],plots_by_participant[[20]],plots_by_participant[[21]],plots_by_participant[[22]],plots_by_participant[[23]],plots_by_participant[[24]],plots_by_participant[[25]],plots_by_participant[[28]],plots_by_participant[[29]],plots_by_participant[[30]],plots_by_participant[[38]],ncol=5)


#brms
censored = as.integer(dist_judgements_melted_mono$distance == 4)
dist_judgements_melted_mono = cbind.data.frame(dist_judgements_melted_mono,censored)

#Models I-VI  ==
model_notrial_mono_bayes = brm(distance  ~ segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_ransubj_bayes = brm(distance  ~ (1|participant) + segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_rancepts_bayes = brm(distance  ~ (1|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_notrial_mono_ranslope_bayes = brm(distance  ~ (1 + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_notrial_mono_translope_bayes = brm(distance  ~ (1 + tonedist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_ranslopes_bayes = brm(distance  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)

make_stancode(distance  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)
fullmodel_brms_standata = make_standata(distance  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)


model_notrial_mono_bayes_censored = brm(distance   | cens(censored)~ segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_ransubj_bayes_censored = brm(distance | cens(censored)  ~ (1|participant) + segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_rancepts_bayes_censored = brm(distance | cens(censored)  ~ (1|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_notrial_mono_ranslope_bayes_censored = brm(distance | cens(censored)  ~ (1 + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_translope_bayes_censored = brm(distance | cens(censored)  ~ (1 + tonedist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)
model_notrial_mono_ranslopes_bayes_censored = brm(distance | cens(censored)  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono) #Full model!

make_stancode(distance | cens(censored) ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)
fullmodel_brms_standata = make_standata(distance | cens(censored) ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)


waic(model_notrial_mono_bayes)
waic(model_notrial_mono_ransubj_bayes)
waic(model_notrial_mono_rancepts_bayes)
waic(model_notrial_mono_ranslope_bayes)
waic(model_notrial_mono_translope_bayes)
waic(model_notrial_mono_ranslopes_bayes)
waic(model_notrial_mono_bayes_censored)
waic(model_notrial_mono_ransubj_bayes_censored)
waic(model_notrial_mono_rancepts_bayes_censored)
waic(model_notrial_mono_ranslope_bayes_censored)
waic(model_notrial_mono_translope_bayes_censored)
waic(model_notrial_mono_ranslopes_bayes_censored)

#Model params
stanplot(model_notrial_mono_ranslopes_bayes_censored, prob_outer=.95)
summary(model_notrial_mono_ranslopes_bayes_censored)

#On average, seg dist is greater?
hypothesis(model_notrial_mono_ranslopes_bayes_censored,"b_segdist-b_tonedist=0",class=NULL)

#Slopes
slopes = cbind(coef(model_notrial_mono_ranslopes_bayes_censored)$participant[,,"segdist"][,1],coef(model_notrial_mono_ranslopes_bayes_censored)$participant[,,"tonedist"][,1])
colnames(slopes)=c("tonebeta","segbeta")
slopes = cbind.data.frame(slopes,as.integer(judgement_nearempty))
#slopes = data.frame(slopes)[!judgement_nearempty,]
ggplot(data=slopes,aes(x=segbeta,y=tonebeta,col=judgement_nearempty)) + xlab("Tone weighting") + ylab("Segment weighting") + geom_point() + scale_x_continuous(limits=c(0,3),breaks=c(0,.5,1,1.5,2,2.5,3))+ scale_y_continuous(limits=c(0,3),breaks=c(0,1,.5,1.5,2,2.5,3)) + guides(col=FALSE)
hypothesis(model_notrial_mono_ranslopes_bayes_censored,c("sd_participant__segdist/b_segdist=0","sd_participant__tonedist/b_tonedist=0","sd_participant__segdist/b_segdist-sd_participant__tonedist/b_tonedist=0"),class=NULL)


#Residuals
model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept_predicts = predict(model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept)[,1]
model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept_predicts[model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept_predicts<0] = 0
model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept_predicts[model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept_predicts>4] = 4
residuals = (model_notrial_mono_ranslopes_bayes_censor_intercept_partnointercept_predicts-dist_judgements_melted_mono$distance)

plot(dist_judgements_melted_mono$participant,residuals)

#Getting more distances
library(phondist)
monosyls = read.csv("monosyl-items.csv")
old_monosyl_segments = paste(monosyls[,1],monosyls[,2],monosyls[,3],sep="")
new_monosyl_segments = paste(monosyls[,5],monosyls[,6],monosyls[,7],sep="")
old_monosyl_tones = monosyls[,4]
new_monosyl_tones = monosyls[,8]

canto.features = read.table("canto-features.txt")
feature.matrix = canto.features
features.hamming.matrix = getDistancesFromFeatures(canto.features,normalise=TRUE)
hamming_stringdists_monosyl = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE, distance.matrix=features.hamming.matrix, tostrings=new_monosyl_segments))
hamming_stringdists_monosyl_new = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE,costs=c(.5,.5,1,1), distance.matrix=features.hamming.matrix, tostrings=new_monosyl_segments))
hamming_stringdists_monosyl = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_stringdists_monosyl)
colnames(hamming_stringdists_monosyl)[1] = "variable"
hamming_stringdists_monosyl_new = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_stringdists_monosyl_new)
colnames(hamming_stringdists_monosyl_new)[1] = "variable"


canto_tone_auto = read.csv("cantonese-auto.csv")
canto_tone_chao = read.csv("cantonese-chao-tl.csv")
canto_tone_oco = read.csv("cantonese-onconoff.csv")

feature.matrix.chaotone = as.matrix(read.csv("cantonese-chao-tl.csv"))
feature.matrix.chaotone[,3] = as.numeric(feature.matrix.chaotone[,3])
feature.matrix.chaotone[,2] = as.numeric(feature.matrix.chaotone[,2])
feature.matrix.chaotone = data.frame(feature.matrix.chaotone)
rownames(feature.matrix.chaotone) = feature.matrix.chaotone$tone
feature.matrix.chaotone = feature.matrix.chaotone[,-1]
getDistancesFromFeatures(feature.matrix.chaotone,mode="euclidean",normalise=T)
getDistancesFromFeatures(feature.matrix.chaotone,mode="manhattan",normalise=T)

feature.matrix.chaotone.combine = paste(feature.matrix.chaotone[,1],feature.matrix.chaotone[,2],sep="")
names(feature.matrix.chaotone.combine) = rownames(feature.matrix.chaotone)
toneletter.differences = matrix(0,nrow=5,ncol=5)
for(i in 1:5) for(j in 1:5) toneletter.differences[i,j] = abs(i-j)
toneletter.differences = data.frame(toneletter.differences)
rownames(toneletter.differences) = seq(1,5,1)
colnames(toneletter.differences) = seq(1,5,1)
getDistancesFromFeatures(feature.matrix.chaotone,mode="manhattan")

hamming_dists_chao_table = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=toneletter.differences,mode="hamming")
hamming_dists_auto_table = stringdist(paste(canto_tone_auto[,2],canto_tone_auto[,3],sep=""),mode="levenshtein")
hamming_dists_oc_table = stringdist(paste(canto_tone_oco[,2],canto_tone_oco[,3],sep=""),mode="levenshtein")
hamming_dists_oco_table = stringdist(paste(canto_tone_oco[,2],canto_tone_oco[,3],canto_tone_oco[,4],sep=""),mode="levenshtein")
hamming_dists_co_table = stringdist(paste(canto_tone_oco[,3],canto_tone_oco[,4],sep=""),mode="levenshtein")

hamming_dists_auto_table = hamming_dists_auto_table / max(hamming_dists_auto_table)
hamming_dists_oc_table = hamming_dists_oc_table / max(hamming_dists_oc_table)
hamming_dists_oco_table = hamming_dists_oco_table / max(hamming_dists_oco_table)
hamming_dists_co_table = hamming_dists_co_table / max(hamming_dists_co_table)
hamming_dists_chao_table = hamming_dists_chao_table / max(hamming_dists_chao_table)

write.csv(hamming_dists_auto_table,"hamming_dists_auto_table.csv")
write.csv(hamming_dists_oc_table,"hamming_dists_oc_table.csv")
write.csv(hamming_dists_oco_table,"hamming_dists_oco_table.csv")
write.csv(hamming_dists_co_table,"hamming_dists_co_table.csv")
write.csv(hamming_dists_chao_table,"hamming_dists_chao_table.csv")

levenshtein_dists_chao_table = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=toneletter.differences,mode="levenshtein",costs=c(1,1,.5,0))
levenshtein_dists_auto_table = stringdist(paste(canto_tone_auto[,2],canto_tone_auto[,3],sep=""),mode="levenshtein",costs=c(1,1,.5,0))
levenshtein_dists_oc_table = stringdist(paste(canto_tone_oco[,2],canto_tone_oco[,3],sep=""),mode="levenshtein",costs=c(1,1,.5,0))
levenshtein_dists_oco_table = stringdist(paste(canto_tone_oco[,2],canto_tone_oco[,3],canto_tone_oco[,4],sep=""),mode="levenshtein",costs=c(1,1,.5,0))
levenshtein_dists_co_table = stringdist(paste(canto_tone_oco[,3],canto_tone_oco[,4],sep=""),mode="levenshtein",costs=c(1,1,.5,0))

levenshtein_dists_chao_table = levenshtein_dists_chao_table / max(levenshtein_dists_chao_table)
levenshtein_dists_auto_table = levenshtein_dists_auto_table / max(levenshtein_dists_auto_table)
levenshtein_dists_oc_table = levenshtein_dists_oc_table / max(levenshtein_dists_oc_table)
levenshtein_dists_oco_table = levenshtein_dists_oco_table / max(levenshtein_dists_oco_table)
levenshtein_dists_co_table = levenshtein_dists_co_table / max(levenshtein_dists_co_table)



#Euclidean and Manhattan
feature.dists.chaotone.euclidean = getDistancesFromFeatures(feature.matrix.chaotone,mode="euclidean",normalise=T)
feature.dists.chaotone.manhattan = getDistancesFromFeatures(feature.matrix.chaotone,mode="manhattan",normalise=T)
euclidean_dists_chao_table = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=feature.dists.chaotone.euclidean,mode="hamming")
manhattan_dists_chao_table = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=feature.dists.chaotone.manhattan,mode="hamming")
manhattan_dists_chao_table = manhattan_dists_chao_table / max(manhattan_dists_chao_table)
euclidean_dists_chao_table = euclidean_dists_chao_table / max(euclidean_dists_chao_table)

write.csv(manhattan_dists_chao_table,"manhattan_dists_chao_table.csv")
write.csv(euclidean_dists_chao_table,"euclidean_dists_chao_table.csv")

tone_dist_tables = list()
tone_dist_tables[[1]]=hamming_dists_auto_table
tone_dist_tables[[2]]=hamming_dists_oc_table
tone_dist_tables[[3]]=hamming_dists_oco_table
tone_dist_tables[[4]]=hamming_dists_co_table
tone_dist_tables[[5]]=hamming_dists_chao_table
tone_dist_tables[[6]]=manhattan_dists_chao_table
tone_dist_tables[[7]]=euclidean_dists_chao_table

tone_dists_monosyl = matrix(nrow=72,ncol=7)
colnames(tone_dists_monosyl) = c("hamming_auto","hamming_oc","hamming_oco","hamming_co","hamming_chao","man_chao","euc_chao")
for(i in 1:ncol(tone_dists_monosyl)){
  currTable = tone_dist_tables[[i]]
  tone_dists_monosyl[,i]=diag(currTable[old_monosyl_tones,new_monosyl_tones])
}
tone_dists_monosyl = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),tone_dists_monosyl)
colnames(tone_dists_monosyl)[1] = "variable"

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, tone_dists_monosyl, by = "variable")

model_mono_hamming_auto = brm(distance | cens(censored) ~ (segdist + hamming_auto | participant) +(1|variable) + segdist + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oco = brm(distance | cens(censored) ~ (segdist +hamming_oco | participant) +(1|variable) + segdist + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co = brm(distance | cens(censored) ~ (segdist + hamming_co | participant) +(1|variable) + segdist + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao = brm(distance | cens(censored) ~ (segdist + hamming_chao | participant) +(1|variable) + segdist + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao = brm(distance | cens(censored) ~ (segdist + man_chao | participant) +(1|variable) + segdist + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao = brm(distance | cens(censored) ~ (segdist + euc_chao | participant) +(1|variable) + segdist + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto)
waic(model_mono_hamming_oco)
waic(model_mono_hamming_co)
waic(model_mono_hamming_chao)
waic(model_mono_man_chao)
waic(model_mono_euc_chao)

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_stringdists_monosyl, by = "variable")
colnames(dist_judgements_melted_mono)[ncol(dist_judgements_melted_mono)] = "hamming_segdists"

model_mono_hamming_auto_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + hamming_auto | participant) +(1|variable) + hamming_segdists + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oc_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + tonedist | participant) +(1|variable) + hamming_segdists + tonedist, data = dist_judgements_melted_mono)
model_mono_hamming_oco_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + hamming_oco | participant) +(1|variable) + hamming_segdists + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + hamming_co | participant) +(1|variable) + hamming_segdists + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + hamming_chao | participant) +(1|variable) + hamming_segdists + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + man_chao | participant) +(1|variable) + hamming_segdists + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao_hamming_seg = brm(distance | cens(censored) ~ (hamming_segdists + euc_chao | participant) +(1|variable) + hamming_segdists + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_hamming_seg)
waic(model_mono_hamming_oc_hamming_seg)
waic(model_mono_hamming_oco_hamming_seg)
waic(model_mono_hamming_co_hamming_seg)
waic(model_mono_hamming_chao_hamming_seg)
waic(model_mono_man_chao_hamming_seg)
waic(model_mono_euc_chao_hamming_seg)

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_stringdists_monosyl_new, by = "variable")
colnames(dist_judgements_melted_mono)[ncol(dist_judgements_melted_mono)] = "hamming_segdists_new"

model_mono_hamming_auto_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + hamming_auto | participant) +(1|variable) + hamming_segdists_new + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oc_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + tonedist | participant) +(1|variable) + hamming_segdists_new + tonedist, data = dist_judgements_melted_mono)
model_mono_hamming_oco_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + hamming_oco | participant) +(1|variable) + hamming_segdists_new + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + hamming_co | participant) +(1|variable) + hamming_segdists_new + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + hamming_chao | participant) +(1|variable) + hamming_segdists_new + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + man_chao | participant) +(1|variable) + hamming_segdists_new + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao_hamming_seg_new = brm(distance | cens(censored) ~ (hamming_segdists_new + euc_chao | participant) +(1|variable) + hamming_segdists_new + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_hamming_seg_new)
waic(model_mono_hamming_oc_hamming_seg_new)
waic(model_mono_hamming_oco_hamming_seg_new)
waic(model_mono_hamming_co_hamming_seg_new)
waic(model_mono_hamming_chao_hamming_seg_new)
waic(model_mono_man_chao_hamming_seg_new)
waic(model_mono_euc_chao_hamming_seg_new)

model_mono_hamming_auto_norand = brm(distance | cens(censored) ~ (1 | participant) +(1|variable) + segdist + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oco_norand = brm(distance | cens(censored) ~ (1 | participant) +(1|variable) + segdist + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co_norand = brm(distance | cens(censored) ~ (1 | participant) +(1|variable) + segdist + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao_norand = brm(distance | cens(censored) ~ (1 | participant) +(1|variable) + segdist + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao_norand = brm(distance | cens(censored) ~ (1 | participant) +(1|variable) + segdist + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao_norand = brm(distance | cens(censored) ~ (1 | participant) +(1|variable) + segdist + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_norand)
waic(model_mono_hamming_oco_norand)
waic(model_mono_hamming_co_norand)
waic(model_mono_hamming_chao_norand)
waic(model_mono_man_chao_norand)
waic(model_mono_euc_chao_norand)


#Hamming distances with dist. features

#Euclidean, Manhattan & Hamming distances with multivalued features

#Info weighting
lexicon = read.csv("syl_freqs.csv",na.strings = "nincompoop")
segments.naive.weights = getInfoGainFromWordlist(canto.features,lexicon$klatt,lexicon$freq,mode="naive")
segments.broe.weights = getInfoGainFromWordlist(canto.features,lexicon$klatt,lexicon$freq,mode="broe")

naive.weighted.matrix = getDistancesFromFeatures(canto.features,normalise=T,weights=segments.naive.weights)
broe.weighted.matrix = getDistancesFromFeatures(canto.features,normalise=T,weights=segments.broe.weights,zeroMultiplier=.5)

hamming_stringdists_monosyl_naive_weighted = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, tostrings=new_monosyl_segments))
hamming_stringdists_monosyl_naive_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_stringdists_monosyl_naive_weighted)
colnames(hamming_stringdists_monosyl_naive_weighted)[1] = "variable"
hamming_stringdists_monosyl_broe_weighted = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, tostrings=new_monosyl_segments))
hamming_stringdists_monosyl_broe_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_stringdists_monosyl_broe_weighted)
colnames(hamming_stringdists_monosyl_broe_weighted)[1] = "variable"

hamming_stringdists_monosyl_new_naive_weighted = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, tostrings=new_monosyl_segments, costs=c(.5,.5,1,1)))
hamming_stringdists_monosyl_new_naive_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_stringdists_monosyl_new_naive_weighted)
colnames(hamming_stringdists_monosyl_new_naive_weighted)[1] = "variable"
hamming_stringdists_monosyl_new_broe_weighted = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, tostrings=new_monosyl_segments, costs=c(.5,.5,1,1)))
hamming_stringdists_monosyl_new_broe_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_stringdists_monosyl_new_broe_weighted)
colnames(hamming_stringdists_monosyl_new_broe_weighted)[1] = "variable"


all_parsed_syls_wt = read.csv('all_parsed_syls_wt.csv',na.strings='nincompoop')
tonefreqs = table(all_parsed_syls_wt$t)
canto_tone_auto_asfeatures = cbind.data.frame(as.factor(canto_tone_auto[,2]),as.factor(substring(canto_tone_auto[,3],1,1)),as.factor(substring(canto_tone_auto[,3],2,2)))
canto_tone_chao_asfeatures = cbind.data.frame(as.factor(canto_tone_chao[,2]),as.factor(canto_tone_oco[,3]))
canto_tone_oco_asfeatures = cbind.data.frame(as.factor(canto_tone_oco[,2]),as.factor(canto_tone_oco[,3]),as.factor(canto_tone_oco[,4]))
rownames(canto_tone_auto_asfeatures) = as.character(c(1,2,3,4,5,6))
rownames(canto_tone_chao_asfeatures) = as.character(c(1,2,3,4,5,6))
rownames(canto_tone_oco_asfeatures) = as.character(c(1,2,3,4,5,6))
auto.weights = getInfoGainFromWordlist(canto_tone_auto_asfeatures, as.character(c(1,2,3,4,5,6)), freqs = tonefreqs)
chao.weights = getInfoGainFromWordlist(canto_tone_chao_asfeatures, as.character(c(1,2,3,4,5,6)), freqs = tonefreqs)
oco.weights = getInfoGainFromWordlist(canto_tone_oco_asfeatures, as.character(c(1,2,3,4,5,6)), freqs = tonefreqs)

hamming_dists_chao_table_weighted = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=toneletter.differences,mode="hamming",algo="hamming",weights=chao.weights)
hamming_dists_auto_table_weighted = stringdist(paste(canto_tone_auto[,2],canto_tone_auto[,3],sep=""),mode="hamming",algo="hamming",weights=auto.weights)
hamming_dists_oc_table_weighted = stringdist(paste(canto_tone_oco[,2],canto_tone_oco[,3],sep=""),mode="hamming",algo="hamming",weights=oco.weights[1:2])
hamming_dists_oco_table_weighted = stringdist(paste(canto_tone_oco[,2],canto_tone_oco[,3],canto_tone_oco[,4],sep=""),mode="hamming",algo="hamming",weights=oco.weights)
hamming_dists_co_table_weighted = stringdist(paste(canto_tone_oco[,3],canto_tone_oco[,4],sep=""),mode="hamming",algo="hamming",weights=oco.weights[2:3])

hamming_dists_auto_table_weighted = hamming_dists_auto_table_weighted / max(hamming_dists_auto_table_weighted)
hamming_dists_oc_table_weighted = hamming_dists_oc_table_weighted / max(hamming_dists_oc_table_weighted)
hamming_dists_oco_table_weighted = hamming_dists_oco_table_weighted / max(hamming_dists_oco_table_weighted)
hamming_dists_co_table_weighted = hamming_dists_co_table_weighted / max(hamming_dists_co_table_weighted)
hamming_dists_chao_table_weighted = hamming_dists_chao_table_weighted / max(hamming_dists_chao_table_weighted)

write.csv(hamming_dists_auto_table_weighted,"hamming_dists_auto_table_weighted.csv")
write.csv(hamming_dists_oc_table_weighted,"hamming_dists_oc_table_weighted.csv")
write.csv(hamming_dists_oco_table_weighted,"hamming_dists_oco_table_weighted.csv")
write.csv(hamming_dists_co_table_weighted,"hamming_dists_co_table_weighted.csv")
write.csv(hamming_dists_chao_table_weighted,"hamming_dists_chao_table_weighted.csv")

feature.dists.chaotone.euclidean_weighted = getDistancesFromFeatures(feature.matrix.chaotone,mode="euclidean",normalise=T,weights=chao.weights)
feature.dists.chaotone.manhattan_weighted = getDistancesFromFeatures(feature.matrix.chaotone,mode="manhattan",normalise=T,weights=chao.weights)
euclidean_dists_chao_table_weighted = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=feature.dists.chaotone.euclidean_weighted,mode="hamming")
manhattan_dists_chao_table_weighted = stringdist(feature.matrix.chaotone.combine,segment.specific.costs = TRUE,distance.matrix=feature.dists.chaotone.manhattan_weighted,mode="hamming")
manhattan_dists_chao_table_weighted = manhattan_dists_chao_table_weighted / max(manhattan_dists_chao_table_weighted)
euclidean_dists_chao_table_weighted = euclidean_dists_chao_table_weighted / max(euclidean_dists_chao_table_weighted)

write.csv(manhattan_dists_chao_table_weighted,"manhattan_dists_chao_table_weighted.csv")
write.csv(euclidean_dists_chao_table_weighted,"euclidean_dists_chao_table_weighted.csv")

tone_dist_tables_weighted = list()
tone_dist_tables_weighted[[1]]=hamming_dists_auto_table_weighted
tone_dist_tables_weighted[[2]]=hamming_dists_oc_table_weighted
tone_dist_tables_weighted[[3]]=hamming_dists_oco_table_weighted
tone_dist_tables_weighted[[4]]=hamming_dists_co_table_weighted
tone_dist_tables_weighted[[5]]=hamming_dists_chao_table_weighted
tone_dist_tables_weighted[[6]]=manhattan_dists_chao_table_weighted
tone_dist_tables_weighted[[7]]=euclidean_dists_chao_table_weighted

tone_dists_monosyl_weighted = matrix(nrow=72,ncol=7)
colnames(tone_dists_monosyl_weighted) = c("hamming_auto_weighted","hamming_oc_weighted","hamming_oco_weighted","hamming_co_weighted","hamming_chao_weighted","man_chao_weighted","euc_chao_weighted")
for(i in 1:ncol(tone_dists_monosyl_weighted)){
  currTable = tone_dist_tables_weighted[[i]]
  tone_dists_monosyl_weighted[,i]=diag(currTable[old_monosyl_tones,new_monosyl_tones])
}
tone_dists_monosyl_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),tone_dists_monosyl_weighted)
colnames(tone_dists_monosyl_weighted)[1] = "variable"

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, tone_dists_monosyl_weighted, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_stringdists_monosyl_naive_weighted, by = "variable")
colnames(dist_judgements_melted_mono)[which(colnames(dist_judgements_melted_mono) == "hamming_stringdists_monosyl_naive_weighted")] = "hamming_stringdists_naive_weighted"
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_stringdists_monosyl_broe_weighted, by = "variable")
colnames(dist_judgements_melted_mono)[which(colnames(dist_judgements_melted_mono) == "hamming_stringdists_monosyl_broe_weighted")] = "hamming_stringdists_broe_weighted"

model_mono_hamming_auto_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + hamming_auto_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_auto_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_oc_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + hamming_oc_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_oc_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_oco_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + hamming_oco_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_oco_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_co_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + hamming_co_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_co_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_chao_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + hamming_chao_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_chao_weighted, data = dist_judgements_melted_mono)
model_mono_man_chao_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + man_chao_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + man_chao_weighted, data = dist_judgements_melted_mono)
model_mono_euc_chao_hamming_seg_weighted = brm(distance | cens(censored) ~ (hamming_stringdists_naive_weighted + euc_chao_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + euc_chao_weighted, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_hamming_seg_weighted)
waic(model_mono_hamming_oc_hamming_seg_weighted)
waic(model_mono_hamming_oco_hamming_seg_weighted)
waic(model_mono_hamming_co_hamming_seg_weighted)
waic(model_mono_hamming_chao_hamming_seg_weighted)
waic(model_mono_man_chao_hamming_seg_weighted)
waic(model_mono_euc_chao_hamming_seg_weighted)

model_mono_hamming_auto_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + hamming_auto_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_auto_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_oc_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + hamming_oc_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_oc_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_oco_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + hamming_oco_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_oco_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_co_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + hamming_co_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_co_weighted, data = dist_judgements_melted_mono)
model_mono_hamming_chao_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + hamming_chao_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_chao_weighted, data = dist_judgements_melted_mono)
model_mono_man_chao_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + man_chao_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + man_chao_weighted, data = dist_judgements_melted_mono)
model_mono_euc_chao_hamming_seg_weighted_broe = brm(distance | cens(censored) ~ (hamming_stringdists_broe_weighted + euc_chao_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + euc_chao_weighted, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_hamming_seg_weighted_broe)
waic(model_mono_hamming_oc_hamming_seg_weighted_broe)
waic(model_mono_hamming_oco_hamming_seg_weighted_broe)
waic(model_mono_hamming_co_hamming_seg_weighted_broe)
waic(model_mono_hamming_chao_hamming_seg_weighted_broe)
waic(model_mono_man_chao_hamming_seg_weighted_broe)
waic(model_mono_euc_chao_hamming_seg_weighted_broe)


model_mono_hamming_auto_hamming_seg_toneweightonly = brm(distance | cens(censored) ~ (hamming_segdists + hamming_auto_weighted | participant) +(1|variable) + hamming_segdists + hamming_auto_weighted, data = dist_judgements_melted_mono)



#Multifeatures

feature.matrix.multivalue = read.csv("cantonese-multifeatures.csv")
rownames(feature.matrix.multivalue) = feature.matrix.multivalue$seg
feature.matrix.multivalue = feature.matrix.multivalue[,-1]
#M is [e], 0 is [o], x is unreleased k
distances.multivalue.euclidean = getDistancesFromFeatures(feature.matrix.multivalue,mode="euclidean",normalise=T)
distances.multivalue.hamming = getDistancesFromFeatures(feature.matrix.multivalue,mode="hamming",normalise=T)
distances.multivalue.manhattan = getDistancesFromFeatures(feature.matrix.multivalue,mode="manhattan",normalise=T)

allophones = function(wordlist){
  wordlist = sub("i([N|k|x])","I\\1",wordlist)
  wordlist = sub("u([N|k|x])","v\\1",wordlist)
  wordlist = sub("ei","Mi",wordlist)
  wordlist = sub("ou","0u",wordlist)
  for(i in 1:length(wordlist)){
    if(substring(wordlist[i],3,3) == "p") wordlist[i] = paste(substring(wordlist[i],1,2),"P",sep="")
    if(substring(wordlist[i],3,3) == "t") wordlist[i] = paste(substring(wordlist[i],1,2),"T",sep="")
    if(substring(wordlist[i],3,3) == "k") wordlist[i] = paste(substring(wordlist[i],1,2),"x",sep="")
  }
  return(wordlist)
}

old_monosyl_segments_allophones = allophones(old_monosyl_segments)
new_monosyl_segments_allophones = allophones(new_monosyl_segments)

euclidean_segdist_multivalue = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1)))
manhattan_segdist_multivalue = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1)))
hamming_segdist_multivalue = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1)))

euclidean_segdist_multivalue = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),euclidean_segdist_multivalue)
colnames(euclidean_segdist_multivalue)[1] = "variable"
manhattan_segdist_multivalue = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),manhattan_segdist_multivalue)
colnames(manhattan_segdist_multivalue)[1] = "variable"
hamming_segdist_multivalue = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_segdist_multivalue)
colnames(hamming_segdist_multivalue)[1] = "variable"

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, euclidean_segdist_multivalue, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, manhattan_segdist_multivalue, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_segdist_multivalue, by = "variable")

model_mono_hamming_auto_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + hamming_auto | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oc_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + hamming_oc | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_oc, data = dist_judgements_melted_mono)
model_mono_hamming_oco_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + hamming_oco | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + hamming_co | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + hamming_chao | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + man_chao | participant) +(1|variable) + euclidean_segdist_multivalue + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao_euclidean_seg = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue + euc_chao | participant) +(1|variable) + euclidean_segdist_multivalue + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_euclidean_seg)
waic(model_mono_hamming_oc_euclidean_seg)
waic(model_mono_hamming_oco_euclidean_seg)
waic(model_mono_hamming_co_euclidean_seg)
waic(model_mono_hamming_chao_euclidean_seg)
waic(model_mono_man_chao_euclidean_seg)
waic(model_mono_euc_chao_euclidean_seg)

model_mono_hamming_auto_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + hamming_auto | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oc_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + hamming_oc | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_oc, data = dist_judgements_melted_mono)
model_mono_hamming_oco_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + hamming_oco | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + hamming_co | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + hamming_chao | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + man_chao | participant) +(1|variable) + manhattan_segdist_multivalue + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao_manhattan_seg = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue + euc_chao | participant) +(1|variable) + manhattan_segdist_multivalue + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_manhattan_seg)
waic(model_mono_hamming_oc_manhattan_seg)
waic(model_mono_hamming_oco_manhattan_seg)
waic(model_mono_hamming_co_manhattan_seg)
waic(model_mono_hamming_chao_manhattan_seg)
waic(model_mono_man_chao_manhattan_seg)
waic(model_mono_euc_chao_manhattan_seg)

model_mono_hamming_auto_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + hamming_auto | participant) +(1|variable) + hamming_segdist_multivalue + hamming_auto, data = dist_judgements_melted_mono)
model_mono_hamming_oc_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + hamming_oc | participant) +(1|variable) + hamming_segdist_multivalue + hamming_oc, data = dist_judgements_melted_mono)
model_mono_hamming_oco_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + hamming_oco | participant) +(1|variable) + hamming_segdist_multivalue + hamming_oco, data = dist_judgements_melted_mono)
model_mono_hamming_co_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + hamming_co | participant) +(1|variable) + hamming_segdist_multivalue + hamming_co, data = dist_judgements_melted_mono)
model_mono_hamming_chao_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + hamming_chao | participant) +(1|variable) + hamming_segdist_multivalue + hamming_chao, data = dist_judgements_melted_mono)
model_mono_man_chao_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + man_chao | participant) +(1|variable) + hamming_segdist_multivalue + man_chao, data = dist_judgements_melted_mono)
model_mono_euc_chao_hamming_seg_multival = brm(distance | cens(censored) ~ (hamming_segdist_multivalue + euc_chao | participant) +(1|variable) + hamming_segdist_multivalue + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_hamming_auto_hamming_seg_multival)
waic(model_mono_hamming_oc_hamming_seg_multival)
waic(model_mono_hamming_oco_hamming_seg_multival)
waic(model_mono_hamming_co_hamming_seg_multival)
waic(model_mono_hamming_chao_hamming_seg_multival)
waic(model_mono_man_chao_hamming_seg_multival)
waic(model_mono_euc_chao_hamming_seg_multival)

euclidean_segdist_multivalue = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1)))
manhattan_segdist_multivalue = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1)))
hamming_segdist_multivalue = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1)))

feature.matrix.multivalue.factors = data.frame(lapply(feature.matrix.multivalue,as.factor))
rownames(feature.matrix.multivalue.factors) = rownames(feature.matrix.multivalue)

multivalue.weights = getInfoGainFromWordlist(feature.matrix.multivalue.factors,allophones(lexicon$klatt),lexicon$freq,mode="naive")

euclidean_segdist_multivalue_weighted = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1),weights=multivalue.weights))
manhattan_segdist_multivalue_weighted = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1),weights=multivalue.weights))
hamming_segdist_multivalue_weighted = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1),weights=multivalue.weights))

euclidean_segdist_multivalue_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),euclidean_segdist_multivalue_weighted)
colnames(euclidean_segdist_multivalue_weighted)[1] = "variable"
manhattan_segdist_multivalue_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),manhattan_segdist_multivalue_weighted)
colnames(manhattan_segdist_multivalue_weighted)[1] = "variable"
hamming_segdist_multivalue_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_segdist_multivalue_weighted)
colnames(hamming_segdist_multivalue_weighted)[1] = "variable"

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, euclidean_segdist_multivalue_weighted, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, manhattan_segdist_multivalue_weighted, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_segdist_multivalue_weighted, by = "variable")

model_mono_hamming_auto_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + hamming_auto_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_auto_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oc_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + hamming_oc_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_oc_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oco_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + hamming_oco_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_oco_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_co_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + hamming_co_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_co_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_chao_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + hamming_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_man_chao_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + man_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + man_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_euc_chao_euclidean_seg_weighted = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted + euc_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + euc_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))

waic(model_mono_hamming_auto_euclidean_seg_weighted)
waic(model_mono_hamming_oc_euclidean_seg_weighted)
waic(model_mono_hamming_oco_euclidean_seg_weighted)
waic(model_mono_hamming_co_euclidean_seg_weighted)
waic(model_mono_hamming_chao_euclidean_seg_weighted)
waic(model_mono_man_chao_euclidean_seg_weighted)
waic(model_mono_euc_chao_euclidean_seg_weighted)

model_mono_hamming_auto_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + hamming_auto_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_auto_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oc_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + hamming_oc_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_oc_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oco_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + hamming_oco_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_oco_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_co_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + hamming_co_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_co_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_chao_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + hammin  g_chao | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_man_chao_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + man_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + man_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_euc_chao_manhattan_seg_weighted = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted + euc_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + euc_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))

waic(model_mono_hamming_auto_manhattan_seg_weighted)
waic(model_mono_hamming_oc_manhattan_seg_weighted)
waic(model_mono_hamming_oco_manhattan_seg_weighted)
waic(model_mono_hamming_co_manhattan_seg_weighted)
waic(model_mono_hamming_chao_manhattan_seg_weighted)
waic(model_mono_man_chao_manhattan_seg_weighted)
waic(model_mono_euc_chao_manhattan_seg_weighted)

model_mono_hamming_auto_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + hamming_auto_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_auto_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oc_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + hamming_oc_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_oc_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oco_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + hamming_oco_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_oco_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_co_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + hamming_co_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_co_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_chao_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + hamming_chao_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_man_chao_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + man_chao_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + man_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_euc_chao_hamming_seg_multival_weighted = brm(distance | cens(censored) ~ (hamming_segdist_multivalue_weighted + euc_chao_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + euc_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))

waic(model_mono_hamming_auto_hamming_seg_multival_weighted)
waic(model_mono_hamming_oc_hamming_seg_multival_weighted)
waic(model_mono_hamming_oco_hamming_seg_multival_weighted)
waic(model_mono_hamming_co_hamming_seg_multival_weighted)
waic(model_mono_hamming_chao_hamming_seg_multival_weighted)
waic(model_mono_man_chao_hamming_seg_multival_weighted)
waic(model_mono_euc_chao_hamming_seg_multival_weighted)

#Natural classes distances
nclass.matrix = getNaturalClassMatrixFromFeatures(feature.matrix)
nclass.weights = getInfoGainFromWordlist(nclass.matrix,lexicon$klatt,lexicon$freq,mode="naive")
nclass.dist.matrix.weighted = getDistancesFromFeatures(nclass.matrix,normalise=TRUE,weights=nclass.weights)
nclass_segdists_weighted = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE,costs=c(.5,.5,1,1), distance.matrix=nclass.dist.matrix.weighted, tostrings=new_monosyl_segments))

nclass_segdists_weighted = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),nclass_segdists_weighted)
colnames(nclass_segdists_weighted)[1] = "variable"

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, nclass_segdists_weighted, by = "variable")

model_mono_hamming_auto_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + hamming_auto_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_auto_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oc_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + hamming_oc_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_oc_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oco_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + hamming_oco_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_oco_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_co_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + hamming_co_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_co_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_chao_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + hamming_chao_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_man_chao_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + man_chao_weighted | participant) +(1|variable) + nclass_segdists_weighted + man_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_euc_chao_nclass_segdists_weighted = brm(distance | cens(censored) ~ (nclass_segdists_weighted + euc_chao_weighted | participant) +(1|variable) + nclass_segdists_weighted + euc_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))

waic(model_mono_hamming_auto_nclass_segdists_weighted)
waic(model_mono_hamming_oc_nclass_segdists_weighted)
waic(model_mono_hamming_oco_nclass_segdists_weighted)
waic(model_mono_hamming_co_nclass_segdists_weighted)
waic(model_mono_hamming_chao_nclass_segdists_weighted)
waic(model_mono_man_chao_nclass_segdists_weighted)
waic(model_mono_euc_chao_nclass_segdists_weighted)


#STAN version
library(rstan)
library(loo)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


stanDat = list(participant = as.integer(dist_judgements_melted_mono$participant),
               variable = as.integer(as.factor(dist_judgements_melted_mono$variable)),
               tonedist = dist_judgements_melted_mono$tonedist,
               segdist = dist_judgements_melted_mono$segdist,
               dist = dist_judgements_melted_mono$distance,
               N = nrow(dist_judgements_melted_mono),
               J = nlevels(dist_judgements_melted_mono$participant),
               K = nlevels(as.factor(dist_judgements_melted_mono$variable)))

fixEfFit <- stan(file = "firsttrial.stan", data = stanDat,
                 iter = 2000, chains = 4)
posterior = as.array(fixEfFit)
mcmc_intervals(posterior,pars=c("beta[1]","beta[2]","beta[3]","sigma_e","sigma_u","sigma_w"))
waic(posterior)

fullmodel <- stan(file = "fullmodel.stan", data = stanDat,
                  iter = 2000, chains = 4)
posterior2 = as.array(fullmodel)

setwd("G:/§Úªº¶³ºÝµwºÐ/Phonotactics/judgement-experiment/distance data")
mcmc_intervals(posterior2,pars=c("beta[1]","beta[2]","beta[3]","sigma_e","Sigma_u[1,1]","Sigma_u[2,2]","Sigma_u[3,3]","Sigma_u[1,2]","Sigma_u[1,3]","Sigma_u[2,3]","sigma_w"))
waic(extract_log_lik(fullmodel, merge_chains = FALSE))

fullmodel_cens_brms_data = make_standata(distance | cens(censored)  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_mono)
fullmodel_cens_brms = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_cens_brms_data, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))
fullmodel_cens_brms_final = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_cens_brms_data, iter = 2500, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 20))
fullmodel_cens_brms = stan(file = "fullmodel_cens_brms_noprior.stan", data = fullmodel_cens_brms_data, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))
fullmodel_cens_brms = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_cens_brms_data, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))

mcmc_intervals(as.array(fullmodel_cens_brms),pars=c("b[1]","b[2]","b_b[1]","b_b[2]","theta[1]","theta[2]"))
mcmc_intervals(as.array(fullmodel_cens_brms_final),pars=c("b[1]","b[2]","b_b[1]","b_b[2]","theta[1]","theta[2]"))
mcmc_intervals(as.array(fullmodel_cens_brms),pars=c("b[1]","b[2]","b_b[1]","b_b[2]","theta[1]","theta[2]","Cor_1[1,1]", "Cor_1[2,2]", "Cor_1[3,3]", "Cor_1_b[1,1]", "Cor_1_b[2,2]", "Cor_1_b[3,3]"))

addDistsToMonoTable = function(dists, name){
  dists = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),dists)
  colnames(dists)[1] = "variable"
  colnames(dists)[2] = name
  dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, dists, by = "variable")
  return(dist_judgements_melted_mono)
}

simple_stringdists_monosyl = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = FALSE, tostrings=new_monosyl_segments))
simple_segdist = simple_stringdists_monosyl / 3
dist_judgements_melted_mono = addDistsToMonoTable(simple_segdist,"simple_segdist")

model_mono_simple_hamming_auto = brm(distance | cens(censored) ~ (simple_segdist + hamming_auto | participant) +(1|variable) + simple_segdist + hamming_auto, data = dist_judgements_melted_mono)
model_mono_simple_hamming_oco = brm(distance | cens(censored) ~ (simple_segdist +hamming_oco | participant) +(1|variable) + simple_segdist + hamming_oco, data = dist_judgements_melted_mono)
model_mono_simple_hamming_co = brm(distance | cens(censored) ~ (simple_segdist + hamming_co | participant) +(1|variable) + simple_segdist + hamming_co, data = dist_judgements_melted_mono)
model_mono_simple_hamming_chao = brm(distance | cens(censored) ~ (simple_segdist + hamming_chao | participant) +(1|variable) + simple_segdist + hamming_chao, data = dist_judgements_melted_mono)     
model_mono_simple_man_chao = brm(distance | cens(censored) ~ (simple_segdist + man_chao | participant) +(1|variable) + simple_segdist + man_chao, data = dist_judgements_melted_mono)
model_mono_simple_euc_chao = brm(distance | cens(censored) ~ (simple_segdist + euc_chao | participant) +(1|variable) + simple_segdist + euc_chao, data = dist_judgements_melted_mono)

waic(model_mono_simple_hamming_auto)
waic(model_mono_simple_hamming_oco)
waic(model_mono_simple_hamming_co)
waic(model_mono_simple_hamming_chao)
waic(model_mono_simple_man_chao)
waic(model_mono_simple_euc_chao)

findMonosylDistance = function(monosyl){
  oldfile = read.table(paste("G:\\§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance audio\\sepaudio\\cochlea\\normalised_MONO-OLD-",monosyl,"_cochlea.txt",sep=""))
  newfile = read.table(paste("G:\\§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance audio\\sepaudio\\cochlea\\normalised_MONO-NEW-",monosyl,"_cochlea.txt",sep=""))
  return(spectrumDistance(oldfile,newfile))
}

#cochleagram

spect_dists_monosyl = sapply(1:72,findMonosylDistance)
dist_judgements_melted_mono = addDistsToMonoTable(spect_dists_monosyl,"acoustic_dist")
spect_dists_monosyl_2 = spect_dists_monosyl / max(spect_dists_monosyl) * 4
dist_judgements_melted_mono = addDistsToMonoTable(spect_dists_monosyl_2,"acoustic_dist_modified")
model_mono_simple_hamming_auto = brm(distance | cens(censored) ~ (acoustic_dist_modified | participant) +(1|variable) + acoustic_dist_modified, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_acoust = model_mono_simple_hamming_auto

plots_by_participant = list()
for(i in 1:nrow(dist_judgements)){
  plots_by_participant[[i]] = ggplot(dist_judgements_melted_mono[dist_judgements_melted_mono$participant==paste("P",i,sep=""),]) +
    geom_point(aes(x=acoustic_dist_modified,y=distance,col=tonedist))+
    theme_classic() + xlab("") + ylab("") + guides(col=FALSE) + theme(plot.margin=grid::unit(c(3,3,3,3), "mm"))
}
plot_grid(plots_by_participant[[1]],plots_by_participant[[2]],plots_by_participant[[3]],plots_by_participant[[4]],plots_by_participant[[5]],plots_by_participant[[6]],plots_by_participant[[7]],plots_by_participant[[8]],plots_by_participant[[9]],plots_by_participant[[10]],plots_by_participant[[11]],plots_by_participant[[12]],plots_by_participant[[13]],plots_by_participant[[14]],plots_by_participant[[15]],plots_by_participant[[16]],plots_by_participant[[17]],plots_by_participant[[18]],plots_by_participant[[19]],plots_by_participant[[20]],plots_by_participant[[21]],plots_by_participant[[22]],plots_by_participant[[23]],plots_by_participant[[24]],plots_by_participant[[25]],plots_by_participant[[28]],plots_by_participant[[29]],plots_by_participant[[30]],plots_by_participant[[38]],ncol=5)l

#onset/nuc/coda sep

monosyl_sepclass = read.csv("segdist_natclass_sep.csv")
colnames(monosyl_sepclass)[1] = "onschange"
addDistsToMonoTable = function(dists, name){
  dists = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),dists)
  colnames(dists)[1] = "variable"
  colnames(dists)[2] = name
  dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, dists, by = "variable")
  return(dist_judgements_melted_mono)
}

dist_judgements_melted_mono = addDistsToMonoTable(monosyl_sepclass$onschange,"onschange")
dist_judgements_melted_mono = addDistsToMonoTable(monosyl_sepclass$nucchange,"nucchange")
dist_judgements_melted_mono = addDistsToMonoTable(monosyl_sepclass$codachange,"codachange")

monosyl_sep = model_mono_simple_hamming_auto = brm(distance | cens(censored) ~ (onschange + nucchange + codachange + tonedist | participant) +(1|variable) + onschange + nucchange + codachange + tonedist, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
hypothesis(monosyl_sep,"b_onschange-b_nucchange=0",class=NULL)
hypothesis(monosyl_sep,"b_nucchange-b_codachange=0",class=NULL)
hypothesis(monosyl_sep,"b_codachange-b_tonedist=0",class=NULL)

#Revisions
nclass.matrix = getNaturalClassMatrixFromFeatures(feature.matrix[-((nrow(feature.matrix)-2):nrow(feature.matrix)),])
nclass.matrix.finalstops = getNaturalClassMatrixFromFeatures(feature.matrix)
changeFinalStops = function(string){
  finalSeg = substring(string,nchar(string),nchar(string))
  if(finalSeg == "p"){
    string = paste0(substring(string,1,nchar(string)-1),"P")
  } else if(finalSeg == "t"){
    string = paste0(substring(string,1,nchar(string)-1),"T")
  }else if(finalSeg == "k"){
    string = paste0(substring(string,1,nchar(string)-1),"H")
  }
  return(string)
}
lexicon_finalstops = sapply(as.character(lexicon$klatt),changeFinalStops)
nclass.dist.matrix = getDistancesFromFeatures(nclass.matrix, normalise = T)
nclass.dist.matrix.finalstops = getDistancesFromFeatures(nclass.matrix.finalstops, normalise = T)
nclass.dist.matrix.finalstops = 1-read.csv("canto-frisch-dists-finalstops.csv",row.names=1) #Different totals
nclass_segdists_finalstops = diag(stringdist(strings=old_monosyl_segments,segment.specific.costs = TRUE,costs=c(.5,.5,1,1), distance.matrix=nclass.dist.matrix.finalstops, tostrings=new_monosyl_segments))

dist_judgements_melted_mono = addDistsToMonoTable(nclass_segdists_finalstops,"nclass_segdists_finalstops")
model_notrial_mono_ranslopes_bayes_censored = brm(distance | cens(censored)  ~ (1 + tonedist + nclass_segdists_finalstops|participant) + (1|variable) + nclass_segdists_finalstops + tonedist, data = dist_judgements_melted_mono) #Full model!
getCharDist = function(old, new, dist.matrix){
  if(as.character(old) != "" & as.character(new) != ""){
    dist = dist.matrix[as.character(old),as.character(new)]
  } else if (xor(as.character(old) != "", as.character(new) != "")){
    return(mean(colMeans(dist.matrix))/2)
  } else{
    return(0)
  }
}

replace_ptk = function(phoneme){
  if(phoneme == "p"){
     "P"
  } else if(phoneme == "t"){
     "T"
  }else if(phoneme == "k"){
    "H"
  } else{
    phoneme
  }
}
coda_old_finalstops = sapply(as.character(monosyls$coda_old),replace_ptk)
coda_new_finalstops = sapply(as.character(monosyls$coda_new),replace_ptk)
ons.change.finalstops = sapply(1:nrow(monosyls),function(x) getCharDist(monosyls$ons_old[x],monosyls$ons_new[x],nclass.dist.matrix.finalstops))
nuc.change.finalstops = sapply(1:nrow(monosyls),function(x) getCharDist(monosyls$nuc_old[x],monosyls$nuc_new[x],nclass.dist.matrix.finalstops))
coda.change.finalstops = sapply(1:nrow(monosyls),function(x) getCharDist(coda_old_finalstops[x],coda_new_finalstops[x],nclass.dist.matrix.finalstops))

dist_judgements_melted_mono = addDistsToMonoTable(ons.change.finalstops,"onschange_fs")
dist_judgements_melted_mono = addDistsToMonoTable(nuc.change.finalstops,"nucchange_fs")
dist_judgements_melted_mono = addDistsToMonoTable(coda.change.finalstops,"codachange_fs")

monosyl_sep_fs =  brm(distance | cens(censored) ~ (onschange_fs + nucchange_fs + codachange_fs + tonedist | participant) +(1|variable) + onschange_fs + nucchange_fs + codachange_fs + tonedist, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
hypothesis(monosyl_sep_fs,"b_onschange_fs-b_nucchange_fs=0",class=NULL)
hypothesis(monosyl_sep_fs,"b_nucchange_fs-b_codachange_fs=0",class=NULL)
hypothesis(monosyl_sep_fs,"b_codachange_fs-b_tonedist=0",class=NULL)


#MFCC
findMonosylMFCCDistance = function(monosyl){
  oldfile = read.table(paste("G:\\§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance audio\\sepaudio\\mfcc\\normalised_MONO-OLD-",monosyl,"_mfcc.txt",sep=""))
  newfile = read.table(paste("G:\\§Úªº¶³ºÝµwºÐ\\Phonotactics\\judgement-experiment\\distance audio\\sepaudio\\mfcc\\normalised_MONO-NEW-",monosyl,"_mfcc.txt",sep=""))
  return(spectrumDistance(oldfile,newfile))
}

mfcc_dists_monosyl = sapply(1:72,findMonosylMFCCDistance)
dist_judgements_melted_mono = addDistsToMonoTable(mfcc_dists_monosyl,"mfcc_acoustic_dist")
mfcc_dists_monosyl_2 = mfcc_dists_monosyl / max(mfcc_dists_monosyl) * 4
dist_judgements_melted_mono = addDistsToMonoTable(mfcc_dists_monosyl_2,"mfcc_acoustic_dist_modified")
model_mono_acoust_mfcc = brm(distance | cens(censored) ~ (mfcc_acoustic_dist_modified | participant) +(1|variable) + mfcc_acoustic_dist_modified, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))






#Continuous weighting
multivalue.weights.cont = getInfoGainFromWordlist(feature.matrix.multivalue.factors,allophones(lexicon$klatt),lexicon$freq,mode="naive", discrete = F)

euclidean_segdist_multivalue_weighted_cont = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1),weights=multivalue.weights.cont))
manhattan_segdist_multivalue_weighted_cont = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1),weights=multivalue.weights.cont))
hamming_segdist_multivalue_weighted_cont = diag(stringdist(old_monosyl_segments_allophones,tostrings=new_monosyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1),weights=multivalue.weights.cont))

euclidean_segdist_multivalue_weighted_cont = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),euclidean_segdist_multivalue_weighted_cont)
colnames(euclidean_segdist_multivalue_weighted_cont)[1] = "variable"
manhattan_segdist_multivalue_weighted_cont = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),manhattan_segdist_multivalue_weighted_cont)
colnames(manhattan_segdist_multivalue_weighted_cont)[1] = "variable"
hamming_segdist_multivalue_weighted_cont = cbind.data.frame(paste("M",seq(1,72,1),"_1",sep=""),hamming_segdist_multivalue_weighted_cont)
colnames(hamming_segdist_multivalue_weighted_cont)[1] = "variable"

dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, euclidean_segdist_multivalue_weighted_cont, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, manhattan_segdist_multivalue_weighted_cont, by = "variable")
dist_judgements_melted_mono = right_join(dist_judgements_melted_mono, hamming_segdist_multivalue_weighted_cont, by = "variable")

model_mono_hamming_auto_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + hamming_auto_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + hamming_auto_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oc_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + hamming_oc_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + hamming_oc_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oco_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + hamming_oco_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + hamming_oco_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_co_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + hamming_co_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + hamming_co_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_chao_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + hamming_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + hamming_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_man_chao_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + man_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + man_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_euc_chao_euclidean_seg_weighted_cont = brm(distance | cens(censored) ~ (euclidean_segdist_multivalue_weighted_cont + euc_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted_cont + euc_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))

waic(model_mono_hamming_auto_euclidean_seg_weighted_cont)
waic(model_mono_hamming_oc_euclidean_seg_weighted_cont)
waic(model_mono_hamming_oco_euclidean_seg_weighted_cont)
waic(model_mono_hamming_co_euclidean_seg_weighted_cont)
waic(model_mono_hamming_chao_euclidean_seg_weighted_cont)
waic(model_mono_man_chao_euclidean_seg_weighted_cont)
waic(model_mono_euc_chao_euclidean_seg_weighted_cont)

hamming_mono_euclidean_weighted_seg_kfold = kfold(model_mono_hamming_auto_euclidean_seg_weighted_cont ,model_mono_hamming_oc_euclidean_seg_weighted_cont ,model_mono_hamming_oco_euclidean_seg_weighted_cont ,model_mono_hamming_co_euclidean_seg_weighted_cont ,model_mono_hamming_chao_euclidean_seg_weighted_cont ,model_mono_man_chao_euclidean_seg_weighted_cont ,model_mono_euc_chao_euclidean_seg_weighted_cont , compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)

model_mono_hamming_auto_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + hamming_auto_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + hamming_auto_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oc_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + hamming_oc_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + hamming_oc_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_oco_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + hamming_oco_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + hamming_oco_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_co_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + hamming_co_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + hamming_co_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_hamming_chao_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + hamming_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + hamming_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_man_chao_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + man_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + man_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))
model_mono_euc_chao_manhattan_seg_weighted_cont = brm(distance | cens(censored) ~ (manhattan_segdist_multivalue_weighted_cont + euc_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted_cont + euc_chao_weighted, data = dist_judgements_melted_mono, cores = getOption("mc.cores", 4L))

waic(model_mono_hamming_auto_manhattan_seg_weighted_cont)
waic(model_mono_hamming_oc_manhattan_seg_weighted_cont)
waic(model_mono_hamming_oco_manhattan_seg_weighted_cont)
waic(model_mono_hamming_co_manhattan_seg_weighted_cont)
waic(model_mono_hamming_chao_manhattan_seg_weighted_cont)
waic(model_mono_man_chao_manhattan_seg_weighted_cont)
waic(model_mono_euc_chao_manhattan_seg_weighted_cont)

hamming_mono_manhattan_weighted_seg_kfold = kfold(model_mono_hamming_auto_manhattan_seg_weighted_cont ,model_mono_hamming_oc_manhattan_seg_weighted_cont ,model_mono_hamming_oco_manhattan_seg_weighted_cont ,model_mono_hamming_co_manhattan_seg_weighted_cont ,model_mono_hamming_chao_manhattan_seg_weighted_cont ,model_mono_man_chao_manhattan_seg_weighted_cont ,model_mono_euc_chao_manhattan_seg_weighted_cont , compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)

