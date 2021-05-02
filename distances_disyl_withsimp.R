dist_judgements_melted_di = dist_judgements_melted_di[,-which(colnames(dist_judgements_melted_di) %in% c("hamming_stringdists_naive_weighted","hamming_stringdists_broe_weighted","euclidean_segdist_multivalue","manhattan_segdist_multivalue","hamming_segdist_multivalue","nclass_segdists_weighted","hamming_segdists_new","simple_segdist"))]
#omitted

dist_judgements_melted_di = dist_judgements_melted[grep("D",dist_judgements_melted$variable),]
dist_judgements_melted_di$distance = dist_judgements_melted_di$distance * 8

fullmodel_brms_standata_di = make_standata(distance  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di)

fullmodel_cens_brms = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_cens_brms_data, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))
fullmodel_cens_brms = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))


#expl
plots_by_participant = list()
paste(paste("plots_by_participant[[",which(!judgement_nearempty),"]],",sep=""),collapse="")
for(i in 1:nrow(dist_judgements)){
  plots_by_participant[[i]] = ggplot(dist_judgements_melted_di[dist_judgements_melted_di$participant==paste("P",i,sep=""),]) +
    geom_point(aes(x=segdist,y=distance,col=tonedist))+
    theme_classic() + xlab("") + ylab("") + guides(col=FALSE) + theme(plot.margin=grid::unit(c(3,3,3,3), "mm"))
}
plot_grid(plots_by_participant[[1]],plots_by_participant[[2]],plots_by_participant[[3]],plots_by_participant[[4]],plots_by_participant[[5]],plots_by_participant[[6]],plots_by_participant[[7]],plots_by_participant[[8]],plots_by_participant[[9]],plots_by_participant[[10]],plots_by_participant[[11]],plots_by_participant[[12]],plots_by_participant[[13]],plots_by_participant[[14]],plots_by_participant[[15]],plots_by_participant[[16]],plots_by_participant[[17]],plots_by_participant[[18]],plots_by_participant[[19]],plots_by_participant[[20]],plots_by_participant[[21]],plots_by_participant[[22]],plots_by_participant[[23]],plots_by_participant[[24]],plots_by_participant[[25]],plots_by_participant[[28]],plots_by_participant[[29]],plots_by_participant[[30]],plots_by_participant[[38]],ncol=5)

plots_by_participant = list()
paste(paste("plots_by_participant[[",which(!judgement_nearempty),"]],",sep=""),collapse="")
for(i in 1:nrow(dist_judgements)){
  plots_by_participant[[i]] = ggplot(dist_judgements_melted_di[dist_judgements_melted_di$participant==paste("P",i,sep=""),]) +
    geom_point(aes(x=tonedist,y=distance,col=segdist))+
    theme_classic() + xlab("") + ylab("") + guides(col=FALSE) + theme(plot.margin=grid::unit(c(3,3,3,3), "mm"))
}
plot_grid(plots_by_participant[[1]],plots_by_participant[[2]],plots_by_participant[[3]],plots_by_participant[[4]],plots_by_participant[[5]],plots_by_participant[[6]],plots_by_participant[[7]],plots_by_participant[[8]],plots_by_participant[[9]],plots_by_participant[[10]],plots_by_participant[[11]],plots_by_participant[[12]],plots_by_participant[[13]],plots_by_participant[[14]],plots_by_participant[[15]],plots_by_participant[[16]],plots_by_participant[[17]],plots_by_participant[[18]],plots_by_participant[[19]],plots_by_participant[[20]],plots_by_participant[[21]],plots_by_participant[[22]],plots_by_participant[[23]],plots_by_participant[[24]],plots_by_participant[[25]],plots_by_participant[[28]],plots_by_participant[[29]],plots_by_participant[[30]],plots_by_participant[[38]],ncol=5)

#brms
censored_di = as.integer(dist_judgements_melted_di$distance == 8)
dist_judgements_melted_di = cbind.data.frame(dist_judgements_melted_di,censored_di)


fullmodel_brms_standata_di = make_standata(distance | cens(censored_di) ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di)

fullmodel_cens_brms = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))
fullmodel_cens_brms2 = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2500, chains = 4, control = list(adapt_delta = 0.95, max_treedepth = 20))
fullmodel_cens_brms2 = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.95, max_treedepth = 20))
fullmodel_cens_brms3 = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 20))
fullmodel_cens_brms4 = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.995, max_treedepth = 20))
fullmodel_cens_brms5 = stan(file = "fullmodel_cens_brms.stan", data = fullmodel_brms_standata_di, iter = 2500, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 20))

fullmodel_cens_brms6 = stan(file = "fullmodel_cens_brms_samecorrel.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))
fullmodel_cens_brms7 = stan(file = "fullmodel_cens_brms_samecorrel.stan", data = fullmodel_brms_standata_di, iter = 2500, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 20))
fullmodel_cens_brms7 = stan(file = "fullmodel_cens_brms_pop2fixedeff_sigmab.stan", data = fullmodel_brms_standata_di, iter = 2000, chains = 4, control = list(adapt_delta = 0.9, max_treedepth = 20))



#Models I-VI
model_notrial_di_bayes = brm(distance  ~ segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_ransubj_bayes = brm(distance  ~ (1|participant) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_rancepts_bayes = brm(distance  ~ (1|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_ranslope_bayes = brm(distance  ~ (1 + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_translope_bayes = brm(distance  ~ (1 + tonedist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_ranslopes_bayes = brm(distance  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))


model_notrial_di_bayes_censored_di = brm(distance   | cens(censored_di)~ segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_ransubj_bayes_censored_di = brm(distance | cens(censored_di)  ~ (1|participant) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_rancepts_bayes_censored_di = brm(distance | cens(censored_di)  ~ (1|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_ranslope_bayes_censored_di = brm(distance | cens(censored_di)  ~ (1 + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_translope_bayes_censored_di = brm(distance | cens(censored_di)  ~ (1 + tonedist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_notrial_di_ranslopes_bayes_censored_di = brm(distance | cens(censored_di)  ~ (1 + tonedist + segdist|participant) + (1|variable) + segdist + tonedist, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L)) #Full model!

waic(model_notrial_di_bayes)
waic(model_notrial_di_ransubj_bayes)
waic(model_notrial_di_rancepts_bayes)
waic(model_notrial_di_ranslope_bayes)
waic(model_notrial_di_translope_bayes)
waic(model_notrial_di_ranslopes_bayes)
waic(model_notrial_di_bayes_censored_di)
waic(model_notrial_di_ransubj_bayes_censored_di)
waic(model_notrial_di_rancepts_bayes_censored_di)
waic(model_notrial_di_ranslope_bayes_censored_di)
waic(model_notrial_di_translope_bayes_censored_di)
waic(model_notrial_di_ranslopes_bayes_censored_di)


#Model params
stanplot(model_notrial_di_ranslopes_bayes_censored_di, prob_outer=.95)
summary(model_notrial_di_ranslopes_bayes_censored_di)

#On average, seg dist is greater?
hypothesis(model_notrial_di_ranslopes_bayes_censored_di,"b_segdist-b_tonedist=0",class=NULL)
hypothesis(model_di_hamming_coplus,"b_segdist-b_hamming_coplus=0",class=NULL)

#Slopes
slopes = cbind(coef(model_notrial_di_ranslopes_bayes_censored_di)$participant[,,"segdist"][,1],coef(model_notrial_di_ranslopes_bayes_censored_di)$participant[,,"tonedist"][,1])
colnames(slopes)=c("tonebeta","segbeta")
slopes = cbind.data.frame(slopes,as.integer(judgement_nearempty))
#slopes = data.frame(slopes)[!judgement_nearempty,]
ggplot(data=slopes,aes(x=segbeta,y=tonebeta,col=judgement_nearempty)) + xlab("Tone weighting") + ylab("Segment weighting") + geom_point() + scale_x_continuous(limits=c(0,3),breaks=c(0,.5,1,1.5,2,2.5,3))+ scale_y_continuous(limits=c(0,3),breaks=c(0,1,.5,1.5,2,2.5,3)) + guides(col=FALSE)
hypothesis(model_notrial_di_ranslopes_bayes_censored_di,c("sd_participant__segdist/b_segdist=0","sd_participant__tonedist/b_tonedist=0","sd_participant__segdist/b_segdist-sd_participant__tonedist/b_tonedist=0"),class=NULL)


#Residuals
model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept_predicts = predict(model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept)[,1]
model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept_predicts[model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept_predicts<0] = 0
model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept_predicts[model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept_predicts>4] = 4
residuals = (model_notrial_di_ranslopes_bayes_censor_intercept_partnointercept_predicts-dist_judgements_melted_di$distance)

plot(dist_judgements_melted_di$participant,residuals)

#Getting more distances
disyls = read.csv("disyl-items.csv")
old_disyl_segments = paste(disyls[,1],disyls[,2],disyls[,3],disyls[,5],disyls[,6],disyls[,7],sep="")
new_disyl_segments = paste(disyls[,9],disyls[,10],disyls[,11],disyls[,13],disyls[,14],disyls[,15],sep="")
old_disyl_segments_syl1 = paste(disyls[,1],disyls[,2],disyls[,3],sep="")
old_disyl_segments_syl2 = paste(disyls[,5],disyls[,6],disyls[,7],sep="")
new_disyl_segments_syl1 = paste(disyls[,9],disyls[,10],disyls[,11],sep="")
new_disyl_segments_syl2 = paste(disyls[,13],disyls[,14],disyls[,15],sep="")
old_disyl_tones = paste(disyls[,4],disyls[,8],sep="")
new_disyl_tones = paste(disyls[,12],disyls[,16],sep="")
old_disyl_tone1s = disyls[,4]
new_disyl_tone1s = disyls[,12]
old_disyl_tone2s = disyls[,8]
new_disyl_tone2s = disyls[,16]


canto.features = read.table("canto-features.txt")
feature.matrix = canto.features
features.hamming.matrix = getDistancesFromFeatures(canto.features,normalise=TRUE)
hamming_stringdists_disyl = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE, distance.matrix=features.hamming.matrix, tostrings=new_disyl_segments))
hamming_stringdists_disyl_new = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE,costs=c(.5,.5,1,1), distance.matrix=features.hamming.matrix,cross=F, tostrings=new_disyl_segments))
hamming_stringdists_disyl = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_stringdists_disyl)
colnames(hamming_stringdists_disyl)[1] = "variable"
hamming_stringdists_disyl_new = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_stringdists_disyl_new)
colnames(hamming_stringdists_disyl_new)[1] = "variable"


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

tone_dists_disyl = matrix(nrow=72,ncol=7)
colnames(tone_dists_disyl) = c("hamming_auto","hamming_oc","hamming_oco","hamming_co","hamming_chao","man_chao","euc_chao")
for(i in 1:ncol(tone_dists_disyl)){
  currTable = tone_dist_tables[[i]]
  tone_dists_disyl[,i]=diag(currTable[old_disyl_tone1s,new_disyl_tone1s])+diag(currTable[old_disyl_tone2s,new_disyl_tone2s])
}
tone_dists_disyl = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),tone_dists_disyl)
colnames(tone_dists_disyl)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, tone_dists_disyl, by = "variable")

#beginmodels
model_di_hamming_auto = brm(distance | cens(censored_di) ~ (segdist + hamming_auto | participant) +(1|variable) + segdist + hamming_auto, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco = brm(distance | cens(censored_di) ~ (segdist +hamming_oco | participant) +(1|variable) + segdist + hamming_oco, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co = brm(distance | cens(censored_di) ~ (segdist + hamming_co | participant) +(1|variable) + segdist + hamming_co, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao = brm(distance | cens(censored_di) ~ (segdist + hamming_chao | participant) +(1|variable) + segdist + hamming_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao = brm(distance | cens(censored_di) ~ (segdist + man_chao | participant) +(1|variable) + segdist + man_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao = brm(distance | cens(censored_di) ~ (segdist + euc_chao | participant) +(1|variable) + segdist + euc_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto)
waic(model_di_hamming_oco)
waic(model_di_hamming_co)
waic(model_di_hamming_chao)
waic(model_di_man_chao)
waic(model_di_euc_chao)

dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_stringdists_disyl_new, by = "variable")
colnames(dist_judgements_melted_di)[ncol(dist_judgements_melted_di)] = "hamming_segdists"

ggggg

model_di_hamming_auto_norand = brm(distance | cens(censored_di) ~ (1 | participant) +(1|variable) + segdist + hamming_auto, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_norand = brm(distance | cens(censored_di) ~ (1 | participant) +(1|variable) + segdist + hamming_oco, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_norand = brm(distance | cens(censored_di) ~ (1 | participant) +(1|variable) + segdist + hamming_co, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_norand = brm(distance | cens(censored_di) ~ (1 | participant) +(1|variable) + segdist + hamming_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_norand = brm(distance | cens(censored_di) ~ (1 | participant) +(1|variable) + segdist + man_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_norand = brm(distance | cens(censored_di) ~ (1 | participant) +(1|variable) + segdist + euc_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_norand)
waic(model_di_hamming_oco_norand)
waic(model_di_hamming_co_norand)
waic(model_di_hamming_chao_norand)
waic(model_di_man_chao_norand)
waic(model_di_euc_chao_norand)


#Hamming distances with dist. features

#Euclidean, Manhattan & Hamming distances with multivalued features

#Info weighting
lexicon = read.csv("syl_freqs.csv",na.strings = "nincompoop")
segments.naive.weights = getInfoGainFromWordlist(canto.features,lexicon$klatt,lexicon$freq,mode="naive")
segments.broe.weights = getInfoGainFromWordlist(canto.features,lexicon$klatt,lexicon$freq,mode="broe")

naive.weighted.matrix = getDistancesFromFeatures(canto.features,normalise=T,weights=segments.naive.weights)
broe.weighted.matrix = getDistancesFromFeatures(canto.features,normalise=T,weights=segments.broe.weights,zeroMultiplier=.5)

hamming_stringdists_disyl_naive_weighted = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, tostrings=new_disyl_segments))
hamming_stringdists_disyl_naive_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_stringdists_disyl_naive_weighted)
colnames(hamming_stringdists_disyl_naive_weighted)[1] = "variable"
hamming_stringdists_disyl_broe_weighted = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, tostrings=new_disyl_segments))
hamming_stringdists_disyl_broe_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_stringdists_disyl_broe_weighted)
colnames(hamming_stringdists_disyl_broe_weighted)[1] = "variable"

hamming_stringdists_disyl_new_naive_weighted = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, cross=F, tostrings=new_disyl_segments, costs=c(.5,.5,1,1)))
hamming_stringdists_disyl_new_naive_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_stringdists_disyl_new_naive_weighted)
colnames(hamming_stringdists_disyl_new_naive_weighted)[1] = "variable"
hamming_stringdists_disyl_new_broe_weighted = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE, distance.matrix=naive.weighted.matrix, cross=F, tostrings=new_disyl_segments, costs=c(.5,.5,1,1)))
hamming_stringdists_disyl_new_broe_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_stringdists_disyl_new_broe_weighted)
colnames(hamming_stringdists_disyl_new_broe_weighted)[1] = "variable"


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

tone_dists_disyl_weighted = matrix(nrow=72,ncol=7)
colnames(tone_dists_disyl_weighted) = c("hamming_auto_weighted","hamming_oc_weighted","hamming_oco_weighted","hamming_co_weighted","hamming_chao_weighted","man_chao_weighted","euc_chao_weighted")
for(i in 1:ncol(tone_dists_disyl_weighted)){
  currTable = tone_dist_tables_weighted[[i]]
  tone_dists_disyl_weighted[,i]=diag(currTable[old_disyl_tone1s,new_disyl_tone1s])+diag(currTable[old_disyl_tone2s,new_disyl_tone2s])
}
tone_dists_disyl_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),tone_dists_disyl_weighted)
colnames(tone_dists_disyl_weighted)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, tone_dists_disyl_weighted, by = "variable")
dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_stringdists_disyl_new_naive_weighted, by = "variable")
colnames(dist_judgements_melted_di)[which(colnames(dist_judgements_melted_di) == "hamming_stringdists_disyl_new_naive_weighted")] = "hamming_stringdists_naive_weighted"
dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_stringdists_disyl_new_broe_weighted, by = "variable")
colnames(dist_judgements_melted_di)[which(colnames(dist_judgements_melted_di) == "hamming_stringdists_disyl_new_broe_weighted")] = "hamming_stringdists_broe_weighted"

model_di_hamming_auto_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_auto_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_oc_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_oc_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_oco_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_oco_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_co_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_co_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_chao_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + man_chao_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + man_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + euc_chao_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + euc_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_hamming_seg_weighted)
waic(model_di_hamming_oc_hamming_seg_weighted)
waic(model_di_hamming_oco_hamming_seg_weighted)
waic(model_di_hamming_co_hamming_seg_weighted)
waic(model_di_hamming_chao_hamming_seg_weighted)
waic(model_di_man_chao_hamming_seg_weighted)
waic(model_di_euc_chao_hamming_seg_weighted)

model_di_hamming_auto_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + hamming_auto_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + hamming_oc_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_oc_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + hamming_oco_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_oco_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + hamming_co_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_co_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + hamming_chao_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + hamming_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + man_chao_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + man_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_hamming_seg_weighted_broe = brm(distance | cens(censored_di) ~ (hamming_stringdists_broe_weighted + euc_chao_weighted | participant) +(1|variable) + hamming_stringdists_broe_weighted + euc_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_hamming_seg_weighted_broe)
waic(model_di_hamming_oc_hamming_seg_weighted_broe)
waic(model_di_hamming_oco_hamming_seg_weighted_broe)
waic(model_di_hamming_co_hamming_seg_weighted_broe)
waic(model_di_hamming_chao_hamming_seg_weighted_broe)
waic(model_di_man_chao_hamming_seg_weighted_broe)
waic(model_di_euc_chao_hamming_seg_weighted_broe)


model_di_hamming_auto_hamming_seg_toneweightonly = brm(distance | cens(censored_di) ~ (hamming_segdists + hamming_auto_weighted | participant) +(1|variable) + hamming_segdists + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))



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

old_disyl_segments_allophones = paste(allophones(old_disyl_segments_syl1),allophones(old_disyl_segments_syl2),sep="")
new_disyl_segments_allophones = paste(allophones(new_disyl_segments_syl1),allophones(new_disyl_segments_syl2),sep="")

euclidean_segdist_multivalue = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1)))
manhattan_segdist_multivalue = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1)))
hamming_segdist_multivalue = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1)))

euclidean_segdist_multivalue = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),euclidean_segdist_multivalue)
colnames(euclidean_segdist_multivalue)[1] = "variable"
manhattan_segdist_multivalue = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),manhattan_segdist_multivalue)
colnames(manhattan_segdist_multivalue)[1] = "variable"
hamming_segdist_multivalue = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_segdist_multivalue)
colnames(hamming_segdist_multivalue)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, euclidean_segdist_multivalue, by = "variable")
dist_judgements_melted_di = right_join(dist_judgements_melted_di, manhattan_segdist_multivalue, by = "variable")
dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_segdist_multivalue, by = "variable")

model_di_hamming_auto_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_auto | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_auto, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_oc | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_oc, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_oco | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_oco, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_co | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_co, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_chao | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + man_chao | participant) +(1|variable) + euclidean_segdist_multivalue + man_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + euc_chao | participant) +(1|variable) + euclidean_segdist_multivalue + euc_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_euclidean_seg)
waic(model_di_hamming_oc_euclidean_seg)
waic(model_di_hamming_oco_euclidean_seg)
waic(model_di_hamming_co_euclidean_seg)
waic(model_di_hamming_chao_euclidean_seg)
waic(model_di_man_chao_euclidean_seg)
waic(model_di_euc_chao_euclidean_seg)

hamming_di_euclidean_seg_kfold = kfold(model_di_hamming_auto_euclidean_seg,model_di_hamming_oc_euclidean_seg,model_di_hamming_oco_euclidean_seg,model_di_hamming_co_euclidean_seg,model_di_hamming_chao_euclidean_seg,model_di_man_chao_euclidean_seg,model_di_euc_chao_euclidean_seg, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


model_di_hamming_auto_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_auto | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_auto, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_oc | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_oc, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_oco | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_oco, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_co | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_co, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_chao | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + man_chao | participant) +(1|variable) + manhattan_segdist_multivalue + man_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + euc_chao | participant) +(1|variable) + manhattan_segdist_multivalue + euc_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_manhattan_seg)
waic(model_di_hamming_oc_manhattan_seg)
waic(model_di_hamming_oco_manhattan_seg)
waic(model_di_hamming_co_manhattan_seg)
waic(model_di_hamming_chao_manhattan_seg)
waic(model_di_man_chao_manhattan_seg)
waic(model_di_euc_chao_manhattan_seg)

hamming_di_manhattan_seg_kfold = kfold(model_di_hamming_auto_manhattan_seg,model_di_hamming_oc_manhattan_seg,model_di_hamming_oco_manhattan_seg,model_di_hamming_co_manhattan_seg,model_di_hamming_chao_manhattan_seg,model_di_man_chao_manhattan_seg,model_di_euc_chao_manhattan_seg, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


model_di_hamming_auto_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_auto | participant) +(1|variable) + hamming_segdist_multivalue + hamming_auto, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_oc | participant) +(1|variable) + hamming_segdist_multivalue + hamming_oc, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_oco | participant) +(1|variable) + hamming_segdist_multivalue + hamming_oco, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_co | participant) +(1|variable) + hamming_segdist_multivalue + hamming_co, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_chao | participant) +(1|variable) + hamming_segdist_multivalue + hamming_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + man_chao | participant) +(1|variable) + hamming_segdist_multivalue + man_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + euc_chao | participant) +(1|variable) + hamming_segdist_multivalue + euc_chao, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_hamming_seg_multival)
waic(model_di_hamming_oc_hamming_seg_multival)
waic(model_di_hamming_oco_hamming_seg_multival)
waic(model_di_hamming_co_hamming_seg_multival)
waic(model_di_hamming_chao_hamming_seg_multival)
waic(model_di_man_chao_hamming_seg_multival)
waic(model_di_euc_chao_hamming_seg_multival)

hamming_di_hamming_multival_seg_kfold = kfold(model_di_hamming_auto_hamming_seg_multival,model_di_hamming_oc_hamming_seg_multival,model_di_hamming_oco_hamming_seg_multival,model_di_hamming_co_hamming_seg_multival,model_di_hamming_chao_hamming_seg_multival,model_di_man_chao_hamming_seg_multival,model_di_euc_chao_hamming_seg_multival, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


euclidean_segdist_multivalue = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1)))
manhattan_segdist_multivalue = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1)))
hamming_segdist_multivalue = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1)))

feature.matrix.multivalue.factors = data.frame(lapply(feature.matrix.multivalue,as.factor))
rownames(feature.matrix.multivalue.factors) = rownames(feature.matrix.multivalue)

multivalue.weights = getInfoGainFromWordlist(feature.matrix.multivalue.factors,allophones(lexicon$klatt),lexicon$freq,mode="naive")

euclidean_segdist_multivalue_weighted = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.euclidean,costs=c(.5,.5,1,1),weights=multivalue.weights))
manhattan_segdist_multivalue_weighted = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.manhattan,costs=c(.5,.5,1,1),weights=multivalue.weights))
hamming_segdist_multivalue_weighted = diag(stringdist(old_disyl_segments_allophones,tostrings=new_disyl_segments_allophones,segment.specific.costs=TRUE,distance.matrix=distances.multivalue.hamming,costs=c(.5,.5,1,1),weights=multivalue.weights))

euclidean_segdist_multivalue_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),euclidean_segdist_multivalue_weighted)
colnames(euclidean_segdist_multivalue_weighted)[1] = "variable"
manhattan_segdist_multivalue_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),manhattan_segdist_multivalue_weighted)
colnames(manhattan_segdist_multivalue_weighted)[1] = "variable"
hamming_segdist_multivalue_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_segdist_multivalue_weighted)
colnames(hamming_segdist_multivalue_weighted)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, euclidean_segdist_multivalue_weighted, by = "variable")
dist_judgements_melted_di = right_join(dist_judgements_melted_di, manhattan_segdist_multivalue_weighted, by = "variable")
dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_segdist_multivalue_weighted, by = "variable")

model_di_hamming_auto_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_auto_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_oc_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_oc_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_oco_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_oco_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_co_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_co_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + man_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + man_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + euc_chao_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + euc_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_euclidean_seg_weighted)
waic(model_di_hamming_oc_euclidean_seg_weighted)
waic(model_di_hamming_oco_euclidean_seg_weighted)
waic(model_di_hamming_co_euclidean_seg_weighted)
waic(model_di_hamming_chao_euclidean_seg_weighted)
waic(model_di_man_chao_euclidean_seg_weighted)
waic(model_di_euc_chao_euclidean_seg_weighted)

hamming_di_euclidean_weighted_seg_kfold = kfold(model_di_hamming_auto_euclidean_seg_weighted,model_di_hamming_oc_euclidean_seg_weighted,model_di_hamming_oco_euclidean_seg_weighted,model_di_hamming_co_euclidean_seg_weighted,model_di_hamming_chao_euclidean_seg_weighted,model_di_man_chao_euclidean_seg_weighted,model_di_euc_chao_euclidean_seg_weighted, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


model_di_hamming_auto_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_auto_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_oc_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_oc_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_oco_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_oco_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_co_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_co_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_chao | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + man_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + man_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + euc_chao_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + euc_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_manhattan_seg_weighted)
waic(model_di_hamming_oc_manhattan_seg_weighted)
waic(model_di_hamming_oco_manhattan_seg_weighted)
waic(model_di_hamming_co_manhattan_seg_weighted)
waic(model_di_hamming_chao_manhattan_seg_weighted)
waic(model_di_man_chao_manhattan_seg_weighted)
waic(model_di_euc_chao_manhattan_seg_weighted)

hamming_di_manhattan_weighted_seg_kfold = kfold(model_di_hamming_auto_manhattan_seg_weighted,model_di_hamming_oc_manhattan_seg_weighted,model_di_hamming_oco_manhattan_seg_weighted,model_di_hamming_co_manhattan_seg_weighted,model_di_hamming_chao_manhattan_seg_weighted,model_di_man_chao_manhattan_seg_weighted,model_di_euc_chao_manhattan_seg_weighted, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


model_di_hamming_auto_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_auto_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_oc_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_oc_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_oco_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_oco_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_co_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_co_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_chao_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + man_chao_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + man_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + euc_chao_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + euc_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_hamming_seg_multival_weighted)
waic(model_di_hamming_oc_hamming_seg_multival_weighted)
waic(model_di_hamming_oco_hamming_seg_multival_weighted)
waic(model_di_hamming_co_hamming_seg_multival_weighted)
waic(model_di_hamming_chao_hamming_seg_multival_weighted)
waic(model_di_man_chao_hamming_seg_multival_weighted)
waic(model_di_euc_chao_hamming_seg_multival_weighted)

hamming_di_hamming_multival_weighted_seg_kfold = kfold(model_di_hamming_auto_hamming_seg_multival_weighted,model_di_hamming_oc_hamming_seg_multival_weighted,model_di_hamming_oco_hamming_seg_multival_weighted,model_di_hamming_co_hamming_seg_multival_weighted,model_di_hamming_chao_hamming_seg_multival_weighted,model_di_man_chao_hamming_seg_multival_weighted,model_di_euc_chao_hamming_seg_multival_weighted, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


#Natural classes distances
nclass.matrix = getNaturalClassMatrixFromFeatures(feature.matrix)
nclass.weights = getInfoGainFromWordlist(nclass.matrix,lexicon$klatt,lexicon$freq,mode="naive")
nclass.dist.matrix.weighted = getDistancesFromFeatures(nclass.matrix,normalise=TRUE,weights=nclass.weights)
nclass_segdists_weighted = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = TRUE,costs=c(.5,.5,1,1), distance.matrix=nclass.dist.matrix.weighted, tostrings=new_disyl_segments,cross=F))

nclass_segdists_weighted = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),nclass_segdists_weighted)
colnames(nclass_segdists_weighted)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, nclass_segdists_weighted, by = "variable")

model_di_hamming_auto_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_auto_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_auto_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oc_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_oc_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_oc_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_oco_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_oco_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_oco_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_co_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_co_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_co_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_chao_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_chao_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_man_chao_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + man_chao_weighted | participant) +(1|variable) + nclass_segdists_weighted + man_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_euc_chao_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + euc_chao_weighted | participant) +(1|variable) + nclass_segdists_weighted + euc_chao_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_auto_nclass_segdists_weighted)
waic(model_di_hamming_oc_nclass_segdists_weighted)
waic(model_di_hamming_oco_nclass_segdists_weighted)
waic(model_di_hamming_co_nclass_segdists_weighted)
waic(model_di_hamming_chao_nclass_segdists_weighted)
waic(model_di_man_chao_nclass_segdists_weighted)
waic(model_di_euc_chao_nclass_segdists_weighted)



#New tone representation
old_disyl_twocolumn = data.frame(t(cbind(old_disyl_tone1s,old_disyl_tone2s)))
new_disyl_twocolumn = data.frame(t(cbind(new_disyl_tone1s,new_disyl_tone2s)))
canto_tone_oco_char = sapply(canto_tone_oco,as.character)
pitches = c("¢ä","L","M","H")
findOCOPlus = function(twocolumn,average=F){
  if(!average){
    difference = which(pitches==canto_tone_oco_char[as.integer(twocolumn[1]),4])[1] - which(pitches==canto_tone_oco_char[as.integer(twocolumn[2]),2])[1]
  } else {
    difference = which(pitches==canto_tone_oco_char[as.integer(twocolumn[1]),2])[1] + which(pitches==canto_tone_oco_char[as.integer(twocolumn[1]),4])[1] - which(pitches==canto_tone_oco_char[as.integer(twocolumn[2]),2])[1] - which(pitches==canto_tone_oco_char[as.integer(twocolumn[2]),4])[1]
  }
  if(difference == 0){
    trans = "L"
  } else if(difference < 0){
    trans = "R"
  } else {
    trans = "F"
  }
  ocoplus = paste(paste(canto_tone_oco_char[as.integer(twocolumn[1]),2:4],sep="",collapse=""),trans,paste(canto_tone_oco_char[as.integer(twocolumn[2]),2:4],sep="",collapse=""),sep="")
  return(ocoplus)
}
old_ocoplusedges = sapply(old_disyl_twocolumn,findOCOPlus)
old_ocoplusavg = sapply(old_disyl_twocolumn,findOCOPlus,average=T)
new_ocoplusedges = sapply(new_disyl_twocolumn,findOCOPlus)
new_ocoplusavg = sapply(new_disyl_twocolumn,findOCOPlus,average=T)

names(old_ocoplusedges) = paste("D",seq(1,72,1),"_1",sep="")
names(old_ocoplusavg) = paste("D",seq(1,72,1),"_1",sep="")
names(new_ocoplusedges) = paste("D",seq(1,72,1),"_1",sep="")
names(new_ocoplusavg) = paste("D",seq(1,72,1),"_1",sep="")

hamming_dists_ocoplusedge = diag(stringdist(old_ocoplusedges,tostrings=new_ocoplusedges,mode="hamming"))
hamming_dists_ocoplusavg = diag(stringdist(old_ocoplusavg,tostrings=new_ocoplusavg,mode="hamming"))

hamming_dists_ocoplusedge = hamming_dists_ocoplusedge / max(hamming_dists_ocoplusedge) * 2
hamming_dists_ocopluseavg = hamming_dists_ocoplusavg / max(hamming_dists_ocoplusavg) * 2
hamming_dists_ocoplusedge = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_dists_ocoplusedge)
hamming_dists_ocopluseavg = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_dists_ocopluseavg)
colnames(hamming_dists_ocoplusedge)[1] = "variable"
colnames(hamming_dists_ocopluseavg)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_dists_ocoplusedge, by = "variable")
dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_dists_ocopluseavg, by = "variable")

colnames(dist_judgements_melted_di)[which(colnames(dist_judgements_melted_di)=="hamming_dists_ocoplusedge")] = "hamming_ocoplusedge"
colnames(dist_judgements_melted_di)[which(colnames(dist_judgements_melted_di)=="hamming_dists_ocopluseavg")] = "hamming_ocoplusavg"

model_di_hamming_ocoplusavg = brm(distance | cens(censored_di) ~ (segdist +hamming_ocoplusavg | participant) +(1|variable) + segdist + hamming_ocoplusavg, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_hamming_seg = brm(distance | cens(censored_di) ~ (hamming_segdists + hamming_ocoplusavg | participant) +(1|variable) + hamming_segdists + hamming_ocoplusavg, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_ocoplusavg | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_ocoplusavg, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_ocoplusavg | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_ocoplusavg, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_ocoplusavg | participant) +(1|variable) + hamming_segdist_multivalue + hamming_ocoplusavg, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_ocoplusavg)
waic(model_di_hamming_ocoplusavg_hamming_seg_new)
waic(model_di_hamming_ocoplusavg_euclidean_seg)
waic(model_di_hamming_ocoplusavg_manhattan_seg)
waic(model_di_hamming_ocoplusavg_hamming_seg_multival)

kfold_di_hamming_ocoplusavg = kfold(model_di_hamming_ocoplusavg,model_di_hamming_ocoplusavg_hamming_seg_new,model_di_hamming_ocoplusavg_euclidean_seg,model_di_hamming_ocoplusavg_manhattan_seg,model_di_hamming_ocoplusavg_hamming_seg_multival, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE) #need to be rerun



model_di_hamming_ocoplusedge = brm(distance | cens(censored_di) ~ (segdist +hamming_ocoplusedge | participant) +(1|variable) + segdist + hamming_ocoplusedge, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_hamming_seg = brm(distance | cens(censored_di) ~ (hamming_segdists + hamming_ocoplusedge | participant) +(1|variable) + hamming_segdists + hamming_ocoplusedge, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_ocoplusedge | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_ocoplusedge, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_ocoplusedge | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_ocoplusedge, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_ocoplusedge | participant) +(1|variable) + hamming_segdist_multivalue + hamming_ocoplusedge, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))


waic(model_di_hamming_ocoplusedge)
waic(model_di_hamming_ocoplusedge_hamming_seg_new)
waic(model_di_hamming_ocoplusedge_euclidean_seg)
waic(model_di_hamming_ocoplusedge_manhattan_seg)
waic(model_di_hamming_ocoplusedge_hamming_seg_multival)

kfold_di_hamming_ocoplusedge = kfold(model_di_hamming_ocoplusedge,model_di_hamming_ocoplusedge_hamming_seg_new,model_di_hamming_ocoplusedge_euclidean_seg,model_di_hamming_ocoplusedge_manhattan_seg,model_di_hamming_ocoplusedge_hamming_seg_multival, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


model_di_hamming_ocoplusavg_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_ocoplusavg_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_ocoplusavg_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_ocoplusavg_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_ocoplusavg_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_ocoplusavg_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_ocoplusavg_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_ocoplusavg_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_ocoplusavg_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusavg_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_ocoplusavg_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_ocoplusavg_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

kfold_di_hamming_ocoplusavg_weighted = kfold(model_di_hamming_ocoplusavg_hamming_seg_weighted,model_di_hamming_ocoplusavg_euclidean_seg_weighted,model_di_hamming_ocoplusavg_manhattan_seg_weighted,model_di_hamming_ocoplusavg_hamming_seg_multival_weighted,model_di_hamming_ocoplusavg_nclass_segdists_weighted, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


model_di_hamming_ocoplusedge_hamming_seg_weighted = brm(distance | cens(censored_di) ~ (hamming_stringdists_naive_weighted + hamming_ocoplusedge_weighted | participant) +(1|variable) + hamming_stringdists_naive_weighted + hamming_ocoplusedge_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_euclidean_seg_weighted = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue_weighted + hamming_ocoplusedge_weighted | participant) +(1|variable) + euclidean_segdist_multivalue_weighted + hamming_ocoplusedge_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_manhattan_seg_weighted = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue_weighted + hamming_ocoplusedge_weighted | participant) +(1|variable) + manhattan_segdist_multivalue_weighted + hamming_ocoplusedge_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_hamming_seg_multival_weighted = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue_weighted + hamming_ocoplusedge_weighted | participant) +(1|variable) + hamming_segdist_multivalue_weighted + hamming_ocoplusedge_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_ocoplusedge_nclass_segdists_weighted = brm(distance | cens(censored_di) ~ (nclass_segdists_weighted + hamming_ocoplusedge_weighted | participant) +(1|variable) + nclass_segdists_weighted + hamming_ocoplusedge_weighted, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

kfold_di_hamming_ocoplusedge_weighted = kfold(model_di_hamming_ocoplusedge_hamming_seg_weighted,model_di_hamming_ocoplusedge_euclidean_seg_weighted,model_di_hamming_ocoplusedge_manhattan_seg_weighted,model_di_hamming_ocoplusedge_hamming_seg_multival_weighted,model_di_hamming_ocoplusedge_nclass_segdists_weighted, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


findCOPlus = function(twocolumn){
  difference = which(pitches==canto_tone_oco_char[as.integer(twocolumn[1]),4])[1] - which(pitches==canto_tone_oco_char[as.integer(twocolumn[2]),4])[1]
  if(difference == 0){
    trans = "L"
  } else if(difference < 0){
    trans = "R"
  } else {
    trans = "F"
  }
  coplus = paste(paste(canto_tone_oco_char[as.integer(twocolumn[1]),3:4],sep="",collapse=""),trans,paste(canto_tone_oco_char[as.integer(twocolumn[2]),3:4],sep="",collapse=""),sep="")
  return(coplus)
}

old_copluss = sapply(old_disyl_twocolumn,findCOPlus)
new_copluss = sapply(new_disyl_twocolumn,findCOPlus)

names(old_copluss) = paste("D",seq(1,72,1),"_1",sep="")
names(new_copluss) = paste("D",seq(1,72,1),"_1",sep="")

hamming_dists_coplus = diag(stringdist(old_copluss,tostrings=new_copluss,mode="hamming"))

hamming_dists_coplus = hamming_dists_coplus / max(hamming_dists_coplus) * 2
hamming_dists_coplus = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),hamming_dists_coplus)
colnames(hamming_dists_coplus)[1] = "variable"

dist_judgements_melted_di = right_join(dist_judgements_melted_di, hamming_dists_coplus, by = "variable")

colnames(dist_judgements_melted_di)[which(colnames(dist_judgements_melted_di)=="hamming_dists_coplus")] = "hamming_coplus"

model_di_hamming_coplus = brm(distance | cens(censored_di) ~ (segdist +hamming_coplus | participant) +(1|variable) + segdist + hamming_coplus, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_coplus_hamming_seg = brm(distance | cens(censored_di) ~ (hamming_segdists + hamming_coplus | participant) +(1|variable) + hamming_segdists + hamming_coplus, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_coplus_euclidean_seg = brm(distance | cens(censored_di) ~ (euclidean_segdist_multivalue + hamming_coplus | participant) +(1|variable) + euclidean_segdist_multivalue + hamming_coplus, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_coplus_manhattan_seg = brm(distance | cens(censored_di) ~ (manhattan_segdist_multivalue + hamming_coplus | participant) +(1|variable) + manhattan_segdist_multivalue + hamming_coplus, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))
model_di_hamming_coplus_hamming_seg_multival = brm(distance | cens(censored_di) ~ (hamming_segdist_multivalue + hamming_coplus | participant) +(1|variable) + hamming_segdist_multivalue + hamming_coplus, data = dist_judgements_melted_di, cores = getOption("mc.cores", 4L))

waic(model_di_hamming_coplus)
waic(model_di_hamming_coplus_hamming_seg_new)
waic(model_di_hamming_coplus_euclidean_seg)
waic(model_di_hamming_coplus_manhattan_seg)
waic(model_di_hamming_coplus_hamming_seg_multival)

kfold_di_hamming_coplus_weighted = kfold(model_di_hamming_coplus,model_di_hamming_coplus_hamming_seg_new,model_di_hamming_coplus_euclidean_seg,model_di_hamming_coplus_manhattan_seg,model_di_hamming_coplus_hamming_seg_multival, compare = TRUE, K = 10, Ksub = NULL, folds = NULL, group = NULL, exact_loo = NULL, resp = NULL, model_names = NULL, save_fits = FALSE)


inference(model_di_hamming_coplus,"segdist","hamming_coplus")-lp0


addDistsToDiTable = function(dists, name){
  dists = cbind.data.frame(paste("D",seq(1,72,1),"_1",sep=""),dists)
  colnames(dists)[1] = "variable"
  colnames(dists)[2] = name
  dist_judgements_melted_di = right_join(dist_judgements_melted_di, dists, by = "variable")
  return(dist_judgements_melted_di)
}



simple_stringdists_disyl = diag(stringdist(strings=old_disyl_segments,segment.specific.costs = FALSE, tostrings=new_disyl_segments, cross=F, cost=c(.5,.5,1,1)))
simple_segdist = simple_stringdists_disyl / 3
dist_judgements_melted_di = addDistsToDiTable(simple_segdist,"simple_segdist")

model_di_simple_hamming_auto = brm(distance | cens(censored_di) ~ (simple_segdist + hamming_auto | participant) +(1|variable) + simple_segdist + hamming_auto, data = dist_judgements_melted_di)
model_di_simple_hamming_oc = brm(distance | cens(censored_di) ~ (simple_segdist +hamming_oco | participant) +(1|variable) + simple_segdist + hamming_oc, data = dist_judgements_melted_di)
model_di_simple_hamming_oco = brm(distance | cens(censored_di) ~ (simple_segdist +hamming_oco | participant) +(1|variable) + simple_segdist + hamming_oco, data = dist_judgements_melted_di)
model_di_simple_hamming_co = brm(distance | cens(censored_di) ~ (simple_segdist + hamming_co | participant) +(1|variable) + simple_segdist + hamming_co, data = dist_judgements_melted_di)
model_di_simple_hamming_chao = brm(distance | cens(censored_di) ~ (simple_segdist + hamming_chao | participant) +(1|variable) + simple_segdist + hamming_chao, data = dist_judgements_melted_di)     
model_di_simple_man_chao = brm(distance | cens(censored_di) ~ (simple_segdist + man_chao | participant) +(1|variable) + simple_segdist + man_chao, data = dist_judgements_melted_di)
model_di_simple_euc_chao = brm(distance | cens(censored_di) ~ (simple_segdist + euc_chao | participant) +(1|variable) + simple_segdist + euc_chao, data = dist_judgements_melted_di)

waic(model_di_simple_hamming_auto)
waic(model_di_simple_hamming_oc)
waic(model_di_simple_hamming_oco)
waic(model_di_simple_hamming_co)
waic(model_di_simple_hamming_chao)
waic(model_di_simple_man_chao)
waic(model_di_simple_euc_chao)


waic(model_di_simple_hamming_ocoplusavg)
waic(model_di_simple_hamming_ocoplusedge)
waic(model_di_simple_hamming_coplus)
