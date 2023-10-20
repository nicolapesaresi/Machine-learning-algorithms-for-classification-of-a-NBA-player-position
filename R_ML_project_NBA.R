######################
#codice progetto ML
#Pesaresi Nicola
#matricola 842468
######################

############
#DATA INPUT
############
library(xtable)

data<-read.csv("nba22-23/2022-2023 NBA Player Stats - Regular.csv",header=T, sep=";")
str(data)

results<-list()

##############################
#Esplorativa e Pre-Processing
##############################

data$Player<- paste(data$Player, " - ", data$Tm)
data<- data[, -c(1,5)] #rimuovo colonne Team e Rank

table(data$Pos) #tre ruoli doppi, con poche oss
data[which(data$Pos=="SF-SG" | data$Pos=="PF-SF" | data$Pos=="SG-PG"),] #tutti giocatori degli spurs
#rimovo questi casi particolari
data<-data[- which(data$Pos=="SF-SG" | data$Pos=="PF-SF" | data$Pos=="SG-PG"),]
data$Pos<- as.factor(data$Pos)

library(GGally)
#ggpairs(data[,-1], aes(color=as.factor(Pos)))

#rimuovo na
#data<-na.omit(data)
#str(data)

#standardizzo dati
library(caret)
data.norm<-preProcess(data,method = "range")
data.norm<-predict(data.norm, data)

#boxplot dati standardizzati (solo variabili di interesse)
boxplot(data.norm[-c(1,2)])

#outlier count
for(i in 3:ncol(data.norm)) print(length(boxplot.stats(data.norm[,i])$out))

#dimensionality reduction
#random forest feature selection
set.seed(123)
library(randomForest)
library(dplyr)
data.unl<-data[,- c(1,2)] #unlabeled data

#supervised random forest
rf.sup<- randomForest(data.unl, y=data$Pos, importance=T, ntree=1000)

#rf.unsup<- randomForest(data, importance = T, ntree=1000)

varImpPlot(rf.sup, type=1, main="Random Forest Feature Selection")
#mantengo le prime 10

imp<- data.frame(predictors=rownames(importance(rf.sup, type=1)),importance(rf.sup, type=1)) 
imp.sort<- arrange(imp,desc(MeanDecreaseAccuracy))
vars.to.keep<-imp.sort$predictors[1:10]

#elimino variabili non utili
data.red<-subset(data, select= vars.to.keep) #reduced database
data.red.lab<-subset(data, select= c("Player","Pos",vars.to.keep))
#data.red<-data[,-c(1,2)]
#data.red.lab<-data
data.red<-subset(data.norm, select=vars.to.keep)
data.red.lab<-subset(data.norm, select= c("Player","Pos",vars.to.keep))


#View(data.red)
#table(data$Pos)
#ggpairs(data.red, aes(color=data$Pos))

##############################
#Clustering
##############################
set.seed(123)

#Distance based - KMEANS
#scelta k
library(cluster)
library(GGally)
silhouette_score <- function(df, k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score, df=data.red)
sil.df<-data.frame(k, avg_sil)
#plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE) #k=2
k.est.sil<-which.max(avg_sil) +1 

ggplot(sil.df, aes(x=k, y=avg_sil)) +
  geom_line() +
  theme_bw() +
  theme(plot.title=element_text(family='', face='bold', size=14, hjust=0.5),) +
  xlab("K") + ylab("Average Silhouette Scores") +
  ggtitle(paste0("K estimation by Silhoette scores - optimal value K = ", k.est.sil))

kmns<-kmeans(data.red, centers = k.est.sil, iter.max = 100, nstart= 10)


clust.results<-function(clusters, real= data.red.lab$Pos, data=data.red) {
  print(ggpairs(data, aes(color = as.factor(clusters))))

  clust.tab<- data.frame(clust=clusters,Pos=real)
  print(table(clust.tab))
  #clust.tab<-apply(clust.tab, 1, function(x) {
  #  index<-x["clust"]
  #  level<-which.max(table(clust.tab)[index,])
  #  x["clust"]<-levels(real)[level]
  #})
  
  #clust.tab<-data.frame(clust.tab, real)
  #print(table(clust.tab))
  
  plots<-list()
  for (i in 1:length(levels(as.factor(clusters)))){
    rows<-which(clust.tab$clust == levels(as.factor(clust.tab$clust))[i] )
    plots[[i]]<- ggplot(clust.tab[rows,], aes(x=as.factor(Pos))) +
      geom_bar(color="black", fill=i+1) +
      theme_bw() +
      theme(plot.title=element_text(family='', face='bold', size=14, hjust=0.5),) +
      xlab("Real positions") + ylab("n") +
      ggtitle(paste0("Cluster ",levels(as.factor(clust.tab$clust))[i]))
  }
  
  library(cowplot)
  plot_grid(plotlist = plots, nrow=1)
}

clust.results(kmns$cluster)
km.pl<-clust.results(kmns$cluster)

#grafico zoom ggpairs
ast.trb<-ggplot(data.red, aes(x=AST, y=TRB)) +
  geom_point(colour=(kmns$cluster+1), size=2) +
  theme_bw() +  #theme(plot.title=element_text(family='', face='bold', size=14, hjust=0.5),) 
 xlab("Assists (AST)") + ylab("Total Rebounds (TRB)") #+  ggtitle(paste0("Cluster ",levels(as.factor(clust.tab$clust))[i]))
x3pa.fg<-ggplot(data, aes(x=X3PA, y=FG.)) +
  geom_point(colour=(kmns$cluster+1), size=2) +
  theme_bw() +  #theme(plot.title=element_text(family='', face='bold', size=14, hjust=0.5),) 
  xlab("3 Points Shots Attempted (X3PA)") + ylab("Field Goals % (FG.)") #+  ggtitle(paste0("Cluster ",levels(as.factor(clust.tab$clust))[i]))

plot_grid(ast.trb, x3pa.fg, nrow=1)

#EM alg - normal mixture
library(mixtools)
library(mclust)

BIC<-mclustBIC(data.red)
BIC
plot.mclustBIC(BIC)

BIC

#K=4
em.alg.4<-Mclust(data.red, G=4)
clust.results(as.vector(em.alg.4$classification))

#K=3
em.alg<- mvnormalmixEM(data.red, k=k.est.sil)
em.alg.clust<-apply(em.alg$posterior, 1, which.max)

#inverto 1 con 3 per avere stessi colore e ordine di kmeans
em.alg.clust[which(em.alg.clust==1)]<-400
em.alg.clust[which(em.alg.clust==3)]<-1
em.alg.clust[which(em.alg.clust==400)]<-3


clust.results(em.alg.clust)
em.pl<-clust.results(em.alg.clust)

#km-em plot
t1 <- ggdraw() + draw_label("K-Means", fontface='bold', size=17)
t2 <- ggdraw() + draw_label("Mixture Model", fontface='bold', size=17)
plot_grid(t1,km.pl,t2,em.pl, nrow=4, rel_heights=c(0.2, 1, 0.2, 1))

#Gerarchico
dist<-dist(data.red)
h.sl<-hclust(dist, method = "single")
h.cl<-hclust(dist, method = "complete")
h.al<-hclust(dist, method = "average")
h.ward<-hclust(dist, method = "ward.D2")

ss.h<-matrix(0, nrow=9,ncol=4) #silhouette scores for hclust

for (i in 2:10){
  k.h<- i
  
  #plot(h.sl)
  #rect.hclust(h.sl, k.h)
  h.sl.clust<-cutree(h.sl, k.h)
  #plot(h.cl)
  #rect.hclust(h.cl, k.h)
  h.cl.clust<-cutree(h.cl, k.h)
  #plot(h.al)
  #rect.hclust(h.al, k.h)
  h.al.clust<-cutree(h.al, k.h)
  #plot(h.ward)
  #rect.hclust(h.ward, k.h)
  h.ward.clust<-cutree(h.ward, k.h)

  ss.h[i-1,1]<- mean(silhouette(h.sl.clust, dist)[,3]) 
  ss.h[i-1,2]<- mean(silhouette(h.cl.clust, dist)[,3])
  ss.h[i-1,3]<- mean(silhouette(h.al.clust, dist)[,3]) 
  ss.h[i-1,4]<- mean(silhouette(h.ward.clust, dist)[,3]) 
}

ss.h

k.h<-2
par(mfrow=c(2,2))
plot(h.sl)
rect.hclust(h.sl, k.h)
plot(h.cl)
rect.hclust(h.cl, k.h)
plot(h.al)
rect.hclust(h.al, k.h)
plot(h.ward)
rect.hclust(h.ward, k.h)

dev.off()

plot(h.ward, labels=F, xlab="", sub="")
rect.hclust(h.ward, k.h)
rect.hclust(h.ward, 3)

##############################
#Classificazione
##############################
library(caret)
set.seed(123)

#divido in training e test set
index<-sample(1:nrow(data.red), 0.70*nrow(data.red))
train<-data.red.lab[index, ]
test<-data.red.lab[-index, ]
#train<-data.red.lab

#KNN
knn.errors<-numeric()
#calcolo errore per 50 valori di K (cross validation)
for (i in 1:50) {
  knn<-knn3Train(train[, -c(1,2)], test[,-c(1,2)], train$Pos, k= i )
  cm<-confusionMatrix(as.factor(knn), reference= test$Pos)
  knn.errors[i]<- 1 - cm$overall[1]
}

k.est<- which.min(knn.errors)

#grafico scelta K
ggplot(data.frame(knn.errors), aes(x = 1:50)) +
  geom_line(aes(y= knn.errors)) +
  xlab("K") + ylab("Test error") +
  theme_bw() +
  theme(plot.title=element_text(family='', face='bold', size=14, hjust=0.5),) +
  ggtitle(paste0("K estimation by cross validation - optimal value K = ", k.est))

#modello migliore

#knn.mod<-knn3Train(train[, -c(1,2)], test[,-c(1,2)], train$Pos, k=k.est)

knn.mod<-knn3(Pos ~ ., train[,-1], k=k.est) #modello
knn.pred<-predict(knn.mod, test[,-c(1,2)]) #predico sul test set
knn.pred<-apply(knn.pred, 1, function(x) levels(train$Pos)[which.max(x)] )

#risultati
knn.cm<-confusionMatrix(as.factor(knn.pred), reference= test$Pos)
knn.cm

tab.out<-as.data.frame(rbind(knn.cm$byClass[,1], as.vector(knn.cm$byClass[,2])), byrow=T, nrow=2)
colnames(tab.out)<-levels(data.red.lab$Pos)
rownames(tab.out)<-c("Sensitivity", "Specificity")

print(xtable(tab.out, label="knntable", caption=paste0("\\textbf{KNN -Accuracy: ",round(knn.cm$overall[1], digits=4),"}")), caption.placement="top")

results[[1]]<-list("KNN", knn.mod, knn.cm$table, knn.cm$overall[1], knn.cm$byClass[,c(1,2)])



#SVM
library(e1071)

#con kernel radiale
tune.out <- tune(svm, Pos ~ ., data = train[,-1], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4))) #1, 0.5
#svm.fit<- svm(Pos ~ . , data=train[,-1], kernel= "radial")
svm.mod<- svm(Pos ~ . , data=train[,-1], kernel= "radial", gamma=tune.out$best.parameters[[2]], cost=tune.out$best.parameters[[1]])
svm.pred<- predict(svm.mod, newdata = test[,-1])
svm.cm<- confusionMatrix(data= as.factor(svm.pred), reference= test$Pos)
svm.cm

tab.out<-as.data.frame(rbind(svm.cm$byClass[,1], as.vector(svm.cm$byClass[,2])), byrow=T, nrow=2)
colnames(tab.out)<-levels(data.red.lab$Pos)
rownames(tab.out)<-c("Sensitivity", "Specificity")

print(xtable(tab.out, label="svmtable", caption=paste0("\\textbf{SVM - radial - Accuracy: ",round(svm.cm$overall[1], digits=4),"}")), caption.placement="top")

results[[2]]<-list("SVM radial", svm.mod, svm.cm$table, svm.cm$overall[1], svm.cm$byClass[,c(1,2)])

#con kernel polinomiale
tune.out.p <- tune(svm, Pos ~ ., data = train[,-1], kernel = "polynomial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4))) #10 0.5
#svm.p.mod<- svm(Pos ~ . , data=train[,-1], kernel= "polynomial")
svm.p.mod<- svm(Pos ~ . , data=train[,-1], kernel= "polynomial", gamma=tune.out.p$best.parameters[[2]],cost=tune.out.p$best.parameters[[1]])
svm.p.pred<- predict(svm.p.mod, newdata = test[,-1])
svm.p.cm<- confusionMatrix(data= as.factor(svm.p.pred), reference= test$Pos)
svm.p.cm

tab.out<-as.data.frame(rbind(svm.p.cm$byClass[,1], as.vector(svm.p.cm$byClass[,2])), byrow=T, nrow=2)
colnames(tab.out)<-levels(data.red.lab$Pos)
rownames(tab.out)<-c("Sensitivity", "Specificity")

print(xtable(tab.out, label="svmPtable", caption=paste0("\\textbf{SVM - polynomial - Accuracy: ",round(svm.p.cm$overall[1], digits=4),"}")), caption.placement="top")

results[[3]]<-list("SVM polynmial", svm.p.mod, svm.p.cm$table, svm.p.cm$overall[1], svm.p.cm$byClass[,c(1,2)])

#RANDOM FOREST
library(randomForest)

#randomForest(train[,-c(1,2)], y=train$Pos, xtest= test[,-c(1,2)], ytest= test$Pos, ntree=1000)

rf.mod<- randomForest(Pos ~ ., data= train[,-1], ntree=1000)
rf.pred<- predict(rf.mod, newdata = test[,-c(1,2)])
rf.cm<- confusionMatrix(data = as.factor(rf.pred), reference = test$Pos)
rf.cm


tab.out<-as.data.frame(rbind(rf.cm$byClass[,1], as.vector(rf.cm$byClass[,2])), byrow=T, nrow=2)
colnames(tab.out)<-levels(data.red.lab$Pos)
rownames(tab.out)<-c("Sensitivity", "Specificity")

print(xtable(tab.out, label="svmPtable", caption=paste0("\\textbf{Random Forest - Accuracy: ",round(rf.cm$overall[1], digits=4),"}")), caption.placement="top")

results[[4]]<-list("Random Forest", rf.mod, rf.cm$table, rf.cm$overall[1], rf.cm$byClass[,c(1,2)])

#NEURAL NET
library(nnet)
library(caret)
#nnet.grid <-  expand.grid(size = 5:15,decay = c(0.01, 0.05,0.1,0.2,0.3,0.4,0.5))
nnet.grid <-  expand.grid(size = 8,
                         decay = 0.1)
train.control<- trainControl(method="cv", number=10)

nnet.tune <- train(Pos ~ ., 
                   data=train[,-1], 
                   method="nnet",
                   maxit=5000, #max number of iteration
                   tuneGrid=nnet.grid,
                   trControl=train.control,
                   trace=F)

#size 8 decay 0.1
nnet.mod<- nnet(Pos ~., data= train[,-1], size= nnet.tune$bestTune[[1]], decay= nnet.tune$bestTune[[2]])
nnet.pred<- predict(nnet.mod, newdata=test[,-1])
nnet.pred<-apply(nnet.pred, 1, function(x) levels(train$Pos)[which.max(x)] )
nnet.cm<-confusionMatrix(data=as.factor(nnet.pred), reference=test$Pos)
nnet.cm


tab.out<-as.data.frame(rbind(nnet.cm$byClass[,1], as.vector(nnet.cm$byClass[,2])), byrow=T, nrow=2)
colnames(tab.out)<-levels(data.red.lab$Pos)
rownames(tab.out)<-c("Sensitivity", "Specificity")

print(xtable(tab.out, label="nnettable", caption=paste0("\\textbf{Neural Network - Accuracy: ",round(nnet.cm$overall[1], digits=4),"}")), caption.placement="top")

results[[5]]<-list("nnet", nnet.mod, nnet.cm$table, nnet.cm$overall[1], nnet.cm$byClass[,c(1,2)])


##############################
#Confronto risultati
##############################

#tab compare tests
tab.mod.comp<-data.frame()
for (i in 1:length(results)) {
  tab.mod.comp["Original test set",i]<- results[[i]] [[4]]
  colnames(tab.mod.comp)[i]<-results[[i]] [[1]]
}

library(cowplot)

printresults<- function(results){
results.table<-data.frame(rep(0,11),rep(0,11),rep(0,11),rep(0,11),rep(0,11), row.names=c("Accuracy", "C - Sensitivity", "PF - Sensitivity", "PG - Sensitivity", "SF - Sensitivity", "SG - Sensitivity", "C - Specificity", "PF - Specificity", "PG - Specificity", "SF - Specificity", "SG - Specificity"))
for (i in 1:length(results)) {
  results.table[,i]<- c(results[[i]] [[4]], results[[i]] [[5]] [,1], results[[i]] [[5]] [,2])
  colnames(results.table)[i]<-results[[i]] [[1]]
}

print(results.table)

plots<-list()
for (i in 1:length(results)){
  cm<-results[[i]] [[3]]
  plt <- as.data.frame(cm)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

  plots[[i]]<- ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
    ggtitle(results[[i]] [[1]]) +
    theme(plot.title = element_text(face="bold",hjust = 0.5)) +
            geom_tile() + geom_text(aes(label=Freq)) +
            scale_fill_gradient(low="white", high="#009194", guide="none") +
            labs(x = "Reference",y = "Prediction")
}

plot_grid(plotlist = plots[-3], nrow=2)
}

predresults<- function(test){
  results<-list()
  
  #KNN
  knn.pred<-predict(knn.mod, test[,-1]) #predico sul test set
  knn.pred<-apply(knn.pred, 1, function(x) levels(train$Pos)[which.max(x)] )
  knn.cm<-confusionMatrix(as.factor(knn.pred), reference= test$Pos)
  knn.cm
  
  results[[1]]<-list("KNN", knn.mod, knn.cm$table, knn.cm$overall[1], knn.cm$byClass[,c(1,2)])
  
  #SVM
  svm.pred<- predict(svm.mod, newdata = test[,-1])
  svm.cm<- confusionMatrix(data= as.factor(svm.pred), reference= test$Pos)
  svm.cm
  
  results[[2]]<-list("SVM radial", svm.mod, svm.cm$table, svm.cm$overall[1], svm.cm$byClass[,c(1,2)])
  
  svm.p.pred<- predict(svm.p.mod, newdata = test[,-1])
  svm.p.cm<- confusionMatrix(data= as.factor(svm.p.pred), reference= test$Pos)
  svm.p.cm
  
  results[[3]]<-list("SVM polynomial", svm.p.mod, svm.p.cm$table, svm.p.cm$overall[1], svm.p.cm$byClass[,c(1,2)])
  
  #Random Forest
  
  rf.pred<- predict(rf.mod, newdata = test[,-1])
  rf.cm<- confusionMatrix(data = as.factor(rf.pred), reference = test$Pos)
  rf.cm
  
  results[[4]]<-list("Random Forest", rf.mod, rf.cm$table, rf.cm$overall[1], rf.cm$byClass[,c(1,2)])
  
  #NNET
  
  nnet.pred<- predict(nnet.mod, newdata=test[,-1])
  nnet.pred<-apply(nnet.pred, 1, function(x) levels(train$Pos)[which.max(x)] )
  nnet.cm<-confusionMatrix(data=as.factor(nnet.pred), reference=test$Pos)
  
  results[[5]]<-list("nnet", nnet.mod, nnet.cm$table, nnet.cm$overall[1], nnet.cm$byClass[,c(1,2)])
  
  return(results)
}

printresults(results)

##############################
#Prediction su altri test set 
##############################

#playoff 23
test.playoff23<-read.csv("nba22-23/2022-2023 NBA Player Stats - Playoffs.csv",header=T, sep=";")
test.playoff23<-test.playoff23[,c("Pos",vars.to.keep)]
test.playoff23$Pos <- as.factor(test.playoff23$Pos)

test.norm.play23<-preProcess(test.playoff23,method = "range")
test.norm.play23<-predict(test.norm.play23, test.playoff23)

playoff23<-predresults(test.norm.play23)
printresults(playoff23)

for (i in 1:length(playoff23)) {
  tab.mod.comp["Playoffs 22-23",i]<- playoff23[[i]] [[4]]
}
colnames(tab.mod.comp)<- c("KNN", "SVM - rad", "SVM - poly", "RF", "NNet")


#regular season 21/22
test.regular22<-read.csv("nba21-22/2021-2022 NBA Player Stats - Regular.csv",header=T, sep=";")
test.regular22<-test.regular22[-which(nchar(test.regular22$Pos)>2),]
test.regular22<-test.regular22[,c("Pos",vars.to.keep)]
test.regular22$Pos <- as.factor(test.regular22$Pos)


test.norm.reg22<-preProcess(test.regular22,method = "range")
test.norm.reg22<-predict(test.norm.reg22, test.regular22)

regular22<- predresults(test.norm.reg22)
printresults(regular22)

for (i in 1:length(regular22)) {
  tab.mod.comp["Regular season 21-22",i]<- regular22[[i]] [[4]]
}


print(xtable(tab.mod.comp, label="testcomptable", align =c("r","c","c","c","c","c"), caption= "\\textbf{Model performances of different test sets}"), caption.placement="top")


##############################
#Classificazione in tre classi
##############################

data.pos2<-subset(data.norm, select= c("Player","Pos",vars.to.keep))
data.pos2<-cbind(data.pos2, Pos2= as.factor(ifelse( data.pos2$Pos == "C", "C",
                                    ifelse(data.pos2$Pos %in% c("PG","SG"), "G", 
                                           ifelse(data.pos2$Pos %in% c("SF", "PF"), "F", NA)))))

index<-sample(1:nrow(data.pos2), 0.7*nrow(data.pos2))
train<-data.pos2[index,]
test<-data.pos2[- index,]

table(train$Pos2)

#rendo il training set a pari numerosità con metodo bootstrap
#train.C<-train[which(train$Pos2=="C"),]
#train.F<-train[which(train$Pos2=="F"),]
#train.G<-train[which(train$Pos2=="G"),]

#size<-nrow(train.C)

#train<-rbind(
  #train.C[sample(1:nrow(train.C), size, replace=T),],
  #train.F[sample(1:nrow(train.F), size, replace=T),],
  #train.G
#) 

#test playoff23
test.norm.play23<-cbind(test.norm.play23, Pos2= as.factor(ifelse( test.norm.play23$Pos == "C", "C",
                                                                ifelse(test.norm.play23$Pos %in% c("PG","SG"), "G", 
                                                                       ifelse(test.norm.play23$Pos %in% c("SF", "PF"), "F", NA)))))

#test reg22
test.norm.reg22<-cbind(test.norm.reg22, Pos2= as.factor(ifelse( test.norm.reg22$Pos == "C", "C",
                                                                ifelse(test.norm.reg22$Pos %in% c("PG","SG"), "G", 
                                                                       ifelse(test.norm.reg22$Pos %in% c("SF", "PF"), "F", NA)))))

#NEURAL NET
library(nnet)
library(caret)

nnet.grid <-  expand.grid(size = 5:15,
                          decay = c(0.01,0.02,0.05,0.1, 0.2,0.3,0.4,0.5))
train.control<-trainControl(method="cv", number=10)

nnet.tune <- train(Pos2 ~ ., 
                   data=train[,-c(1,2)], 
                   method="nnet",
                   maxit=50000, #max number of iteration
                   tuneGrid=nnet.grid,
                   trControl=train.control,
                   trace=F)

#size 6 decay 0.2
nnet.mod<- nnet(Pos2 ~., data= train[,-c(1,2)], size= nnet.tune$bestTune[[1]], decay= nnet.tune$bestTune[[2]])


tab<-data.frame()
cm.3class<-list()

#pred su test og
nnet.pred<- predict(nnet.mod, newdata=test[,-1])
nnet.pred<-apply(nnet.pred, 1, function(x) levels(train$Pos2)[which.max(x)] )
nnet.cm<-confusionMatrix(data=as.factor(nnet.pred), reference=test$Pos2)
nnet.cm

tab["Original test set", "Accuracy"]<- round(nnet.cm$overall[1],2)
tab["Original test set", "C - Sens."]<- round(nnet.cm$byClass[1,1],2)
tab["Original test set", "G - Sens."]<- round(nnet.cm$byClass[1,3],2)
tab["Original test set", "F - Sens."]<- round(nnet.cm$byClass[1,2],2)

cm.3class[[1]]<- nnet.cm


#pred su playoff 23
nnet.pred<- predict(nnet.mod, newdata=test.norm.play23[,-1])
nnet.pred<-apply(nnet.pred, 1, function(x) levels(test.norm.play23$Pos2)[which.max(x)] )
nnet.cm<-confusionMatrix(data=as.factor(nnet.pred), reference=test.norm.play23$Pos2)
nnet.cm

tab["Playoffs 22-23", "Accuracy"]<- round(nnet.cm$overall[1],2)
tab["Playoffs 22-23", "C - Sens."]<- round(nnet.cm$byClass[1,1],2)
tab["Playoffs 22-23", "G - Sens."]<- round(nnet.cm$byClass[1,3],2)
tab["Playoffs 22-23", "F - Sens."]<- round(nnet.cm$byClass[1,2],2)

cm.3class[[2]]<- nnet.cm

#pred su regular 22
nnet.pred<- predict(nnet.mod, newdata=test.norm.reg22[,-1])
nnet.pred<-apply(nnet.pred, 1, function(x) levels(train$Pos2)[which.max(x)] )
nnet.cm<-confusionMatrix(data=as.factor(nnet.pred), reference=test.norm.reg22$Pos2)
nnet.cm

tab["Regular season 21-22", "Accuracy"]<- round(nnet.cm$overall[1],2)
tab["Regular season 21-22", "C - Sens."]<- round(nnet.cm$byClass[1,1],2)
tab["Regular season 21-22", "G - Sens."]<- round(nnet.cm$byClass[1,3],2)
tab["Regular season 21-22", "F - Sens."]<- round(nnet.cm$byClass[1,2],2)

cm.3class[[3]]<- nnet.cm

#tab output
tab

print(xtable(tab, label="3classtable", align =c("r","c","c","c","c"), caption= "\\textbf{3 class Neural Network classifier}"), caption.placement="top")

#cm plot
plots.3class<-list()
titles<-c("Original test set", "Playoffs 22-23", "Regular 21-22")
for (i in 1:3){
  plt <- as.data.frame(cm.3class[[1]]$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  
  plots.3class[[i]]<- ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
    ggtitle(titles[i]) +
    theme(plot.title = element_text(face="bold",hjust = 0.5)) +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="#009194", guide="none") +
    labs(x = "Reference",y = "Prediction")
}

plot_grid(plotlist = plots.3class, nrow=1)
