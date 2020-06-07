
library(readxl)

rank.mis <- read_excel('./economist mba 2010.xlsx',sheet = "rank2010",na= "NA")


#0. fill NA with mice(continuous data use Predictive Mean Matching method)
library(mice)
table(is.na(rank.mis))
#aggr(rank.mis,prop=F,numbers=T)

rank.fix <- mice(rank.mis,m=5,maxit=50,method = 'pmm',seed = 123)
#summary(rank.fix)
rank <- complete(rank.fix)

#1. recreate Z-socres
library(ggplot2)
library(patchwork)

weight <- c(0.088,0.088,0.088,0.088,0.018,0.035,0.035,0.066,0.022,0.029,0.029,0.029,0.022,0.022,0.022,0.022,0.05,0.15)

# 1.1 caculate Zij
rank.scale <- rank
for (v in 4:21){
  rank.scale[,v] <- scale(rank[,v],center = T,scale = T)
}

# 1.2 caculate Total Score
rank.scale$TS  <- c(1:100)

for (i in 1:100){
  scores <- rank.scale[i,4:21]
  rank.scale[i,]$TS <- (as.numeric(scores) %*% weight)*100
}

# 1.3 new rank
rank.scale$new.rank <- 101 -rank(rank.scale$TS)

#1.2 visualize
ggplot(rank.scale)+
  geom_point(aes(TS,Rank))+
  #scale_y_continuous(breaks =seq(0,100,10))+
  scale_x_continuous(breaks =seq(-100,100,50))+
  xlab("Reconstructed Total Score")+ylab("Rank2010")+
  scale_y_reverse(breaks =seq(0,100,10))


ggplot(rank.scale)+
  geom_point(aes(new.rank,Rank))+
  geom_abline(slope = 1)+
  #geom_smooth(method = 'lm',formula="y~x",color='black',aes(new.rank,Rank,color='black'))+
  scale_y_reverse(breaks =seq(0,100,10))+
  scale_x_reverse(breaks =seq(0,100,10))+
  #scale_y_continuous(breaks =seq(0,100,10))+
  #scale_x_continuous(breaks =seq(0,100,10))+
  xlab("Reconstructed Rank")+ylab("Rank2010")

#hist(rank.scale$TS)

ggplot(rank.scale,aes(TS))+
  geom_histogram(bins=20)+
  xlab("Reconstruted Total Score")+ylab("Count")


# 1-20 21-40 41-60 61-80 81-100

rank$group <- c(1:100)

rank[1:20,]$group <- 5
rank[21:40,]$group <- 4
rank[41:60,]$group <- 3
rank[61:80,]$group <- 2
rank[81:100,]$group <- 1

set.seed(123)
rank.lda <- lda(group~A1+A2+A3+A4+B11+B12+B13+B21+B22+B31+B32+B33+B41+B42+B43+B44+C1+C2, data = rank)

loadings <- rank.lda$scaling
findLoading <- function(i,na.print=print()){
  A <- sum(loadings[1:4,i]) 
  
  B <- sum(loadings[5:16,i])
  
  B1<-sum(loadings[5:7,i]) 
  B2<-sum(loadings[7:8,i]) 
  B3<-sum(loadings[9:11,i]) 
  B4<-sum(loadings[12:16,i]) 
  
  C <- sum(loadings[17:18,i])
  
  #l <- c(A,B1,B2,B3,B4,C)
  #scale <- scale(l,center = F,scale = T)
  
  s<-sum(A,B,B1,B2,B3,B4,C)
  result <- c(A/s,B1/s,B2/s,B3/s,B4/s,C/s)
  
  print("loadings:")
  print(loadings[,i])
  print(result)
 
}
findLoading(4)



#2. geographical location with languages offer

attach(rank)
table(Country)

ggplot(data = rank,mapping = aes(x=Country,y=B43,fill=factor(Country)))+
  geom_boxplot()

l1 <- aggregate(rank$B43,list(rank$Country),mean)
l1[order(-l1$x),]


l2 <- aggregate(101-rank(rank$B43),list(rank$Country),mean)
l2[order(l2$x),]

#l3 <- aggregate(rank$B43,list(rank$Country),sd)
#l3[order(l2$x),]

library(car)
# one-way anova :single variable, multiple group

#qqplot(lm(B43~Country,data=rank),simulate = TRUE, main = "QQ Plot", labels = FALSE)

l.avo <- aov(B43~Country,data = rank)
summary(l.avo)

#bartlett.test(B43~Country,data = rank)

# outlier Test
outlierTest(l.avo) #p NA means that there is outliers



#3. British schoools VS American : PCA



rank.AB <- subset(rank,Country=='America'|Country=='Britain')


PCA.AB <- function(a){
  pca <- prcomp(rank.AB[,a])
  pcs <- data.frame(pca$x,Country=rank.AB$Country)  
  ggplot(pcs,aes(PC1,PC2,color=Country))+
    geom_point()#+
    #ggtitle("Brirain vs America")+
    #theme(plot.title = element_text(hjust = 0.5)) 
}

PA <- PCA.AB(c(4,5,6,7))
PA
PB1 <- PCA.AB(c(8,9,10))
PB2 <- PCA.AB(c(11,12))
PB3 <- PCA.AB(c(13,14,15))
PB4 <- PCA.AB(c(16,17,18,19))
PC <- PCA.AB(c(20,21))

(PA+PB1+PB2)/(PB3+PB4+PC)

#Manova
attach(rank.AB)
score.manova <- manova(cbind(A1,A2,A3,A4) ~ Country) #:: 0.0424
summary(score.manova,test ="Hotelling")

score.manova <- manova(cbind(B11,B12,B13) ~ Country) # 0.003
summary(score.manova,test ="Hotelling")

score.manova <- manova(cbind(B21,B22) ~ Country) # 0.00008
summary(score.manova,test ="Hotelling")

score.manova <- manova(cbind(B31,B32,B33) ~ Country) # 0.0003
summary(score.manova,test ="Hotelling")

score.manova <- manova(cbind(B41,B42,B43) ~ Country) # 0.0053
summary(score.manova,test ="Hotelling")

score.manova <- manova(cbind(C1,C2) ~ Country) # 0.4
summary(score.manova,test ="Hotelling")

aggregate(cbind(rank.AB$Rank,rank.AB$B21,rank.AB$B22,rank.AB$B31,rank.AB$B32,rank.AB$B33),list(rank.AB$Country),mean)


#4. relationship between faculty quality and education experiance
library(corrplot)

attach(rank)

rank.cor=rank[,c(8,9,10,16,17,18,19)]

corrplot(cor(rank.cor))

rank.scale$rankScore <- 101-rank$Rank
corrplot(cor(rank.scale[,-c(1,2,3,23.24)]))

### First copy this function into R: #############
##
cancor2<-function(x,y,dec=4){
  #Canonical Correlation Analysis to mimic SAS PROC CANCOR output.
  #Basic formulas can be found in Chapter 10 of Mardia, Kent, and Bibby (1979).
  # The approximate F statistic is exercise 3.7.6b.
  x<-as.matrix(x);y<-as.matrix(y)
  n<-dim(x)[1];q1<-dim(x)[2];q2<-dim(y)[2];q<-min(q1,q2)
  S11<-cov(x);S12<-cov(x,y);S21<-t(S12);S22<-cov(y)
  E1<-eigen(solve(S11)%*%S12%*%solve(S22)%*%S21);E2<-eigen(solve(S22)%*%S21%*%solve(S11)%*%S12)
  #    rsquared<-as.real(E1$values[1:q])
  rsquared<-E1$values[1:q]
  LR<-NULL;pp<-NULL;qq<-NULL;tt<-NULL
  for (i in 1:q){
    LR<-c(LR,prod(1-rsquared[i:q]))
    pp<-c(pp,q1-i+1)
    qq<-c(qq,q2-i+1)
    tt<-c(tt,n-1-i+1)}
  m<-tt-0.5*(pp+qq+1);lambda<-(1/4)*(pp*qq-2);s<-sqrt((pp^2*qq^2-4)/(pp^2+qq^2-5))
  F<-((m*s-2*lambda)/(pp*qq))*((1-LR^(1/s))/LR^(1/s));df1<-pp*qq;df2<-(m*s-2*lambda);pval<-1-pf(F,df1,df2)
  outmat<-round(cbind(sqrt(rsquared),rsquared,LR,F,df1,df2,pval),dec)
  colnames(outmat)=list("R","RSquared","LR","ApproxF","NumDF","DenDF","pvalue")
  rownames(outmat)=as.character(1:q);xrels<-round(cor(x,x%*%E1$vectors)[,1:q],dec)
  colnames(xrels)<-apply(cbind(rep("U",q),as.character(1:q)),1,paste,collapse="")
  yrels<-round(cor(y,y%*%E2$vectors)[,1:q],dec)
  colnames(yrels)<-apply(cbind(rep("V",q),as.character(1:q)),1,paste,collapse="")
  list(Summary=outmat,a.Coefficients=E1$vectors,b.Coefficients=E2$vectors,
       XUCorrelations=xrels,YVCorrelations=yrels)
} 
## END FUNCTION
#################################################

iris.std <- sweep(iris[,-5], 2, sqrt(apply(iris[,-5],2,var)), FUN="/")

sepal.meas <- iris.std[,1:2]
petal.meas <- iris.std[,3:4]

cancor2(sepal.meas, petal.meas)

rank.std <- sweep(rank.cor,2,sqrt(apply(rank.cor, 2, var)),FUN ='/')

B2 <-rank.std[,1:3]
B4 <- rank.std[,4:7]
cancor2(B2,B4)



#5. similar schools, relationship with rank

# hierarchical cluster

std <- apply(rank[,-c(1,2,3)],2,sd)
rank.std <- sweep(rank[,-c(1,2,3)],2,std,FUN = '/')
rank.dist <- dist(rank.std)

methods <- c('single','average','complete')
op <- par(mfrow=c(3,1))
for (m in methods){
  rank.link <- hclust(rank.dist,method = m )
  plot(rank.link,labels = rank[,1],ylab = "Distance",main = m)
  
}
par(op)

rank.link <- hclust(rank.dist,method = 'complete' )
plot(rank.link,labels = rank[,1],ylab = "Distance",main = m)


cut.6 <- cutree(rank.link, k=6)
rank$cluster <- factor(cut.6)

library(ggalt)
library(ggfortify)

rank.pca <- prcomp(rank[,-c(1,2,3,22,23,24)])

rank.pcs <- data.frame(rank.pca$x,Cluster = rank$cluster,Country=rank$Country)
rank.pcs[rank.pcs$Cluster == 1,]$Cluster <- "A"
rank.pcs[rank.pcs$Cluster == 2,]$Cluster <- "B"
rank.pcs[rank.pcs$Cluster == 3,]$Cluster <- "C"
rank.pcs[rank.pcs$Cluster == 4,]$Cluster <- "D"
rank.pcs[rank.pcs$Cluster == 5,]$Cluster <- "E"
rank.pcs[rank.pcs$Cluster == 6,]$Cluster <- "F"

ggplot(rank.pcs,aes(PC1,PC2,color=Cluster))+
  geom_point(aes(shape=Cluster),size=2)
  



# 
aggregate(rank[,4:21],list(rank$cluster),mean)
aggregate(rank[,3],list(rank$cluster),max)
aggregate(rank[,3],list(rank$cluster),min)
table(rank$Country,rank$cluster)

rank.B <- subset(rank,cluster==2)
rank.B[,c(1,3)]

rank.A <- subset(rank,cluster==1)
rank.A[,c(1,3)]

rank.C <- subset(rank,cluster==3)
rank.C[,c(1,3)]


