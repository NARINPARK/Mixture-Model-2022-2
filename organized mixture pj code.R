####################################################################################
######## 0. Install Packages
####################################################################################

# By the way, for all examples in this article, you´ll need some more packages:

#install.packages("recommenderlab")
#install.packages("LCA")
#install.packages("poLCA")
#install.packages("nnet")
#install.packages("dplyr")
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("igraph")
#install.packages("tidyr")
#install.packages("knitr")



library(dplyr)
library(poLCA)
library(recommenderlab)
library(nnet)
library(Hmisc)
library(LCA)
library(ggplot2)
library(gridExtra)
library(reshape2)

library("reshape2")
library("plyr")
library("dplyr")
library("poLCA")
library("ggplot2")
library("ggparallel")
library("igraph")
library("tidyr")
library("knitr")




####################################################################################
######## 1. Load Data 
####################################################################################

#set.seed(12345)

N<-read.csv(file="C:\\R_Program_Files\\data\\neflix.csv",header=T)
N<-as.data.frame(N)



####################################################################################
######## 2. Data Processing
####################################################################################


head(N)
summary(N)
str(N)


######## 2.1 Missing Value

colnames(N)
colSums(is.na(N))                    #전체 결측지 개수 구하기

table(rowSums(N[,16:23],na.rm=TRUE)) # 문제16번 대답한 사람 (중복문항)
table(rowSums(N[,29:36],na.rm=TRUE)) # 문제22번 대답한 사람 (중복문항)
table(rowSums(N[,40:47],na.rm=TRUE)) # 문제26번 대답한 사람 (중복문항)
table(rowSums(N[,48:55],na.rm=TRUE)) # 문제27번 대답한 사람 (중복문항)



######## 2.2 Create variables

######## 2.2.1 Create Using otot variable


a<-as.data.frame(ifelse(N[,5]==3,0,1))      # 1,2 -> 1(user)    3 -> 2 (user x)
b<-as.data.frame(ifelse(N[,6]==3,0,1))      # 1,2 -> 1(user)    3 -> 2 (user x)
c<-as.data.frame(ifelse(N[,7]==3,0,1))      # 1,2 -> 1(user)    3 -> 2 (user x)
d<-as.data.frame(ifelse(N[,8]==3,0,1))      # 1,2 -> 1(user)    3 -> 2 (user x)
e<-as.data.frame(ifelse(N[,9]==3,0,1))      # 1,2 -> 1(user)    3 -> 2 (user x)
f<-as.data.frame(ifelse(N[,10]==3,0,1))     # 1,2 -> 1(user)    3 -> 2 (user x)
g<-as.data.frame(ifelse(N[,11]==3,0,1))     # 1,2 -> 1(user)    3 -> 2 (user x)
h<-as.data.frame(ifelse(N[,12]==3,0,1))     # 1,2 -> 1(user)    3 -> 2 (user x)


z<-data.frame(c(a,b,c,d,e,f,g,h))
colnames(z)<-c("a","b","c","d","e","f","g","h")
ot<-as.data.frame(rowSums(z[,2:8]))



otot<-data.frame(rep(0,1000))

for (i in 1:1000) {
  
  if((z[i,"a"]==0)&(ot[i,1]==0)){
    otot[i,1]<-4
  }else if((z[i,"a"]==1)&(ot[i,1]==0)){
    otot[i,1]<-1
  }else if((z[i,"a"]==1)&(ot[i,1]>=1)){
    otot[i,1]<-2
  }else {otot[i,1]<-3}
  
}

head(otot)
colnames(otot)<-"ott"
head(otot)
str(otot)
table(otot)



######## 2.2.2 Create # of using otot variable

cn<-rowSums(z)
cn<-as.data.frame(cn)
table(cn) # add user 별 사용하는 otot 개수 변수

ct<-data.frame(rep(0,1000))

for (i in 1:1000) {
  if(cn[i,1]==0){ct[i,1]<-1}
  else if(cn[i,1]==1){
    ct[i,1]<-2}
  else if(cn[i,1]==2){
    ct[i,1]<-3}
  else{ct[i,1]<-4}
  
}

colnames(ct)<-"cnt"
str(ct)
table(ct)      # group as 4 if user use over 4 otot






######## 2.2.3 Create Netfilix Experience variable

colnames(N)

sum(is.na(N[(otot[,1]==1)|(otot[,1]==2),"q15"]))
sum(is.na(N[(otot[,1]==3)|(otot[,1]==4),"q15"]))
                # 넷플릭스 과거 경험 여부 대답한 사람들은 현재 넷플릭스 이용자가 아님

table(N$q15)
N$q15_1 <- ifelse(is.na(N$q15), 3, N$q15)
table(N$q15_1)
colnames(N)

ex<-data.frame(rep(0,1000))

for (i in 1:1000) {
  
  if((N$q15_1[i]==1)&(otot[i,1]==3)){
    ex[i,1]<-2
  }else if((N$q15_1[i]==1)&(otot[i,1]==4)){
    ex[i,1]<-2
  }else if((N$q15_1[i]==2)&(otot[i,1]==3)){
    ex[i,1]<-3
  }else if((N$q15_1[i]==2)&(otot[i,1]==4)){
    ex[i,1]<-3
  }else {ex[i,1]<-1}
  
}

head(ex)
colnames(ex)<-"experience"
table(ex)

              # ex 변수 설명
              # 1 present netflix user
              # 2 past user 
              # 3 never use netflix    (q15를 세분화 한거라고 생각하면 될듯)
colorful2 <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl)))+
  geom_bar()+
  scale_fill_brewer(palette="RdPu")
p1<-ggplot(otot, aes(x = factor(ott), fill = factor(ott)))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(legend.position = "bottom")+ geom_text(stat = "count", aes(label=..count..),
                                                                                                                                                        position = position_stack(vjust=0.5))
p2<-ggplot(ct, aes(x = factor(cnt), fill = factor(cnt)))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(legend.position = "bottom")+ geom_text(stat = "count", aes(label=..count..),
                                                                                                                                                      position = position_stack(vjust=0.5))
p3<-ggplot(ex, aes(x = factor(experience), fill = factor(experience)))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(legend.position = "bottom")+ geom_text(stat = "count", aes(label=..count..),
                                                                                                                                                                    position = position_stack(vjust=0.5))

grid.arrange(p1,p2,p3, nrow=1, ncol=3)



######## 2.3 Give factor to missing value in q32,q33,q34,q35
 # 무조건 이용하는 사람들은 이용여부 요인 중요하지 않으므로 다 1번으로 줌..@

######## 2.3.1 Give factor to missing value in q33

for (i in 1:1000) {
  if(N[i,"q31"]==3){N[i,"q33"]<-1}
  else{N[i,"q33"]<-N[i,"q33"]}
}

table(N[,"q33"])

######## 2.3.2 Give factor to missing value in q34

for (i in 1:1000) {
  if(N[i,"q31"]==3){N[i,"q34"]<-1}
  else{N[i,"q34"]<-N[i,"q34"]}
}

table(N[,"q34"])


######## 2.3.3 Give factor to missing value in q35

for (i in 1:1000) {
  if(N[i,"q31"]==3){N[i,"q35"]<-1}
  else{N[i,"q35"]<-N[i,"q35"]}
}

table(N[,"q35"])

######## 2.3.3 Give factor to missing value in q32

for (i in 1:1000) {
  if(N[i,"q31"]==3){N[i,"q32"]<-1}
  else{N[i,"q32"]<-N[i,"q32"]}
}

table(N[,"q32"])



######## 2.4 Remove variables which is not meaningful
colnames(N)
RN= subset(N,select=-c(q2_1,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13_1,q14,q15
                       ,q16_1,q16_2,q16_3,q16_4,q16_5,q16_6,q16_7,q16_8
                       ,q20,q21,q22_1,q22_2,q22_3,q22_4,q22_5,q22_6,q22_7,q22_8
                       ,q23,q24,q25,q26_1,q26_2,q26_3,q26_4,q26_5,q26_6,q26_7,q26_8
                       ,q27_1,q27_2,q27_3,q27_4,q27_5,q27_6,q27_7,q27_8,q15_1))


colnames(RN)
str(RN)
summary(RN)


g1<-MRN4 %>% ggplot(aes(x=q19, fill = q1)) + geom_bar()+
  scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
            position = position_stack(vjust=0.5))+theme(legend.position = "bottom")
g2<-MRN4 %>% ggplot(aes(x=q32, fill = q1)) + geom_bar()+scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
                                                                                                      position = position_stack(vjust=0.5))+theme(legend.position = "bottom")
g3<-MRN4 %>% ggplot(aes(x=q33, fill = q1)) + geom_bar()+scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
                                                                                                      position = position_stack(vjust=0.5))+theme(legend.position = "bottom")
g4<-MRN4 %>% ggplot(aes(x=q34, fill = q1)) + geom_bar()+scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
                                                                                                      position = position_stack(vjust=0.5))+theme(legend.position = "bottom")
g5<-MRN4 %>% ggplot(aes(x=q35, fill = q1)) + geom_bar()+scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
                                                                                                      position = position_stack(vjust=0.5))+theme(legend.position = "bottom")
g6<-MRN4 %>% ggplot(aes(x=q36, fill = q1)) + geom_bar()+scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
                                                                                                      position = position_stack(vjust=0.5))+theme(legend.position = "bottom")
g7<-MRN4 %>% ggplot(aes(x=q37, fill = q1)) + geom_bar()+scale_fill_brewer(palette="Pastel1") + geom_text(stat = "count", aes(label=..count..),
                                                                                                      position = position_stack(vjust=0.5))+theme(legend.position = "bottom")


grid.arrange(g1,g2,g3,g4,nrow=1, ncol=4)
grid.arrange(g5,g6,g7,nrow=1, ncol=3)



g1<-ggplot(MRN) + geom_bar(aes(q19, col=q1), position="identity", alpha=0.6)
g2<-ggplot(MRN) + geom_bar(aes(q32, col=q1), position="identity", alpha=0.6)
g3<-ggplot(MRN) + geom_bar(aes(q33, col=q1), position="identity", alpha=0.6)
g4<-ggplot(MRN) + geom_bar(aes(q34, col=q1), position="identity", alpha=0.6)
g5<-ggplot(MRN) + geom_bar(aes(q35, col=q1), position="identity", alpha=0.6)
g6<-ggplot(MRN) + geom_bar(aes(q36, col=q1), position="identity", alpha=0.6)
g7<-ggplot(MRN) + geom_bar(aes(q37, col=q1), position="identity", alpha=0.6)

grid.arrange(g1,g2,g3,g4,g5,g6,g7,nrow=2, ncol=4)




g1<-ggplot(MRN) + geom_bar(aes(q19, fill=q1), position="dodge", alpha=0.7)
g2<-ggplot(MRN) + geom_bar(aes(q32, fill=q1), position="dodge", alpha=0.7)
g3<-ggplot(MRN) + geom_bar(aes(q33, fill=q1), position="dodge", alpha=0.7)
g4<-ggplot(MRN) + geom_bar(aes(q34, fill=q1), position="dodge", alpha=0.7)
g5<-ggplot(MRN) + geom_bar(aes(q35, fill=q1), position="dodge", alpha=0.7)
g6<-ggplot(MRN) + geom_bar(aes(q36, fill=q1), position="dodge", alpha=0.7)
g7<-ggplot(MRN) + geom_bar(aes(q37, fill=q1), position="dodge", alpha=0.7)

grid.arrange(g1,g2,g3,g4,g5,g6,g7,nrow=2, ncol=4)


######## 2.5 Merge all the variables as 1 data frame


MRN<-as.data.frame(c(otot,ct,ex,RN))
summary(MRN)


######## 2.6 change the class of the variables as factor

MRN2<-MRN[(MRN[,"q31"]==2)|(MRN[,"q31"]==3),]    
summary(MRN2)
colSums(is.na(MRN2))

MRN3<-MRN[(MRN[,"ott"]==1)|(MRN[,"ott"]==2),]
summary(MRN3)
colSums(is.na(MRN3))

MRN4<-MRN2[(MRN2[,"ott"]==1)|(MRN2[,"ott"]==2),]
summary(MRN4)
colSums(is.na(MRN4))

dim(MRN3) #전체 결측지 사라짐

#table(MRN[,"q31"])


for(n_col in 1:ncol(MRN)){
  MRN[,n_col] =as.factor(MRN[,n_col])
}


for(n_col in 1:ncol(MRN2)){
  MRN2[,n_col] =as.factor(MRN2[,n_col])
}


for(n_col in 1:ncol(MRN3)){
  MRN3[,n_col] =as.factor(MRN3[,n_col])
}
 
for(n_col in 1:ncol(MRN4)){
  MRN4[,n_col] =as.factor(MRN4[,n_col])
}
summary(MRN4)
str(MRN4)

table(MRN$q40)





####################################################################################
######## 3. Latent Class Analysis 
####################################################################################
ggplot(MRN4, aes(x = q33)) + geom_bar(fill="blue")


####################################################################################
######## 3.1 Use Whole Data
####################################################################################

colnames(MRN3)
ncol(MRN3)
dimnames(MRN3)

summary(MRN2)
summary(MRN3)
summary(MRN4)


f<-cbind(q19,q32,q33,q34,q35,q36,q37) ~ q1  # define LCA model
lca.c3<-poLCA(f,MRN4,nclass=3,graphs=TRUE,na.rm=FALSE,nrep=5,maxiter = 100000)
print(lca.c3$probs)

f<-cbind(q17,q19,q32,q33,q34,q35) ~ 1 # define LCA model
lca.c311<-poLCA(f,MRN4[MRN4[,"q1"]==1,],nclass=3,graphs=TRUE,na.rm=FALSE,nrep=5,maxiter = 100000)
print(lca.c31$probs)

f<-cbind(q19,q32,q33,q34,q35,q36,q37) ~ 1 # define LCA model
lca.c32<-poLCA(f,MRN4[MRN4[,"q1"]==2,],nclass=3,graphs=TRUE,na.rm=FALSE,nrep=5,maxiter = 100000)
print(lca.c32$probs)

z=cbind(MRN4,lca.c3$predclass)

colnames(z)[24]<-"group"

z[,"group"] =as.factor(z[,"group"])
won_z<-z[z[,"q1"]==1,]
man_z<-z[z[,"q1"]==2,]
table(won_z[,"q19"],won_z$group)/nrow(won_z)
table(man_z[,"q19"],man_z$group)/nrow(man_z)




hist(lca.c3$predclass)


poLCA.entropy(lca.c32) #calculate the entropy of a cross-classification table
?par



summary(mydata)

# select variables
mydata <- MRN4%>% dplyr::select(ott,cnt,experience,q1,q3,q17,q18,q19,q32,q33,q34,q35,q36,q37,q38,q39,q40)
head(mydata)
# define functionz
f<-with(mydata, cbind(q19,q32,q33,q34,q35,q36,q37)~1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f, mydata, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model




########################

#select data

# define function
f<-with(mydata, cbind(q17,q19,q32,q33,q34,q35,q36,q37)~1) 

## models with different number of groups without covariates:
set.seed(01012)
lc1<-poLCA(f, data=mydata, nclass=1, na.rm = FALSE, nrep=30, maxiter=3000) #Loglinear independence model.
lc2<-poLCA(f, data=mydata, nclass=2, na.rm = FALSE, nrep=30, maxiter=3000)
lc3<-poLCA(f, data=mydata, nclass=3, na.rm = FALSE, nrep=30, maxiter=3000)
lc4<-poLCA(f, data=mydata, nclass=4, na.rm = FALSE, nrep=30, maxiter=3000) 
lc5<-poLCA(f, data=mydata, nclass=5, na.rm = FALSE, nrep=30, maxiter=3000)
lc6<-poLCA(f, data=mydata, nclass=6, na.rm = FALSE, nrep=30, maxiter=3000)

# generate dataframe with fit-values

results <- data.frame(Modell=c("Modell 1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)

results$Modell<-as.integer(results$Modell)

results[1,1]<-c("Modell 1")
results[2,1]<-c("Modell 2")
results[3,1]<-c("Modell 3")
results[4,1]<-c("Modell 4")
results[5,1]<-c("Modell 5")
results[6,1]<-c("Modell 6")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df

results[2,4]<-lc2$bic
results[3,4]<-lc3$bic
results[4,4]<-lc4$bic
results[5,4]<-lc5$bic
results[6,4]<-lc6$bic

results[2,5]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,5]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,5]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,5]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,5]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)

results[2,6]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,6]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,6]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,6]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,6]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))

results[2,7]<-lc2$Gsq
results[3,7]<-lc3$Gsq
results[4,7]<-lc4$Gsq
results[5,7]<-lc5$Gsq
results[6,7]<-lc6$Gsq


entropy<-function (p) sum(-p*log(p))

results$R2_entropy
results[1,8]<-c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,8]<-round(((error_prior-error_post) / error_prior),3)

# combining results to a dataframe
colnames(results)<-c("Model","log-likelihood","resid. df","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
lca_results<-results
print(lca_results)

poLCA.entropy(lc1)
poLCA.entropy(lc2)
poLCA.entropy(lc3)
poLCA.entropy(lc4)
poLCA.entropy(lc5)
poLCA.entropy(lc6)


############################################################
##########elbow plot
########################################################


# Order categories of results$model in order of appearance
#install.packages("forcats")
#library("forcats")

results$Model <- as.factor(results$Model) 

#convert to long format
results2<-tidyr::gather(results,Kriterium,Guete,4:7)
results2

#plot
fit.plot<-ggplot(results2) + 
  geom_point(aes(x=Model,y=Guete),size=3) +
  geom_line(aes(Model, Guete, group = 1)) +
  theme_bw()+
  labs(x = "", y="", title = "") + 
  facet_grid(Kriterium ~. ,scales = "free") +
  theme_bw(base_size = 16, base_family = "") +   
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(colour="grey", size=0.5),
        legend.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text=  element_text(size=16),
        axis.line = element_line(colour = "black")) # Achsen etwas dicker


# save 650 x 800
fit.plot











####### 

####### 

####### 

####### 

####### 





