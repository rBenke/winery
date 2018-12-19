library(tidyverse)
library(data.table)
data <- read.csv("C:\\Users\\robert\\Desktop\\R\\winery\\winequality.csv") %>% as.data.table()
validationIDX <- read.csv("C:\\Users\\robert\\Desktop\\R\\winery\\index.csv")
validation <- data[validationIDX$index,]
data <- data[-validationIDX$index,]
head(data)
summary(data)
data <- data[,N := 1:1300]
data <- data[,good := ifelse(quality>5.5,1,0),by= N]
data[,':='(quality=NULL,N= NULL)]
data2 <- copy(data)
data2[,':='(pH=NULL,residual.sugar=NULL)]

#CZY ODJECIE NIEPOTRZEBNCYH ZMIENNYCH POMAGA DRZEWA?
index <-lapply(1:1000,FUN =  function(a){sample(1:1299,400)})
library(rpart)
results1 <- lapply(1:1000,FUN = function(i){
  a <- rpart(good~.,data = data[-index[[i]],])  
  b <- predict(a,data[index[[i]],])
  (ifelse(b>0.5,1,0)-data[index[[i]]]$good) %>% abs() %>% sum()
  }) 
results2 <- lapply(1:1000,FUN = function(i) {
  a <-  rpart(good~.,data = data2[-index[[i]],])  
  b <- predict(a,data2[index[[i]],])
  (ifelse(b>0.5,1,0)-data2[index[[i]]]$good) %>% abs() %>% sum()
}) 
results1 %>% unlist() %>% mean()
results2 %>% unlist() %>% mean()
results1 %>% unlist() %>% hist
results2 %>% unlist() %>% hist

#CZY ZDJECIE NIEPOTRZEBNYCH ZMIENNYCH POMAGA RF?
data3 = copy(data2)
data3 <- data3[,':='(free.sulfur.dioxide=NULL,citric.acid=NULL)]
library(randomForest)
data3 <- data3[,good:=as.factor(good)]
data2 <- data2[,good:=as.factor(good)]
data <- data[,good:=as.factor(good)]
RF1 <- randomForest(good~.,data = data[-index[[1]],])
RF2 <- randomForest(good~.,data = data2[-index[[1]],])
RF3 <- randomForest(good~.,data = data3[-index[[1]],])

pred1 = (as.numeric(predict(RF1,data[index[[1]],]))- as.numeric(data[index[[1]],good])) %>% abs() %>% sum()
pred1/400
pred2 = (as.numeric(predict(RF2,data2[index[[1]],]))- as.numeric(data2[index[[1]],good])) %>% abs() %>% sum()
pred2/400
pred3 = (as.numeric(predict(RF3,data3[index[[1]],]))- as.numeric(data3[index[[1]],good])) %>% abs() %>% sum()
pred3/400

RF1$importance 
varImpPlot(RF1,type=2)
RF2$importance 
varImpPlot(RF2,type=2)
RF3$importance 
varImpPlot(RF3,type=2)
str(data)
#KORELACJA
CorMat <- cor(as.data.frame(data[,good:=as.numeric(good)]), method = "spearman", use="pairwise.complete.obs")
corrplot::corrplot(CorMat,method ="number")

#PCA
PCAdata <- prcomp(data[,-12,with=FALSE],center = TRUE,scale. = TRUE)
summary(PCAdata)
PCAdata <- as.matrix(data[,-12,with=FALSE]) %*% PCAdata$rotation
PCAdata <- as.data.table(PCAdata)
PCAdata <- PCAdata[,good := data$good]
summary(PCAdata)
RFpca <- randomForest(good~.,data = PCAdata[-index[[1]],])
predpca = (as.numeric(predict(RFpca,PCAdata[index[[1]],]))- as.numeric(PCAdata[index[[1]],good])) %>% abs() %>% sum()
predpca/400
print(RFpca)

#DLACZEGO PCA NIE ZADZIALALO
ggplot(data = PCAdata, aes(x=PC1,y=PC2)) + geom_point(aes(colour = as.factor(good)))
varImpPlot(RFpca,type=2)
ggplot(data = PCAdata, aes(x=PC8,y=PC9)) + geom_point(aes(colour = as.factor(good)))


#ZRBOMY T-SNE!
library(Rtsne)
tsne = Rtsne(unique(data[,-12,with=FALSE]),dim=2,perplexity=50)
plot(tsne$Y, col= unique(data)[,good],asp=1) #SLABO :(

#Feature engineering
#cut PH 3.0 3.2 3.4 3.5 3.7 zalezne od TOTAL acid

#jesli TA i sugar sa na odpowiednim poziomie to PH tez powinno - OK
#alkohol wyczuwalny do 0.2
#total sulfur dioxide ponizej 50 niewyczuwalne -  OK
#density zalezy od alkoholu i cukru - OK
data[,N:=1:1300]
data[,acidity_sugar_Balance := (fixed.acidity+volatile.acidity)/residual.sugar,by=N]
data[,rain:=acidity_sugar_Balance/pH,by=N]
data[,total.sulfur.dioxide:= ifelse(total.sulfur.dioxide>0.45,total.sulfur.dioxide,0),by=N]
data[,densi_Alco.Sugar:= alcohol/(residual.sugar*density),by=N]
summary(data)
data[,N:=NULL]
RF1 <- randomForest(good~.,data = data[-index[[1]],])
pred1 = (as.numeric(predict(RF1,data[index[[1]],]))- as.numeric(data[index[[1]],good])) %>% abs() %>% sum()
pred1/400
RF1$importance
varImpPlot(RF1,type=2)


library(glmnet)
f <- as.formula(y ~ .*.*.)
y <- data$good
# Second step: using model.matrix to take advantage of f
x <- model.matrix(f, data[,-12,with=F])[, -1]
glmmod <- glmnet(x,as.factor(y), alpha=1, family="binomial",lambda = 0.00439575)
glmmod
 coef(glmmod)[, 1] %>% as.data.frame()
cv.glmmod <- cv.glmnet(x,y,alpha=1, family="binomial")
plot(cv.glmmod)
# Plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod, xvar="lambda")
(best.lambda <- cv.glmmod$lambda.min)

validation[,quality:=NULL]
f <- as.formula(y ~ .*.*.)
y <- validation$good
x <- model.matrix(f,validation[,-12,with=F])[, -1]
AA <- table(ifelse(predict(glmmod,x,type = "response")>0.5,1,0),validation$good)


validation <- validation[,N := 1:1300]
validation <- validation[,good := ifelse(quality>5.5,1,0),by= N]
validation[,acidity_sugar_Balance := (fixed.acidity+volatile.acidity)/residual.sugar,by=N]
validation[,rain:=acidity_sugar_Balance/pH,by=N]
validation[,total.sulfur.dioxide:= ifelse(total.sulfur.dioxide>0.45,total.sulfur.dioxide,0),by=N]
validation[,densi_Alco.Sugar:= alcohol/(residual.sugar*density),by=N]
validation[,N:=NULL]

data %>% names
data[,N:=1:2000]
data3 <- copy(data)
data3 <- 
  data3[,':='(den.pH = density*pH,Vacid.pH = volatile.acidity*pH,
           VolAcid.pH.Densi=volatile.acidity*pH*density, Vacid.AciSugBal.rain = volatile.acidity*acidity_sugar_Balance*rain,
           Chlor.AciSugBal.DenAlcSug = chlorides*acidity_sugar_Balance*densi_Alco.Sugar,
           FixAci.CitricAcid.ResidSugar = fixed.acidity*citric.acid*residual.sugar,
           Chlor.freeSul.AciSugBalance=chlorides*free.sulfur.dioxide*acidity_sugar_Balance),by=N]
data4 <- copy(data3)
data4 <- 
  data4[,':='(fixed.acidity=NULL,volatile.acidity=NULL,
              citric.acid=NULL,residual.sugar=NULL,chlorides=NULL,
              free.sulfur.dioxide =NULL ,total.sulfur.dioxide=NULL,
              density=NULL ,pH=NULL , sulphates=NULL, alcohol=NULL)]
data3 <- data3[,N:=NULL]
data4 <- data4[,N:=NULL]

index <-lapply(1:1000,FUN =  function(a){sample(1:1299,400)})
library(rpart)
data4 = data4[,good:=as.factor(good)]
data3 = data3[,good:=as.factor(good)]
results4 <- lapply(1:1000,FUN = function(i){
  a <- rpart(good~.,data = data4[-index[[i]],])  
  b <- predict(a,data4[index[[i]],])
  A <- table(ifelse(b>0.5,1,0)[,2],data4[index[[i]],]$good) 
  A[1,2]+A[2,1]
}) 
results4 %>% unlist() %>% mean()

library(rpart)
results3 <- lapply(1:1000,FUN = function(i){
  a <- rpart(good~.,data = data3[-index[[i]],])  
  b <- predict(a,data3[index[[i]],])
  A <- table(ifelse(b>0.5,1,0)[,2],data3[index[[i]],]$good) 
  A[1,2]+A[2,1]
  
}) 
results3 %>% unlist() %>% mean()


results1 %>% unlist() %>% hist
results2 %>% unlist() %>% hist
results3 %>% unlist() %>% hist
results4 %>% unlist() %>% hist

df1 <- data.frame(matrix(unlist(results1), nrow=1000, byrow=T))
df2 <- data.frame(matrix(unlist(results2), nrow=1000, byrow=T))
df3 <- data.frame(matrix(unlist(results3), nrow=1000, byrow=T))
df4 <- data.frame(matrix(unlist(results4), nrow=1000, byrow=T))

df1 <- mutate(df1,group=1)
df2 <- mutate(df2,group=2)
df3 <- mutate(df3,group=3)
df4 <- mutate(df4,group=4)
names(df1) <- c("blednie","grupa")
names(df2) <- c("blednie","grupa")
names(df3) <- c("blednie","grupa")
names(df4) <- c("blednie","grupa")

dataX <- rbind(df1,df2,df3,df4)

ggplot(data=dataX,aes(x = as.factor(grupa), y =blednie))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)



validation <- validation[,N := 1:1300]
validation <- validation[,good := ifelse(quality>5.5,1,0),by= N]
validation[,acidity_sugar_Balance := (fixed.acidity+volatile.acidity)/residual.sugar,by=N]
validation[,rain:=acidity_sugar_Balance/pH,by=N]
validation[,total.sulfur.dioxide:= ifelse(total.sulfur.dioxide>0.45,total.sulfur.dioxide,0),by=N]
validation[,densi_Alco.Sugar:= alcohol/(residual.sugar*density),by=N]
validation[,N:=NULL]

validation <- 
  validation [,':='(den.pH = density*pH,Vacid.pH = volatile.acidity*pH,
              VolAcid.pH.Densi=volatile.acidity*pH*density, Vacid.AciSugBal.rain = volatile.acidity*acidity_sugar_Balance*rain,
              Chlor.AciSugBal.DenAlcSug = chlorides*acidity_sugar_Balance*densi_Alco.Sugar,
              FixAci.CitricAcid.ResidSugar = fixed.acidity*citric.acid*residual.sugar,
              Chlor.freeSul.AciSugBalance=chlorides*free.sulfur.dioxide*acidity_sugar_Balance),by=N]

data4[,good:=as.factor(good)]
RFlast <- randomForest(good~.,data = data3, ntree=521,nodesize=1,mtry=3)
A = table(predict(RFlast,validation),validation$good)
(diag(A) %>% sum)/300

