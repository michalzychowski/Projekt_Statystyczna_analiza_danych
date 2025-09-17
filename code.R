# Zaimportowanie naszych danych oraz potrzebnych pakietów
raw_data <- read.csv('data/dane.csv',header = TRUE, sep = ',')
library(MASS)
library(ipred)
library(class)

# Przygotowanie danych
c <- ncol(raw_data)
data <- raw_data[,2:c]
head(data,n = 10)

# Utworzenie zbioru testowego i uczącego
indexes<-sample(1:nrow(data),nrow(data)/2,replace = FALSE)
ZU = data[indexes, ]
ZT = data[-indexes, ]

## Zbiór uczący
head(ZU)

## Zbiór testowy
head(ZT)

# Tworzenie naszego modelu
K1 <- 2
K2 <- 3

KNN_for_2 <- ipredknn(ZU[,8]~.,data=ZU,k=K1)
KNN_for_3 <- ipredknn(ZU[,8]~.,data=ZU,k=K2)

## Dla 2 sąsiadów
GOF2 <- data.frame(KNN_for_2$learn)
T1 <- table(GOF2$y,GOF2$X.Quality)
T1

G1 <- sum(diag(T1))/sum(T1)
G1

## Dla 3 sąsiadów
GOF3 <- data.frame(KNN_for_3$learn)
T2 <- table(GOF3$y,GOF3$X.Quality)
T2

G2 <- sum(diag(T2))/sum(T2)
G2

# Przewidujemy na zbiorze testowym
## Dla 2 sąsiadów
pred1 <- predict(KNN_for_2,ZT,"class")
S1 <- data.frame(pred1,ZT[,8])

T3 <- table(predykacja=pred1,prawdziwe=ZT[,8])
T3
G3 <- sum(diag(T3))/sum(T3)
G3

pred_p_1<-predict(KNN_for_2,ZT,"prob")
S2<-data.frame(pred_p_1)

FS1<-cbind(S1,S2)
colnames(FS1)<-c("Predykcja","Rzeczywistość","Prawdopodobieństwo dla predykcji")
head(FS1, n = 30)

## Dla 3 sąsiadów
pred2 <- predict(KNN_for_3,ZT,"class")
S3 <- data.frame(pred2,ZT[,8])

T4 <- table(predykacja=pred1,prawdziwe=ZT[,8])
T4
G4 <- sum(diag(T4))/sum(T4)
G4

pred_p_2<-predict(KNN_for_3,ZT,"prob")
S4<-data.frame(pred_p_2)


FS2<-cbind(S3,S4)
colnames(FS2)<-c("Predykcja","Rzeczywistość","Prawdopodobieństwo dla predykcji")
head(FS2, n=30)