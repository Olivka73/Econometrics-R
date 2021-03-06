setwd("D:/Olivkis/")
library("forecast") 
T<-read.table("reg.txt", header=TRUE)
T<-T[order(T$A1),] # ����������� ������ � ������� � �� ����������� �1

# ��� �1 � �2
regA2A1<-lm(formula=T$A2 ~T$A1)

format(coef(regA2A1), digits = 12) #����� ������������� ��������� 
summary(regA2A1) #����� ���������� � ���������

mean(regA2A1$residuals) #��� �������� ��������
	plot(regA2A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA2A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA2A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA2A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A2) # ��������� ���������
	abline(regA2A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �3
regA3A1<-lm(formula=T$A3 ~T$A1)
format(coef(regA3A1), digits = 12) #����� ������������� ��������� 

summary(regA3A1) #����� ���������� � ���������

mean(regA3A1$residuals) #��� �������� ��������
	plot(regA3A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA3A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA3A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA3A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A3) # ��������� ���������
	abline(regA3A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �4
regA4A1<-lm(formula=T$A4 ~T$A1)
format(coef(regA4A1), digits = 12) #����� ������������� ��������� 

summary(regA4A1) #����� ���������� � ���������

mean(regA4A1$residuals) #��� �������� ��������
	plot(regA4A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA4A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA4A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA4A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A4) # ��������� ���������
	abline(regA4A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �5
regA5A1<-lm(formula=T$A5 ~T$A1)
format(coef(regA5A1), digits = 5) #����� ������������� ��������� 

summary(regA5A1) #����� ���������� � ���������

mean(regA5A1$residuals) #��� �������� ��������
	plot(regA5A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA5A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA5A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA5A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A5) # ��������� ���������
	abline(regA5A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �6
regA6A1<-lm(formula=T$A6 ~T$A1)
format(coef(regA6A1), digits = 5) #����� ������������� ��������� 

summary(regA6A1) #����� ���������� � ���������

mean(regA6A1$residuals) #��� �������� ��������
	plot(regA6A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

abline(h=240, col="red")
T3<-T[-which(T$A6>240),c(1,6)]
regA6A1<-lm(formula=T3$A6 ~T3$A1)

accuracy(regA6A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA6A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA6A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A6) # ��������� ���������
	abline(regA6A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �7
regA7A1<-lm(formula=T$A7 ~T$A1)
format(coef(regA7A1), digits = 5) #����� ������������� ��������� 

summary(regA7A1) #����� ���������� � ���������

mean(regA7A1$residuals) #��� �������� ��������
	plot(regA7A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA7A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA7A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA7A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A7) # ��������� ���������
	abline(regA7A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �8
regA8A1<-lm(formula=T$A8 ~T$A1)
format(coef(regA8A1), digits = 5) #����� ������������� ��������� 

summary(regA8A1) #����� ���������� � ���������

mean(regA8A1$residuals) #��� �������� ��������
	plot(regA8A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA8A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 1350 � ���������� A1
format(predict(regA8A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA8A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A8) # ��������� ���������
	abline(regA8A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �9
regA9A1<-lm(formula=T$A9 ~T$A1)
format(coef(regA9A1), digits = 5) #����� ������������� ��������� 

summary(regA9A1) #����� ���������� � ���������

mean(regA9A1$residuals) #��� �������� ��������
	plot(regA9A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA9A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA9A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA9A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A9) # ��������� ���������
	abline(regA9A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �10
regA10A1<-lm(formula=T$A10 ~T$A1)
format(coef(regA10A1), digits = 5) #����� ������������� ��������� 

summary(regA10A1) #����� ���������� � ���������

mean(regA10A1$residuals) #��� �������� ��������
	plot(regA10A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA10A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA10A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA10A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A10) # ��������� ���������
	abline(regA10A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������


# ��� �1 � �11
regA11A1<-lm(formula=T$A11 ~T$A1)
format(coef(regA11A1), digits = 5) #����� ������������� ��������� 

summary(regA11A1) #����� ���������� � ���������

mean(regA11A1$residuals) #��� �������� ��������
	plot(regA11A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA11A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA11A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA11A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A11) # ��������� ���������
	abline(regA11A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������



# ��� �1 � �12
regA12A1<-lm(formula=T$A12 ~T$A1)
format(coef(regA12A1), digits = 5) #����� ������������� ��������� 

summary(regA12A1) #����� ���������� � ���������

mean(regA12A1$residuals) #��� �������� ��������
	plot(regA12A1$residuals, main="������ ��������", xlab="����� ����������",
	ylab="�������") #������ ��������
	abline(h=0)

accuracy(regA12A1) # ����� ������ �������� ���������, MAPE

T<-T[1,] # �������� ������ ���� ������ � ������� �
T$A1<- 1350 # �������� �������� 85 � ���������� F1
format(predict(regA12A1, newdata=T, interval="confidence", level=0.9), digits=10) #����� ���������

P<-predict(regA12A1, newdata=T, interval="confidence", level=0.9) # �������� ������� � P
T<-read.table("reg.txt", header=TRUE) # ����� ��������� ������ �� �����
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A12) # ��������� ���������
	abline(regA12A1) # ����� ���������
points(x=1350, y=P[1], col="red") # �������� �������
lines(x=c(1350,1350,1350), y=P, col="red") # ������������ �������

