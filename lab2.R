setwd("D:/Olivkis/")
T<-read.table("reg.txt", header=TRUE)

#����������� �1 � �2
plot(x=T$A1, y=T$A2) #����������� �����������
cor(T$A1, T$A2)
cor.test(T$A1, T$A2)


#����������� �1 � �3
plot(x=T$A1, y=T$A3) #����������� �������
cor(T$A1, T$A3) #���������� ��
cor.test(T$A1, T$A3)

abline(h=750, col="red")
T3<-T[-which(T$A3>750),c(1,3)]

plot(x=T3$A1, y=T3$A3)
cor(T3$A1, T3$A3) #���������� �����
cor.test(T3$A1, T3$A3)


#����������� �1 � �4
plot(x=T$A1, y=T$A4) #����������� �����������
cor(T$A1, T$A4)
cor.test(T$A1, T$A4)


#����������� �1 � �5
plot(x=T$A1, y=T$A5) #����������� �����������
cor(T$A1, T$A5)
cor.test(T$A1, T$A5)


#����������� �1 � �6
plot(x=T$A1, y=T$A6) #����������� ���������
cor(T$A1, T$A6) #���������� ��
cor.test(T$A1, T$A6)

abline(h=400, col="red")
T3<-T[-which(T$A6>400),c(1,6)]

plot(x=T3$A1, y=T3$A6)
cor(T3$A1, T3$A6) #���������� �����
cor.test(T3$A1, T3$A6)


#����������� �1 � �7
plot(x=T$A1, y=T$A7) #����������� ���������
cor(T$A1, T$A7)
cor.test(T$A1, T$A7)


#����������� �1 � �8
plot(x=T$A1, y=T$A8) #����������� ���������
cor(T$A1, T$A8)
cor.test(T$A1, T$A8)


#����������� �1 � �9
plot(x=T$A1, y=T$A9) #����������� �������
cor(T$A1, T$A9)
cor.test(T$A1, T$A9)


#����������� �1 � �10
plot(x=T$A1, y=T$A10) #����������� �������� ��������
cor(T$A1, T$A10)
cor.test(T$A1, T$A10)


#����������� �1 � �11
plot(x=T$A1, y=T$A11) #����������� �������� ��������
cor(T$A1, T$A11)
cor.test(T$A1, T$A11)


#����������� �1 � �12
plot(x=T$A1, y=T$A12) #����������� �������� ��������
cor(T$A1, T$A12) #���������� ��
cor.test(T$A1, T$A12)

abline(h=600, col="red")
T3<-T[-which(T$A12<600),c(1,12)]

plot(x=T3$A1, y=T3$A12)
cor(T3$A1, T3$A12) #���������� �����
cor.test(T3$A1, T3$A12)


