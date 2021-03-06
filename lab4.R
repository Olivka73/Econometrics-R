setwd("D:/Olivkis/")
library("forecast") 
T<-read.table("reg.txt", header=TRUE)
T<-T[order(T$A1),] # ����������� ������ � ������� � �� ����������� �1

# ��� A1 � A7
reg7<-lm(formula=T$A7 ~T$A1) # ��������� �������� ���������
 #reg7.1<-lm(formula=T$A7 ~log(T$A1)) # ��������� ��������������� ���������
reg7.1<-nls(T$A7~p1 + p2*T$A1 + p3*(T$A1)^2, data=T, start=list(p1=500, p2=-10000, p3=-1000))

summary(reg7.1)

plot(x=T$A1,y=T$A7) # ���������� ��������� ���������
abline(reg7, col="red") # ���������� �������� ���������
curve(coef(reg7.1)[1] + coef(reg7.1)[2]*x + coef(reg7.1)[3]*x*x, add=TRUE, col="green")

#
#plot(reg7.1$residuals, main="������ ��������", xlab="����� ����������", ylab="�������")
#abline(h=0, col="red")
#accuracy(reg7.1) # ���������� ������ ��������� MAPE


reg7.2<-nls(T$A7~p1 + p2/(p3 + T$A1), data=T, start=list(p1=500, p2=-10000, p3=-1000)) # ��������� ���������� ��������� � �����������
summary(reg7.2) # ������� ������������ ���������
P<-coef(reg7.2) # ��������� ������������ ��������� � ���������� �
plot(x=T$A1,y=T$A7) # ���������� ��������� ���������
abline(reg7, col="red") #���������� �������� ���������
curve(coef(reg7.1)[1] + coef(reg7.1)[2]*x + coef(reg7.1)[3]*x*x, add=TRUE, col="green")
#curve(coef(reg7.1)[1] + coef(reg7.1)[2]*log(x), add=TRUE, col="green") # ���������� ���������� ���������
curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue") # ���������� ���������� ���������

plot(reg7.2$m$resid(), main="������ ��������", xlab="����� ����������", ylab="�������")
abline(h=0, col="red")
sum(abs((T$A7 - (P[1]+ P[2]/(P[3]+T$A1)))/T$A7))/length(T$A7)*100 # ���������� MAPE

P[1] + P[2]/(P[3] + 1350)
P[1] + P[2]*1350 + P[3]*1350*1350


# ��� A1 � A6
T3<-T[-which(T$A6>400),c(1,6)] #������� ������

reg6<-lm(formula=T3$A6 ~T3$A1) # ��������� �������� ���������
reg6.1<-lm(formula=T3$A6 ~log(T3$A1)) # ��������� ��������������� ���������
reg6.1<-nls(T3$A6~p1 + p2*T3$A1 + p3*(T3$A1)^2, data=T3, start=list(p1=500, p2=-10000, p3=-1000))
summary(reg6.1)

plot(x=T3$A1,y=T3$A6) # ���������� ��������� ���������
abline(reg6, col="red") # ���������� �������� ���������
curve(coef(reg6.1)[1] + coef(reg6.1)[2]*x + coef(reg6.1)[3]*x*x, add=TRUE, col="green") # ���������� ���������� ���������

#plot(reg6.1$residuals, main="������ ��������", xlab="����� ����������", ylab="�������")
#abline(h=0, col="red")
#accuracy(reg6.1) # ���������� ������ ��������� MAPE


reg6.2<-nls(T3$A6~p1 + p2/(p3 + T3$A1), data=T3, start=list(p1=1, p2=1, p3=1)) 
summary(reg6.2) # ������� ������������ ���������
P<-coef(reg6.2) # ��������� ������������ ��������� � ���������� �
plot(x=T3$A1,y=T3$A6) # ���������� ��������� ���������
abline(reg6, col="red") #���������� �������� ���������
curve(coef(reg6.1)[1] + coef(reg6.1)[2]*x + coef(reg6.1)[3]*x*x, add=TRUE, col="green") # ���������� ���������� ���������
curve(coef(reg6.1)[1] + coef(reg6.1)[2]*log(x), add=TRUE, col="green") # ���������� ���������� ���������
curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue") # ���������� ���������� ���������

plot(reg6.1$m$resid(), main="������ ��������", xlab="����� ����������", ylab="�������")
abline(h=0, col="red")
sum(abs((T$A6 - (P[1]+ P[2]/(P[3]+T$A1)))/T$A6))/length(T$A6)*100 # ���������� MAPE

P[1] + P[2]/(P[3] + 1350)
P[1] + P[2]*1350 + P[3]*1350*1350


# ��� A1 � A8
reg8<-lm(formula=T$A8 ~T$A1) # ��������� �������� ���������
reg8.1<-nls(T$A8~p1 + p2*T$A1 + p3*(T$A1)^2, data=T, start=list(p1=500, p2=-10000, p3=-1000))
summary(reg8.1)


reg8<-lm(formula=T$A8 ~T$A1) # ��������� �������� ���������
reg8.1<-lm(formula=T$A8 ~log(T$A1)) # ��������� ��������������� ���������
summary(reg8.1)

plot(x=T$A1,y=T$A8) # ���������� ��������� ���������
abline(reg8, col="red") # ���������� �������� ���������
curve(coef(reg8.1)[1] + coef(reg8.1)[2]*x + coef(reg8.1)[3]*x*x, add=TRUE, col="green") # ���������� ���������� ���������

#curve(coef(reg8.1)[1] + coef(reg8.1)[2]*log(x), add=TRUE, col="green")
plot(reg8.1$residuals, main="������ ��������", xlab="����� ����������", ylab="�������")
abline(h=0, col="red")
accuracy(reg8.1) # ���������� ������ ��������� MAPE


reg8.2<-nls(T$A8~p1 + p2/(p3 + T$A1), data=T, start=list(p1=500, p2=-10000, p3=-1000)) # ��������� ���������� ��������� � �����������
summary(reg8.2) # ������� ������������ ���������
P<-coef(reg8.2) # ��������� ������������ ��������� � ���������� �
plot(x=T$A1,y=T$A8) # ���������� ��������� ���������
abline(reg8, col="red") #���������� �������� ���������
curve(coef(reg8.1)[1] + coef(reg8.1)[2]*log(x), add=TRUE, col="green") # ���������� ���������� ���������
curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue") # ���������� ���������� ���������
plot(reg8.2$m$resid(), main="������ ��������", xlab="����� ����������", ylab="�������")
abline(h=0, col="red")
sum(abs((T$A8 - (P[1]+ P[2]/(P[3]+T$A1)))/T$A8))/length(T$A8)*100 # ���������� MAPE

P[1] + P[2]/(P[3] + 1350)
P[1] + P[2]*1350 + P[3]*1350*1350

