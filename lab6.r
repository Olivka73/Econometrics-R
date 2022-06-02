setwd("D:/Olivkis/")
library("forecast")
M<-read.csv2("Milk09-12.csv", header=FALSE, sep=";", dec = ",", col.names = c("Month","Price1","Price2"))

# ��������� ������ ���������� ���� ��� Price1
plot(M$Price1, main="���� �� ������", ylab="����, ���.", xlab="�����, �����", type="o")
x<-seq(from=1, to=length(M$Price1))
reg1<-lm(M$Price1~x)
abline(reg1)
# ��������� ��� ����� �������� ������������ ���������.

# ���������� ������������ ���������
MM1 = data.frame(y = as.vector(M$Price1), x = seq(from=1, to=length(M$Price1)))
reg1.2 <- nls(y ~ p1 * x +  p2 * sin(1/2 * x + p3) + p4, data = MM1, start = list(p1 = 1, p2 = 1, p3 = 0, p4 = 0))
# ���������� ���������� ���������
curve(coef(reg1.2)[1]*x + coef(reg1.2)[2]*sin(1/2*x + coef(reg1.2)[3]) + coef(reg1.2)[4], add=TRUE, col="red") 
summary(reg1.2)
accuracy(reg1)

plot(residuals(reg1), main = '������� �������� ������')
abline(h=0, col="red")
plot(residuals(reg1.2), main = '������� ���������� ������')
abline(h=0, col="red")





#��������� ������ ���������� ���� ��� Price2
plot(M$Price2, main="���� �� ������", ylab="����, ���.", xlab="�����, �����", type="o")
#��������� ��� ����� ������������ ���������� ���������.
reg2<-lm(M$Price2~x)
abline(reg2)

# ���������� ������������ ���������
MM2 = data.frame(y = as.vector(M$Price2), x = seq(1, length(M[,1])))

# ���������������� ���������
reg2.2 = nls(y ~ p1 + exp(x * p2), data = MM2, start=list(p1 = 10, p2 = 0))
# ���������� ���������� ��������� (����������)
curve(coef(reg2.2)[1] + exp(x* coef(reg2.2)[2]), add=TRUE, col="green")


# ������������ ���������
reg2.3 = nls(y ~ p1*x + x^2 * p2 + p3, data = MM2, start=list(p1 = 10, p2 = 0, p3 = 0))
# ���������� ���������� ��������� 
curve(coef(reg2.3)[1]*x + x^2 * coef(reg2.3)[2] +coef(reg2.3)[3], add=TRUE, col="red")

summary(reg2.2)
accuracy(reg2)

plot(residuals(reg2), main = '������� �������� ������')
abline(h=0, col="red")
plot(residuals(reg2.2), main = '������� ���������� ������')
abline(h=0, col="red")
plot(residuals(reg2.3), main = '������� ���������� ������')
abline(h=0, col="red")

