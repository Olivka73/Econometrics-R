setwd("D:/Olivkis/")
library("forecast") 
T<-read.table("reg.txt", header=TRUE)
T<-T[order(T$A1),] # сортировать строки в таблице Т по возрастанию А1

# для A1 и A7
reg7<-lm(formula=T$A7 ~T$A1) # построить линейную регрессию
 #reg7.1<-lm(formula=T$A7 ~log(T$A1)) # построить логарифмическую регрессию
reg7.1<-nls(T$A7~p1 + p2*T$A1 + p3*(T$A1)^2, data=T, start=list(p1=500, p2=-10000, p3=-1000))

summary(reg7.1)

plot(x=T$A1,y=T$A7) # нарисовать диаграмму рассеяния
abline(reg7, col="red") # нарисовать линейную регрессию
curve(coef(reg7.1)[1] + coef(reg7.1)[2]*x + coef(reg7.1)[3]*x*x, add=TRUE, col="green")

#
#plot(reg7.1$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
#abline(h=0, col="red")
#accuracy(reg7.1) # рассчитать оценку регрессии MAPE


reg7.2<-nls(T$A7~p1 + p2/(p3 + T$A1), data=T, start=list(p1=500, p2=-10000, p3=-1000)) # построить нелинейную регрессию с параметрами
summary(reg7.2) # вывести коэффициенты регрессии
P<-coef(reg7.2) # сохранить коэффициенты регрессии в переменной Р
plot(x=T$A1,y=T$A7) # нарисовать диаграмму рассеяния
abline(reg7, col="red") #нарисовать линейную регрессию
curve(coef(reg7.1)[1] + coef(reg7.1)[2]*x + coef(reg7.1)[3]*x*x, add=TRUE, col="green")
#curve(coef(reg7.1)[1] + coef(reg7.1)[2]*log(x), add=TRUE, col="green") # нарисовать нелинейную регрессию
curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue") # нарисовать нелинейную регрессию

plot(reg7.2$m$resid(), main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
sum(abs((T$A7 - (P[1]+ P[2]/(P[3]+T$A1)))/T$A7))/length(T$A7)*100 # рассчитать MAPE

P[1] + P[2]/(P[3] + 1350)
P[1] + P[2]*1350 + P[3]*1350*1350


# для A1 и A6
T3<-T[-which(T$A6>400),c(1,6)] #убираем выброс

reg6<-lm(formula=T3$A6 ~T3$A1) # построить линейную регрессию
reg6.1<-lm(formula=T3$A6 ~log(T3$A1)) # построить логарифмическую регрессию
reg6.1<-nls(T3$A6~p1 + p2*T3$A1 + p3*(T3$A1)^2, data=T3, start=list(p1=500, p2=-10000, p3=-1000))
summary(reg6.1)

plot(x=T3$A1,y=T3$A6) # нарисовать диаграмму рассеяния
abline(reg6, col="red") # нарисовать линейную регрессию
curve(coef(reg6.1)[1] + coef(reg6.1)[2]*x + coef(reg6.1)[3]*x*x, add=TRUE, col="green") # нарисовать нелинейную регрессию

#plot(reg6.1$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
#abline(h=0, col="red")
#accuracy(reg6.1) # рассчитать оценку регрессии MAPE


reg6.2<-nls(T3$A6~p1 + p2/(p3 + T3$A1), data=T3, start=list(p1=1, p2=1, p3=1)) 
summary(reg6.2) # вывести коэффициенты регрессии
P<-coef(reg6.2) # сохранить коэффициенты регрессии в переменной Р
plot(x=T3$A1,y=T3$A6) # нарисовать диаграмму рассеяния
abline(reg6, col="red") #нарисовать линейную регрессию
curve(coef(reg6.1)[1] + coef(reg6.1)[2]*x + coef(reg6.1)[3]*x*x, add=TRUE, col="green") # нарисовать нелинейную регрессию
curve(coef(reg6.1)[1] + coef(reg6.1)[2]*log(x), add=TRUE, col="green") # нарисовать нелинейную регрессию
curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue") # нарисовать нелинейную регрессию

plot(reg6.1$m$resid(), main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
sum(abs((T$A6 - (P[1]+ P[2]/(P[3]+T$A1)))/T$A6))/length(T$A6)*100 # рассчитать MAPE

P[1] + P[2]/(P[3] + 1350)
P[1] + P[2]*1350 + P[3]*1350*1350


# для A1 и A8
reg8<-lm(formula=T$A8 ~T$A1) # построить линейную регрессию
reg8.1<-nls(T$A8~p1 + p2*T$A1 + p3*(T$A1)^2, data=T, start=list(p1=500, p2=-10000, p3=-1000))
summary(reg8.1)


reg8<-lm(formula=T$A8 ~T$A1) # построить линейную регрессию
reg8.1<-lm(formula=T$A8 ~log(T$A1)) # построить логарифмическую регрессию
summary(reg8.1)

plot(x=T$A1,y=T$A8) # нарисовать диаграмму рассеяния
abline(reg8, col="red") # нарисовать линейную регрессию
curve(coef(reg8.1)[1] + coef(reg8.1)[2]*x + coef(reg8.1)[3]*x*x, add=TRUE, col="green") # нарисовать нелинейную регрессию

#curve(coef(reg8.1)[1] + coef(reg8.1)[2]*log(x), add=TRUE, col="green")
plot(reg8.1$residuals, main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
accuracy(reg8.1) # рассчитать оценку регрессии MAPE


reg8.2<-nls(T$A8~p1 + p2/(p3 + T$A1), data=T, start=list(p1=500, p2=-10000, p3=-1000)) # построить нелинейную регрессию с параметрами
summary(reg8.2) # вывести коэффициенты регрессии
P<-coef(reg8.2) # сохранить коэффициенты регрессии в переменной Р
plot(x=T$A1,y=T$A8) # нарисовать диаграмму рассеяния
abline(reg8, col="red") #нарисовать линейную регрессию
curve(coef(reg8.1)[1] + coef(reg8.1)[2]*log(x), add=TRUE, col="green") # нарисовать нелинейную регрессию
curve(P[1] + P[2]/(x+P[3]), add=TRUE, col="blue") # нарисовать нелинейную регрессию
plot(reg8.2$m$resid(), main="График остатков", xlab="Номер наблюдения", ylab="Остатки")
abline(h=0, col="red")
sum(abs((T$A8 - (P[1]+ P[2]/(P[3]+T$A1)))/T$A8))/length(T$A8)*100 # рассчитать MAPE

P[1] + P[2]/(P[3] + 1350)
P[1] + P[2]*1350 + P[3]*1350*1350

