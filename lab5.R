setwd("D:/Olivkis/")
M<-read.csv2("Milk.csv", header=TRUE, sep=";" , dec = ",");
View(M)
library("forecast")
#корреляционный анализ переменных
cor(data.frame(M$MilkPrice,M$Income,M$Population))


#диаграмма рассеяния для ЦЕН на молоко и Среднедушевых ДОХОДОВ
plot(x=M$Income,y=M$MilkPrice, main="Зависимость цены от дохода")
M<-M[order(M$Income),]
#регрессия для ЦЕН на молоко и Среднедушевых ДОХОДОВ
reg1<-lm(M$MilkPrice~M$Income)
summary(reg1)
abline(reg1)
#остатки
plot(residuals(reg1))
abline(h=0, col="red")
#MAPE
accuracy(reg1)


#регрессия для ЦЕН на молоко и Численности населения
plot(x=M$Population,y=M$MilkPrice, main="Зависимость цены от численности населения" ,
     xlab = "Среднедушевые доходы", ylab = "Цена на молоко")
M<-M[order(M$Population),]
reg2<-lm(M$MilkPrice~M$Population)
summary(reg2)
abline(reg2)
#остатки
plot(residuals(reg2))
abline(h=0, col="red")
#MAPE
accuracy(reg2)


#множественная ЛИНЕЙНАЯ регрессия для  
# ЦЕН на молоко, Среднедушевых ДОХОДОВ и Численности населения
reg3<-lm(M$MilkPrice~M$Income+M$Population)
summary(reg3)
accuracy(reg3)
plot(residuals(reg3))
abline(h=0, col="red")


#множественная НЕЛИНЕЙНАЯ регрессия для  
# ЦЕН на молоко, Среднедушевых ДОХОДОВ и Численности населения

#отсортируем по возрастанию Среднедушевых доходов
M<-M[order(M$Income),]
reg3.1<-lm(M$MilkPrice~M$Income+M$Population)
M<-M[order(M$Income),]
summary(reg3.1)
accuracy(reg3.1)
plot(residuals(reg3.1))
abline(h=0, col="red")

#отсортируем по возрастанию Численности населения
M<-M[order(M$Population),]
reg3.2<-lm(M$MilkPrice~M$Income+M$Population)
M<-M[order(M$Income),]
summary(reg3.2)
accuracy(reg3.2)
plot(residuals(reg3.2))
abline(h=0, col="red")

###
###
###частные уравнения регрессии
coefficients(reg3.2)
# MilkPrice = 2.004313e+01 + 9.768220e-04*Income - 2.176969e-06*Population + e
-2.176969e-06*mean(M$Population)+2.004313e+01
# MilkPrice(Income) = 16.20918 + 9.768220e-04*Income + e
9.768220e-04*mean(M$Income)+2.004313e+01
# MilkPrice(Population) = 40.4781 - 2.176969e-06*Population + e

###
###
###графики для частных коэффициентов эластичности
milk = 2.004313e+01 + 9.768220e-04*mean(M$Income) - 2.176969e-06*mean(M$Population)
#средняя эластичность
#производная функции по доходам * среднее значение доходов / среднее значение milk
9.768220e-04 * mean(M$Income) / milk
#производная функции по численности населения * среднее значение численности населения / среднее значение milk
-2.176969e-06 * mean(M$Population) / milk
#график эластичности для доходов
plot(9.768220e-04 * M[order(M$Income),]$Income / milk, type = 'l', main = 'Эластичность для доходов',
     xlab = 'Наблюдение', ylab = 'Значение эластичности', col='blue')
#график эластичности для численности населения
plot(-2.176969e-06 * M[order(M$Population),]$Population / milk, type = 'l', main = 'Эластичность для численности населения',
     xlab = 'Наблюдение', ylab = 'Значение эластичности', col='green')


#множественная нелинейная регрессия для Цен на молоко,
#доходов, Численности населения и Федерального округа
#Пусть фактор Федеральный округ влияет на свободный член
#Создадим 8 фиктивных переменных и заполним их данными
ZCFO<-rep(0, times=length(M$FO))
ZUFO<-rep(0, times=length(M$FO))
ZSZFO<-rep(0, times=length(M$FO))
ZUrFO<-rep(0, times=length(M$FO))
ZPFO<-rep(0, times=length(M$FO))
ZSKFO<-rep(0, times=length(M$FO))
ZSFO<-rep(0, times=length(M$FO))
ZDVFO<-rep(0, times=length(M$FO))
ZCFO[M$FO=="ЦФО"]<-1
ZUFO[M$FO=="ЮФО"]<-1
ZSZFO[M$FO=="СЗФО"]<-1
ZUrFO[M$FO=="УФО"]<-1
ZPFO[M$FO=="ПФО"]<-1
ZSKFO[M$FO=="СКФО"]<-1
ZSFO[M$FO=="СФО"]<-1
ZDVFO[M$FO=="ДВФО"]<-1
M1<-data.frame(M,ZCFO,ZUFO,ZSZFO,ZUrFO,ZPFO,ZSKFO,ZSFO,ZDVFO)
rm(ZCFO,ZUFO,ZSZFO,ZUrFO,ZPFO,ZSKFO,ZSFO, ZDVFO)

#регрессия с учетом фиктивных переменных
reg4.1<-lm(M1$MilkPrice ~ M1$Income +M1$Population + M1$ZCFO + M1$ZUFO + M1$ZSZFO
           + M1$ZUrFO + M1$ZPFO + M1$ZSKFO + M1$ZSFO + M1$ZDVFO)
summary(reg4.1)
#Оказывает ли фактор Федеральный округ влияние на Цены на молоко?
#да оказывает, все p value коэф значимы
accuracy(reg4.1)

# MilkPrice = 3.556e+01 + 8.276e-04*Income - 1.314e-06*Population 
#             - 1.831e+01*ZCFO - 1.434e+01*ZUFO - 1.453e+01*ZSZFO 
#             - 1.565e+01*ZUrFO - 1.793e+01*ZPFO - 1.265e+01*ZSKFO - 1.104e+01*ZSFO


plot(residuals(reg4.1))
abline(h=0, col="red")
#Можно ли использовать построенную модель для прогнозирования?
#использовать нельзя, остатки распределены неравномерно и зависимы

mean(M1[-which(M1$FO != "ЦФО"), c(2,5)]$MilkPrice)
# 31.33
mean(M1[-which(M1$FO != "ЮФО"), c(2,5)]$MilkPrice)
# 31.95167
mean(M1[-which(M1$FO != "СЗФО"), c(2,5)]$MilkPrice)
# 40.46636
mean(M1[-which(M1$FO != "УФО"), c(2,5)]$MilkPrice)
# 42.75833
#самые низкие цены
mean(M1[-which(M1$FO != "ПФО"), c(2,5)]$MilkPrice)
#29.62786
mean(M1[-which(M1$FO != "СКФО"), c(2,5)]$MilkPrice)
# 33.89143
mean(M1[-which(M1$FO != "СФО"), c(2,5)]$MilkPrice)
# 36.64917
#самые высокие цены
mean(M1[-which(M1$FO != "ДВФО"), c(2,5)]$MilkPrice)
# 56.95875

#самые низкие цены на молоко
mean(M1$ZSFO) # 0.1463415
mean(M1$ZDVFO) # 0.09756098

#Запишем для каждого федерального округа соответствующее уравнение регрессии
mean(coefficients(reg4.1)[4:10])

# ZCFO:   17.24628 + 8.736e-05*Income - 1.314e-06*Population + e
# ZUFO:   21.22323 + 8.736e-05*Income - 1.314e-06*Population + e
# ZSZFO:  21.03489 + 8.736e-05*Income - 1.314e-06*Population + e
# ZUrFO:  19.91181 + 8.736e-05*Income - 1.314e-06*Population + e
# ZPFO:   17.63019 + 8.736e-05*Income - 1.314e-06*Population + e
# ZSKFO:  22.90860 + 8.736e-05*Income - 1.314e-06*Population + e
# ZSFO:   24.51846 + 8.736e-05*Income - 1.314e-06*Population + e
# ZDVFO:  NA

#Проверьте по критерию Чоу наличие неоднородности в данных.
#сумма квадратов остатков короткой модели (без фиктивных переменных)
Q_s <- sum(residuals(reg3.2)^2) 
k <- 2
q <- 8
n <- length(M[,1])
n
#сумма квадратов остатков длинной модели (с фиктивными переменными)
Q_l <- sum(residuals(reg4.1)^2)
F_Chow <- ((Q_s - Q_l)/q)/(Q_l/(n - k - q - 1))
F_crit <- 2.07 # F_cr(8, 71)
F_Chow
#Модель с фиктивными переменными имеет смысл




plot(x=M$Income, y=M$MilkPrice, main="Зависимость цены от численности населения" ,
     xlab = "Среднедушевые доходы", ylab = "Цена на молоко")
points(x=M$Income[M$FO=="ЦФО"], y=M$MilkPrice[M$FO=="ЦФО"], col=1)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="ЦФО"])+coef(reg4.1)[1]+coef(reg4.1)[4], b=coef(reg4.1)[2], col=1)
points(x=M$Income[M$FO=="ЮФО"], y=M$MilkPrice[M$FO=="ЮФО"], col=2)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="ЮФО"])+coef(reg4.1)[1]+coef(reg4.1)[5], b=coef(reg4.1)[2], col=2)
points(x=M$Income[M$FO=="СЗФО"], y=M$MilkPrice[M$FO=="СЗФО"], col=3)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="СЗФО"])+coef(reg4.1)[1]+coef(reg4.1)[6], b=coef(reg4.1)[2], col=3)
points(x=M$Income[M$FO=="УФО"], y=M$MilkPrice[M$FO=="УФО"], col=4)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="УФО"])+coef(reg4.1)[1]+coef(reg4.1)[7]
       , b=coef(reg4.1)[2], col=4)
points(x=M$Income[M$FO=="ПФО"], y=M$MilkPrice[M$FO=="ПФО"], col=5)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="ПФО"])+coef(reg4.1)[1]+coef(reg4.1)[8], b=coef(reg4.1)[2], col=5)
points(x=M$Income[M$FO=="СКФО"], y=M$MilkPrice[M$FO=="СКФО"], col=6)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="СКФО"])+coef(reg4.1)[1]+coef(reg4.1)[9], b=coef(reg4.1)[2], col=6)
points(x=M$Income[M$FO=="СФО"], y=M$MilkPrice[M$FO=="СФО"], col=7)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="СФО"])+coef(reg4.1)[1]+coef(reg4.1)[10], b=coef(reg4.1)[2], col=7)
points(x=M$Income[M$FO=="ДВФО"], y=M$MilkPrice[M$FO=="ДВФО"], col=8)
abline(a=coef(reg4.1)[3]*mean(M$Population[M$FO=="ДВФО"])+coef(reg4.1)[1],
       b=coef(reg4.1)[2], col=8)
legend(10000,83,legend = c("ЦФО","ЮФО","СЗФО","УФО","ПФО","СКФО","СФО","ДВФО"),
       col = c(1,2,3,4,5,6,7,8), lwd=1, pch = c(1, 1, 1,1,1,1,1,1))



#Ввoд в регрессию фиктивную переменную для
#Дальневосточного федерального округа вместо фиктивной переменной для
#центрального федерального округа
reg4.2<-lm(M1$MilkPrice ~ M1$Income + M1$Population+ M1$ZUFO + M1$ZSZFO + M1$ZUrFO + M1$ZPFO + M1$ZSKFO + M1$ZSFO + M1$ZDVFO)
summary(reg4.2)
accuracy(reg4.2)


#Ввoд в регрессию фиктивную переменную для
#Дальневосточного федерального округа вместо фиктивной переменной для южного
#федерального округа.
reg4.3<-lm(M1$MilkPrice ~ M1$Income + M1$Population + M1$ZCFO + M1$ZSZFO + M1$ZUrFO + M1$ZPFO + M1$ZSKFO + M1$ZSFO + M1$ZDVFO)
summary(reg4.3)
accuracy(reg4.3)


#Постройте множественную регрессию для Цен на молоко, Среднедушевых
#доходов, Численности населения и Федерального округа. При этом, фактор
#Федеральный округ влияет на коэффициент при факторе Среднедушевые доходы
reg4.4<-lm(M$MilkPrice ~ M$Income * M$FO + M$Population)
summary(reg4.4)
accuracy(reg4.4)


#Постройте множественную регрессию для Цен на молоко, Среднедушевых
#доходов, Численности населения и Федерального округа. При этом, фактор
#Федеральный округ влияет на коэффициент при факторе Численность населения
reg4.5<-lm(M$MilkPrice ~ M$Income + M$Population * M$FO)
summary(reg4.5)
accuracy(reg4.5)


#Выберите наилучшую модель для описания изменений Цен на молоко
accuracy(reg4.1)[5]
accuracy(reg4.2)[5]
accuracy(reg4.3)[5]
accuracy(reg4.4)[5]
accuracy(reg4.5)[5]