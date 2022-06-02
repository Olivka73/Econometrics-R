setwd("D:/Olivkis/")
library("forecast") 
T<-read.table("reg.txt", header=TRUE)
T<-T[order(T$A1),] # сортировать строки в таблице Т по возрастанию А1

# для А1 и А2
regA2A1<-lm(formula=T$A2 ~T$A1)

format(coef(regA2A1), digits = 12) #вывод коэффициентов регрессии 
summary(regA2A1) #вывод информации о регрессии

mean(regA2A1$residuals) #мат ожидание остатков
	plot(regA2A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA2A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA2A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA2A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A2) # диаграмма рассеяния
	abline(regA2A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А3
regA3A1<-lm(formula=T$A3 ~T$A1)
format(coef(regA3A1), digits = 12) #вывод коэффициентов регрессии 

summary(regA3A1) #вывод информации о регрессии

mean(regA3A1$residuals) #мат ожидание остатков
	plot(regA3A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA3A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA3A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA3A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A3) # диаграмма рассеяния
	abline(regA3A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А4
regA4A1<-lm(formula=T$A4 ~T$A1)
format(coef(regA4A1), digits = 12) #вывод коэффициентов регрессии 

summary(regA4A1) #вывод информации о регрессии

mean(regA4A1$residuals) #мат ожидание остатков
	plot(regA4A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA4A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA4A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA4A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A4) # диаграмма рассеяния
	abline(regA4A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А5
regA5A1<-lm(formula=T$A5 ~T$A1)
format(coef(regA5A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA5A1) #вывод информации о регрессии

mean(regA5A1$residuals) #мат ожидание остатков
	plot(regA5A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA5A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA5A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA5A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A5) # диаграмма рассеяния
	abline(regA5A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А6
regA6A1<-lm(formula=T$A6 ~T$A1)
format(coef(regA6A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA6A1) #вывод информации о регрессии

mean(regA6A1$residuals) #мат ожидание остатков
	plot(regA6A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

abline(h=240, col="red")
T3<-T[-which(T$A6>240),c(1,6)]
regA6A1<-lm(formula=T3$A6 ~T3$A1)

accuracy(regA6A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA6A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA6A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A6) # диаграмма рассеяния
	abline(regA6A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А7
regA7A1<-lm(formula=T$A7 ~T$A1)
format(coef(regA7A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA7A1) #вывод информации о регрессии

mean(regA7A1$residuals) #мат ожидание остатков
	plot(regA7A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA7A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA7A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA7A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A7) # диаграмма рассеяния
	abline(regA7A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А8
regA8A1<-lm(formula=T$A8 ~T$A1)
format(coef(regA8A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA8A1) #вывод информации о регрессии

mean(regA8A1$residuals) #мат ожидание остатков
	plot(regA8A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA8A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 1350 в переменную A1
format(predict(regA8A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA8A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A8) # диаграмма рассеяния
	abline(regA8A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А9
regA9A1<-lm(formula=T$A9 ~T$A1)
format(coef(regA9A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA9A1) #вывод информации о регрессии

mean(regA9A1$residuals) #мат ожидание остатков
	plot(regA9A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA9A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA9A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA9A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A9) # диаграмма рассеяния
	abline(regA9A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А10
regA10A1<-lm(formula=T$A10 ~T$A1)
format(coef(regA10A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA10A1) #вывод информации о регрессии

mean(regA10A1$residuals) #мат ожидание остатков
	plot(regA10A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA10A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA10A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA10A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A10) # диаграмма рассеяния
	abline(regA10A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз


# для А1 и А11
regA11A1<-lm(formula=T$A11 ~T$A1)
format(coef(regA11A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA11A1) #вывод информации о регрессии

mean(regA11A1$residuals) #мат ожидание остатков
	plot(regA11A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA11A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA11A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA11A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A11) # диаграмма рассеяния
	abline(regA11A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз



# для А1 и А12
regA12A1<-lm(formula=T$A12 ~T$A1)
format(coef(regA12A1), digits = 5) #вывод коэффициентов регрессии 

summary(regA12A1) #вывод информации о регрессии

mean(regA12A1$residuals) #мат ожидание остатков
	plot(regA12A1$residuals, main="График остатков", xlab="Номер наблюдения",
	ylab="Остатки") #график остатков
	abline(h=0)

accuracy(regA12A1) # вывод оценок качества регрессии, MAPE

T<-T[1,] # оставить только одну строку в таблице Т
T$A1<- 1350 # записать значение 85 в переменную F1
format(predict(regA12A1, newdata=T, interval="confidence", level=0.9), digits=10) #вывод прогнозов

P<-predict(regA12A1, newdata=T, interval="confidence", level=0.9) # записать прогноз в P
T<-read.table("reg.txt", header=TRUE) # снова прочитать данные из файла
T<-T[order(T$A1),]
	plot(x=T$A1, y=T$A12) # диаграмма рассеяния
	abline(regA12A1) # линия регрессии
points(x=1350, y=P[1], col="red") # точечный прогноз
lines(x=c(1350,1350,1350), y=P, col="red") # интервальный прогноз

