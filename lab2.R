setwd("D:/Olivkis/")
T<-read.table("reg.txt", header=TRUE)

#зависимость А1 и А2
plot(x=T$A1, y=T$A2) #зависимость отсутствует
cor(T$A1, T$A2)
cor.test(T$A1, T$A2)


#зависимость А1 и А3
plot(x=T$A1, y=T$A3) #зависимость линейна
cor(T$A1, T$A3) #корреляция до
cor.test(T$A1, T$A3)

abline(h=750, col="red")
T3<-T[-which(T$A3>750),c(1,3)]

plot(x=T3$A1, y=T3$A3)
cor(T3$A1, T3$A3) #корреляция после
cor.test(T3$A1, T3$A3)


#зависимость А1 и А4
plot(x=T$A1, y=T$A4) #зависимость отсутствует
cor(T$A1, T$A4)
cor.test(T$A1, T$A4)


#зависимость А1 и А5
plot(x=T$A1, y=T$A5) #зависимость отсутствует
cor(T$A1, T$A5)
cor.test(T$A1, T$A5)


#зависимость А1 и А6
plot(x=T$A1, y=T$A6) #зависимость нелинейна
cor(T$A1, T$A6) #корреляция до
cor.test(T$A1, T$A6)

abline(h=400, col="red")
T3<-T[-which(T$A6>400),c(1,6)]

plot(x=T3$A1, y=T3$A6)
cor(T3$A1, T3$A6) #корреляция после
cor.test(T3$A1, T3$A6)


#зависимость А1 и А7
plot(x=T$A1, y=T$A7) #зависимость нелинейна
cor(T$A1, T$A7)
cor.test(T$A1, T$A7)


#зависимость А1 и А8
plot(x=T$A1, y=T$A8) #зависимость нелинейна
cor(T$A1, T$A8)
cor.test(T$A1, T$A8)


#зависимость А1 и А9
plot(x=T$A1, y=T$A9) #зависимость линейна
cor(T$A1, T$A9)
cor.test(T$A1, T$A9)


#зависимость А1 и А10
plot(x=T$A1, y=T$A10) #зависимость линейная обратная
cor(T$A1, T$A10)
cor.test(T$A1, T$A10)


#зависимость А1 и А11
plot(x=T$A1, y=T$A11) #зависимость линейная обратная
cor(T$A1, T$A11)
cor.test(T$A1, T$A11)


#зависимость А1 и А12
plot(x=T$A1, y=T$A12) #зависимость линейная обратная
cor(T$A1, T$A12) #корреляция до
cor.test(T$A1, T$A12)

abline(h=600, col="red")
T3<-T[-which(T$A12<600),c(1,12)]

plot(x=T3$A1, y=T3$A12)
cor(T3$A1, T3$A12) #корреляция после
cor.test(T3$A1, T3$A12)


