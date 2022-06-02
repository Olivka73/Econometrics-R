# Кластеризация K-средних, 
# при которой каждый кластер представлен центром или 
# средним значением точек данных, принадлежащих кластеру.

#
# 1. Загрузка данных
#
# Этот конкретный набор данных содержит количество 
# арестов за убийства, нападения и изнасилования 
# в каждом из 50 штатов в 1973 году, а также процент жителей штата, 
# проживающих в городских районах.
data("USArrests")
rawdf <- na.omit(USArrests)
head(rawdf)
# [,1] Аресты за убийства (на 100 000 человек)
# [,2] Аресты за нападения (на 100 000 человек)
# [,3] Процент городского населения
# [,4] Аресты за изнасилование (на 100 000)


#
# 2. Масштабирование данных
#
desc_stats <- data.frame(
  Min = apply(rawdf, 2, min), # minimum
  Med = apply(rawdf, 2, median), # median
  Mean = apply(rawdf, 2, mean), # mean
  SD = apply(rawdf, 2, sd), # Standard deviation
  Max = apply(rawdf, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 1)
head(desc_stats)
df <- scale(USArrests)
head(df)


#
#3. Вычисление k-средних
#
set.seed(123)
km.res <- kmeans(scale(USArrests), 4, nstart = 25)
km.res
# среднее значение каждой переменной в кластере
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

#
# 4. Визуализация
#
library("factoextra")
fviz_cluster(km.res, data = df,
             palette = c("#00AFBB","#df842e", "#E7B800", "#FC4E07", "#df2ecd"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)