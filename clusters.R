df1 <- read.csv('winequality-red.csv',sep=';')
df2 <- read.csv('winequality-white.csv',sep=';')
df1<-winequality_red
df2<-winequality_white
df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})
head(df1)
head(df2)
wine <- rbind(df1,df2)
str(wine)
library(ggplot2)

pl <- ggplot(wine,aes(x=wine$`residual sugar`)) + geom_histogram(aes(fill=label),color='black',bins=50)

pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()



pl <- ggplot(wine,aes(x=wine$`residual sugar`)) + geom_histogram(aes(fill=label),color='black',bins=50)

pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()
pl <- ggplot(wine,aes(x=citric.acid)) + geom_histogram(aes(fill=label),color='black',bins=50)

pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()
pl <- ggplot(wine,aes(x=alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50)

pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

#tworzenie scatterplota kwas - cukier

pl <- ggplot(wine,aes(x=wine$`citric acid`,y=wine$`residual sugar`)) + geom_point(aes(color=label),alpha=0.2)

pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

pl <- ggplot(wine,aes(x=wine$`volatile acidity`,y=wine$`residual sugar`)) + geom_point(aes(color=label),alpha=0.2)

pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()


clus.data <- wine[,1:12]

wine.cluster <- kmeans(wine[1:12],2)
x=wine$`residual sugar`
print(wine.cluster$centers)
table(wine$label,wine.cluster$cluster)







