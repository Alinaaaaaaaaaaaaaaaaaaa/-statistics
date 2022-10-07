#установка и подключение библиотек
install.packages("ggplot2") 
install.packages("ggthemes") 
library(ggplot2)
library(ggthemes)

data(InsectSprays) #встроенный набор данных
dataframe <- InsectSprays #создали датафрейм со встроенным набором данных
#создание графика со значениями датафрейма
ggplot(dataframe, aes(x=spray, y=count, fill=spray, label=rownames(df))) + geom_boxplot(alpha = 0.7, position = position_dodge(1)) + 
  #подписи графика и осей
  labs(
    title = "Insecticide effectiveness",
    x = "Insecticides",
    y = "Number of surviving insects"
  ) + 
  theme_fivethirtyeight(base_size = 12 ,  base_family = "sans") + #базовый шрифт и его размер
  theme(axis.title = element_text())+ #взяли за аргументы все текстовые элементы
  stat_summary(geom="point", size = 3, color="MidnightBlue") #форма, размер и цвет точек

library(ggplot2)
library(ggthemes)

ToothGrowth$dose<-as.factor(ToothGrowth$dose)
ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  scale_fill_brewer(palette= "magma") + 
  geom_violin(trim=FALSE) +
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) +
  theme (legend.position="bottom") +
  stat_summary(fun=mean, geom="point", size=3, color="GreenYellow") +
  stat_summary(fun=var, geom="point", size=3, color="Purple")


znach = rnorm(500, mean=1000)
alina <- hist(znach, col="Tan", xlab="Distribution of quantity", ylab="
Frequency", main="Normal distribution")
text(alina$mids, alina$counts, labels=alina$counts, adj=c(0.5, -0.5))
par(new=TRUE)
plot(density(znach), col="Navy", lwd=5, yaxt="n", xaxt="n", bty="n", xlab="", ylab="", main="")

set.seed(13579)
N <- 500
znach_1 <- rexp(N, rate = 5) 
alina_1 <- hist(znach_1, col="PowderBlue", xlab="Distribution of quantity", ylab="Frequency", main="Exponential distribution")
text(alina_1$mids, alina_1$counts, labels=alina_1$counts, adj=c(0.5, -0.5))
par(new=TRUE)
plot(density(znach_1), col="Violet", lwd=5, yaxt="n", xaxt="n", bty="n", xlab="", ylab="", main="")

library(plotrix)

data <- c(21.6, 21.2, 20.4, 20.2, 18.4, 17.0, 16.7)
label <- c("Abakan", "Petrozavodsk", "Tver", "Naryan-Mar", "Ufa", "Magadan", "Veliky novgorod")

pie3D(data, labels = label, explode = 0.1, main = "Top 7 most drinking cities in Russia", shade = 0.7, col = hcl.colors(length(data), "Spectra"), labelcex = 1.2, labelcol = "blue")