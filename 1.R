gector <- runif(n = 5, min = -100, max = 55)
ser <- runif(n = 5, min = -100, max = 55)
i_sila <- sample(-100:55, size = 5)
i_krasiva <- sample(-100:55, size = 5)
ser_gector <- sample(-100:55, size = 5)

df1 <- data.frame(gector, ser, ser_gector)
df2 <- data.frame(i_sila, i_krasiva)

p_max <- apply(df1, 2, max)
pop_max <- apply(df2, 2, max)

colunm_sum <- function(df) colSums(replace(df, df<0, NA), na.rm = TRUE)
colunm_sum(df1)
colunm_sum(df2)

my_df <- cbind(df1, df2)

negative_values <- function(df)
{for (nm in names (df))
if (max(df[nm]<0)) print(setNames(list(df[df[nm]<0, nm]), nm))}
negative_values(my_df)

for(i in 1:ncol(my_df)) my_df[is.na(my_df[,i]), i]<-mean(my_df[,i],na.rm = T)

al <- seq(from = -1, to = 15, length.out = 50)
yravn <- sapply(al, function(n) n**2 + 3*n -19)
yravn <- lapply(al, function(n) n**2 + 3*n -19)

my_df2 <- mtcars

library(dplyr)
A <- filter(my_df2, row_number() %% 2 == 1)
B <- filter(my_df2, row_number() %% 2 == 0)
data_frame <- rbind(A, B)

my_df2 <- filter(my_df2[,1:4], mpg > mean(mpg) & cyl < mean(cyl))
head(arrange(my_df2, desc(my_df2[1])), 10)

negative_values_plot <- function(df)
{
  np_v <- c("negative"=c(), "positive"=c())
  positive <- list()
  negative <- list()
  for (i in df){
    positive <- append(positive, length(which(i>0)))
    negative <- append(negative, length(which(i<0)))}
  }

plot(c(1:length(my_df)), type="o", xlab = "A", ylab = "C")
lines(c(1:length(my_df)), "positive", col = "black", type="o")
lines(c(1:length(my_df)), "negative", col = "red", type="o")
