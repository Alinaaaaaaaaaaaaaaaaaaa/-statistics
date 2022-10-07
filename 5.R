MO = 3
disp = sqrt(MO)
Q = sqrt(disp)

first = rnorm(500, MO, Q)
second = rnorm(500, MO, Q)

t.test(first, second)

dfalina <- data.frame(iris)
shapiro.test(dfalina$Sepal.Length)
shapiro.test(dfalina$Petal.Length)
shapiro.test(dfalina$Sepal.Width)
shapiro.test(dfalina$Petal.Width)

N = 200
k = 10

norm_k <- rnorm(N, k, sqrt(sqrt(k)))
k_ks <- rchisq(N, k)
ks.test(norm_k, k_ks)
