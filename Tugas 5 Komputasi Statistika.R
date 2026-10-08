# 1. Distribusi Eksponensial 
# Rata-rata mu = 5 menit -> lambda = 1/5 = 0.2
# Mencari P(X > 5)
mu_1 <- 5
lambda_1 <- 1 / mu_1

# Hitung P(X > 5)
soal_1 <- pexp(5, rate = lambda_1, lower.tail = FALSE)
soal_1

x_dexp <- seq(1, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.2)
plot(x_dexp, y_dexp, type="l", col="pink", lwd=2,
     main="PDF Distribusi Eksponensial (λ=0.2)",
     xlab="x", ylab="f(x)")

# 2. Distribusi Uniform Kontinu 
# Interval waktu a = 0 menit, b = 20 menit
# Mencari Var(X) = (b - a)^2 / 12
n <- 100
a <- 0
b <- 20

# Hitung Varian
soal_2 <- (b - a)^2 / 12
soal_2
soal_2_1 <- (a+b)/2
soal_2_1
# Generate sampel
x <- runif(n, min = a, max = b)
x
mean(x)
var(x)
# Nilai density / CDF / quantile contoh
d_values <- dunif(c(0, 3.5, 10), min = a, max = b)
d_values
p_values <- punif(c(0, 3.5, 10), min = a, max = b)
p_values
q_values <- qunif(c(0.25, 0.5, 0.75), min = a, max = b)
q_values


# Plot: histogram sampel + overlay PDF teoritis
hist(x, breaks = 30, probability = TRUE,
     main = "Histogram sampel U(2,5) dengan PDF teoritis",
     xlab = "x")

curve(dunif(x, min = a, max = b), from = a, to = b, add = TRUE, lwd = 2)



# 3. Distribusi Eksponensial 
# Rata-rata mu = 10 tahun -> rate (lambda) = 1/10 = 0.1
# Mencari P(X < 5)
mu_3 <- 10
lambda_3 <- 1 / mu_3

# Hitung P(X < 5)
soal_3 <- pexp(5, rate = lambda_3)
soal_3

x_3 <- seq(1, 30, by = 1)
y_3 <- dexp(x_3, rate = 0.1)
plot(x_3, y_3, type="l", col="purple", lwd=2,
     main="PDF Distribusi Eksponensial (λ=0.1)",
     xlab="x", ylab="f(x)")


# 4. Distribusi Normal 
# mu = 250 gram, sigma = 5 gram
# Mencari proporsi P(X < 240)
n4 <- 100
mu_4 <- 250
sigma_4 <- 5

# Hitung P(X < 240)
soal_4 <- pnorm(240, mean = mu_4, sd = sigma_4)
soal_4

x <- rnorm(n4, mean = mu_4, sd = sigma_4)

# Statistik sampel
(x_bar <- mean(x))   
(mle_sigma2 <- mean((x - x_bar)^2))
(sd_sample <- sd(x))

hist(x, breaks = 30, probability = TRUE,
     main = "Histogram sampel N(2, 1.5^2) dengan PDF teoritis",
     xlab = "x")


curve(dnorm(x, mean = mu_4, sd = sigma_4), from = mu_4-4*sigma_4, to = mu_4+4*sigma_4, add = TRUE, lwd = 2)
abline(v = x_bar, col = "brown", lwd = 2)   
abline(v = mu_4, col = "grey", lwd = 2, lty = 2) 


legend("topright", legend = c("PDF teoritis", "mean sampel", "mean true"),
       lty = c(1,1,2), col = c("yellow","blue","red"), bty = "n")