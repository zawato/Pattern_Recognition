# 平均既知
mu <- 5.0

data <- c(4.7, 3.3, 5.3, 2.4, 4.9)

# 最尤推定
sigma2_ML <- sum((data-5.0)^2)/(length(data))

# ガンマ分布
curve(dgamma(x, 7.2, 7.2), from=0, to=2, xlab="x", ylab="probability", main="Gamma distribution")
ruiseki <- pgamma(1/0.5, 7.2, 7.2) - pgamma(1/1.5, 7.2, 7.2)

# MAP推定
a_N <- 7.2 + length(data) / 2
b_N <- 7.2 + length(data) / 2 * sigma2_ML

curve(dgamma(x, a_N, b_N), from=0, to=2, xlab="x", ylab="probability", main="Gamma distribution")

likelihood <- function(x) {
  dgamma(x, a_N, b_N)
}

map <- optimize(likelihood, interval=c(0, 2), maximum=TRUE)$maximum
1/map
