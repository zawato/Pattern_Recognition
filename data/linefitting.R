# データの読み込み
data <- matrix(scan("linefitting.txt"), ncol = 2, byrow = TRUE)

# 基底関数と計画行列
basis.poly <- function(x, M) {
  fv <- sapply(0:M, function(n) x^n)
  return(matrix(fv, ncol = M + 1))
}

# 描画
plot.gauss <- function(mu, sigma, range) {
  x <- y <- seq(range[1], range[2], len = 100)
  z <- outer(x, y, function(x, y) dmvnorm(cbind(x, y), mu, sigma))
  image(x, y, z, col = rev(rainbow(100, end = .7)))
}

# パラメータ設定
mu_0 <- cbind(c(0, 0))
S_0 <- cbind(c(0.5,0),c(0,0.5)) 
beta <- 25

phi_1 <- basis.poly(data[1,1], 1)

# 事後分布
S_1 <- 1/(diag(2)+beta*(t(phi_1)%*%phi_1))
m_1 <- beta*(S_1%*%t(phi_1))*data[1,2]

# 描画
plot.gauss(m_1, S_1, c(-1,1))
