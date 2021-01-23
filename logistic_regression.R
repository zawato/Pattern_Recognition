# データの読み込み
dat <- matrix(scan("classification1.txt"), ncol = 3, byrow = TRUE)

# ロジスティック関数
logi <- function(a) {
  return (1 / (1 + exp(-a)))
}
Phi <- cbind(matrix(1, nrow=100), dat[,1:2])
w <- matrix(1, nrow=3)
w_old <- matrix(0, nrow=3)
y <- matrix(0, nrow=100)
R <- matrix(0, nrow=100, ncol=100)

# 更新回数
epoch = 50
for (i in 1:epoch) {
  w_old <- w
 for (j in 1:100) {
    y[j] <- logi(t(w) %*% Phi[j,])
    R[j,j] <- y[j]*(1-y[j])
  }
  w <- w_old - solve(t(Phi) %*% R %*% Phi) %*% t(Phi) %*% (y-dat[,3])
}

# 描画
dat <- read.table("classification1.txt")
colnames(dat) <- c("x1", "x2", "t")
plot(dat$x1, dat$x2, col = ifelse(dat$t == 1, "red", "blue"))
abline(-w[1] / w[3], -w[2] / w[3], col = "green")