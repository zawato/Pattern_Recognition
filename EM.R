rm(list=ls())

library(mvtnorm)

# データの読み込み
dat <- matrix(scan("faithful_scaled.txt"), ncol = 2, byrow = TRUE)

# パラメータ設定
N <- 272
pi_1 <- 0.5
mu_1 <- c(-1.5, 1)
Sig_1 <- matrix(c(0.7, 0, 0, 0.7), nr = 2)
pi_2 <- 0.5
mu_2 <- c(1.5, -1)
Sig_2 <- matrix(c(0.7, 0, 0, 0.7), nr = 2)

# マハラノビス距離rの円
mahalanobis.ellipse <- function(Sig, Mu, n, r = 1) {
  th <- (0:n) * (2 * pi / n)
  x <- rbind(r * cos(th), r * sin(th))
  L <- t(chol(Sig))
  return(t(L %*% x + Mu))
}

# EM algorithm
epoch = 50 # 更新回数
for (i in 1:epoch) {
  # Eステップ
  gamma_1 <- pi_1*dmvnorm(dat[1:N,1:2], mu_1, Sig_1)/(pi_1*dmvnorm(dat[1:N,1:2], mu_1, Sig_1)+pi_2*dmvnorm(dat[1:N,1:2], mu_2, Sig_2))
  gamma_2 <- pi_2*dmvnorm(dat[1:N,1:2], mu_2, Sig_2)/(pi_1*dmvnorm(dat[1:N,1:2], mu_1, Sig_1)+pi_2*dmvnorm(dat[1:N,1:2], mu_2, Sig_2))

  # Mステップ
  pi_1 <- sum(gamma_1)/N
  pi_2 <- sum(gamma_2)/N
  mu_1 <- colSums(gamma_1*dat[1:N,1:2]/sum(gamma_1))
  mu_2 <- colSums(gamma_2*dat[1:N,1:2]/sum(gamma_2))
  Sig_1 <- matrix(c(0, 0, 0, 0), nr = 2)
  Sig_2 <- matrix(c(0, 0, 0, 0), nr = 2)
  for (i in 1:N){
    Sig_1 <- Sig_1 + gamma_1[i]*(dat[i,1:2]-mu_1)%*%t(dat[i,1:2]-mu_1)/sum(gamma_1)
    Sig_2 <- Sig_2 + gamma_2[i]*(dat[i,1:2]-mu_2)%*%t(dat[i,1:2]-mu_2)/sum(gamma_2)
  }
}

# 描画
plot(dat[1:N,1], dat[1:N,2], col="green", pch=20, xlim=c(-2,2), ylim=c(-2,2), xlab = "", ylab = "")
par(new=T)
plot(mu_1[1], mu_1[2], col="red", pch=18, xlim=c(-2,2), ylim=c(-2,2), xlab = "", ylab = "")
p <- mahalanobis.ellipse(Sig_1, mu_1, 50, 1)
par(new=T)
plot(p[,1], p[,2], type = "l", col = "red", xlim=c(-2,2), ylim=c(-2,2), xlab = "", ylab = "")
par(new=T)
plot(mu_2[1], mu_2[2], col="blue", pch=18, xlim=c(-2,2), ylim=c(-2,2), xlab = "", ylab = "")
p <- mahalanobis.ellipse(Sig_2, mu_2, 50, 1)
par(new=T)    
plot(p[,1], p[,2], type = "l", col = "blue", xlim=c(-2,2), ylim=c(-2,2), xlab = "", ylab = "")
