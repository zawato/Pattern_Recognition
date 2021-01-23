# データの読み込み
dat <- matrix(scan("curvefitting.txt"), ncol = 2, byrow = TRUE)

# カーネル関数
kernel <- function(a, b) {
  return(exp(-(a-b)^2/0.08))
}

# 網掛けグラフの表示
plot.mean.sd <- function(x, mean, sd, ylim = range(c(mean + sd, mean - sd))) {
  plot(NULL, xlim = range(x), ylim = ylim, xlab = "", ylab = "")
  polygon(c(x, rev(x)), c(mean + sd, rev(mean - sd)), col = rgb(1, 0.75, 0.75), border = NA)
  lines(x, mean, type = "l", col = "blue")
}

# ハイパーパラメータ
beta = 25
N <- 7
C <- matrix(0, nrow=N, ncol=N)
k <- matrix(0, nrow=N)
m <- matrix(0, nrow=100)
sd <- matrix(0, nrow=100)

x_plot <- seq(0, 1, len=100)

for (h in 1:length(x_plot)) {
  for (i in 1:N) {
    k[i] <- kernel(dat[i, 1], x_plot[h])
    for (j in 1:N) {
      if (i==j) { delta <- 1 }
      else { delta <- 0 }
      C[i,j] = kernel(dat[i, 1], dat[j, 1]) + delta/beta
    }
  }
  c <- kernel(x_plot[h], x_plot[h]) + 1/beta
  m[h] <- t(k) %*% solve(C) %*% dat[1:N, 2]
  sd[h] <- c - t(k) %*% solve(C) %*% k
}

plot(x_plot, m[,1], type = "l", ylim = c(-1.0, 1.5))
lines(x_plot, m[,1] + sqrt(sd[,1]), lty = 2)
lines(x_plot, m[,1] - sqrt(sd[,1]), lty = 2)
plot.mean.sd(x_plot, m[,1], sqrt(sd[,1]), ylim = c(-1.0, 1.5))
par(new=T)
plot(dat[1:7,1],dat[1:7,2],xlim=c(0,1),ylim = c(-1.0, 1.5))