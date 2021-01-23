# データの読み込み
data <- matrix(scan("curvefitting.txt"), ncol = 2, byrow = TRUE)

# パラメータ設定
mu <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
alpha <- 1
beta <- 25

# 基底関数の設定
basis.gauss <- function(x, mu, sig){
  fv <- sapply(x, function(xx) exp(-0.5*(xx-mu)^2/sig^2))
  return(cbind(1, matrix(fv, ncol=length(mu), byrow=TRUE)))
}

# 網掛けグラフの表示
plot.mean.sd <- function(x, mean, sd, ylim = range(c(mean + sd, mean - sd))) {
  plot(NULL, xlim = range(x), ylim = ylim, xlab = "", ylab = "")
  polygon(c(x, rev(x)), c(mean + sd, rev(mean - sd)), col = rgb(1, 0.75, 0.75), border = NA)
  lines(x, mean, type = "l", col = "blue")
}

# 5点まで観測された時の予測分布
x <- data[1:5,1]
t <- data[1:5,2]
phi <- basis.gauss(x, mu, 0.1)

S_N <- solve(alpha*diag(7)+beta*(t(phi)%*%phi))
m_N <- beta*(S_N%*%t(phi)%*%t)

x_plot <- seq(0, 1, len=100)
phi <- basis.gauss(x_plot, mu, 0.1)
y_plot <- t(m_N)%*%t(phi)
plot(x_plot, y_plot, type = "l", ylim = c(-1.0, 1.5))

sd <- 1/beta+phi%*%S_N%*%t(phi)
lines(x_plot, y_plot + diag(sd), lty = 2)
lines(x_plot, y_plot - diag(sd), lty = 2)
plot.mean.sd(x_plot, y_plot, diag(sd), ylim = c(-1.0, 1.5))
par(new=T)
plot(x,t,xlim=c(0,1),ylim = c(-1.0, 1.5))

# 10点まで観測された時の予測分布
x <- data[1:10,1]
t <- data[1:10,2]
phi <- basis.gauss(x, mu, 0.1)

S_N <- solve(diag(7)+beta*(t(phi)%*%phi))
m_N <- beta*(S_N%*%t(phi)%*%t)

phi <- basis.gauss(x_plot, mu, 0.1)
x_plot <- seq(0, 1, len=100)
y_plot <- t(m_N)%*%t(phi)
plot(x_plot, y_plot, type = "l", ylim = c(-1.0, 1.5))

sd <- 1/beta+phi%*%S_N%*%t(phi)
lines(x_plot, y_plot + diag(sd), lty = 2)
lines(x_plot, y_plot - diag(sd), lty = 2)
plot.mean.sd(x_plot, y_plot, diag(sd), ylim = c(-1.0, 1.5))
par(new=T)
plot(x,t,xlim=c(0,1),ylim = c(-1.0, 1.5))