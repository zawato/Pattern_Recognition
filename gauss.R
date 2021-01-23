# mvtnorm パッケージの読み込み
library(mvtnorm)

# パラメータ設定
mu <- c(0, 0)
Sig <- matrix(c(0.7, 0.8, 0.8, 1.5), ncol=2)

# ランダムな点を1000個生成
x <- rmvnorm(1000, mu, Sig)
plot(x)

# ヒストグラムの描画
hist(x[,1])
mean_x = mean(x[,1])
var_x = var(x[,1])
print(mean_x)
print(var_x)

hist(x[,2])
mean_y = mean(x[,2])
var_y = var(x[,2])
print(mean_y)
print(var_y)