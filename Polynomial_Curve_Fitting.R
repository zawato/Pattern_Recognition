# データの読み込み
d <- matrix(scan("curvefitting.txt"), ncol = 2, byrow = TRUE)

# 値の代入
X <- d[,1]
T <- d[,2]

# 描画（ylim:y軸の範囲を指定）
plot(X, T, ylim = c(-1.1, 1.1))

# col:色指定, add:前のグラフに重ねて描画
curve(sin(2 * pi * x), col = "green", add = TRUE)

calc.w <- function(X, T, M) {
  # サイズを指定して, 空のA, bを作成
  A <- matrix(NA, nrow = M + 1, ncol = M + 1)
  b <- rep(NA, M + 1)
  for (i in 0:M) {
    for (j in 0:M)
      A[i + 1, j + 1] <- sum(X^(i + j))
    b[i + 1] <- sum(T * X^i)
  }
  # solve(A):逆行列
  # solve(A, b):A^{-1}b
  return(solve(A, b))
}

y <- function(x, w) {
  M <- length(w) - 1
  s <- 0
  for (i in 0:M)
    s <- s + w[i + 1] * x^i
  return(s)
}
y.vec <- function(X, w) {
  return(sapply(X, function(x) y(x, w)))
}

# # 簡略ver
# y <- function(x, w) {
#   M <- length(w) - 1
#   return(sum(w * sapply(0:M, function(n) x^n)))
# }

# 実行
write("", './result_w.txt', append=FALSE)
for (i in 0:9) {
  # wの値を保存
  write(paste(i, "次の場合", sep=""), './result_w.txt', append=TRUE)
  write((w <- calc.w(X, T, i)), './result_w.txt', append=TRUE)
  
  # 描画
  fig = paste("w",i,".png", sep = "")
  
  png(fig) 
  plot(X, T, ylim = c(-1.1, 1.1))
  x <- seq(0, 1, len = 100)
  # lines() は,その時点でプロットされているグラフに上書きして曲線を描画
  lines(x, y.vec(x, w), col = "red")
  
  dev.off() 
}

