# 二項分布 dbinom(m, N, mu)
dbinom(50, 100, 1/2)
sum(dbinom(51:100, 100, 1/2))

# 多項分布 dmultinom(c(m1, m2,..., mk), prob = c(mu1, mu2,..., muk))
dmultinom(c(5, 5, 5, 5), prob = c(0.28, 0.24, 0.24, 0.24))
p = 0
for (i in 0:4) {
  for (j in 0:20) {
    for (k in 0:20) {
      if (i + j + k <= 20) {
        p <- p + dmultinom(c(i, j, k, 20-i-j-k)
                           , prob = c(0.28, 0.24, 0.24, 0.24))
      }
      else {break}
    }
  }
}
print(p)
