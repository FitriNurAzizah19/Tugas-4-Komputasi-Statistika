# 1. Jika rata-rata pelanggan datang ke toko adalah 3 orang per jam
# modelkan dengan Poisson dan hitung P(X >= 5)
lambda <- 3  # rata-rata pelanggan per jam
# P(X >= 5)
Peluang_Poisson <- 1 - ppois(4, lambda)
Peluang_Poisson

# PMF Distribusi Poisson
x <- 0:12
pmf <- dpois(x, lambda)
data.frame(k = x, P = pmf)

# Plot PMF Poisson
plot(x, pmf,
     type = "h",
     lwd = 3,
     main = "Poisson (λ = 3)",
     xlab = "k (Jumlah pelanggan)",
     ylab = "P(X = k)")

# 2. Dari 100 bola (20 berwarna merah), diambil 10 tanpa pengembalian. 
# Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat.
# Distribusi Hypergeometric: 
N <- 100   # Jumlah seluruh bola
K <- 20    # Jumlah bola merah
n <- 10    # Jumlah bola yang diambil

# Domain k (Kemungkinan Jumlah bola merah yang terambil)
k <- seq(from = max(0, n + K - N), to = min(n, K))

# PMF: P(X = k)
pmf <- dhyper(k,
              m = K,
              n = N - K,
              k = n)
data.frame(k = k, P = pmf)

# Plot PMF Hypergeometric
plot(k, pmf,
     type = "h",
     lwd = 3,
     main = "Hypergeometric (N=100, K=20, n=10)",
     xlab = "k (Jumlah bola merah dalam sampel)",
     ylab = "P(X = k)")

# Simulasi sampling tanpa pengembalian
m <- 1000
samp <- rhyper(m,
               m = K,
               n = N - K,
               k = n)
mean(samp)

# Nilai rata-rata teoritis jumlah bola merah ysng terambil
n * K / N

# 3. Simulasikan 1.000 percobaan Binomial (n = 15, p = 0.4) 
# dan bandingkan histogram hasil simulasi dengan PMF teoretis.
# Simulasi Distribusi Binomia:
n <- 15      # jumlah percobaan
p <- 0.4     # peluang sukses
m <- 1000    # jumlah simulasi

# Simulasi 1.000 percobaan Binomial
set.seed(123)
samp <- rbinom(m,size = n,prob = p)
head(samp)

# Histogram hasil simulasi
hist(samp,
     breaks = seq(-0.5, n + 0.5, by = 1),
     probability = TRUE,
     main = "Histogram Simulasi Binomial (n=15, p=0.4)",
     xlab = "Jumlah sukses (X)",
     ylab = "Probabilitas")

# PMF Teoretis Binomial
x <- 0:n
pmf <- dbinom(x,size = n,prob = p)
data.frame(k = x, P = pmf)

# Plot PMF Teoretis
plot(x, pmf,
     type = "h",
     lwd = 3,
     main = "PMF Teoretis Binomial (n=15, p=0.4)",
     xlab = "k (jumlah sukses)",
     ylab = "P(X = k)")

# Perbandingan Histogram Simulasi dengan PMF Teoretis
hist(samp,
     breaks = seq(-0.5, n + 0.5, by = 1),
     probability = TRUE,
     main = "Simulasi vs PMF Teoretis Binomial",
     xlab = "Jumlah sukses (X)",
     ylab = "Probabilitas")
points(x, pmf,pch = 19,col = "blue")
lines(x, pmf, type = "h",lwd = 3,col = "red")