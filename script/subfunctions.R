#sampling DDR X_tは一次元ベクトル前提
r_DDR <- function(X_t, q_qnorm, rho, beta) {
  return ((q_qnorm - sqrt(rho)*sqrt(beta)*X_t) / sqrt(1 - rho) - sqrt(rho)*sqrt(1 - beta) / sqrt(1 - rho) * rnorm(length(X_t)))
}

#Dynamicdefaultrateでの密度関数　実質正規分布　DRを正規分布の逆関数で変換する必要があることに注意
g_DR_dinamic <- function(tilde_DR, X_t_1, q_qnorm, beta, rho) {
  return (dnorm(tilde_DR, (q_qnorm - sqrt(rho)*sqrt(beta)*X_t_1) / sqrt(1 - rho), sqrt(rho)*sqrt(1 - beta) / sqrt(1 - rho)))
}
