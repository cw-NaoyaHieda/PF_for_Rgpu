
beta_est <- beta
q_qnorm_est  <- q_qnorm
rho_est <- rho
X_0_est <- X_0

particle_filter <- function(N, dT, beta_est, q_qnorm_est, rho_est, X_0_est, state_X_mean, predict_Y_mean){
  
  #初期分布から　時点0と考えて
  pred_X <- gpuVector(sqrt(beta_est)*X_0_est - sqrt(1 - beta_est) * rnorm(N))
  
  #重みの計算
  weight <- g_DR_dinamic(DR[1], pred_X, q_qnorm, beta, rho)
  
  #weightの正規化 一部の計算は並列計算できないからしょうがない
  cs_weight <- gpuVector(cumsum(weight[,]))
  weight <- weight / cs_weight[N]
  cs_weight <- cs_weight / cs_weight[N]
  resample_check_weight = weight^2
  resample_check_weight <- sum(resample_check_weight[,])
  
  S
  
  
}
