
beta_est <- beta
q_qnorm_est  <- q_qnorm
rho_est <- rho
X_0_est <- X_0

particle_filter <- function(N, dT, beta_est, q_qnorm_est, rho_est, X_0_est, state_X_mean, predict_Y_mean){
  #重み
  sum_weight = 0;
  resample_check_weight = 0;
  
  #時点1でのフィルタリング開始
  
  #初期分布から　時点0と考えて
  pred_X <- gpuVector(sqrt(beta_est)*X_0_est - sqrt(1 - beta_est) * rnorm(N))
  #重みの計算
  weight <- g_DR_dinamic(DR[1], pred_X, q_qnorm, beta, rho)
  sum_weight <- colSums(weight)
  weight <- weight / sum_weight
  #これは並列計算できないからしょうがない
  cs_weight <-  cumsum(weight[,])
  
  
  
}
