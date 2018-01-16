particle_filter <- function(N, dT, beta_est, q_qnorm_est, rho_est, X_0_est, state_X_mean, predict_Y_mean){
  #時点tの予測値格納
  pred_X <- gpuVector(rep(0, N))
  weight <- gpuVector(rep(0, N))
  #途中の処理用変数
  cs_weight <- gpuVector(rep(0, N))
  resample_numbers <- gpuVector(rep(0, N))
  #重み
  sum_weight = 0;
  resample_check_weight = 0;
  
  
  #時点1でのフィルタリング開始
  
  #初期分布から　時点0と考える
  pred_X <- gpuVector(sqrt(beta_est)*X_0_est - sqrt(1 - beta_est) * rnorm(N))
  #重みの計算
  weight <- gpuVector(g_DR_dinamic(DR[1], pred_X[,], q_qnorm_est, beta_est, rho_est))
  cs_weight <-  gpuVector(cumsum(weight[,]))
  
}