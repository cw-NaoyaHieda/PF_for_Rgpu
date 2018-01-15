particle_filter <- function(beta_est, q_qnorm_est, rho_est, X_0_est, state_X_mean, predict_Y_mean){
  #時点tの予測値格納
  pred_X <- gpuVector(rep(0, 1000))
  weight <- gpuVector(rep(0, 1000))
  #途中の処理用変数
  cs_weight <- gpuVector(rep(0, 1000))
  resample_numbers <- gpuVector(rep(0, 1000))
  #重み
  sum_weight = 0;
  resample_check_weight = 0;
  
  
  #時点1でのフィルタリング開始
  
  #初期分布から　時点0と考える
  pred_X= sqrt(beta_est)*X_0_est - sqrt(1 - beta_est) * rnorm(1000);
  #重みの計算
  weight = g_DR_dinamic(DR[1], pred_X, q_qnorm_est, beta_est, rho_est);
}