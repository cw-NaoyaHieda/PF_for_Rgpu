
particle_filter <- function(N, dT, beta_est, q_qnorm_est, rho_est, X_0_est, filter_X, filter_weigth, state_X_mean){
  
  #時点がdT-1で終わることに注意(観測値に対して、使用するXが一期前であるため)
  for(dt in seq(1, dT - 1)){
    
    if(dt == 1){
      #初期分布から　時点0と考えて
      pred_X <- gpuVector(sqrt(beta_est)*X_0_est - sqrt(1 - beta_est) * rnorm(N))
      #重みの計算
      weight <- g_DR_dinamic(DR[2], pred_X, q_qnorm, beta, rho)
    }else{
      pred_X <- sqrt(beta_est)*prior_X - gpuVector(sqrt(1 - beta_est) * rnorm(N))
      weight <- g_DR_dinamic(DR[dt + 1], pred_X, q_qnorm, beta, rho) * prior_weight
    }
    
    
    #weightの正規化 一部の計算は並列計算できないからしょうがない
    cs_weight <- gpuVector(cumsum(weight[,]))
    weight <- weight / cs_weight[N]
    cs_weight <- cs_weight / cs_weight[N]
    resample_check_weight = weight^2
    resample_check_weight <- sum(resample_check_weight[,])
    
    #リサンプリング (resample)とり会えず並列にしない
    if (1 / resample_check_weight < N / 10){
      weight <- weight[]
      weight <- gpuVector(weight[resample(cs_weight[],runif(N))])
      }
    
    filter_X[dt,] <- pred_X[,]
    prior_X <- pred_X * 1
    filter_weigth[dt,] <- weight[,]
    prior_weight <- weight * 1
    state_X_mean[dt] <- pred_X %*% weight
    }
}


