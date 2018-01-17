
  
  
tmp <- rowSums(vclMatrix(matrix(filter_weight[dt,], ncol = N, nrow = N)) *
  g_DR_dinamic_potencial_cross(vclMatrix(matrix(filter_X[dt + 1,],nrow=N)),
                               vclMatrix(matrix(filter_X[dt,],nrow = 1)),
                               beta_est))


#Q_weightを計算

Q_weight_calc <- function(beta_est) {
  #pragma omp parallel for
  for (t in seq(dT - 3, 1){
    for (n2 in seq(0, N) {
      bunbo = 0
      #for (n in seq(0, N) {
      #分母計算
      #bunbo = filter_weight[t][n] * dnorm(filter_X[t + 1][n2], sqrt(beta_est) * filter_X[t][n], sqrt(1 - beta_est));
      bunbo <- rowSums(vclMatrix(matrix(filter_weight[dt,], ncol = N, nrow = N)) *
                     g_DR_dinamic_potencial_cross(vclMatrix(matrix(filter_X[dt + 1,],nrow=N)),
                                                  vclMatrix(matrix(filter_X[dt,],nrow = 1)),
                                                  beta_est))
      #}
      bunsi <- 
      for (n in seq(0, N) {
        #分子計算しつつ代入
        Q_weight[t + 1][n][n2] = filter_weight[t][n] * smoother_weight[t + 1][n2] *
          dnorm(filter_X[t + 1][n2], sqrt(beta_est) * filter_X[t][n], sqrt(1 - beta_est)) / bunbo;
      }
    }
    
  }
  
}

