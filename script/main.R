library(Rgpu)

dt = 100
N = 1000
#有効なGPUがあるか確認する
detectGPUs()
#Particle用の変数準備
filter_X <- gpuMatrix(rep(0,dt*N), nrow=dt, ncol=N)
filter_weigth <- gpuMatrix(rep(0,dt*N), nrow=dt, ncol=N)
smoother_weight <- gpuMatrix(rep(0,dt*N), nrow=dt, ncol=N)
Q_weight <- gpuMatrix(rep(0,dt*N), dim=c(dt, N, N))

#計算時間の記録用
calc_time <- rep(0, 100)
#パラメータの一個前の値
beta_est_pre <- 0
rho_est_pre <- 0
q_qnorm_pre <- 0
#フィルタリングと平滑化、予測値の結果 記録用
filter_X_mean <- rep(0, 100)
smmother_X_mean <- rep(0, 100)
predict_Y_mean <- rep(0, 100)

X[0] = sqrt(beta)*X_0 + sqrt(1 - beta) * rnorm(0, 1);
DR[0] = -2;
for (t = 1; t < T; t++) {
  X[t] = sqrt(beta)*X[t - 1] + sqrt(1 - beta) * rnorm(0, 1);
  DR[t] = r_DDR(X[t - 1], q_qnorm, rho, beta);
}

