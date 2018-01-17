library(gpuR)
library(ggplot2)
library(reshape2)
library(dplyr)
source("script/PF_DDR.R",encoding = "utf-8")
source("script/PS_DDR.R",encoding = "utf-8")
source("script/subfunctions.R",encoding = "utf-8")


X_0 <- -2.5
beta <- 0.75
rho <- 0.08
q_qnorm <- qnorm(0.02)
X_0 <- -2.5
dT <- 100
N <- 1000
#有効なGPUがあるか確認する
detectGPUs()
#Particle用の変数準備
filter_X <- vclMatrix(rep(0,dT*N), nrow=dT, ncol=N)
filter_weight <- vclMatrix(rep(0,dT*N), nrow=dT, ncol=N)
smoother_weight <- vclMatrix(rep(0,dT*N), nrow=dT, ncol=N)
#Q_weight <- vclMatrix(rep(0,dt*N), dim=c(dt, N, N))

#パラメータの一個前の値
beta_est_pre <- 0
rho_est_pre <- 0
q_qnorm_pre <- 0
#フィルタリングと平滑化、予測値の結果 記録用
filter_X_mean <- vclVector(rep(0,dT))
smoother_X_mean <- vclVector(rep(0,dT))
predict_Y_mean <- rep(0, 100)
#Answer
X <- rep(0,100)
DR <- rep(0,100)
#サンプルパスの発生
X[1] <- sqrt(beta)*X_0 + sqrt(1 - beta) * rnorm(1)
for (dt in 2:dT)  {
  X[dt] = sqrt(beta)*X[dt - 1] + sqrt(1 - beta) * rnorm(1);
  DR[dt] = r_DDR(X[dt - 1], q_qnorm, rho, beta);
}
DR[1] <- DR[2]*(rnorm(1)*0.05+1)
#確認
ggplot(data.frame(dt = seq(1, dT), X, DR = pnorm(DR)) %>% melt("dt")) +
  geom_line(aes(x = dt, y = value, colour = variable)) +
  facet_grid(variable~.,scales = "free") +
  theme_bw()


particle_filter(N, dT, beta, q_qnorm, rho, X_0, filter_X, filter_weight, filter_X_mean)

particle_smoother(N, dT, beta, filter_X, filter_weight, filter_X_mean, smoother_weight, smoother_X_mean)

ggplot(data.frame(dt = seq(1, dT), X, f_X = filter_X_mean[], s_X = smoother_X_mean[]) %>% melt("dt")) +
  geom_line(aes(x = dt, y = value, colour = variable)) +
  theme_bw()



