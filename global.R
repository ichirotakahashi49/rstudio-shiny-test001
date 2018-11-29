library(shiny)
library(DT)


##############################
#
#関数定義lof_eval
#
# 引数：
#  input_data データセット
#  learn_data_start 学習データの開始行の設定
#  learn_data_end 学習データの終了行の設定
#  target_start 評価対象データの開始行の設定
#  target_end 評価対象データの終了行の設定
#
# 出力：
#  model_summary 線形モデル式のサマリー
#  model_eval 線形モデル式の学習データに対する精度
#  target 評価対象データに対する評価結果
#  learn 学習データに対する線形モデル式の値
#
##############################


lof_eval <- function(input_data,learn_data_start,learn_data_end,target_start,target_end){
  
  ##############################
  #
  #③ 学習データ
  #
  ##############################
  
  #トレンド除去後のデータ生成(学習期間＋評価期間)
  input_data_trend <- as.data.frame(cbind(input_data$sales,0:(nrow(input_data)-1)))
  colnames(input_data_trend) <- c("sales","time")
  Trend_Pred <- predict.lm(lmTrendModel,newdata=input_data_trend)
  
  Trend_Pred_res <- input_data_trend$sales - Trend_Pred
  
  #学習データの設定
  learn_data01 <- ts(Trend_Pred_res)
  learn_data02 <- cbind(learn_data01,lag(learn_data01 ,-3),lag(learn_data01 ,-12),0:(nrow(input_data)+11))
  if (learn_data_start < 13){
    learn_data03 <- cbind(input_data$sales[13:learn_data_end],learn_data02[13:learn_data_end,-1])
  } else {
    learn_data03 <- cbind(input_data$sales[learn_data_start:learn_data_end],learn_data02[13:learn_data_end,-1])
  }
  
  learn_data <- as.data.frame(learn_data03)
  colnames(learn_data ) <- c("y","lag3","lag12","time")
  rownames(learn_data)<- c((learn_data_end-nrow(learn_data)+1):learn_data_end)
  
  #評価対象データの実現値の設定
  target_data <- cbind(input_data$sales[target_start:target_end],learn_data02[target_start:target_end,-1])
  target_data <- as.data.frame(target_data)
  colnames(target_data) <- c("y","lag3","lag12","time")
  rownames(target_data) <- c(target_start:target_end)
  
  ##############################
  #
  #④回帰モデル
  #
  ##############################
  
  lmModel <- lm(y~.,data=learn_data)
  
  ##############################
  #
  #⑤残差
  #
  ##############################
  
  #線形回帰モデルの残差（学習期間）
  lmModel_res <- residuals(lmModel)
  
  #線形回帰モデルの予測値（学習期間）
  lmModel_fitted <- learn_data$y-lmModel_res
  
  #重決定 R2
  cor(lmModel_fitted,learn_data$y)^2
  
  #平均絶対パーセント誤差
  APE <- abs(lmModel_res)/learn_data[,1]
  MAPE <- mean(APE[!is.infinite(APE)])
  
  ##############################
  #
  #⑥残差の平均値と標準偏差を求める
  #
  ##############################
  
  #残差（学習期間）の平均値
  lmModel_res_mean <- mean(lmModel_res)
  
  #残差（学習期間）の標準偏差
  lmModel_res_sd <- sd(lmModel_res)
  
  ##############################
  #
  #⑦ピアソン残差と外れ値スコアを求める
  #
  ##############################
  
  #評価対象データの予測値
  lmModel_yosoku <- predict.lm(lmModel,newdata=target_data)
  
  #評価対象データの残差
  lmModel_gap <- target_data[,1]-lmModel_yosoku
  
  #評価対象データのピアソン残差
  lmModel_gap_std <- (lmModel_gap-lmModel_res_mean)/lmModel_res_sd
  
  #外れ値スコア計算
  LOF <- -log(dnorm(lmModel_gap,lmModel_res_mean,lmModel_res_sd))
  
  ##############################
  #
  #出力
  #
  ##############################
  
  output_data1 <- c(cor(lmModel_fitted,learn_data$y),
                    cor(lmModel_fitted,learn_data$y)^2,
                    MAPE,
                    lmModel_res_mean,
                    lmModel_res_sd)
  output_name1 <- c("R",
                    "R2",
                    "MAPE",
                    "Mean", 
                    "SD")
  names(output_data1) <- output_name1 
  
  output_data2 <- cbind(target_data[,1],
                        lmModel_yosoku,
                        lmModel_gap,
                        lmModel_gap_std,
                        LOF)
  output_colname2 <- c("Measured value",
                       "Predicted value",
                       "Gap(Residuals)",
                       "Pearson Residuals",
                       "LOF")
  colnames(output_data2) <- output_colname2 
  
  output_data3 <- cbind(learn_data$y,
                        lmModel_fitted,
                        lmModel_res)
  output_colname3 <- c("Measured value",
                       "Fitted values",
                       "Residuals")
  colnames(output_data3) <- output_colname3
  
  #関数の戻り値の集約
  result <- list(summary(lmModel),output_data1,output_data2,output_data3)
  names(result) <- c("model_summary","model_eval","target","learn")
  
  #関数の戻り値
  return(result)
  
}