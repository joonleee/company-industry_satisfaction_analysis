# library(tibble)
# library(dplyr)
# library(readr)
# library(corrgram)
# library(corrplot)

#  repeat for all companies
xx_cor <- read.csv("xx.csv")

sd(xx_cor$star)
sd(xx_cor$promo)
sd(xx_cor$welSal)
sd(xx_cor$wlb)
sd(xx_cor$culture)
sd(xx_cor$board)
sd(xx_cor$calcStar)

cor_star_promo <-
  cor(xx_cor$star, xx_cor$promo, method="pearson") # 0.4838787
cor_star_welSal <- 
  cor(xx_cor$star, xx_cor$welSal, method="pearson") # 0.440073
cor_star_wlb <- 
  cor(xx_cor$star, xx_cor$wlb, method="pearson") # 0.5298443
cor_star_culture <- 
  cor(xx_cor$star, xx_cor$culture, method="pearson") # 0.5595679
cor_star_board <-
  cor(xx_cor$star, xx_cor$board, method="pearson") # 0.566098
cor_star_calcStar <- 
  cor(xx_cor$star, xx_cor$calcStar, method="pearson") # 0.6986216

names(xx_cor)
xx_cor_ratings <- xx_cor[-c(1:6,13:17)] #필요없는 칼럼 제거
str(xx_cor_ratings)

cor_all_samsung <- cor(xx_cor_ratings, method="pearson")

corrgram(xx_cor_ratings, method="pearson")
corrgram(xx_cor_ratings, lower.panel=panel.conf) #수치 함께 표시

jpeg("xx_corr.jpg") #JPEG 저장
corrgram(xx_cor_ratings, lower.panel=panel.conf)
dev.off()


######################################
 # add cor_all[n] of all companies

cor_star_promo <- (cor_all_xx[2] + cor_all_yy[2] + ...)


cor_star_welSal <- (cor_all_xx[3] + cor_all_yy[3] + ...)

cor_star_wlb <- (cor_all_xx[4] + cor_all_yy[4]...)

cor_star_culture <- (cor_all_xx[5] + cor_all_yy[5] + ...)

cor_star_board <- (cor_all_xx[6] + cor_all_yy[6] + ...)

cor_star_calcStar <-(cor_all_xx[7] + cor_all_yy[7] + ...)


cor_star_promo_avg <- (cor_star_promo/10)
cor_star_welSal_avg <- (cor_star_welSal/10)
cor_star_wlb_avg <- (cor_star_wlb/10)
cor_star_culture_avg <- (cor_star_culture/10)
cor_star_board_avg <- (cor_star_board/10)
cor_star_calcStar_avg <- (cor_star_calcStar/10)


cor_all_avg <- cbind(cor_star_promo_avg,
      cor_star_welSal_avg,
      cor_star_wlb_avg,
      cor_star_culture_avg,
      cor_star_board_avg,
      cor_star_calcStar_avg)

cor_all_avg_1 <- rbind(cor_star_promo_avg,
                     cor_star_welSal_avg,
                     cor_star_wlb_avg,
                     cor_star_culture_avg,
                     cor_star_board_avg,
                     cor_star_calcStar_avg)