
#
# Library
#
library(data.table) #大規模データ操作
library(foreach)    # Loop処理
library(dplyr)　    #集約処理

#
# 
#
click.data <- (fread('click_data_sample.csv')) #csv読み込み

#
# ユーザー毎にクリック数を合計し、多い順に並べる。
#

click.user.data <- 
  click.data %>%
  group_by(user.id) %>%
  summarise(click.num=length(click.at)) %>%
  as.data.frame()%>%
  arrange(desc(click.num)) %>%
  mutate(no=1:length(user.id))

head(click.user.data)

#
#
#
library(ggplot2)
ggplot(click.user.data, aes(x=no, y=click.num)) + 
  geom_line() +
  geom_point() +
  xlab("User") +
  ylab("Clicks") +
  theme_bw()

#
# 上位5000ユーザーに絞る
#
library(ggplot2)
ggplot(click.user.data[1:5000,], aes(x=no, y=click.num)) + 
  geom_line() +
  geom_point() +
  xlab("User") +
  ylab("Clicks") +
  theme_bw()

#
# ykmeansで分類
#
library(ykmeans)
click.user.data <- ykmeans(click.user.data, 'click.num', 'click.num', 3)

library(ggplot2)
ggplot(click.user.data[1:5000,], aes(x=no, y=click.num)) + 
  geom_line() +
  geom_point() +
  xlab("User") +
  ylab("Clicks") +
  theme_bw()

table(click.user.data$cluster)

#
# 分類結果をプロット
#
ggplot(click.user.data[1:5000,], 
       aes(x=no, y=click.num,
           col = as.factor(cluster),
           shape = as.factor(cluster)
           )
       ) +
  geom_line() +
  geom_point() +
  xlab("User") + 
  ylab("Clicks") +
  theme_bw() + 
  theme(legend.position="none") #+
  #scale_color_brewer(palett="Paired")

#
# ユーザーをクラスタ2以上に絞ったデータ作成
#
target.click.user <- click.user.data %>% filter(cluster >= 2)


#
# target.click.userに合致したユーザーだけを元データに入れる
#
click.data <- click.data %>% filter(user.id %in% target.click.user$user.id)


#
# キャンペーン毎に集計
#
click.data.campaign <- click.data %>% group_by(user.id, campaign.id) %>% 
  summarise(click.num = length(click.at)) %>%
  as.data.frame()

#
# キャンペーンを列に展開
#
click.data.cast <- as.data.frame(
  dcast.data.table(data=as.data.table(click.data.campaign),
                   formula = user.id~campaign.id,
                   value.var = 'click.num',
                   fun.aggregate = sum)
)

#
# 総クリック数計算
#
click.data.cast$total <- rowSums(click.data.cast[,-1])
head(click.data.cast, 2)