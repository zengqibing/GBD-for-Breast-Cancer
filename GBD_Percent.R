setwd('E:/BC/GBD') ##���ù���·��
library(ggplot2)
library(ggsci)
LC <- read.csv('BC_Percent.csv',header = T)  ## ��ȡ���ǵ�����
order <- read.csv('order.csv',header = F) 
order$V1 <- rev(order$V1)


case <- subset(LC,LC$year==2019 & 
                 
                 LC$metric== 'Number' &
                 LC$measure=='Deaths') ## ��ȡ2019��EC����У���󷢲���

#### ���������ݼ�
LC_percent <- data.frame(location=rep(unique(LC$location),each=length(unique(LC$cause))),
                         cause=rep(unique(LC$cause),length(unique(LC$location))),
                         percent=rep(NA,times=length(unique(LC$cause))*length(unique(LC$location))))

a <- unique(LC$location)
b <- unique(LC$cause)
for (i in 1:length(unique(LC$location))){
  location_i <- a[i]
  data <- subset(case,case$location==location_i)[,c(2,5,8)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(LC$cause))) {
    cause_j <- b[j]
    LC_percent[which(LC_percent$location==location_i & LC_percent$cause==cause_j),3] <- data$percent[which(data$cause==cause_j)]
  }
}

LC_2019 <- LC_percent
LC_2019$year <- 2019



####  1990
case <- subset(LC,LC$year==1990 & 
                 
                 LC$metric== 'Number' &
                 LC$measure=='Deaths') ## ��ȡ2019��EC����У���󷢲���


LC_percent <- data.frame(location=rep(unique(LC$location),each=length(unique(LC$cause))),
                         cause=rep(unique(LC$cause),length(unique(LC$location))),
                         percent=rep(NA,times=length(unique(LC$cause))*length(unique(LC$location))))

a <- unique(LC$location)
b <- unique(LC$cause)
for (i in 1:length(unique(LC$location))){
  location_i <- a[i]
  data <- subset(case,case$location==location_i)[,c(2,5,8)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(LC$cause))) {
    cause_j <- b[j]
    LC_percent[which(LC_percent$location==location_i & LC_percent$cause==cause_j),3] <- data$percent[which(data$cause==cause_j)]
  }
}

LC_1990 <- LC_percent
LC_1990$year <- 1990

LC_1990_2019 <- rbind(LC_1990,LC_2019)
LC_1990_2019$text <- as.character(round(LC_1990_2019$percent*100,2))  ### ���ӱ�ǩ������������ʾ��ͼ����
#### ����ͼ�е�˳��ָ����������˳��
LC_1990_2019$location <- factor(LC_1990_2019$location, 
                        levels=order$V1, 
                        ordered=TRUE)
LC_1990_2019$cause <- factor(LC_1990_2019$cause, 
                             levels=c("Low physical activity",
                                      "Tobacco",
                                      "Dietary risks",
                                      "Alcohol use",
                                      "Drug use",
                                      "Metabolic risks",
                                      
                                      "Unsafe sex"), 
                             ordered=TRUE)
### ��ͼ
ggplot(data = LC_1990_2019, aes(x = location, y= percent,fill =cause))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') + 
  scale_fill_lancet() + theme_classic()+
  coord_flip() + facet_grid(.~year) + theme_light() +
  geom_text(aes(label=text, y=percent), ### �����ֵҪ����ʵ����ͼ������е���
            position=position_stack(.5), vjust=0.3,
            size = 2.5) 





### ��ͼ
ggplot(data = LC_1990_2019, aes(x = location, y= percent,fill =cause))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') + 
    scale_fill_manual(values = c("#FFC312", "#C4E538", "#12CBC4", "#FDA7DF", "#67e6dc", "#96e6dc", "#3ae374"))+
  theme_void()+
  coord_flip() + facet_grid(.~year) + theme_light() +
  geom_text(aes(label=text, y=percent), ### �����ֵҪ����ʵ����ͼ������е���
            position=position_stack(.5), vjust=0.3,
            size = 2.5) 