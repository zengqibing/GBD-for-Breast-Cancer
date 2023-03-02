setwd('E:/BC/GBD') ##���ù���·��
library(dplyr)                            ## ��ȡ��Ҫ��R��
library(ggplot2)
EC <- read.csv('BC_region.csv',header = T)  ## ��ȡ���ǵ�����
order <- read.csv('order.csv',header = F)
EC$location <- factor(EC$location, 
                             levels=order$V1, 
                             ordered=TRUE)



## 1990��������
EC_1990 <- subset(EC,EC$year==1990 & 
                    
                    EC$metric== 'Number' &
                    EC$measure=='Incidence')

EC_1990 <- EC_1990[,c(2,8,9,10)]  ### ֻȡ��Ҫ�ı����������Լ���Ӧ����ֵ
EC_1990$val <- round(EC_1990$val,2)  ###ȡ��
EC_1990$lower <- round(EC_1990$lower,2)###ȡ��
EC_1990$upper <- round(EC_1990$upper,2) ###ȡ��
EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## ��-����95%UI������ֵ
EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UIǰ�������             
EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UIǰ�������
EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##���ݺ�95%UI�ÿո������

## 2019��������
EC_2019 <- subset(EC,EC$year==2019 & 
                    
                    EC$metric== 'Number' &
                    EC$measure=='Incidence')

EC_2019 <- EC_2019[,c(2,8,9,10)]  ### ֻȡ��Ҫ�ı����������Լ���Ӧ����ֵ
EC_2019$val <- round(EC_2019$val,2)  ###ȡ��
EC_2019$lower <- round(EC_2019$lower,2)###ȡ��
EC_2019$upper <- round(EC_2019$upper,2) ###ȡ��
EC_2019$Num_2019 <- paste(EC_2019$lower,EC_2019$upper,sep = '-') ## ��-����95%UI������ֵ
EC_2019$Num_2019 <- paste(EC_2019$Num_2019,')',sep = '')  ##95%UIǰ�������             
EC_2019$Num_2019 <- paste('(',EC_2019$Num_2019,sep = '')  ##95%UIǰ�������
EC_2019$Num_2019 <- paste(EC_2019$val,EC_2019$Num_2019,sep = ' ') ##���ݺ�95%UI�ÿո������


## 1990 ASR
ASR_1990 <- subset(EC,EC$year==1990 & 
                    EC$age=='Age-standardized' & 
                    EC$metric== 'Rate' &
                    EC$measure=='Incidence')

ASR_1990 <- ASR_1990[,c(2,8,9,10)]  ### ֻȡ��Ҫ�ı����������Լ���Ӧ����ֵ
ASR_1990$val <- round(ASR_1990$val,2)  ###ȡ��
ASR_1990$lower <- round(ASR_1990$lower,2)###ȡ��
ASR_1990$upper <- round(ASR_1990$upper,2) ###ȡ��
ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## ��-����95%UI������ֵ
ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UIǰ�������             
ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UIǰ�������
ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##���ݺ�95%UI�ÿո������


## 2019 ASR
ASR_2019 <- subset(EC,EC$year==2019 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence')

ASR_2019 <- ASR_2019[,c(2,8,9,10)]  ### ֻȡ��Ҫ�ı����������Լ���Ӧ����ֵ
ASR_2019$val <- round(ASR_2019$val,2)  ###ȡ��
ASR_2019$lower <- round(ASR_2019$lower,2)###ȡ��
ASR_2019$upper <- round(ASR_2019$upper,2) ###ȡ��
ASR_2019$ASR_2019 <- paste(ASR_2019$lower,ASR_2019$upper,sep = '-') ## ��-����95%UI������ֵ
ASR_2019$ASR_2019 <- paste(ASR_2019$ASR_2019,')',sep = '')  ##95%UIǰ�������             
ASR_2019$ASR_2019 <- paste('(',ASR_2019$ASR_2019,sep = '')  ##95%UIǰ�������
ASR_2019$ASR_2019 <- paste(ASR_2019$val,ASR_2019$ASR_2019,sep = ' ') ##���ݺ�95%UI�ÿո������

EC_incidence <- subset(EC, EC$age_name=='Age-standardized' & 
                         EC$metric_name== 'Rate' &
                         EC$measure_name=='Incidence')

##### EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')
EAPC <- EAPC[,c(2,7,8)] ##��ȡ����������Լ���Ӧ����ֵ



country <- EC_1990$location
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=22),UCI=rep(0,times=22),LCI=rep(0,times=22)) 
for (i in 1:22){  ###�ܹ�22������������ѭ��22��
  country_cal <- as.character(EAPC_cal[i,1]) ### ����ȡ��Ӧ�ĵ���
  a <- subset(EAPC, EAPC$location==country_cal)  ##ȡ��Ӧ�����������Ӽ�
  a$y <- log(a$val)  ##����EAPC���㷽������yֵ
  mod_simp_reg<-lm(y~year,data=a) ##����EAPC���㷽�������Իع鷽��
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##����EAPC���㷽��ȡ����betaֵ������EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### ����EAPC��95%�������������ֵ
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### ����EAPC��95%�������������ֵ
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}

EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##����2λС����
EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-') 
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '') 
EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '') 
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')  

### ��������
EC_1990 <- EC_1990[,c(1,5)]  ###ȡ���������Ϻõı���
ASR_1990 <- ASR_1990[,c(1,5)]
EC_2019 <- EC_2019[,c(1,5)]
ASR_2019 <- ASR_2019[,c(1,5)]
EAPC_cal <- EAPC_cal[,c(1,5)]
Incidence <- merge(EC_1990,ASR_1990,by='location')
Incidence <- merge(Incidence,EC_2019,by='location')
Incidence <- merge(Incidence,ASR_2019,by='location')
Incidence <- merge(Incidence,EAPC_cal,by='location')
write.csv(Incidence,'Results for incidence222.csv')