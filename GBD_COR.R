setwd('E:/BC/GBD') ##���ù���·��
library(dplyr)
EC <- read.csv('EC_nation.csv',header = T)  ## ��ȡ���ǵ�����

####  1990 ASIR_EAPC
##��ȡASIR
ASIR_1990 <- subset(EC,EC$year==1990 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence') ## ��ȡ1990��EC����У���󷢲���
ASIR_1990 <- ASIR_1990[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(ASIR_1990)[2] <- 'ASR'
###��ȡ���Է�����
Incidence_case_1990 <- subset(EC,EC$year==1990 & 
                               
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_1990 <- Incidence_case_1990[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Incidence_case_1990)[2] <- 'case'
#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(2,7,8)]

country <- ASIR_1990$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###�ϲ��������ݳ�һ�����ݼ�
Total <- merge(Incidence_case_1990,EAPC_cal, by='location')
Total <- merge(Total,ASIR_1990, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'  ###ָʾ������ʾ������Ϊ����������






####  1990 ASDR_EAPC
##��ȡASDR
ASDR_1990 <- subset(EC,EC$year==1990 & 
                      EC$age=='Age-standardized' & 
                      EC$metric== 'Rate' &
                      EC$measure=='Deaths') ## ��ȡ1990��EC����У���󷢲���
ASDR_1990 <- ASDR_1990[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(ASDR_1990)[2] <- 'ASR'
###��ȡ���Է�����
Deaths_case_1990 <- subset(EC,EC$year==1990 & 
                                
                                EC$metric== 'Number' &
                                EC$measure=='Deaths')
Deaths_case_1990 <- Deaths_case_1990[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Deaths_case_1990)[2] <- 'case'
#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(2,7,8)]

country <- ASDR_1990$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###�ϲ��������ݳ�һ�����ݼ�
Total <- merge(Deaths_case_1990,EAPC_cal, by='location')
Total <- merge(Total,ASDR_1990, by='location')
Total_Deaths <- Total
Total_Deaths$group <- 'ASDR'  ###ָʾ������ʾ������Ϊ����������

## �ϲ������ʼ����������ݼ�
Total <- rbind(Total_incidence,Total_Deaths)
###����ͼ��
library(ggplot2)
Total$group <- factor(Total$group, 
                      levels=c('ASIR','ASDR'), 
                      ordered=TRUE)
p1 <- ggplot(Total,aes(ASR, EAPC, size = case))+  
  geom_point(color='#0fb9b1')+
  geom_smooth(data = Total,aes(ASR, EAPC),se = .8,colour='red',span=1) +
  scale_size(name = 'Cases in 1990', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) + facet_grid(.~group,scales="free") +
  theme_light()
p1

### ����pearson���ϵ������ӦPֵ
cor.test(Total_incidence$EAPC,Total_incidence$ASR,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$ASR,method="pearson")








####  2019 ASIR_EAPC
##��ȡASIR
ASIR_2019 <- subset(EC,EC$year==2019 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence') ## ��ȡ2019��EC����У���󷢲���
ASIR_2019 <- ASIR_2019[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(ASIR_2019)[2] <- 'ASR'
###��ȡ���Է�����
Incidence_case_2019 <- subset(EC,EC$year==2019 & 
                               
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_2019 <- Incidence_case_2019[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Incidence_case_2019)[2] <- 'case'
#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(2,7,8)]

country <- ASIR_2019$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###�ϲ��������ݳ�һ�����ݼ�
Total <- merge(Incidence_case_2019,EAPC_cal, by='location')
Total <- merge(Total,ASIR_2019, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'  ###ָʾ������ʾ������Ϊ����������






####  2019 ASDR_EAPC
##��ȡASDR
ASDR_2019 <- subset(EC,EC$year==2019 & 
                      EC$age=='Age-standardized' & 
                      EC$metric== 'Rate' &
                      EC$measure=='Deaths') ## ��ȡ2019��EC����У���󷢲���
ASDR_2019 <- ASDR_2019[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(ASDR_2019)[2] <- 'ASR'
###��ȡ���Է�����
Deaths_case_2019 <- subset(EC,EC$year==2019 & 
                                
                                EC$metric== 'Number' &
                                EC$measure=='Deaths')
Deaths_case_2019 <- Deaths_case_2019[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Deaths_case_2019)[2] <- 'case'
#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(2,7,8)]

country <- ASDR_2019$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###�ϲ��������ݳ�һ�����ݼ�
Total <- merge(Deaths_case_2019,EAPC_cal, by='location')
Total <- merge(Total,ASDR_2019, by='location')
Total_Deaths <- Total
Total_Deaths$group <- 'ASDR'  ###ָʾ������ʾ������Ϊ����������

## �ϲ������ʼ����������ݼ�
Total <- rbind(Total_incidence,Total_Deaths)
###����ͼ��
library(ggplot2)
Total$group <- factor(Total$group, 
                      levels=c('ASIR','ASDR'), 
                      ordered=TRUE)
p1 <- ggplot(Total,aes(ASR, EAPC, size = case))+  
  geom_point(color='#A3CB38')+
  geom_smooth(data = Total,aes(ASR, EAPC),se = .8,colour='red',span=1) +
  scale_size(name = 'Cases in 2019', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) + facet_grid(.~group,scales="free") +
  theme_light()
p1

### ����pearson���ϵ������ӦPֵ
cor.test(Total_incidence$EAPC,Total_incidence$ASR,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$ASR,method="pearson")


####  1990 HDI_EAPC
####  1990 ASIR_HDI
### ��ȡ HDI����
HDI <- read.csv('HDI 1990.csv',header = T)
names(HDI) <- c('location','HDI')

###��ȡ���Է�����
Incidence_case_1990 <- subset(EC,EC$year==1990 & 
                               
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_1990 <- Incidence_case_1990[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Incidence_case_1990)[2] <- 'case'

#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(2,7,8)]

country <- Incidence_case_1990$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### �ϲ���������
Total <- merge(Incidence_case_1990,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'

###��ȡ���Է�����
Deaths_case_1990 <- subset(EC,EC$year==1990 & 
                               
                                EC$metric== 'Number' &
                                EC$measure=='Deaths')
Deaths_case_1990 <- Deaths_case_1990[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Deaths_case_1990)[2] <- 'case'

#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(2,7,8)]

country <- Deaths_case_1990$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### �ϲ���������
Total <- merge(Deaths_case_1990,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_Deaths <- Total
Total_Deaths$group <- 'ASDR'

###�ϲ��������Լ�����������
Total <- rbind(Total_incidence,Total_Deaths)
### ��ͼ
library(ggplot2)
Total$group <- factor(Total$group, 
                      levels=c('ASIR','ASDR'), 
                      ordered=TRUE)
p1 <- ggplot(Total,aes(HDI, EAPC, size = case))+  
  geom_point(color='#FDA7DF')+
  geom_smooth(data = Total,aes(HDI, EAPC),se = .8,colour='red',span=1) +
  scale_size(name = 'Cases in 1990', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) + facet_grid(.~group,scales="free") +
  theme_light()
p1
### ����pearson�����ϵ��
cor.test(Total_incidence$EAPC,Total_incidence$HDI,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$HDI,method="pearson")




####  2019 HDI_EAPC
####  2019 ASIR_HDI
### ��ȡ HDI����
HDI <- read.csv('HDI.csv',header = T)
names(HDI) <- c('location','HDI')

###��ȡ���Է�����
Incidence_case_2019 <- subset(EC,EC$year==2019 & 
                               
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_2019 <- Incidence_case_2019[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Incidence_case_2019)[2] <- 'case'

#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(2,7,8)]

country <- Incidence_case_2019$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### �ϲ���������
Total <- merge(Incidence_case_2019,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'

###��ȡ���Է�����
Deaths_case_2019 <- subset(EC,EC$year==2019 & 
                               
                                EC$metric== 'Number' &
                                EC$measure=='Deaths')
Deaths_case_2019 <- Deaths_case_2019[,c(2,8)]  ###ֻȡ��Ҫ�ı���
names(Deaths_case_2019)[2] <- 'case'

#### ����EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(2,7,8)]

country <- Deaths_case_2019$location  ###��ȡ��������
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### �ϲ���������
Total <- merge(Deaths_case_2019,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_Deaths <- Total
Total_Deaths$group <- 'ASDR'

###�ϲ��������Լ�����������
Total <- rbind(Total_incidence,Total_Deaths)
### ��ͼ
library(ggplot2)
Total$group <- factor(Total$group, 
                      levels=c('ASIR','ASDR'), 
                      ordered=TRUE)
p1 <- ggplot(Total,aes(HDI, EAPC, size = case))+  
  geom_point(color='#9980FA')+
  geom_smooth(data = Total,aes(HDI, EAPC),se = .8,colour='red',span=1) +
  scale_size(name = 'Cases in 2019', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) + facet_grid(.~group,scales="free") +
  theme_light()
p1
### ����pearson�����ϵ��
cor.test(Total_incidence$EAPC,Total_incidence$HDI,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$HDI,method="pearson")