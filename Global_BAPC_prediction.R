setwd('E:/BC/GBD') ##���ù���·��

#��װnordpred��; �Ȱ�װdevtools
#install.packages("devtools")
#library(devtools)
#devtools::install_github("haraldwf/nordpred")

#��װINLA��
#install.packages("INLA",repos = "https://inla.r-inla-download.org/R/stable/bin/windows/contrib/4.1",dependencies = TRUE)
#install.packages("INLA",repos = "http://www.math.ntnu.no/inla/R/testing",dependencies = TRUE)

#��װBAPC��
#install.packages("caTools")
#install.packages("fanplot")
#install.packages("Epi")
#install.packages("BAPC",repos="http://R-Forge.R-project.org")



setwd('E:/BC/GBD') ##���ù���·��
library(BAPC)
library(INLA)
library(nordpred)
library(reshape)
library(data.table)
library(tidyr)
library(tidyverse)
library(epitools)
library(ggplot2)
library(reshape2)
### ���ݶ�ȡa
EC <-  read.csv('BC_predict.csv')
age_stand <- read.csv('GBD2019 world population age standard.csv')
#### ������������Ҫ������ֲ�
ages <- c("1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29","30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59","60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89","90 to 94", "95 plus")

#### ��Ⱥ������Ҫ������ֲ�
ages_2 <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29","30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59","60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89","90 to 94", "95 plus")

###### ����ģ�͵�����ṹ
age_3 <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29","30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59","60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89","90 to 94", "95 plus")
#��ȡ��׼���˿�����
age_stand <- subset(age_stand,age %in% ages_2)
wstand <- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(), 
            age_stand$std_population[3:21] %>% as.numeric())/sum(age_stand$std_population[1:21])

### for incidence for Male and female
EC_Male_incidence <- subset(EC,age %in% ages & 
                              sex == 'Male' &
                              metric == 'Number' &
                              measure == 'Incidence' &
                              location== 'China')[,c(3,4,7,8)]
EC_Male_incidence_n <- reshape2::dcast(data = EC_Male_incidence, year~age, value.var = "val")
rownames(EC_Male_incidence_n) <- EC_Male_incidence_n$year
EC_Male_incidence_n <- EC_Male_incidence_n[,-1]
##### ����0-19�������
#EC_Male_incidence_n[,'0 to 4'] <- 0
#EC_Male_incidence_n[,'5 to 9'] <- 0
#EC_Male_incidence_n[,'10 to 14'] <- 0
#EC_Male_incidence_n[,'15 to 19'] <- 0
EC_Male_incidence_n <- EC_Male_incidence_n[,c(10,1:20)]  ####����Ӵ�С����
EC_Male_incidence_n <- EC_Male_incidence_n[,c(2,1:21)]
EC_Male_incidence_n <- EC_Male_incidence_n[,-3]
EC_Male_incidence_n <- EC_Male_incidence_n[,-11]

EC_Male_incidence_n <- apply(EC_Male_incidence_n, c(1,2), as.integer) %>% as.data.frame()
EC_Male_incidence_n <- apply(EC_Male_incidence_n, c(1,2), round) %>% as.data.frame()


EC_Female_incidence <- subset(EC,age %in% ages & 
                                sex == 'Female' &
                                metric == 'Number' &
                                measure == 'Incidence' &
                                location == 'China')[,c(3,4,7,8)]
EC_Female_incidence_n <- reshape2::dcast(data = EC_Female_incidence, year~age, value.var = "val")
rownames(EC_Female_incidence_n) <- EC_Female_incidence_n$year
EC_Female_incidence_n <- EC_Female_incidence_n[,-1]
##### ����0-19�������
####����Ӵ�С����
EC_Female_incidence_n <- EC_Female_incidence_n[,c(10,1:20)]
EC_Female_incidence_n <- EC_Female_incidence_n[,c(2,1:21)]
EC_Female_incidence_n <- EC_Female_incidence_n[,-3]
EC_Female_incidence_n <- EC_Female_incidence_n[,-11]


EC_Female_incidence_n <- apply(EC_Female_incidence_n, c(1,2), as.integer) %>% as.data.frame()
EC_Female_incidence_n <- apply(EC_Female_incidence_n, c(1,2), round) %>% as.data.frame()

##### ��ȡ1990-2019�˿�ѧ����
dirname <- dir("/Users/Robin/Downloads/BAPC/GBD_Population")  ###��ȡĿ¼�ļ��е��ļ�
file <- paste0("/Users/Robin/Downloads/BAPC/GBD_Population/",dirname)  ### ���ļ�����·��
var_name <- c("location_name","sex_name","year_id","age_group_name","val")  ##������Ҫ�ı���

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name))) 
names(GBD_population)=var_name
for (a in file) {
  data <- fread(a) %>% select(all_of(var_name)) %>% 
    filter(age_group_name %in% ages_2)
  GBD_population <- rbind(GBD_population,data)
}  ###ѭ��Ŀ���ǽ�1990-2019��Ĳ�ͬ��������Ⱥ����ȡ����
GBD_population$sex_name[GBD_population$sex_name=='both'] <- 'Both'
GBD_population$sex_name[GBD_population$sex_name=='male'] <- 'Male'
GBD_population$sex_name[GBD_population$sex_name=='female'] <- 'Female'
GBD_population <- GBD_population[!duplicated(GBD_population),]

#### ��ȺԤ����������
prediction_var_name <- c("location_name","sex","year_id","age_group_name","val")
GBD_population_prediction <- fread('IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv') %>% 
  select(all_of(prediction_var_name)) %>% 
  filter(age_group_name %in% ages_2 & year_id %in% 2020:2030)
names(GBD_population_prediction) <- var_name
##### �ϲ���������
GBD <- rbind(GBD_population,GBD_population_prediction)
# #### �ϲ�0-19�����Ⱥ����
# GBD_age4 <- GBD %>% subset(age_group_name %in% c("<1 year","1 to 4")) %>% 
#   group_by(location_name,sex_name,year_id) %>% 
#   summarize(val=sum(val))
# GBD_age4$age_group_name <- '0 to 4'
# GBD_age4 <- GBD_age4[,c(1:3,5,4)]
# GBD <- subset(GBD, age_group_name %in% age_3[-1])
# GBD <- rbind(GBD,GBD_age4)
# GBD <- GBD %>% mutate(age_group_name=fct_relevel(age_group_name,age_3)) %>% 
#   arrange(age_group_name)
# names(GBD)

#####################  ���˿����ݵ�������Ҫ�ĸ�ʽ
GBD_Global_Male <- subset(GBD,location_name=='China' & sex_name == 'Male')
GBD_Global_Female <- subset(GBD,location_name=='China' & sex_name == 'Female')

GBD_Global_Male_n <- reshape2::dcast(data = GBD_Global_Male, year_id ~ age_group_name,value.var = c("val"))
GBD_Global_Female_n <- reshape2::dcast(data = GBD_Global_Female, year_id ~ age_group_name,value.var = c("val"))

rownames(GBD_Global_Male_n) <- GBD_Global_Male_n$year_id
rownames(GBD_Global_Female_n) <- GBD_Global_Female_n$year_id

GBD_Global_Male_n <- GBD_Global_Male_n[,-1]
GBD_Global_Female_n <- GBD_Global_Female_n[,-1]
GBD_Global_Male_n <- apply(GBD_Global_Male_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_Female_n <- apply(GBD_Global_Female_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_Male_n <- apply(GBD_Global_Male_n, c(1,2), round) %>% as.data.frame()
GBD_Global_Female_n <- apply(GBD_Global_Female_n, c(1,2), round) %>% as.data.frame()

GBD_Global_Both_n <- GBD_Global_Female_n + GBD_Global_Male_n
#

#����û�з��������ݵ����
EC_pro <- matrix(data = NA, nrow = 2030-2019, ncol = ncol(GBD_Global_Male_n)) %>% as.data.frame() 
rownames(EC_pro) <- seq(2020,2030,1)
colnames(EC_pro) <-  names(EC_Male_incidence_n)
EC_pro <- EC_pro[,-21]

EC_Male_incidence_n  <- rbind(EC_Male_incidence_n , EC_pro)
EC_Female_incidence_n  <- rbind(EC_Female_incidence_n , EC_pro)

####### ģ��Ԥ��
GBD_Global_Male_n <- GBD_Global_Male_n[,-21]
Male_esoph <- APCList(EC_Male_incidence_n, GBD_Global_Male_n, gf = 5)
Male_bapc_result <- BAPC(Male_esoph, predict = list(npredict = 10, retro = T),secondDiff = FALSE, stdweight = wstand, verbose = F) 

GBD_Global_Female_n <- GBD_Global_Female_n[,-21]
Female_esoph <- APCList(EC_Female_incidence_n, GBD_Global_Female_n, gf = 5)
Female_bapc_result <- BAPC(Female_esoph, predict = list(npredict = 10, retro = T),secondDiff = FALSE, stdweight = wstand, verbose = F) 

##### ��ͬ����㷢������
Male_proj <- agespec.proj(x = Male_bapc_result) %>% as.data.frame()   ### ����Ԥ����Ⱥ
Male_proj_mean <- Male_proj[,colnames(Male_proj) %like% 'mean']
colnames(Male_proj_mean) <- age_3

Female_proj <- agespec.proj(x = Female_bapc_result) %>% as.data.frame()   ### ����Ԥ����Ⱥ
Female_proj_mean <- Female_proj[,colnames(Female_proj) %like% 'mean']
colnames(Female_proj_mean) <- age_3

Both_proj_mean <- Female_proj_mean+ Male_proj_mean

##### ��ͬ����㷢����
Male_rate <- agespec.rate(x = Male_bapc_result) %>% as.data.frame()
Male_rate_mean <- Male_rate[,colnames(Male_rate) %like% 'mean']*100000
colnames(Male_rate_mean) <- age_3

Female_rate <- agespec.rate(x = Female_bapc_result) %>% as.data.frame()
Female_rate_mean <- Female_rate[,colnames(Female_rate) %like% 'mean']*100000
colnames(Female_rate_mean) <- age_3

GBD_Global_Both_n <- GBD_Global_Both_n[,-21]
Both_rate_mean <- Both_proj_mean/GBD_Global_Both_n*100000


######  ��������У����ķ�����
Male_ASR <- agestd.rate(x = Male_bapc_result) %>% as.data.frame()
Male_ASR$mean <- Male_ASR$mean*100000
Male_ASR$year <- rownames(Male_ASR)

Female_ASR <- agestd.rate(x =Female_bapc_result) %>% as.data.frame()
Female_ASR$mean <- Female_ASR$mean*100000
Female_ASR$year <- rownames(Female_ASR)

year_index <- 1990:2030
Both_ASR <- matrix(nrow = 0, ncol = 2) %>% as.data.frame() 
names(Both_ASR) <- c("crude.rate","adj.rate")
#i=1
for (i in 1:(2030-1989)) {
  asr = ageadjust.direct(count = Both_proj_mean[i,], pop = GBD_Global_Both_n[i,], 
                         stdpop = wstand)
  Both_ASR[i,1:2] <- round(asr[1:2]*10^5,2)
}
Both_ASR$year <- 1990:2030


#####�ܷ�������
Male_sum_year <- apply(Male_proj_mean, 1, sum) %>% as.data.frame()
colnames(Male_sum_year) <- 'number'
Male_sum_year$year <- rownames(Male_sum_year)


Female_sum_year <- apply(Female_proj_mean, 1, sum) %>% as.data.frame()
colnames(Female_sum_year) <- 'number'
Female_sum_year$year <- rownames(Female_sum_year)

Male_sum_year <- apply(Male_sum_year, 2, as.numeric) %>% as.data.frame()
Female_sum_year <- apply(Female_sum_year, 2, as.numeric) %>% as.data.frame()
Both_proj_mean <- Female_proj_mean + Male_proj_mean
Both_sum_year <- Male_sum_year + Female_sum_year
Both_sum_year$year <- 1990:2030





plotBAPC(Male_bapc_result, scale=10^5, type = 'ageStdProj', showdata = TRUE)


plotBAPC(Female_bapc_result, scale=10^5, type = 'ageStdRate', showdata = TRUE)
