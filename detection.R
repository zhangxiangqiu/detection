library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(forecast)
library(RColorBrewer)
library(e1071)
library(nnet)
library(DMwR)
library(zoo)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(urca)
library(hms)
library(tidyr)
library(reshape2)
library(data.table)
library(RColorBrewer)
library(scales)
library(car)
library(urca)
library(rpart)
library(rpart.plot)

set.seed(1234)

# 数据格式见图1
rawdata <- read.table("meandata.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",", encoding = 'UTF-8') #读入按五分钟平均的原始数据
# 去掉rawdata的第一列（X）、第七列（month）和第八列（time），并将结果赋值给simplif_data，见图2
simplif_data <- rawdata[,c(-1,-7,-8)]
# 将simplif_data的group列乘5
simplif_data$group <- simplif_data$group*5

# 定义一个函数，函数名为timearrange，
# 传入一个矩阵，
# 将矩阵的date列作为time列的日期（“yyyy-mm-dd”格式），
# 矩阵的hour列作为time列的小时，
# 矩阵的group列作为time列的分钟数，
# 组织time列的格式为“yyyy-mm-dd hh:MM:ss”，
# 按time列进行排序
# 最后返回矩阵的“pressure”列和“time”列
timearrange <- function(a){
  #a[,a$pressure>1]<- NULL
  a$time<- ymd(a$date)
  hour(a$time) <- a$hour
  minute(a$time) <- a$group
  a$time <- as.POSIXct(a$time)
  a <- arrange(a, time)
  return(a[,c("pressure","time")])
}

# 筛选出传感器名称为“植物园压力”的数据，并将筛选的结果赋值给subdata
subdata<- simplif_data[simplif_data$name == '植物园压力',]
# 调用上面定义的timearrange函数，并将返回的矩阵赋值给myts， 格式见图3
myts<- timearrange(subdata)
# 筛选出传感器名称为“武警医院压力”的数据，并将筛选的结果赋值给subdata
subdata<- simplif_data[simplif_data$name == '武警医院压力',]
# “timearrange(subdata)”调用上面定义的timearrange函数，并与之前“植物园压力”的数据按time列合并，合并的结果见图4
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '华侨医院压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '中山大道深涌北压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '大观站出水压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '黄村射箭场压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '华南大桥测流点压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '员村大街压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '沙太京溪压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '华南理工压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '黄埔大道深涌DN1000压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '棠下站出水压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
subdata<- simplif_data[simplif_data$name == '岑村小学压力',]
myts<- merge(myts,timearrange(subdata),by = "time")
# 将myts矩阵的列名重命名为"time","zwy","wjyy","hqyy","zsdd","dgz","hc","hndq","ycdj","stjx","hnlg","hpdd","txz","ccxx"，（压力计名称的拼音缩写）
names(myts) <- c("time","zwy","wjyy","hqyy","zsdd","dgz","hc","hndq","ycdj","stjx","hnlg","hpdd","txz","ccxx")
# 将myts矩阵的time列以空格为分隔符，分为“date”和“time”两列，并删除原time列
myts <- separate(myts, col = "time", into = c("date","time"), sep = " ", remove = TRUE)
# 将处理完的数据保存到文件，方便下次直接读取（太浪费时间了）， row.names = FALSE表示不保存行名，默认为TRUE，表示保存行名
write.csv(myts, "myts.csv", row.names = FALSE)
# 使用下面的语句读取数据，下次不用跑这行以上的代码，直接用下面的代码读取数据就行了，又节约了玩一把LOL的时间，嘻嘻
myts <- read.csv("myts.csv")

# myts[,-1]表示去掉第一列的矩阵，
mytimeseries <- ts(myts[,-1], frequency = 12, start = 2018)#转为时间序列格式
autoplot(mytimeseries, facets = TRUE) #将时间序列可视化，并分面显示，图片见“时间序列可视化.png”
# 截取从2018-01-01开始的340天数据，即第1行到第288*340行
mytss <- myts[1:(288*340),]
# 给mytss增加一列，列名为index，值为每天的分钟数，如time列为“01:05:00”表示是一天中的第65分钟，则这行数据的index列为65
mytss$index <- rep(seq(0,287)*5,340)
# 保存至文件
write.csv(mytss, "mytss.csv", row.names = FALSE)

# myts <- read.csv("myts.csv")
# mytss <- read.csv("mytss.csv")

# mytss[mytss$date=="2018-01-04",] 表示筛选出2018-01-04这天的数据,横坐标（x轴）为index列，纵坐标（y轴）为zwy列（植物园压力）
ggplot(mytss[mytss$date=="2018-01-04",],aes(x = index))+
  geom_point(aes(y = zwy), color = brewer.pal(11,"Spectral")[3], shape =15, size = 1) +
  geom_line(aes(y = zwy), color = brewer.pal(11,"Spectral")[1]) +
  theme_test() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(0,1435), breaks=seq(0,1440,180))+
  scale_y_continuous(limits=c(0.1,0.19), breaks=seq(0.04,0.18,0.02))+
  labs(title = "", x = "Time(min)", y = "Pressure(MPa)")

# 将上面的画图代码抽象成函数，可以画一天内的压力值变化曲线，传入date参数（如“2018-03-01”）和col参数（如“zwy”）
plot_pressure_inday <- function(date, col) {
  filtered_mytss <- mytss[mytss$date==date,]
  max <- max(filtered_mytss[, col], na.rm = TRUE)
  min <- min(filtered_mytss[, col], na.rm = TRUE)
  max <- ceiling(max * 100) / 100
  min <- floor(min * 100) / 100
  ggplot(filtered_mytss, aes(x = index))+
    theme_test() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(aes_string(y = col), color = brewer.pal(11,"Spectral")[3], shape =15, size = 1) +
    geom_line(aes_string(y = col), color = brewer.pal(11,"Spectral")[1]) +
    scale_x_continuous(limits=c(0,1460), breaks=seq(0,1440,180), labels = c("00:00","03:00","06:00","09:00","12:00","15:00","18:00","21:00","24:00"))+
    scale_y_continuous(limits=c(min, max), breaks=seq(min, max, 0.01))+
    labs(title = "", x = "Time(hour)", y = "Pressure(MPa)")
}
# 调用函数plot_pressure_inday画出压力曲线
plot_pressure_inday("2018-06-01", "hc")


#SVR预测

get_parameter_2<- function(x,y){
  options(scipen=100, digits=4) #取消科学计数，保留4位
  a <- mean((x-y)^2,na.rm = TRUE) #MSE  
  b <- sqrt(mean((x-y)^2,na.rm = TRUE)) #RMSE  
  c <- mean(abs(x-y),na.rm = TRUE) #MAE 
  d <- mean(abs((x-y)/x),na.rm = TRUE) #MAPE 
  e <- mean(abs(x-y)/((abs(x)+abs(y))/2),na.rm = TRUE) #SMAPE
  f <- 1-sum((x-y)^2,na.rm = TRUE)/sum((x-mean(x,na.rm = TRUE))^2,na.rm = TRUE) # R-square
  g <- (sum(x*y,na.rm = TRUE)-(sum(x,na.rm = TRUE)*sum(y))/length(x))/
    sqrt((sum(x^2,na.rm = TRUE)-((sum(x,na.rm = TRUE))^2)/length(x))*(sum(y^2)-((sum(y))^2)/length(x))) #相关系数R 
  #g <- sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))
  Parameter <- list(MSE = a ,RMSE = b, MAE = c, MAPE = d, SMAPE = e, R_s = f, R = g)
  return(Parameter)
}
singpoint_svr <- function(s,m,n){ #用之前s天数据训练模型，用之前m个数据作为模型输入，第n号监测点
  sel_data <- na.omit(myts[(5472-288*s+1):5472,n+1])#1月19日及之前的数据
  indata <- sel_data
  for (i in 1:m){
    indata <- cbind(indata,sel_data[-c(1:i)])
  }
  indata <- as.data.frame(indata[-(nrow(indata)-m+1:nrow(indata)),])
  train_sub <- sample(nrow(indata),0.8*nrow(indata)) #0.8训练集，0.2测试集
  train_set <- indata[train_sub,]
  test_set <- indata[-train_sub,]
  
  fit_tuned <- tune.svm(train_set[,-(m+1)],train_set[,m+1],gamma = 2^(-2:2), cost = 2^(-3:3))
  pre_results <- predict(fit_tuned$best.model,test_set[,-(m+1)])
  parameters <- get_parameter_2(test_set[,m+1],pre_results)
  return(list(fit_tuned$best.model,parameters))
}

k=288*18+1
s=14
m=1
n=1
# sel_data为第[4,18]天的植物园压力数据
sel_data <- myts[(k-1-288*s+1):(k-1),n+2] 
#indata <- sel_data[(3+1):length(sel_data)] #仅前三个时刻输入（1）
#indata <- sel_data[(288*2+1):length(sel_data)] #没有前一周输入 仅有前一天输入（2）
indata <- sel_data[(288*7+1):length(sel_data)] #有前一周输入（3）
for (i in 1:m){
  #indata <- cbind(indata,sel_data[(288*2-i+1):(length(sel_data)-i)]) #前m个时刻作为输入（2）
  indata <- cbind(indata,sel_data[(288*7-i+1):(length(sel_data)-i)]) #前m个时刻作为输入（3）
  #indata <- cbind(indata,sel_data[(m-i+1):(length(sel_data)-i)]) #前m个时刻作为输入（1）
}
indata <- cbind(indata,sel_data[(288*7+1-288):(length(sel_data)-288)]) #前一天同时刻作为输入（2）
#indata <- cbind(indata,sel_data[(288*2+1-288*2):(length(sel_data)-288*2)]) 
indata <- cbind(indata,sel_data[(288*7+1-288*7):(length(sel_data)-288*7)]) #前一周同一天同时刻作为输入（3）
indata <- na.omit(as.data.frame(indata))
names(indata) <- c("now", "last_time", "last_day", "last_week")
train_sub <- sample(nrow(indata),0.8*nrow(indata)) #0.8训练集，0.2测试集
train_set <- indata[train_sub,]
test_set <- indata[-train_sub,]

fit_tuned <- tune.svm(train_set[,-1],train_set[,1],gamma = 2^(-2:2), cost = 2^(-3:3))
pre_results <- predict(fit_tuned$best.model,test_set[,-1])
parameters <- get_parameter_2(test_set[,1],pre_results)
parameters
fit_tuned
fit_tuned$best.model$residuals

sel_data <- myts[1:(k+287),1:3] 
sel_data[,3] <- na.approx(sel_data[,3], na.rm = FALSE) 
indata <- sel_data[k:(k+287),1:3] 
for (i in 1:m){
  indata <- cbind(indata,sel_data[(k-i):(k+287-i),3]) 
}
indata <- cbind(indata,sel_data[(k-288):(k+287-288),3]) 
indata <- cbind(indata,sel_data[(k-288*7):(k+287-288*7),3]) 
pre_results <- predict(fit_tuned$best.model,indata[,c(-1,-2,-3)])
parameters <- get_parameter_2(indata[,3],pre_results)
parameters <- get_parameter_2(indata[,3],tmpt_results)#ARIMA
parameters
#comparasion_frame_wide <- cbind(indata[,1:3],pre_results)
comparasion_frame_wide <- cbind(indata[,1:3],pre_results,tmpt_results)#与ARIMA对比时
names(comparasion_frame_wide) <- c("date","time_min", "observe","SVR","ARIMA")
#names(comparasion_frame_wide) <- c("date","time_min", "observe","forecast")
comparasion_frame_wide$time_min <- c(0:287) *5
ggplot(comparasion_frame_wide,aes(x = time_min))+
  theme_test() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(y = observe), color = "black", shape =16, size = 1) + 
  geom_line(aes(y = SVR), color = brewer.pal(11,"Set1")[1], linetype = "longdash",size=0.8,alpha=0.85) + 
  geom_line(aes(y = ARIMA), color = brewer.pal(11,"Set1")[2], linetype = "twodash",size=0.8,alpha=0.85) + 
  geom_line(aes(y = observe), color = brewer.pal(11,"Set1")[9], linetype = "solid",size=0.65) + 
  scale_x_continuous(expand = c(0,0), limits=c(0,1460), breaks=seq(0,1440,180), labels = c("00:00","03:00","06:00","09:00","12:00","15:00","18:00","21:00","24:00")) + 
  labs(title = "comparasion Forecast Results", x = "Time", y = "Pressure(MPa)")
  
  
s <- 
m <- 
n <- 1
fit_singpoint_svr <- singpoint_svr(s,m,n)
sel_data <- na.omit(myts[(52129-m):(52416+288*1),(n+1)]) 
indata <- sel_data
for (i in 1:m){
  indata <- cbind(indata,sel_data[-c(1:i)])
}
indata <- as.data.frame(indata[-(nrow(indata)-m+1:nrow(indata)),])
singpoint_results <- predict(fit_singpoint_svr[[1]],indata[,-(m+1)])
singpoint_observe <- indata[,(m+1)] 
parameters <- get_parameter_2(singpoint_observe,singpoint_results)
parameters
time_train <- 1:(length(sel_data)-m)
ggplot(data=data.frame(time_train,singpoint_observe,singpoint_results),aes(x=time_train))+
  geom_point(aes(y=singpoint_observe), pch=17)+geom_line(aes(y=singpoint_observe),col="black")+
  geom_point(aes(y=singpoint_results), pch=20)+geom_line(aes(y=singpoint_results),col="blue") 


#ARIMA

p <- 288*7
lis_train <- (52128-p+1):52128
lis_train <- (5472+108-p+1):(5472+108)
lis_test <- 52129:61056
zwy_1 <- ts(myts[lis_train,"zwy"], start = 0, frequency = 12) 
summary(zwy_1)
zwy_1 <- na.approx(zwy_1, na.rm = FALSE)
plot.ts(zwy_1,ylab="Pressure(MPa)", xaxt = "n")
axis(1, at = c("0","24","48","72","96","120","144","167"),
        labels = c("06.25 00:00","06.26 00:00","06.27 00:00","06.28 00:00","06.29 00:00","06.30 00:00","06.31 00:00","06.31 23:55")) #画图判断直观性
summary(ur.kpss(zwy_1))
plot(decompose(zwy_1)) #估计趋势部分和季节部分
nsdiffs(zwy_1,12,test = "ocsb",max.D = 25)
adf.test(zwy_1) #ADF检验平稳，p>0.01
summary(ur.df(zwy_1)) #ur.df检验平稳
ndiffs(zwy_1,12,test = "pp") #结果为1
dzwy_1 <- diff(zwy_1, diff = 1) 
Acf(dzwy_1) 
Pacf(dzwy_1)
autoplot(dzwy_1) 
adf.test(dzwy_1) 
summary(ur.df(dzwy_1))
dzwy_2 <- diff(dzwy_1, diff = 288)  
Acf(dzwy_2) 
Pacf(dzwy_2)
autoplot(dzwy_2) 
adf.test(dzwy_2) 
summary(ur.df(dzwy_2)) 
Acf(zwy_1) 
Pacf(zwy_1)
fit <- auto.arima(zwy_1,trace = TRUE,lambda = "auto") 
accuracy(fit)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box") 
time_train <- lis_train
obsvalue_train <- as.vector(fit$x)
prevalue_train <- as.vector(fit$fitted)
ggplot(data=data.frame(time_train,obsvalue_train,prevalue_train),aes(x=time_train))+
  geom_point(aes(y=obsvalue_train), pch=17)+geom_line(aes(y=obsvalue_train),col="black")+
  geom_point(aes(y=prevalue_train), pch=20)+geom_line(aes(y=prevalue_train),col="blue") 
plot(forecast(fit,12))
forecast(fit,12)

#一步预测，对7.1数据进行预测并测试模型效果 rollapply实现
p <- 288*7
zwy_3 <- as.zoo(myts[(52128-p+1):(52128+287),"zwy"])
tmpt_zoo <- rollapply(zwy_3,FUN = arima_pre, width =p)
tmpt_frame <- as.data.frame(tmpt_zoo)
tmpt_matrix <- cbind((0:287)*5,
                     sapply(tmpt_frame$mean,subdata),
                     sapply(tmpt_frame$upper,subdata),
                     sapply(tmpt_frame$lower,subdata))
tmpt_frame_2 <- as.data.frame(tmpt_matrix)
names(tmpt_frame_2) <- c("time_min","forecast","upper","lower")
tmpt_results <- tmpt_frame_2$forecast
rownames(tmpt_frame_2) <- NULL
results_frame <- melt(tmpt_frame_2,
                      id = "time_min", variable.name = "class", value.name = "pressure_value")
observation_frame <- as.data.frame(myts[52129:(52129+287),"zwy"])
observation_frame <- cbind(rep("observe",288),c(0:287) *5, observation_frame)
names(observation_frame) <- c("class","time_min","pressure_value")
comparasion_frame <- rbind(results_frame,observation_frame)
comparasion_frame_wide <- dcast(comparasion_frame,time_min~class)
get_parameter_2(comparasion_frame_wide$forecast,comparasion_frame_wide$observe)
ggplot(comparasion_frame_wide,aes(x = time_min))+
  geom_line(aes(y = upper), color = brewer.pal(11,"RdGy")[9], linetype = "twodash") +
  geom_line(aes(y = lower), color = brewer.pal(11,"RdGy")[9], linetype = "twodash") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill= brewer.pal(11,"RdGy")[8], alpha = 0.5) +
  geom_line(aes(y = forecast), color = brewer.pal(11,"Spectral")[3], linetype = "longdash") + 
  geom_line(aes(y = observe), color = brewer.pal(11,"Spectral")[10], linetype = "solid") + 
  geom_point(aes(y = forecast), color = brewer.pal(11,"Spectral")[3], shape =16, size = 1) +
  geom_point(aes(y = observe), color = brewer.pal(11,"Spectral")[10], shape =16, size = 1)+
  scale_x_continuous(expand = c(0,0),
                     limits=c(0,1460), breaks=c(seq(0,1435,180),1435),
                     labels = c("00:00","03:00","06:00","09:00","12:00","15:00","18:00","21:00","23:55"))+
  theme_test() + theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "ARIMA Forecast Results", x = "Time", y = "Pressure(MPa)")


y_Date <- as.Date("2018-07-01") + 1:31 -1
parameter_gather<- NULL
arima_pre <- function(train_data){
  train_data <- na.approx(train_data, na.rm = FALSE)
  train_data <- ts(train_data,frequency = 12)
  fit_pre <- auto.arima(train_data)
  results <- list(mean = forecast(fit_pre,1)$mean, 
                  upper = forecast(fit_pre,1)$upper[1,2], 
                  lower = forecast(fit_pre,1)$lower[1,2]) 
  return(results)
}
rollapply_adpt <- function(data_zoo, i, period){ 
  tmpt_zoo <- rollapply(data_zoo,FUN = arima_pre, width = period, by.column = TRUE)
  tmpt_frame <- as.data.frame(tmpt_zoo)
  tmpt_matrix <- cbind(c(rep((i-1)*5 ,31)),
                       sapply(tmpt_frame$mean,subdata),
                       sapply(tmpt_frame$upper,subdata),
                       sapply(tmpt_frame$lower,subdata))
  tmpt_frame_2 <- as.data.frame(tmpt_matrix)
  tmpt_frame_2 <- cbind(y_Date,tmpt_frame_2)
  names(tmpt_frame_2) <- c("date","time_min","forecast","upper","lower")
  rownames(tmpt_frame_2) <- NULL
  results_frame <- melt(tmpt_frame_2,
                        id = c("date","time_min"), variable.name = "class", value.name = "pressure_value")
  return(results_frame)
}
subdata<- function(x){
  return(x[1])
}
observation_data_frame <- as.data.frame(mytimeseries[52129:61056,"zwy"])
observation_data_frame <- cbind(rep("observe",31),rep(y_Date,each = 288),rep(c(0:287) *5, 31), observation_data_frame)
names(observation_data_frame) <- c("class","date","time_min","pressure_value")

# 计算压力绝对值的u和sigma
meanval <- NULL
sdval <- NULL
timeseq <- unique(mytss$time)
for (i in 1:288){
  zwy_00 <- mytss[mytss$time==timeseq[i],c(1,2,3)]
  zwy_00[is.na(zwy_00$zwy),3] <- 0
  zwy_00[zwy_00$zwy< 0.05|zwy_00$zwy>0.5,3]<- NA
  meanval[i] <- mean(zwy_00$zwy,na.rm = TRUE)
  sdval[i] <- sd(zwy_00$zwy, na.rm = TRUE)
}
meanval <- as.data.frame(meanval)
meanval <- cbind(timeseq,meanval)
sdval <- as.data.frame(sdval)
sdval <- cbind(timeseq,sdval)


qqnorm(zwy_00$zwy)
qqline(zwy_00$zwy)
ggplot(zwy_00,aes(x=zwy))+
  geom_histogram(colour = "black",fill = "gray",binwidth = 0.003)+
  geom_density(color = brewer.pal(11,"Spectral")[10],fill =  brewer.pal(11,"Spectral")[10] , linetype = "longdash", alpha= 0.5)+
  scale_x_continuous(expand = c(0,0.005),
                     limits=c(0.02,0.24),
                     breaks=seq(0,0.3,0.04))+
  labs(x = "Pressure(MPa)", y = "Count") +
  theme_test() + theme(plot.title = element_text(hjust = 0.5)) 
3*(sd(zwy_00$zwy,na.rm=TRUE))
zwy_time <- mytss[1:3]
ggplot(zwy_time[zwy_time$time == '00:30:00'|zwy_time$time == '03:30:00' |zwy_time$time == '06:30:00'
                |zwy_time$time == '09:30:00'|zwy_time$time == '12:30:00'|zwy_time$time == '15:30:00'
                |zwy_time$time == '18:30:00'|zwy_time$time == '21:30:00'|zwy_time$time == '23:30:00',],
       aes(x = zwy)) +
  geom_density(color = brewer.pal(11,"Spectral")[10],fill =  brewer.pal(11,"Spectral")[10],alpha = .3)+
  labs(x = "Pressure(MPa)") +
  facet_wrap(~time, ncol = 3) 

# 计算预测残差的u和sigma
svr_res <- NULL
svr_res <- fit_tuned$best.model$residuals
qqnorm(svr_res)
qqline(svr_res)
meanres <- mean(svr_res,na.rm = TRUE)
meanres
sdres<- sd(svr_res,na.rm=TRUE)
sdres

# 计算压力变化值得u和sigma
timeseq <- unique(mytss$time)
zwy_00 <- mytss[,(1:3)]
dzwy_00 <- diff(zwy_00$zwy)
dzwy_00 <- cbind(zwy_00[-1,1:2],dzwy_00)
meanvar <- NULL
sdvar <- NULL
for (i in 1:288){
  dzwy <- dzwy_00[dzwy_00$time==timeseq[i],(1:3)]
  meanvar[i] <- mean(dzwy$dzwy_00, na.rm = TRUE)
  sdvar[i] <- sd(dzwy$dzwy_00, na.rm = TRUE)
}
meanvar <- as.data.frame(meanvar)
meanvar <- cbind(timeseq,meanvar)
sdvar <- as.data.frame(sdvar)
sdvar <- cbind(timeseq,sdvar)

qqnorm(dzwy$dzwy_00)
qqline(dzwy$dzwy_00)

# 开始准备treedata
treedata <- mytss[(18*288+1):(151*288),1:3]
start <- 18*288+1
end <- 151*288
sel_data <- mytss[1:end,1:3]
sel_data[,3] <- na.approx(sel_data[,3], na.rm = FALSE) 
indata <- sel_data[start:end,1:3] 
for (i in 1:m){
  indata <- cbind(indata,sel_data[(start-i):(end-i),3]) 
}
indata <- cbind(indata,sel_data[(start-288):(end-288),3]) 
indata <- cbind(indata,sel_data[(start-288*7):(end-288*7),3]) 
pre_results <- predict(fit_tuned$best.model,indata[,c(-1,-2,-3)])
res_results <- indata[,3]-pre_results
treedata <- cbind(treedata,res_results)
zwy_00 <- mytss[(18*288):(151*288),(1:3)]
dzwy_00 <- diff(zwy_00$zwy)
treedata <- cbind(treedata,dzwy_00)
treedata <- cbind(treedata,rep(0,nrow(treedata)))
treedata$class <- 0
names(treedata)<- c("date","time","val","res","var","class")
row.names(treedata)<-1:nrow(treedata)
treedata_multi <- treedata
treedata_multi$resmulti <- (treedata$res-meanres)/sdres
treedata_multi <- merge(treedata_multi,meanval,by.x="time",by.y = "timeseq",sort = FALSE)
treedata_multi <- merge(treedata_multi,sdval,by.x="time",by.y = "timeseq",sort = FALSE)
treedata_multi <- merge(treedata_multi,meanvar,by.x="time",by.y = "timeseq",sort = FALSE)
treedata_multi <- merge(treedata_multi,sdvar,by.x="time",by.y = "timeseq",sort = FALSE)
treedata_multi$valmulti <- (treedata_multi$val-treedata_multi$meanval)/treedata_multi$sdval
treedata_multi$varmulti <- (treedata_multi$var-treedata_multi$meanvar)/treedata_multi$sdvar
treedata_multi_results <- treedata_multi[,c("date","time","class","valmulti","resmulti","varmulti")]
treedata_score <- treedata[,c(1,2,3,6)]
treedata_score <- merge(treedata_score,treedata_multi_results,by=c("date","time","class"),sort = FALSE)
getscore <- function(vec) {
  vec <- as.data.frame(vec)
  names(vec)<- "multi"
  vec$sd4 <- 0
  vec[(!is.na(vec$multi))&(vec$multi<(-4.0))&(vec$multi>(-9.0)),"sd4"]<- 1
  vec$sd3 <- 0
  vec[(!is.na(vec$multi))&(vec$multi<(-3.0))&(vec$multi>(-9.0)),"sd3"]<- 1
  vec$sd2 <- 0
  vec[(!is.na(vec$multi))&(vec$multi<(-2.0))&(vec$multi>(-9.0)),"sd2"]<- 1
  vec$sd1 <- 0
  vec[(!is.na(vec$multi))&(vec$multi<(-1.0))&(vec$multi>(-9.0)),"sd1"]<- 1
  vec$score <- vec$sd4
  for(i in 8:nrow(vec)) {
    vec[i,"sum3"]<- sum(vec[(i-2):i,"sd3"])
    vec[i,"sum2"]<- sum(vec[(i-4):i,"sd2"])
    vec[i,"sum1"]<- sum(vec[(i-7):i,"sd1"])
    }
  vec[(!is.na(vec$sum3))&(vec$sum3>=2),"score"]<- vec[(!is.na(vec$sum3))&(vec$sum3>=2),"score"]+1
  vec[(!is.na(vec$sum2))&(vec$sum2>=4),"score"]<- vec[(!is.na(vec$sum2))&(vec$sum2>=4),"score"]+1
  vec[(!is.na(vec$sum1))&(vec$sum1>=7),"score"]<- vec[(!is.na(vec$sum1))&(vec$sum1>=7),"score"]+1
  return(vec$score)
}
treedata_score$scoreval <- getscore(treedata_score$valmulti)
treedata_score$scoreres <- getscore(treedata_score$resmulti)
treedata_score$scorevar <- getscore(treedata_score$varmulti)
# 打分完成

# 给数据打标签
# 当三个维度（压力绝对值，预测偏差值，压力变化值的WEC打分）的打分都是0的时候，标记为0，表示正常
treedata_score[(treedata_score$scoreval==0)&(treedata_score$scoreres==0)&(treedata_score$scorevar==0),"class"] <- 0
showtreedata_score <- treedata_score
# "2018-01-20 09:05:00"到"2018-01-20 17:10:00"的所有数据都标记为1，表示爆管
start <- as.numeric(rownames(subset(showtreedata_score,(date=="2018-01-20")&(time=="09:05:00"))))
end <- as.numeric(rownames(subset(showtreedata_score,(date=="2018-01-20")&(time=="17:10:00"))))
showtreedata_score[start:end,]$class<- 1 
# "2018-01-20 17:45:00"到"2018-01-20 19:00:00"的所有数据都标记为1，表示爆管
start <- as.numeric(rownames(subset(showtreedata_score,(date=="2018-01-20")&(time=="17:45:00"))))
end <- as.numeric(rownames(subset(showtreedata_score,(date=="2018-01-20")&(time=="19:00:00"))))
showtreedata_score[start:end,]$class<- 1 
#####

showtreedata_score[((!is.na(showtreedata_score$val))&(showtreedata_score$val==0)),"class"] <- NA
showtreedata_score <- na.omit(showtreedata_score[1:16717,])



df <- showtreedata_score[,c("class","scoreval","scoreres","scorevar")]
df <- na.omit(df)
df$class <- factor(df$class,levels = c(0,1),labels=c("Normal","Burst"))
train <- sample(nrow(df),0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
dtree <- rpart(class~., data=df.train, method = "class",parms = list(split="information"))
prp(dtree,type = 2,extra = 104,fallen.leaves = TRUE,main="titie")
dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree,cp=0.0125)

prp(dtree.pruned,type = 2,extra = 104,fallen.leaves = TRUE,main="titie")
dtree.pred <- predict(dtree.pruned,df.validate,type="class")
dtree.perf <- table(df.validate$class,dtree.pred,dnn = c("Actual","Predicted"))
dtree.perf

