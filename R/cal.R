library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)
library(lubridate)
library(gridExtra)
library(ggExtra)
library(dplyr)

set.seed(45)
A<-sample(0:2,31,replace=TRUE)
B<-sample(0:2,31,replace=TRUE)
C<-sample(0:2,31,replace=TRUE)
D<-sample(10:50,31,replace=TRUE)
E<-sample(10:50,31,replace=TRUE)
G<-sample(10:50,31,replace=TRUE)
H<-sample(1:10,31,replace=TRUE)
I<-sample(1:10,31,replace=TRUE)
J<-sample(1:10,31,replace=TRUE)
K<-sample(100:300,31,replace=TRUE)
L<-sample(100:300,31,replace=TRUE)
M<-sample(100:300,31,replace=TRUE)

data<- data.frame(A,B,C,D,E,G,H,I,J,K,L,M)

ReportDate<-as.Date('2016-12-1')
ReportDate<-seq.Date(ReportDate,by='day',length.out = 31)
#add new date range to original data frame
data$ReportDate<-ReportDate


df<- tbl_df(data)

df2<-gather(df,key=Metric,value = Value,-ReportDate)

df2<- df2 %>% group_by(Metric)%>%
  mutate(Rescaled = scales::rescale(Value))
df2$Metric<-as.factor(df2$Metric)
df2$Metric=with(df2,factor(Metric, levels=rev(levels(Metric))))

p<-ggplot(df2,aes(ReportDate,Metric,fill=factor(Value)))+
  geom_tile(colour="white",size=.1) +
  coord_equal()+
  scale_fill_viridis(discrete = TRUE,option = "C", direction = -1)+
  guides(fill=guide_legend(title="# By Day"))+
  scale_x_date(date_breaks = "1 day",date_labels="%d-%b-%y")+
  theme_minimal(base_size = 10, base_family = "Trebuchet MS")+
  removeGrid()+rotateTextX()+
  ggtitle("Example Company Indicators - Events per weekday Dec 2016",
          subtitle = "# Events per metric per day")+
  labs(x=NULL, y=NULL)+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="none")
p

ggsave("2016-11-27-Heatmapnolabels.png",width = 8.74,height = 4.84)
labels1df<-filter(df2,Value<=29)
labels2df<-filter(df2,Value>=30)

p<-p+geom_text(data=labels1df,aes(ReportDate,Metric,label=Value,fontface="bold"),size=2.5)
p<-p+geom_text(data=labels2df,aes(ReportDate,Metric,label=Value,fontface="bold"),colour="white",size=2.5)
p
ggsave("2016-11-27-Heatmapwithlabels.png",height = 4.84,width = 8.74)



### Calendar plot
df2$dow <-wday(df2$ReportDate,label=TRUE)
#now reverse levels of dow so that it plots Sun to Sat top down
#http://stackoverflow.com/questions/8713462/ggplot2-change-order-of-display-of-a-factor-variable-on-an-axis
df2$dow = with(df2, factor(dow, levels = rev(levels(dow))))
df2$week<-week(df2$ReportDate)


#add year/week and reformat for straddling weeks
#http://stackoverflow.com/questions/21329882/ggplot2-boxplots-by-week
df2$weeks <- format(df2$ReportDate, "%W") # was originally "%Y/%W"
df2$weeks <- factor(df2$weeks, levels = unique(df2$weeks))

#http://stackoverflow.com/questions/15468511/count-daily-observations-by-week?rq=1
#gets start date of week
#use weekStart as a calendar style plot
df2$weekStart <- df2$ReportDate - as.POSIXlt(df2$ReportDate)$wday

df2$Metric = with(df2, factor(Metric, levels = rev(levels(Metric))))


p2<-ggplot(df2,aes(weekStart,dow,fill=factor(Value)))+
  geom_tile(colour="white",size=.1) +
  scale_fill_viridis(discrete = TRUE,option = "C", direction = -1)+
  guides(fill=guide_legend(title="# Company Indicators - Activity By Day"))+
  scale_x_date(date_breaks = "1 week",date_labels="%d-%b-%y")+
  theme_minimal(base_size = 10, base_family = "Trebuchet MS")+
  removeGrid()+rotateTextX()+
  ggtitle("Company Indicators - Events per weekday Dec 2016",subtitle = "# Events per metric per day")+
  labs(x="Week Beginning", y=NULL)+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="none")
p2<-p2+facet_wrap(~Metric,nrow = 3)
p2
ggsave("2016-11-27-MetricCalendarHeatmap.png",height = 5.84,width = 8.74)

labels1df<-filter(df2,Value <=29)
labels2df<-filter(df2,Value >30)

p2<-p2+geom_text(data=labels1df,aes(weekStart,dow,label=Value,fontface="bold"),size=2.5)
p2<-p2+geom_text(data=labels2df,aes(weekStart,dow,label=Value,fontface="bold"),colour="white",size=2.5)
p2
ggsave("2016-MetricCalendarHeatmapWithLabels.png",height = 5.84,width = 8.74)





labels1df<-filter(df2,Value==0)
labels2df<-filter(df2,Value >=1 & Value <=29)
labels3df<-filter(df2,Value  >29)

p4<-ggplot(df2,aes(ReportDate,Metric,fill=factor(Value)))+
  geom_tile(colour="grey40",size=.1) +
  coord_equal()+
  scale_fill_viridis(discrete = TRUE,option = "C", direction = -1)+
  guides(fill=guide_legend(title="# By Day"))+
  scale_x_date(date_breaks = "1 day",date_labels="%d-%b-%y")+
  theme_minimal(base_size = 10, base_family = "Trebuchet MS")+
  removeGrid()+rotateTextX()+
  ggtitle("Example Company Indicators - Events per weekday Dec 2016",subtitle = "# Events per metric per day")+
  labs(x=NULL, y=NULL)+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="none")

p4 <-p4 +geom_tile(data=dfZ,colour="grey40",fill="white",size=0.1,aes(ReportDate,Metric))
p4<-p4+geom_text(data=labels2df,aes(ReportDate,Metric,label=Value,fontface="bold"),size=2.5)
p4<-p4+geom_text(data=labels3df,aes(ReportDate,Metric,label=Value,fontface="bold"),colour="white",size=2.5)
p4
ggsave ("2016-11-27-HeatmapWhiteZeroWithLabels.png", width = 8.74,height = 4.84 )

