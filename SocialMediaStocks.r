#load packages
library(dplyr)
library(ggplot2)
library(plotly)
library(devtools)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(extrafont)

#Question: Which company saw the most growth due to the pandemic?

#load data
stocks<-read.csv('smstocks.csv')
head(stocks)

#separate the date column into individual day,month,and year columns
stocks$newDate<- as.Date(stocks$Date,"%Y-%m-%d")
stocks$yr<-format(as.Date(stocks$newDate, format="%Y-%m-%d"),"%Y")


#create different dataframe for only selected year where all companies traded
stocks_all<-stocks%>%
  filter(yr==2019 | yr==2020| yr==2021)
View(stocks_all)

# count of different values
stocks%>%
  group_by(Symbol)%>%
  summarise(
    count=n()
  )

#visualize section
xlabels<- stocks$yr
mycolors<- c("#eb6d20","#4267B2","#E60023","#FFFC00","#1DA1F2")
platforms<- c("ETSY","Facebook","Pinterest","Snapchat","Twitter")

# Trend of all platforms over time all in one (Final visualization used for presentation)
one<- ggplot(stocks_all, aes(x=newDate,
                       y=Close,group=Symbol,
                       col=Symbol, 
                       text = paste("Platform=",platforms,"\nOpen=",round(Open,2),
                                    "\nClose=",round(Close,2),
                                    "\nVolume=",Volume,
                                    "\nDate=", Date))) + 
  geom_line(size=0.6,alpha=0.8) +
  labs(title ="What is the closing price of each stock over time?",
       x="Year",
       y="Closing Price",
       color="Social Media Platform")+
  theme(plot.subtitle = element_text(hjust = 1.5))+
  scale_color_manual(values=mycolors,labels=platforms)+
  theme(
    panel.background = element_blank(),
    text = element_text(family='Rubik',size = 20),
    panel.grid = element_line(color="#808080",size=0.60)
  )
  
ggplotly(one,tooltip="text")%>%
  layout(title = list(text = paste(
                                   'Which company saw the most growth due to the pandemic?',
                                   '\n'
                                    )))
         





# trend over time separated by platform using facet_wrap
ggplotly(ggplot(stocks_all, aes(x=newDate,
                                y=Close,
                                group=Symbol,
                                col=Symbol,
                                text = paste("\nOpen=", round(Open,2),
                                             "\nClose=",round(Close,2),
                                             "\nVolume=",Volume,
                                             "\nDate=", Date )
                                )) + 
  geom_line() + 
  facet_wrap(~ Symbol, scales = "free_x",
             labeller = labeller(Symbol=c("ETSY"="ETSY",
                                          "FB"="Facebook",
                                          "PINS"="Pinterest",
                                          "SNAP"="Snapchat",
                                          "TWTR"="Twitter"
                                          )))+
  labs(title="Social Media Stock Value (2019-2022)",
       x="Year",
       y="Closing Price")+
  scale_color_manual(values=mycolors,labels=platforms)+
  theme(plot.background = element_rect(fill = "gray"))+
  theme_set(theme_classic())+
    theme(legend.position = "none"),
  tooltip="text")





