library(tidyverse)
library(extrafont)
library(patchwork)
library(scales)
library(ggplot2)
options(scipen = 999)


#View(data)
library(tidyverse)

df <- read_csv("C:/Users/Desktop/HFMD.csv")
glimpse(df)


df <- df %>% 
  mutate(Date = lubridate::ym(paste0(Year, "-", Month)),
         Date2 = sprintf("%d-%02d", Year,Month))

ylim.prim <- c(0, 12000)
ylim.sec <- c(-4, 250000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

p <- ggplot(df, aes(x = Date))+
  geom_col(aes(y = Sentinel_data), color = "white", width = 30, fill = "#4682B4")+
  geom_line(aes(y = a+Claims_data*b), colour = "black", lwd=0.7) +
  # geom_line(aes(y = Patient_visit), colour = "blue", linetype = "dashed") +
  scale_y_continuous("Sentinel data\n", sec.axis = sec_axis(~ (. - a)/b, name = "Claims data\n"),expand=c(0,0))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0,0))+
  theme(axis.text.x=element_text(size=12, color = "black"),
        axis.text.y=element_text(size=16, color = "black"),
        axis.text.y.left=element_text(size=16, color = "#4682B4"),
        axis.title.y.right = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background =  element_rect (fill = "white"),
        axis.title=element_text(size=16, face = "bold"),
        strip.text = element_text(size=16),
        axis.title.y.left = element_text(size=16, color="#4682B4"),
        axis.line.y = element_line(linewidth=1),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.y.left = element_line(color = "#4682B4"),
        axis.line.x = element_line(linewidth=1),
        axis.line.y.right = element_line(color="black"))
p1 <- p + labs(x = "\nDate")
p1



library(magrittr)

df1 <- read_csv("C:/Users/ACHANGWA CHIARA/Desktop/HFMD.csv")
glimpse(df1)

df1 <- df1 %>% 
  mutate(Date = lubridate::ym(paste0(Year, "-", Month)),
         Date2 = sprintf("%d-%02d", Year,Month))
View(df1)


df1 <- df1[df$Date > "2019-12-01" &    # Extract data frame subset
             df1$Date < "2022-07-01", ]
df1

ggplot(df1, aes(x = Date))+
  geom_col(aes(y = Sentinel_data), color = "white", width = 30, fill = "#4682B4")+
  geom_line(aes(y = a+Claims_data*b), colour = "black", lwd=0.7) +
  # geom_line(aes(y = Patient_visit), colour = "blue", linetype = "dashed") +
  scale_y_continuous("Sentinel data\n", sec.axis = sec_axis(~ (. - a)/b, name = "Claims data\n"),expand=c(0,0))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0,0))+
  theme(axis.text.x=element_text(size=12, color = "black"),
        axis.text.y=element_text(size=16, color = "black"),
        axis.text.y.left=element_text(size=16, color = "#4682B4"),
        axis.title.y.right = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background =  element_rect (fill = "white"),
        axis.title=element_text(size=16, face = "bold"),
        strip.text = element_text(size=16),
        axis.title.y.left = element_text(size=16, color="#4682B4"),
        axis.line.y = element_line(linewidth=1),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.y.left = element_line(color = "#4682B4"),
        axis.line.x = element_line(linewidth=1),
        axis.line.y.right = element_line(color="black"))



