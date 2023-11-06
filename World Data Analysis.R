library(tidyverse)
library(stringr)
library(scales)
library(ggcorrplot)
library(plotly)
dataset<-read.csv("world-data-2023.csv")
glimpse(dataset)

#convert to numerics
c.names<-colnames(dataset)
cols.to.format<-c.names[-c(1,3, 9, 13, 21, 25)]
cols.that.can.stay<-c.names[c(1,3, 9, 13, 21, 25)]
dataset<-dataset |> mutate(across(.cols = all_of(cols.to.format),.fns = ~str_remove_all(.x, "[$,%]")|> as.numeric()))
dataset<- dataset |> mutate(GDP.per.capita = round((GDP/Population), 2))

#general cor plot
general<-dataset |> select_if(is.numeric) |> na.omit()
general<-cor(general)
general.cor.plot<-ggcorrplot(general, hc.order = TRUE, type = "upper")+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "General Correlations between Variables")+
  theme(plot.title = element_text(hjust = 0.5))

general.cor.plot<-ggplotly(general.cor.plot)
print(general.cor.plot)
