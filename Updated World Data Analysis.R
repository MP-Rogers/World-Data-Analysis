lapply(c("tidyverse", "plotly", "scales", "rnaturalearth", "rnaturalearthdata", "ggcorrplot"),
       library,  character.only = TRUE)
Dataset<-read.csv("world-data-2023.csv")

glimpse(Dataset)
col.names<-colnames(Dataset)
numeric.cols<-col.names[-c(1, 3, 8, 9,13, 21, 25)]
Dataset <- Dataset |> 
  mutate(across(.cols = numeric.cols, .fns = ~str_remove_all(.x, "[$,%]") |> as.numeric()))
Dataset <- Dataset |> mutate(GDP.per.capita = GDP/Population)


numerics<-Dataset |> select_if(is.numeric) |> na.omit()
c.matrix<-cor(numerics)
corplot<-ggcorrplot(c.matrix, type = "upper", hc.order = TRUE)+
  labs(title = "General Correlation in the Dataset")+
  theme_minimal()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())
corplot<-ggplotly(corplot)
print(corplot)



gdp.per.capita.plot<-plot_ly(data = Dataset, type = "choropleth", z = ~GDP.per.capita, locations = ~Country, 
                   locationmode = "country names", colorscale = "heat")
print(gdp.per.capita.plot)

min.wage.gdp<-ggplot(Dataset, mapping = aes(x = Minimum.wage, y = GDP.per.capita, colour = Country))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(title = "GDP per Capita and Minimum Wage around the World",
       x = "Minimum Wage (USD)", y = "GDP per Capita (USD)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_y_continuous(labels = dollar_format())+
  scale_x_continuous(labels = dollar_format())
min.wage.gdp<-ggplotly(min.wage.gdp)
print(min.wage.gdp)

birth.rate.plot<-ggplot(Dataset, mapping = aes(y = Fertility.Rate, x = GDP.per.capita, colour = Country))+
  geom_point()+
  labs(title = "fertility Rate of different countries",
       x = "GDP per Capita", y = "fertility rate")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_x_continuous(labels = dollar_format())
bplot<-ggplotly(birth.rate.plot)
print(bplot)