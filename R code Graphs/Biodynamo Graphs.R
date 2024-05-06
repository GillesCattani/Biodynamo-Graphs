# Packages needed
library("ggplot2")
library("rjson")

## Percentage infected population with confidence bands of 10 simulations

# Folder where data are stored
setwd("C:\\Users\\Maintenant prêt!\\Desktop\\HIV\\Biodynamo")
files = list.files(pattern="\\.json$")

dataJson <- lapply(files, function(x) fromJSON(file=x))

data1 = dataJson[[1]]$data_[[83]]$second$y_values
data2 = dataJson[[2]]$data_[[83]]$second$y_values
data3 = dataJson[[3]]$data_[[83]]$second$y_values
data4 = dataJson[[4]]$data_[[83]]$second$y_values
data5 = dataJson[[5]]$data_[[83]]$second$y_values
data6 = dataJson[[6]]$data_[[83]]$second$y_values
data7 = dataJson[[7]]$data_[[83]]$second$y_values
data8 = dataJson[[8]]$data_[[83]]$second$y_values
data9 = dataJson[[9]]$data_[[83]]$second$y_values
data10 = dataJson[[10]]$data_[[83]]$second$y_values

year = c("1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986",
         "1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998",
         "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
         "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022",
         "2023","2024","2025","2026","2027","2028","2029","2030")

data = as.data.frame(cbind(as.numeric(year[1:50]),as.numeric(data10[1:50])))
names(data) = c("year", "Prevalence")


max = which.max(c(data1[25],data2[25],data3[25],data4[25],data5[25],data6[25],
                  data7[25],data8[25],data9[25],data10[25]))
min = which.min(c(data1[25],data2[25],data3[25],data4[25],data5[25],data6[25],
                  data7[25],data8[25],data9[25],data10[25]))

# Dataframe corresponding to highest values
max
# Dataframe corresponding to smalles values
min

# Change in geom_ribbon ymin = data$number_of_data corresponding to smallest value
# Change in geom_ribbon ymax = data$number_of_data corresponding to highest value
ggplot(data, aes(x = year, y = Prevalence)) +
  #geom_point(aes(color=model)) +
  #geom_line(aes(color=model),size = 0.8) +
  geom_ribbon(aes(ymin = data4, ymax = data1), alpha = 0.3) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=17,face="bold"),
        legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  xlab("Year") +
  ylab("Prevalence")


## Percentage infected population with real data

myData <- fromJSON(file="C:\\Users\\Maintenant prêt!\\Desktop\\HIV\\Biodynamo\\data.json")
myData$data_[[83]]$second$y_values

Biodynamo = c(0, myData$data_[[83]]$second$y_values)

year = c("1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986",
         "1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998",
         "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
         "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022",
         "2023","2024","2025","2026","2027","2028","2029","2030")

data = as.data.frame(cbind(as.numeric(year[1:50]),as.numeric(Biodynamo[1:50])))
names(data) = c("year", "Prevalence")

unaids = read.csv("C:\\Users\\Maintenant prêt!\\Desktop\\HIV\\Biodynamo\\UNAIDS.csv")
unaids = unaids[unaids$Country=="Malawi",]

unaids_lower = unaids[,seq(3,133,4)]
unaids_upper = unaids[,seq(4,133,4)]

data = cbind(data[16:48,],as.numeric(t(unaids_lower)),as.numeric(t(unaids_upper)))
names(data) = c("year", "Prevalence","lower","upper")

ggplot(data, aes(x = year, y = Prevalence)) +
  geom_point() +
  geom_line(size = 0.8) +
  geom_line(aes(y=upper/100),size = 0.8, linetype = "dashed") +
  geom_line(aes(y=lower/100),size = 0.8, linetype = "dashed") +
  #geom_ribbon(aes(ymin = y_min, ymax = y_max, fill = model), alpha = 0.3) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=17,face="bold"),
        legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  xlab("Year") +
  ylab("Prevalence")



