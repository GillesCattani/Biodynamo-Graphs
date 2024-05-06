# Packages needed
library("ggplot2")
library("rjson")

## Percentage infected population comparison between Biodynamo and R

# Folder where data are stored
setwd("C:\\Users\\Maintenant prêt!\\Desktop\\HIV\\Biodynamo")
files = list.files(pattern="\\.json$")

dataJson <- lapply(files, function(x) fromJSON(file=x))

data1 = dataJson[[1]]$data_[[43]]$second$y_values
data2 = dataJson[[2]]$data_[[43]]$second$y_values
data3 = dataJson[[3]]$data_[[43]]$second$y_values
data4 = dataJson[[4]]$data_[[43]]$second$y_values
data5 = dataJson[[5]]$data_[[43]]$second$y_values
data6 = dataJson[[6]]$data_[[43]]$second$y_values
data7 = dataJson[[7]]$data_[[43]]$second$y_values
data8 = dataJson[[8]]$data_[[43]]$second$y_values
data9 = dataJson[[9]]$data_[[43]]$second$y_values
data10 = dataJson[[10]]$data_[[43]]$second$y_values

year = c("1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986",
         "1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998",
         "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
         "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022",
         "2023","2024","2025","2026","2027","2028","2029","2030")

data = as.data.frame(cbind(as.numeric(year[1:50]),as.numeric(data10[1:50])))
names(data) = c("year", "Prevalence")


files = list.files(pattern="\\.csv$")
dataCsv <- lapply(files, function(x) read.csv(file=x))

dataR1 = dataCsv[[1]]
dataR2 = dataCsv[[2]]
dataR3 = dataCsv[[3]]
dataR4 = dataCsv[[4]]
dataR5 = dataCsv[[5]]
dataR6 = dataCsv[[6]]
dataR7 = dataCsv[[7]]
dataR8 = dataCsv[[8]]
dataR9 = dataCsv[[9]]
dataR10 = dataCsv[[10]]

maxB = which.max(c(data1[25],data2[25],data3[25],data4[25],data5[25],data6[25],
                  data7[25],data8[25],data9[25],data10[25]))
minB = which.min(c(data1[25],data2[25],data3[25],data4[25],data5[25],data6[25],
                  data7[25],data8[25],data9[25],data10[25]))

maxR = which.max(c(dataR1[25],dataR2[25],dataR3[25],dataR4[25],dataR5[25],dataR6[25],
                   dataR7[25],dataR8[25],dataR9[25],dataR10[25]))
minR = which.min(c(dataR1[25],dataR2[25],dataR3[25],dataR4[25],dataR5[25],dataR6[25],
                   dataR7[25],dataR8[25],dataR9[25],dataR10[25]))

# Dataframe corresponding to highest values
maxB
# Dataframe corresponding to smalles values
minB

# Dataframe corresponding to highest values
maxR
# Dataframe corresponding to smalles values
minR


# Change the data set corresponding to the lowest and highest value for each model
small_R = dataR10[2:27]
Big_R = dataR2[2:27]
small_B = data4[1:26]
Big_B = data1[1:26]

model = c(rep("R",26),rep("Biodynamo",26))

data = data.frame(y_max = c(t(Big_R)[,1],Big_B),
                  y_min = c(t(small_R)[,1],small_B),
                  year = c(year[1:26],year[1:26]))
cbind(data,model)

ggplot(data, aes(x = year, group = model)) +
  geom_ribbon(aes(ymin = y_min, ymax = y_max, fill = model), alpha = 0.3) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=17,face="bold"),
        legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  xlab("Year") +
  ylab("Infected")
