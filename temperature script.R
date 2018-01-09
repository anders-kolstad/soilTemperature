


#*****************************************************##

# FULL YEAR DATA ####

#*****************************************************##

#Raw data:
#  Temperature 2-4 times a day, 3 loggers per plot, total 12 sites and 72 samples
#  Two datasets from two data off-loads:
#  .	May-Aug for each logger
#  .	Aug-May for each logger
#	Total 144 csv files. Compiling requires automation, probably for-loops.

#Questions I want to answer: 
#  1.	How does daily mean soil temperature vary through the year, and does this differ between treatments?
#  2.	Does treatments influence soil frost duration and spring onset?
#  3.	How does daily soil temperature fluctuations change throughout the year, and does it differ between treatments?


#to_install <- (c("readr",  "readxl", "stringr",
#             "plyr", "dplyr", "ggplot2", "data.table", "plotrix",
#             "reshape2", "gridExtra", "car", "geoR", "plotly"))
#install.packages(to_install)

# Packages ####
library(readr)
library(readxl)
library(stringr) # str_sub()
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table) # %between%
library(plotrix)    # ablineclip
library(reshape2)
library(gridExtra)
library(car)
library(geoR) #variog
library(plotly) 
library(lavaan)
library(semPlot)
library(piecewiseSEM)
library(glmmTMB)
library(nlme)
library(lme4)

# !!!  Jump from here.... !!! ####



#*****************************************************##

# Import fullYear datafiles: #

#*****************************************************##

# Using xlsx didn't wok because excel fecked up the time zones:
#bjoellaaa_6_B_NH_10890911 <- read_excel("M:/Anders L Kolstad/systherb data/HOBOs/fullYear/bjoellaaa_6_B_NH_10890911.xlsx", 
#                                        col_types = c("date", "numeric", "numeric"))


# So I impoort from csv. The two read-out are in two different folders, 
# and files have the same names inside each folder.



setwd("M:\\Anders L Kolstad\\systherb data\\HOBOs\\HOBO_bulk_export\\export 2016")
temp = list.files(pattern="*.csv")
master <- read.csv(temp[1])
master <- master[,1:3]
colnames(master) <- c("Date", "Time", "Temp")
master$logger <- temp[1]


for (i in 2:length(temp)) {
  temp_file <- read.csv(temp[i])
  temp_file <- temp_file[,1:3]
  colnames(temp_file) <- c("Date", "Time", "Temp")
  temp_file$logger <- temp[i]
  master <- rbind(master, temp_file)
}
rm(temp_file)

#(unique(master$logger))

setwd("M:\\Anders L Kolstad\\systherb data\\HOBOs\\HOBO_bulk_export\\export 2016_2017")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) {
  temp_file <- read.csv(temp[i])
  temp_file <- temp_file[,1:3]
  colnames(temp_file) <- c("Date", "Time", "Temp")
  temp_file$logger <- temp[i]
  master <- rbind(master, temp_file)
}
#(unique(master$logger))

rm(temp_file)
rm(i)
rm(temp)

master2 <- master
rm(master)




# split the lgger column info into seperate columns:
a <- strsplit(master2$logger, "_")
b <- unlist(a)
c <- matrix(b, ncol = 5, byrow = T)
d <- data.frame(c)
colnames(d) <- c("site", "TID", "trt", "subplot", "loggerID")
master3 <- cbind(master2, d)
rm(list = c("a","b","c","d","master2"))


# Order the columns sensibly
master4 <- select(master3,
                  TID,
                  site,
                  trt,
                  subplot,
                  Date,
                  Time,
                  loggerID,
                  Temp)
rm(master3)


# remove the extension at the end of the lggerID
master4$loggerID <- as.numeric(str_sub(master4$loggerID, start=1, end=8))

table(master4$site)
master4$site[master4$site=="floneset"] <- "floeneset"
master4$site[master4$site=="kalddal"] <- "kalddalsbekken"
master4$site[master4$site=="seterdalsv"] <- "saeterdalsveien"
master4$site[master4$site=="skorholmsskogen"] <- "skjorholmsskogen"
master4$site[master4$site=="vaalaa"] <- "vaalaavatnet"
master4$site <- factor(master4$site)
table(master4$site)


#write.csv(master4, file = "fullYearSoilTempSeries.csv", row.names = F)
# ... to here. ####




#*****************************************************##

# IMPORT ####

#*****************************************************##
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
master4 <- read.csv("fullYearSoilTempSeries.csv")


# Standardise and get time and dates in order
master4$Date <- as.Date(master4$Date, format = c("%d.%m.%y"))
master4$Time <- as.POSIXct(master4$Time, format="%H:%M", tz = "GMT")
# Time2 gets given todays date for all the rows 

master4$TID <- factor(master4$TID)










#*****************************************************##

# TRUNCATE ####

#*****************************************************##
# I could exlude rows that are not between the deployment and retreivement days for each logger.
# Alternatively, and this is what I'll di, truncate teh dataseries according to the shortest lenght:
# Potential high temperatures recorded during logger read-off are deleted manually

# import metadata
Soil_temperature <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
  sheet = "fullYear", col_types = c("numeric", 
                                  "numeric", "text", "text", "text", 
                                  "text", "date", "date", "date", "numeric", 
                                  "text","text", "numeric", "numeric", "numeric", 
                                  "text")) 


duration <- c(max(Soil_temperature$deployment_date1),          # 2016-05-18
              min(Soil_temperature$retrieval_date2, na.rm = T)) # 2017-04-28 
duration
duration <- as.Date(c("2016-05-19", "2017-04-27"), format = c("%Y-%m-%d"))
duration
# OBS, no retrieval date for one site

master4$Date[1]
# ... same format

# filter/truncate the dataset:
master5 <- master4[master4$Date %between% duration,]


rm(list = c("master4", "Soil_temperature"))

plot(master5$Time)



#*****************************************************##
# Jump from here...####
# QC - plot all individual time series             ##
# 
#*****************************************************##
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")

master5$logg_series <- paste(master5$TID, master5$trt, master5$subplot, sep="_")

pdf("soilTemp_QC4.pdf")  
for (cat in unique(master5$logg_series)){
  d <- subset(master5, logg_series == cat)
  
  p <-  ggplot(data = d, 
               aes(x = Date, 
                   y = Temp)) 
  p <- p+   geom_line()   
  p <- p + xlab("Date") 
  p <- p + ylab("Soil temperature")
  p <- p + theme(text = element_text(size=15))
  p <- p + ggtitle(cat)
  print(p)
}
dev.off()


# all series are checked manually and edits are done in the raw data.
# Now I can reimport the csv-files and all should be fine. 
#All edits are saved in the matadata document 'Soil Temperature.xlsx' in the fullYear sheet.


# ... rerun importig steps...
# ... to here. ####






#*****************************************************##

# Data exploration.            ####

#*****************************************************##
table(master5$TID)
table(master5$site)
table(master5$trt)
table(master5$subplot)

table(master5$site, master5$TID)




#*****************************************************##

# Aggregate and plot              ####

#*****************************************************##
# Label according to site productivity and thinning yes/no
master5$prod_class <- ifelse(master5$TID == "8"| 
                               master5$TID =="6"|
                               master5$TID =="1"|
                               master5$TID =="14"|
                               master5$TID =="12", 
                             "High", "Low")
master5$prod_class <- ifelse(master5$TID == "13"| 
                               master5$TID =="9"|
                               master5$TID =="10",
                          "High + Thinned", master5$prod_class)

# get daily means / max / min / range (and SE)
t <- aggregate(data = master5, 
               cbind(Mean_daily_temperature = Temp) ~ 
                 TID+
                 site+
                 trt+
                 subplot+
                 prod_class+
                 Date, 
                 FUN = function(x) c(mn = mean(x), max = max(x), min = min(x) )  )

t2 <- do.call(data.frame, t)
#table(t$TID)
t2$daily_range <- t2$Mean_daily_temperature.max-t2$Mean_daily_temperature.min
head(t2)


# get mean per plot
t3 <- aggregate(data = t2, 
               cbind(Mean_daily_temperature = Mean_daily_temperature.mn,
                    daily_range = daily_range) ~ 
                 TID+
                 site+
                 trt+
                 prod_class+
                 Date, 
               FUN = mean )

head(t3, 20)
table(t3$trt)
table(t3$prod_class)

# remove thinned sites ####
t3.2 <- filter(t3,
               prod_class != "High + Thinned")
table(t3.2$site)



# get means per treatment
t4 <- aggregate(data = t3.2, 
                cbind(Mean_daily_temperature = Mean_daily_temperature,
                      daily_range = daily_range) ~ 
                  trt+
                  Date, 
                FUN = function(x) c(mn = mean(x), length = length(x), SD = sd(x) ) )



t4 <- do.call(data.frame, t4)
 
table(t4$Mean_daily_temperature.length) # n=12 for all days
t4$SE_mean_temp <- t4$Mean_daily_temperature.SD/t4$Mean_daily_temperature.length

head(t4)



# get means per treatment per productivity class and thinned
t4.2 <- t3
head(t4.2, 20)

t4.3 <- aggregate(data = t4.2, 
                cbind(Mean_daily_temperature = Mean_daily_temperature,
                      daily_range = daily_range) ~ 
                  prod_class+
                  trt+
                  Date, 
                FUN = function(x) c(mn = mean(x), length = length(x), SD = sd(x) ) )



t4.3 <- do.call(data.frame, t4.3)

table(t4.3$Mean_daily_temperature.length) # n=12 for all days
t4.3$SE_mean_temp <- t4.3$Mean_daily_temperature.SD/t4.3$Mean_daily_temperature.length

head(t4.3)

#*****************************************************##
# Plot 1:####


p <-  ggplot(data = t4, 
             aes(x = Date, 
                 y = Mean_daily_temperature.mn, 
                 group = trt,
                 colour = trt)) 
            
p <- p+   geom_line(size=2)   
p <- p + xlab("") 
p <- p + ylab(expression(atop("Soil temperature",( degree~C))))
p <- p + theme(text = element_text(size=15))
p <- p + scale_colour_discrete(name="",
                                 breaks=c("B", "UB"),
                                 labels=c("Open plots", "Exclosure"))
p <- p + theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))
p <- p + theme(axis.title =  element_text(hjust = 0.5))  #default
p
# lines are too close to draw errors.



# Rate of change plot ####
# take the full dataset:
head(t2)
t2$logger <- paste(t2$site, t2$trt, t2$subplot, t2$prod_class, sep="_")
length(unique(t2$logger))
#summary(t2)

# get each logger in a seperate column
t4_lag <- dcast(t2, Date~logger, value.var = "Mean_daily_temperature.mn")
head(t4_lag)
t4_lagx <- t4_lag

# calculate 1 day lag
for(i in 2:ncol(t4_lagx)){
  t4_lagx[,i] <-  t4_lagx[,i]-lag(t4_lagx[,i], 1)
  }
head(t4_lagx)

t4_lagxx <- t4_lag
# calculate 5 day lag
for(i in 2:ncol(t4_lagxx)){
  t4_lagxx[,i] <-  t4_lagxx[,i]-lag(t4_lagxx[,i], 5)
}


# melt it again
t4_meltx <- melt(data = t4_lagx, id.vars = "Date",
                measure.vars = names(t4_lag[-1]))
t4_meltxx <- melt(data = t4_lagxx, id.vars = "Date",
                 measure.vars = names(t4_lag[-1]))
t4_melt <- melt(data = t4_lag, id.vars = "Date",
                measure.vars = names(t4_lag[-1]))
head(t4_meltx) # rel.values 1 day shift
head(t4_meltxx) # rel.values 5 day shift
head(t4_melt) # real values for QC

# combine
rel_shift <- data.frame(t4_melt, one_day = t4_meltx$value, five_days = t4_meltxx$value)
head(rel_shift)

# split the logger column info into seperate columns:
str(rel_shift)
rel_shift$ch_logger <- as.character(rel_shift$variable)
a <- strsplit(rel_shift$ch_logger, "_")
b <- unlist(a)
c <- matrix(b, ncol = 4, byrow = T)
d <- data.frame(c)
colnames(d) <- c("site", "trt", "subplot", "prod_class")
rel_shift <- cbind(rel_shift, d)
rm(list = c("a","b","c","d"))
head(rel_shift)

colnames(rel_shift)<- c("Date", "logger", "soil_temp","one_day", "five_days", 
                        "ch_logger", "site", "TRT", "subplot", "prod_class")


# look at it
#summary(rel_shift)
Boxplot(rel_shift$one_day)
Boxplot(rel_shift$five_days)

Boxplot(rel_shift$one_day~rel_shift$logger, las=2)
# the extreme values are found evenly distributed across loggers



# plot all the lines to look for outliers
p_lag1 <-  ggplot(data = rel_shift, 
                   aes(x = Date, 
                       y = one_day, 
                       group = logger,
                       colour = logger))+ 
  geom_line(size=1.1)+   
  xlab("") +
  ylab("")+
  theme(text = element_text(size=15))+
  theme(legend.position="none")+
  theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))+
  theme(axis.title =  element_text(hjust = 0.5))

p_lag5 <-  ggplot(data = rel_shift, 
                  aes(x = Date, 
                      y = five_days, 
                      group = logger,
                      colour = logger))+ 
  geom_line(size=1.1)+   
  xlab("") +
  ylab("")+
  theme(text = element_text(size=15))+
  theme(legend.position="none")+
  theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))+
  theme(axis.title =  element_text(hjust = 0.5))
grid.arrange(p_lag1, p_lag5, nrow = 2)

# Looks good. More variation in summer.

# plot means per treatment:
rel_shift2 <- aggregate(data = rel_shift, 
                        cbind(one_day = one_day, five_days = five_days)~
                        Date+site+TRT+prod_class, FUN = mean)
rel_shift3 <- aggregate(data = rel_shift, 
                        cbind(one_day = one_day, five_days = five_days)~
                          Date+TRT, FUN = mean)
rel_shift4 <- aggregate(data = rel_shift, 
                        cbind(one_day = one_day, five_days = five_days)~
                          Date+prod_class, FUN = mean)

(p_lag3 <-  ggplot(data = rel_shift3, 
                  aes(x = Date, 
                      y = one_day, 
                      group = TRT,
                      colour = TRT))+
  geom_line(size = 1.1, colour = "black")+
  #geom_area()+
  geom_smooth(method = 'loess', span = 0.2)+
  xlab("Date") +
  ylab(expression(atop("Change in soil temperature", "since yesterday  " ( degree~C))))+
  theme(text = element_text(size=15))+
  scale_colour_discrete(
    name="",
    breaks=c("B", "UB"),
    labels=c("Open plots", "Exclosure"))+
  theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))+
  theme(axis.title =  element_text(hjust = 0.5)))

# rate of change alsmost identical between treatments. Pattern matches p_1

p_lag3.2 <-  ggplot(data = rel_shift4, 
                  aes(x = Date, 
                      y = five_days, 
                      group = prod_class,
                      colour = prod_class))+ 
  geom_line()+
  xlab("Date") +
  ylab(expression("Change in soil temperature since yesterday  " ( degree~C)))+
  theme(text = element_text(size=15))+
  scale_colour_discrete(
    name="",
    breaks=c("B", "UB"),
    labels=c("Browsed", "Exclosure"))+
  theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))+
  theme(axis.title =  element_text(hjust = 0.5))
# rate of change similar between productivity classes

p_lag4 <-  ggplot(data = rel_shift3, 
                  aes(x = Date, 
                      y = five_days, 
                      group = TRT,
                      colour = TRT))+ 
  #geom_line(size=1.1)+   
  geom_smooth(method = 'loess', span = 0.2)+
  xlab("") +
  ylab("")+
  theme(text = element_text(size=15))+
  theme(legend.position="none")+
  theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))+
  theme(axis.title =  element_text(hjust = 0.5))
# the five day lag is similar to the 1 day lag, only the y-axis is less relatable

# I like plot#3 best

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#pdf("Daily change in soil temperature_4.pdf", height = 5, width = 12)
p_lag3
dev.off()

#*****************************************************##
# Plot 2:####
#effect size over time


head(t3.2)
t3.3 <- t3.2
t3.3$link <- paste(t3.3$TID, t3.3$Date, sep = "")
t5 <- filter(t3.3,
             trt == "B")
t6 <- filter(t3.3,
             trt == "UB")
t5 <- select(t5,
             -trt)
colnames(t5)[colnames(t5)=="Mean_daily_temperature"] <- "B_mean"
colnames(t5)[colnames(t5)=="daily_range"] <- "B_range"

t5$UB_mean <- t6$Mean_daily_temperature[match(t5$link, t6$link)] 
t5$UB_range <- t6$daily_range[match(t5$link, t6$link)] 
t5$diff_mean <- t5$UB_mean-t5$B_mean
t5$diff_range <- t5$UB_range-t5$B_range
head(t5)

t7 <- aggregate(data = t5,
                cbind(diff_mean, diff_range)~Date,
                FUN = function(x) c(mn = mean(x), length = length(x), SD = sd(x) ))
t7 <- do.call(data.frame, t7)
t7$SE_mean_temp <- t7$diff_mean.SD/t7$diff_mean.length
t7$SE_range_temp <- t7$diff_range.SD/t7$diff_range.length

head(t7)

  
p2 <-  ggplot(data = t7, 
             aes(x = Date, 
                 y = diff_mean.mn)) 
p2 <- p2 + geom_ribbon(data = t7, 
                       aes(x = Date, 
                           ymax = diff_mean.mn+diff_mean.SD, 
                           ymin = diff_mean.mn-diff_mean.SD), alpha = 0.2)
p2 <- p2 + geom_ribbon(data = t7, 
                       aes(x = Date, 
                           ymax = diff_mean.mn+1.96*SE_mean_temp, 
                           ymin = diff_mean.mn-1.96*SE_mean_temp),alpha = 0.5)
#p2 <- p2 +   geom_line(size=1, colour = "white")   
p2 <- p2 + xlab("") 
p2 <- p2 + ylab(expression(atop(paste(Delta," Soil temperature "), ( degree~C))))
p2 <- p2 + theme(text = element_text(size=15)) 
p2 <- p2 + geom_hline(yintercept=0, size =2)

p2
#p <- p + facet_grid(. ~ trt, scales = "fixed")


#*****************************************************##
# Plot 2.2: ####
# effect size over time conditional or prod_class


head(t3.2)
head(t4.2)

t4.4 <- t4.2
t4.4$link <- paste(t4.4$TID, t4.4$Date, sep = "")
t4.5 <- filter(t4.4,
             trt == "B")
t4.6 <- filter(t4.4,
             trt == "UB")
t4.5 <- select(t4.5,
             -trt)
colnames(t4.5)[colnames(t4.5)=="Mean_daily_temperature"] <- "B_mean"
colnames(t4.5)[colnames(t4.5)=="daily_range"] <- "B_range"

t4.5$UB_mean <- t4.6$Mean_daily_temperature[match(t4.5$link, t4.6$link)] 
t4.5$UB_range <- t4.6$daily_range[match(t4.5$link, t4.6$link)] 
t4.5$diff_mean <- t4.5$UB_mean-t4.5$B_mean
t4.5$diff_range <- t4.5$UB_range-t4.5$B_range
head(t4.5)

t4.7 <- aggregate(data = t4.5,
                cbind(diff_mean, diff_range)~Date+prod_class,
                FUN = function(x) c(mn = mean(x), length = length(x), SD = sd(x) ))
t4.7 <- do.call(data.frame, t4.7)

t4.7$SE_mean_temp <- t4.7$diff_mean.SD/t4.7$diff_mean.length
t4.7$SE_range_temp <- t4.7$diff_range.SD/t4.7$diff_range.length

head(t4.7)


p2.2 <-  ggplot(data = t4.7, 
              aes(x = Date, 
                  y = diff_mean.mn,
                  group = prod_class,
                  colour = prod_class)) 

p2.2 <- p2.2 +   geom_line(size=2)   
p2.2 <- p2 + xlab("") 

p2.2 <- p2.2 + ylab(expression(atop(paste(Delta," Soil temperature "), ( degree~C))))
p2.2 <- p2.2 + theme(text = element_text(size=15)) 
p2.2 <- p2.2 + geom_hline(yintercept=0, size =2)
p2.2 <- p2.2 + theme(legend.position="none")
p2.2 <- p2.2 + theme(plot.margin=unit(c(0.1,1,0.1,2),"cm"))
p2.2


# superimpose p2.2 on p2

p2.3 <- p2 + geom_line(data = t4.7, 
                       aes(x = Date, 
                           y = diff_mean.mn,
                           group = prod_class,
                           colour = prod_class), size = 1.2)
p2.3 <- p2.3 + scale_colour_discrete(
                         name="Site\nproductivity",
                         breaks=c("High", "Low", "High + Thinned"),
                         labels=c("High (n=5)", "Low (n=7)","High +\nThinned (n=3)"))
#p2.3 <- p2.3 + theme(legend.position="bottom")
p2.3 <- p2.3 + theme(plot.margin=unit(c(0.1,1,0.1,1),"cm"))
p2.3
#*****************************************************##
# Plot 3:####
#effect daliy fluctuations over time
head(t4)
p3 <-  ggplot(data = t4, 
             aes(x = Date, 
                 y = daily_range.mn, 
                 group = trt,
                 colour = trt)) 

p3 <- p3 +   geom_line(size=1)   
p3 <- p3 + xlab("Date") 
p3 <- p3 + ylab(expression(atop("  Daily soil\ntemperature", "fluctuations " ( degree~C))))
p3 <- p3 + theme(text = element_text(size=15)) 
p3 <- p3 + scale_colour_discrete(name="",
                               breaks=c("B", "UB"),
                               labels=c("Browsed", "Exclosure"))
p3 <- p3 + theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))
#p3 <- p3 + theme(legend.position="bottom")
p3


# with smoother
p3.2 <-  ggplot(data = t4, 
              aes(x = Date, 
                  y = daily_range.mn, 
                  group = trt,
                  colour = trt)) 
#p3.2 <- p3.2 + geom_point()
p3.2 <- p3.2 +   geom_smooth(method = 'loess', span = 0.2)   
p3.2 <- p3.2 + xlab("Date") 
p3.2 <- p3.2 + ylab(expression(atop("  Daily soil\ntemperature", "fluctuations " ( degree~C))))
p3.2 <- p3.2 + theme(text = element_text(size=15)) 
p3.2 <- p3.2 + scale_colour_discrete(name="",
                                 breaks=c("B", "UB"),
                                 labels=c("Open plots", "Exclosure"))
p3.2 <- p3.2 + theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))
#p3.2 <- p3.2 + theme(legend.position="bottom")
p3.2




# Multiple plot function #########
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## END MULTIPLOT function ####

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")
#pdf("Full year series6.pdf")
tiff("full_year_multiplot.tiff", height = 30, width = 20, units = "cm", res = 300)
multiplot(p, p2.3,p_lag3,  p3.2, cols = 1)
dev.off()




# Plot site avg against treatment difference ####

head(t4)    #TRT and date
head(t3)    #Plot and date


t3.4 <- dcast(t3, TID+Date~trt, value.var = "Mean_daily_temperature", fun.aggregate = mean)
t3.4$diff <- t3.4$UB-t3.4$B
t3.5 <- aggregate(data = t3, Mean_daily_temperature~
                  TID+site+Date, FUN = mean)
t3.5$link <- paste(t3.5$TID, t3.5$Date)
t3.4$link <- paste(t3.4$TID, t3.4$Date)
t3.4$plot_mean <- t3.5$Mean_daily_temperature[match(t3.4$link, t3.5$link)]

plot.new()
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
pdf("plot temp against trt diff_4.pdf",width=16,height=16)

par(mfrow=c(4,4), new = TRUE)

plot(t3.4$diff~t3.4$plot_mean, abline(h=0), 
     xlab="plot mean t", ylab= "trt diff", 
     main = "All site", add=T)
for(cat in unique(t3.4$TID)){
  plot(t3.4$diff[t3.4$TID== cat] ~ t3.4$plot_mean[t3.4$TID== cat],
       abline(h=0), xlab="plot mean t", ylab= "trt diff", main = cat)
}
dev.off()


#*****************************************************##

# Moss Depth ####

#*****************************************************##

# The summer 2017 is best for analysing the role of moss depth on 
# summer soil temperatures. Here I focus on winter temperatures.
# It is difficult to visualise interactions between two continous
# variables. Instead I will get the mean temperature jan-march
# for all loggers and plot this agains the devaince from the plot mean.
# This will control for the treatment effect, which I will deal with 
# using the other dataset. For this I can also use the thinned sites.


# This takes us back to the dataset before we did any aggretating
head(master5)



# Truncate
winter <- as.Date(c("2017-01-01", "2017-03-31"), format = c("%Y-%m-%d"))
summer <- as.Date(c("2016-06-01", "2016-08-31"), format = c("%Y-%m-%d"))

master6 <- master5[master5$Date %between% winter,]
master7 <- master5[master5$Date %between% summer,]
head(master6)


# create a link function
master6$link <- paste(master6$TID, master6$trt)


# get winter means per subplot
winter2 <- aggregate(data = master6, 
               cbind(Mean_winter_temperature = Temp) ~ 
                 TID+
                 site+
                 trt+
                 subplot+
                 link+
                 prod_class,
               FUN = function(x) c(mn = mean(x), max = max(x), min = min(x) )  )

winter2 <- do.call(data.frame, winter2)
#table(t$TID)
winter2$mean_daily_range <- winter2$Mean_winter_temperature.max-winter2$Mean_winter_temperature.min
head(winter2)





# import moss depth
Soil_temperature <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                               sheet = "fullYear", col_types = c("numeric", 
                                                                 "numeric", "text", "text", "text", 
                                                                 "text", "date", "date", "date", "numeric", 
                                                                 "text","text", "numeric", "numeric", "numeric", 
                                                                 "text")) 
Soil_temperature$treatment[Soil_temperature$treatment=="Browsed Control"] <- "B"
Soil_temperature$treatment[Soil_temperature$treatment=="Exclosure"] <- "UB"

Soil_temperature$link2 <- paste(Soil_temperature$locationTrondelag, Soil_temperature$treatment, Soil_temperature$subplot)
winter2$link2 <- paste(winter2$TID, winter2$trt, winter2$subplot)
winter2$moss_depth <- Soil_temperature$Moss_depth_cm[match(winter2$link2, Soil_temperature$link2)]









# get winter means per plot
winter3 <- aggregate(data = winter2, 
                     cbind(Mean_winter_plot = Mean_winter_temperature.mn,
                           mean_daily_range_plot = mean_daily_range,
                           mean_moss_plot = moss_depth) ~ 
                       TID+
                       site+
                       trt+
                       link+
                       prod_class,
                     FUN = mean  )

winter3 <- do.call(data.frame, winter3)
#table(t$TID)
head(winter3)

# paste plot means into winter 2
winter2$plot_mean_winter <- winter3$Mean_winter_plot[match(winter2$link, winter3$link)]
winter2$plot_mean_range_winter <- winter3$mean_daily_range_plot[match(winter2$link, winter3$link)]
winter2$plot_mean_moss_winter <- winter3$mean_moss_plot[match(winter2$link, winter3$link)]

winter2$diff_in_mean <- winter2$Mean_winter_temperature.mn-winter2$plot_mean_winter
winter2$diff_in_range <- winter2$mean_daily_range-winter2$plot_mean_range_winter
winter2$diff_in_moss <- winter2$moss_depth-winter2$plot_mean_moss_winter

head(winter2)







par(mfrow=c(2,1), mar=c(5,6,5,2))
plot(winter2$diff_in_moss, winter2$diff_in_mean, 
     xlab = "Diff. from plot mean\nmoss depth (cm)", 
     ylab = expression("Diff. from plot mean\n soil temperature" ( degree~C)),
     main = "Mean soil temperature\n(jan-mar)",
     ylim = c(-1,1), pch = 20)
ablineclip(summary(lm(winter2$diff_in_mean~winter2$diff_in_moss)),
           x1 = min(winter2$diff_in_moss, na.rm = T), x2= max(winter2$diff_in_moss, na.rm = T))
text(expression(paste(beta, " = 0.035; R2 = 0.04; P = 0.03")), 
                x=0, y=0.8)
plot(winter2$diff_in_moss, winter2$diff_in_range, 
     xlab = "Diff. from plot mean\nmoss depth (cm)", 
     ylab= "Diff. from plot mean\n soil temperature fluctuation",
     main = "Diff. from plot mean\n soil temperature fluctuation",
     ylim = c(-1.2,1.2), pch = 20)
#abline(h=0)  

#summary(lm(winter2$diff_in_mean~winter2$diff_in_moss))
#summary(lm(winter2$diff_in_range~winter2$diff_in_moss))



p4 <-  ggplot(data = winter2, 
              aes(x = diff_in_moss, y = diff_in_mean))
  
p4 <- p4 + geom_point(shape = 1)

p4 <- p4 + geom_smooth(method=lm)

p4 <- p4 + xlab("") 
p4 <- p4 + ylab(expression(atop("Difference from plot mean", "winter soil temperature " ( degree~C))))
p4 <- p4 + theme(text = element_text(size=15)) 

p4 <- p4 + theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))
#p4 <- p4 + ggtitle(expression(atop("Mean soil temperature","(jan-mar)")))
p4 <- p4 + annotate("text", x=-2, y=0.6, 
                    label= "slope = 0.035\nR-sq = 0.04\np-value = 0.03")

p4.2 <-  ggplot(data = winter2, 
              aes(x = diff_in_moss, y = diff_in_range))

p4.2 <- p4.2 + geom_point(shape = 1)

p4.2 <- p4.2 + xlab("Difference from plot mean\nmoss depth (cm)") 
p4.2 <- p4.2 + ylab(expression(atop("Difference from plot mean", "winter soil temperature fluctuations")))
p4.2 <- p4.2 + theme(text = element_text(size=15)) 

p4.2 <- p4.2 + theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))
#p4.2 <- p4.2 + ggtitle(expression(atop("Mean soil temperature","fluctuations (jan-mar)")))


setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")
#pdf("Moss_winter_effect6.pdf",width=8,height=16)
tiff("Moss_winter_effect.tiff", height = 30, width = 15, units = "cm", res = 300)
multiplot(p4, p4.2, cols = 1)
dev.off()



#*****************************************************##

# Mean jan-march ####

#*****************************************************##
mean_winter <- aggregate(data = master6, 
                     cbind(Mean_winter_temperature = Temp) ~ 
                       trt + TID + prod_class,
                       FUN = function(x) c(mn = mean(x), max = max(x), min = min(x) )  )

mean_winter <- do.call(data.frame, mean_winter)
head(mean_winter)

mean_summer <- aggregate(data = master7, 
                         cbind(Mean_summer_temperature = Temp) ~ 
                           trt + TID + prod_class,
                         FUN = function(x) c(mn = mean(x), max = max(x), min = min(x) )  )

mean_summer <- do.call(data.frame, mean_summer)
head(mean_summer)

(pwinter <- ggplot(data = mean_winter, aes(x=trt, y=Mean_winter_temperature.mn))+
geom_boxplot()+
xlab("Treatment") +
ylab(expression(atop("Mean winter soil", "temperature " ( degree~C))))+
theme(text = element_text(size=15))+
    theme_classic()+
    scale_x_discrete(labels=c("B" = "Open plots", "UB" = "Exclosed")))

(psummer <- ggplot(data = mean_summer, aes(x=trt, y=Mean_summer_temperature.mn))+
    geom_boxplot()+
    xlab("Treatment") +
    ylab(expression(atop("Mean summer soil", "temperature " ( degree~C))))+
    theme(text = element_text(size=15))+
    theme_classic()+
    scale_x_discrete(labels=c("B" = "Open plots", "UB" = "Exclosed"))
    )


setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
tiff("mean_winter_and_summer_temperature.tiff",width=20,height=12, units = "cm", res = 300)
grid.arrange(pwinter, psummer, ncol = 2)
dev.off()



#***********************************************####

# SUMMER 2017 DATA ####

#*************************************************##

#Raw data:
#  Hourly soil temperature 
#  10 loggers per plot, one for each PVQ
#  Total 5 (productive) sites -> 100 loggers and 10 csv-files


#Questions I want to answer: 
#  1.	Can we extend the model from Kolstad et al 201x and quantify the indirect effect of moose on soil temperature?
#  2.	What are the main determinants of soil temperature?
#    a.	Canopy Cover
#    b.	Field layer biomass (or number of PI hits) as a measure of field layer cover
#    c.	Moss depth
#  3.	Where does the majority of the variation in soil temperature originate? Is it between sites, plots/treatments, or subplots?
#  4.	Is there spatial autocorrelation between subplots and for how long a distance is this detectable?
#  5.	What are the main determinants of daily soil temperature fluctuations?
#  6.	Is there a relationship between daily temperature fluctuations and plant diversity?
#  7.	Is there a relationship between soil temperature and plant functional types?
#  8.	Is there a relationship between temperature (and light, CCI) heterogeneity and plot level diversity?


# IMPORT raw data and compile
# Jump from here...####
setwd("M:\\Anders L Kolstad\\systherb data\\HOBOs\\HOBO_bulk_export\\export 2017")
temp = list.files(pattern="*.csv")
master <- read.csv(temp[1])
master <- master[,1:3]
colnames(master) <- c("Date", "Time", "Temp")
master$loggerID <- temp[1]


for (i in 2:length(temp)) {
  temp_file <- read.csv(temp[i])
  temp_file <- temp_file[,1:3]
  colnames(temp_file) <- c("Date", "Time", "Temp")
  temp_file$loggerID <- temp[i]
  master <- rbind(master, temp_file)
}
rm(temp_file)

length(unique(master$loggerID)) ## 99 loggers

master$Time <- as.POSIXct(master$Time, format="%H:%M", tz = "GMT")
master$Date <- as.Date(master$Date, format = c("%d.%m.%y"))





meta_data <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                        sheet = "summer2017")
master$loggerID <- as.numeric(str_sub(master$loggerID, start=1, end=8))
master2 <- merge(master, meta_data, by = "loggerID", all.x = T)

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
write.csv(master2, file = "summer2017_soilTempData.csv", row.names = F)
# 25kb



#  ... to here. ####

setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER")
dat <- read_csv("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/summer2017_soilTempData.csv")
dat <- data.frame(dat)
dat$Date <- as.Date(dat$Date, format = c("%Y.%m.%d"))
head(dat)
# This object is rather large



#*****************************************************##

# TRUNCATE ####

#*****************************************************##

duration <- as.Date(c("2017-06-09", "2017-08-06"), format = c("%Y-%m-%d"))


dat2 <- dat[dat$Date %between% duration,]

  
rm(dat,master,master2,meta_data,duration,i,temp)

dat2$f_loggerID <- factor(dat2$loggerID)
dat2$f_TRT <- factor(dat2$TRT)


# averages are added to this file:
temp_data <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                        sheet = "summer2017")

temp_data$TRT <- factor(temp_data$TRT)
temp_data$TID <- factor(temp_data$TID)
temp_data$avg_CCI <- as.numeric(temp_data$avg_CCI)
temp_data$avg_moss_depth_cm <- as.numeric(temp_data$avg_moss_depth_cm)
temp_data$avg_temp <- as.numeric(temp_data$avg_temp)
temp_data$avg_daily_temp_range <- as.numeric(temp_data$avg_daily_temp_range)


#*****************************************************##

# Plot means  ####

#*****************************************************##
temp2 <- aggregate(data = temp_data,
                   cbind(CCI_plot = avg_CCI,
                         temp_plot = avg_temp,
                         range_plot = avg_daily_temp_range,
                         moss_plot = avg_moss_depth_cm)~
                     TID+TRT,
                   FUN = mean)
temp2$link <- paste(temp2$TID, temp2$TRT, sep="")
temp_data$link <- paste(temp_data$TID, temp_data$TRT, sep="")
temp3 <- merge(temp_data, temp2[,3:7], by = "link", all.x = T)

# calculate differences
temp3$temp_diff <- temp3$avg_temp-temp3$temp_plot
temp3$range_diff <- temp3$avg_daily_temp_range-temp3$range_plot
temp3$moss_diff <- temp3$avg_moss_depth_cm-temp3$moss_plot
temp3$CCI_diff <- temp3$avg_CCI-temp3$CCI_plot


# Exploration
plot(temp3$avg_temp)
plot(temp3$avg_CCI)
plot(temp3$avg_moss_depth_cm)
plot(temp3$avg_daily_temp_range)


plot(temp3$avg_temp,temp3$TID)
plot(temp3$avg_CCI,temp3$TID)
plot(temp3$avg_moss_depth_cm,temp3$TID)
plot(temp3$avg_daily_temp_range,temp3$TID)


table(temp3$LocalityName3)
table(temp3$TRT)


#*****************************************************##

# QC  ####

#*****************************************************##
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")

dat3 <- aggregate(data = dat2,
                                    Temp~f_loggerID+Date,
                                    FUN = mean)



#pdf("soilTemp_QC7.pdf")  
#for (cat in unique(dat3$f_loggerID)){
#  d <- subset(dat3, f_loggerID == cat)
#  
#  p <-  ggplot(data = d, 
#               aes(x = Date, 
#                   y = Temp)) 
#  p <- p+   geom_line()   
#  p <- p + xlab("Date") 
#  p <- p + ylab("Soil temperature")
#  p <- p + theme(text = element_text(size=15))
#  p <- p + ggtitle(cat)
#  print(p)
#}
#dev.off()
# Looks good

# END QC ####

#*****************************************************##







#*****************************************************##

# Daily fluctuations  ####

#*****************************************************##

# a quick plot...:

dat4 <- aggregate(data = dat2,
                  Temp~Time+f_TRT,
                  FUN = mean)





p_daily <- ggplot(data = dat4, aes(x=Time, y=Temp,
                                   group = f_TRT,
                                   colour = f_TRT))+
  geom_line(size = 2)+
  ylab("")+
  scale_x_datetime(date_breaks = "6 hour",
                 date_labels = "%H:%M")+
  scale_colour_discrete(name="",
                        breaks=c("B", "UB"),
                        labels=c("Open plots", "Exclosure"))+
  theme(text = element_text(size=15))
p_daily

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")
#pdf("Daily Summer Soil Temp Fluctuations.pdf") 
#tiff("Daily Summer Soil Temp Fluctuations.tiff", height = 15, width = 15, units = "cm", res = 300)
p_daily
#dev.off()
# no point having error ribbons because there's a temporal trend with 
# larger variation than the daily fluctuations






# Getting avg moss depth :
# JUMP from here ... ####
dat4.2 <- aggregate(data = dat2,
                  Temp~
                    Date+
                    TID+
                    f_TRT+
                    Subplot,
                  FUN = function(x) c(min = min(x), max = max(x) ))
dat4.3 <- do.call(data.frame, dat4.2)
dat4.3$daily_range <- dat4.3$Temp.max-dat4.3$Temp.min 
dat4.4 <- aggregate(data = dat4.3,
                    cbind(mean_daily_range = daily_range)~
                      TID+
                      f_TRT+
                      Subplot,
                    FUN = mean)
head(dat4.4)
meta_data <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                        sheet = "summer2017")
meta_data$link <- paste(meta_data$TID, meta_data$TRT, meta_data$Subplot, sep="_")
dat4.4$link <- paste(dat4.4$TID, dat4.4$f_TRT, dat4.4$Subplot, sep="_")
dat4.4$moss <- meta_data$avg_moss_depth_cm[match(dat4.4$link, meta_data$link)]

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
write.csv(dat4.4, file = "temp_range.csv", row.names = F)
# the average daily soil temp fluctuations is now added to the meta data file


# ... to here####







#*****************************************************##

# Difference in mean soil temperature  ####

#*****************************************************##
#dat5 <- aggregate(data = dat2,
#                  Temp~
#                    f_loggerID+Subplot+
#                    TID+
#                    TRT,
#                  FUN = mean)

#setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#write.csv(dat5, file = "mean_temp.csv", row.names = F)
# the average daily soil temp  is now added to the meta data file



# SPAGETTI PLOT

dat5.2 <- aggregate(data = temp_data,
                  avg_temp~
                    TID+
                    TRT,
                  FUN = mean)

p_mean <- ggplot(data= dat5.2, aes(x=TRT, y= avg_temp, group = TID))+
  geom_line(size = 2)+
  xlab("") +
  ylab(expression(atop("Mean summer soil", "temperature " ( degree~C))))+
  theme(text = element_text(size=15)) +
  annotate("text", x=2.1, y=c(11.7, 11.2, 11, 10,9.8), 
           label= c("12", "8", "1", "6", "14"))+
  scale_x_discrete(labels=c("B" = "Open plots", "UB" = "Exclosure"))
p_mean




# LINE GRAPH

dat6 <- aggregate(data = dat2,
                  Temp~
                    f_loggerID+Subplot+
                    TID+
                    TRT+
                    Date,
                  FUN = mean)
dat6.2 <- aggregate(data = dat6,
                  Temp~
                    TID+
                    TRT+
                    Date,
                  FUN = mean)
dat6.3 <- aggregate(data = dat6.2,
                    Temp~
                      TRT+
                      Date,
                    FUN = function(x) c(mn = mean(x), length = length(x), SD = sd(x) ))

dat6.3 <- do.call(data.frame, dat6.3)


p_line <- ggplot(data= dat6.3, aes(x=Date, y= Temp.mn, group = TRT, colour = TRT))+
  geom_line(size = 2)+
  xlab("Date") +
  ylab(expression(atop("Mean daily soil", "temperature " ( degree~C))))+
  theme(text = element_text(size=15)) +
  theme(legend.position="none")
  
p_line



setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")
#pdf("Mean soil temp summer2017_5.pdf")
#multiplot(p_mean, p_line, p_daily, cols = 2)
tiff("Mean soil temp summer2017.tiff", height = 20, width = 24, units = "cm", res=300)
grid.arrange(p_line, p_mean, p_daily, 
             ncol =2, nrow=2,
             layout_matrix = rbind(c(1,1), c(2,3)))
dev.off()










#*************************************************##

# CCI and moss depth ####

#*************************************************##

# -------------------###
# adding moss depth and CCI to the dataset
# Jump from here.....####

# aggregating moss depth and pasting it into the meata data excel file:
moss_depths <- read_csv("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/moss_depths.csv")
moss_depths <- data.frame(moss_depths)
moss_depths$f_LocalityCode <- factor(moss_depths$LocalityCode)
moss_depths$f_LocalityName <- factor(moss_depths$LocalityName)
moss_depths$TRT <- factor(moss_depths$Treatment)
moss_depths$f_plot <- factor(moss_depths$Plot)
levels(moss_depths$f_LocalityName)

moss1 <- aggregate(data=moss_depths,
                   cbind(Moss_depth = Bryophyte.height)~
                     f_LocalityName+
                     TRT+
                     f_plot,
                   FUN = mean)

plot(moss1$Moss_depth)

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#write.csv(moss1, file = "moss.csv", row.names = F)
# the average moss depth is now added to the meta data file


# Import canopy cover data
CCI <- read_delim("M:/Anders L Kolstad/systherb data/exported cvs/CanopyCover.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

# Housekeeping
CCI$Plot <- factor(CCI$Plot)
CCI$Treatment <- factor(CCI$Treatment)
CCI <- CCI[CCI$Plot!= "Experiment area",]




# The site names don't match...
dat2$LocalityName3 <- factor(dat2$LocalityName2)
levels(dat2$LocalityName3)
dat2$LocalityName3 <- revalue(dat2$LocalityName3, 
                              c("Bjoellaaa"="verdal_2VB",
                                "Bratsberg"="Bratsberg",
                                "Kalddalsbekken"="namdalseid_1kub",
                                "Seterdalsveien"="Sl_Tydal",
                                "Slindsvann"="Selbu_Sl"))





CCI$link <- paste(CCI$LocalityName, CCI$Treatment, CCI$Plot, sep="_")
dat2$link <- paste(dat2$LocalityName3, dat2$TRT, dat2$Subplot, sep="_")
CCI$link[1:5]
dat2$link[1:5]

dat5 <- aggregate(data = dat2,
                  Temp~
                    f_loggerID+Subplot+link+LocalityName3+
                    TID+
                    TRT,
                  FUN = mean)
CCI2 <- aggregate(data = CCI,
                  CanopyCoverIndex~
                    LocalityName+
                    Treatment+
                    Plot+link,
                  FUN = mean)
CCI2$LocalityName <- factor(CCI2$LocalityName)

CCI3 <- subset(CCI2, LocalityName== "verdal_2VB" |
                 LocalityName== "Bratsberg" |
                 LocalityName== "namdalseid_1kub" |
                 LocalityName== "Sl_Tydal"      |
                 LocalityName==  "Selbu_Sl" )
dat5$CCI <- CCI3$CanopyCoverIndex[match(dat5$link, CCI3$link)]


# two NA's, no problem
#setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#write.csv(CCI3, file = "CCI.csv", row.names = F)
# the average CCI is now added to the meta data file


# ....To here ! ####




#  plot it
plot(temp3$CCI_diff, temp3$temp_diff)
plot(temp3$moss_diff, temp3$temp_diff)
plot(temp3$moss_diff, temp3$CCI_diff)

p_CCI_temp <- ggplot(data = temp3,
                     aes(x=CCI_diff, y=temp_diff))+
  geom_point(shape = 1)+
  geom_smooth(method = 'lm')+
  xlab("Difference from plot mean\ncanopy cover index") +
  ylab(expression(atop("Difference from plot mean", "soil temperature " ( degree~C))))+
  theme(text = element_text(size=15)) +
  annotate("text", x=-30, y=-1.2, 
           label= "slope = -0.013\nR-sq = 0.29\np-value < 0.001")

p_CCI_temp
summary(lm(dat5$temp_diff~dat5$CCI_diff))


p_moss_temp <- ggplot(data = temp3,
                     aes(x=moss_diff, y=temp_diff))+
  geom_point(shape = 1)+
  xlab("Difference from plot mean\nmoss depth (cm)") +
  ylab(expression(atop("Difference from plot mean", "soil temperature " ( degree~C))))+
  theme(text = element_text(size=15))
p_moss_temp

summary(lm(dat5$temp_diff~dat5$moss_diff))


setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#pdf("CCI and moss effect on soil temperature_2.pdf",width=5,height=10)
tiff("CCI and moss effect on soil temperature.tiff", height = 20, width = 20, units = "cm", res=300)
multiplot(p_CCI_temp, p_moss_temp, cols = 1)
dev.off()


# plot the same agains temp fluctuations

p_CCI_tempF <- ggplot(data = temp3,
                     aes(x=CCI_diff, y=range_diff))+
  geom_point(shape = 1)+
  geom_smooth(method = 'lm')+
  xlab("Difference from plot mean\ncanopy cover index") +
  ylab(expression(atop("Difference from plot mean", "soil temperature fluctuations " ( degree~C))))+
  theme(text = element_text(size=15)) +
  annotate("text", x=-30, y=-0.8, 
           label= "Beta = -0.001\nR2 = 0.16\nP < 0.001")

p_CCI_tempF
summary(lm(temp3$range_diff~temp3$CCI_diff))
# shaded plots are less variable

p_moss_tempF <- ggplot(data = temp3,
                      aes(x=moss_diff, y=range_diff))+
  geom_point(shape = 1)+
  xlab("Difference from plot mean\nmoss depth (cm)") +
  ylab(expression(atop("Difference from plot mean", "soil temperature fluctuations " ( degree~C))))+
  theme(text = element_text(size=15))
p_moss_tempF

summary(lm(temp3$range_diff~temp3$moss_diff))


setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
pdf("CCI and moss effect on soil temperature fluctuations.pdf",width=5,height=10)
multiplot(p_CCI_tempF, p_moss_tempF, cols = 1)
dev.off()



# variogram ####
varDat <- select(temp_data,
                 TID, TRT, Subplot, avg_temp, x, y)
head(varDat)

loc <- unique(varDat$TID)


# Browsed first:
Browsed <- filter(varDat, TRT == "B")

master <- Browsed[Browsed$TID == loc[1],]
master <- na.omit(master)
coors <- as.matrix(master[,c("x", "y")])
distance <- dist(coors, method = "euclidian")
distance2 <- as.matrix(distance)
distance3 <- melt(distance2)


dat <- master$avg_temp
distance_var <- dist(dat, method = "euclidian")
distance_var2 <- as.matrix(distance_var)
distance_var3 <- melt(distance_var2)
df <- data.frame(distance = distance3$value,
                 temp_distance = distance_var3$value)
df <- filter (df, distance != 0)
plot(df$distance, df$temp_distance)


for(i in 2:length(loc)){
  temp <- Browsed[Browsed$TID == loc[i],]
  temp <- na.omit(temp)
  temp_coors <- as.matrix(temp[,c("x", "y")])
  temp_distance <- dist(temp_coors, method = "euclidian")
  temp_distance2 <- as.matrix(temp_distance)
  temp_distance3 <- melt(temp_distance2)
  
  temp_dat <- temp$avg_temp
  temp_distance_var <- dist(temp_dat, method = "euclidian")
  temp_distance_var2 <- as.matrix(temp_distance_var)
  temp_distance_var3 <- melt(temp_distance_var2)
  temp_df <- data.frame(distance = temp_distance3$value,
                   temp_distance = temp_distance_var3$value)
  temp_df <- filter (temp_df, distance != 0)
  df <- rbind(df, temp_df)
}

plot(df$distance, df$temp_distance)

# Then the Un-browsed
UNBrowsed <- filter(varDat, TRT == "UB")

master <- UNBrowsed[UNBrowsed$TID == loc[1],]
master <- na.omit(master)
coors <- as.matrix(master[,c("x", "y")])
distance <- dist(coors, method = "euclidian")
distance2 <- as.matrix(distance)
distance3 <- melt(distance2)


dat <- master$avg_temp
distance_var <- dist(dat, method = "euclidian")
distance_var2 <- as.matrix(distance_var)
distance_var3 <- melt(distance_var2)
df2 <- data.frame(distance = distance3$value,
                 temp_distance = distance_var3$value)
df2 <- filter (df2, distance != 0)
plot(df2$distance, df2$temp_distance)


for(i in 2:length(loc)){
  temp <- UNBrowsed[UNBrowsed$TID == loc[i],]
  temp <- na.omit(temp)
  temp_coors <- as.matrix(temp[,c("x", "y")])
  temp_distance <- dist(temp_coors, method = "euclidian")
  temp_distance2 <- as.matrix(temp_distance)
  temp_distance3 <- melt(temp_distance2)
  
  temp_dat <- temp$avg_temp
  temp_distance_var <- dist(temp_dat, method = "euclidian")
  temp_distance_var2 <- as.matrix(temp_distance_var)
  temp_distance_var3 <- melt(temp_distance_var2)
  temp_df <- data.frame(distance = temp_distance3$value,
                        temp_distance = temp_distance_var3$value)
  temp_df <- filter (temp_df, distance != 0)
  df2 <- rbind(df2, temp_df)
}
plot(df2$distance, df2$temp_distance)










# Marginal histograms ####



library(ggExtra)

theme_set(theme_bw())  # pre-set the bw theme.

sp_cor_B <- ggplot(df, aes(x = distance,
                          y = temp_distance)) + 
        geom_count() + 
        geom_smooth(method="loess", span = 0.2)+
        xlab("") +
        ylab(expression(atop("Pairwise absolute difference", "in soil temperature " ( degree~C))))+
        theme(text = element_text(size=15))+
        theme(legend.position="none")+
        xlim(0, 23)+
        annotate("text", x = 20, y = 2.5, label = "Open plots", size = 10)
sp_cor_UB <- ggplot(df2, aes(x = distance,
                          y = temp_distance)) + 
  geom_count() + 
  geom_smooth(method="loess", span = 0.2)+
  xlab("Distance (m)") +
  ylab(expression(atop("Pairwise absolute difference", "in soil temperature " ( degree~C))))+
  theme(text = element_text(size=15))+
  theme(legend.position="none")+
  xlim(0, 23)    +
  annotate("text", x = 20, y = 2.5, label = "Exclosure", size = 10)
        


B <- ggMarginal(sp_cor_B, type = "histogram", fill="transparent")
UB <- ggMarginal(sp_cor_UB, type = "histogram", fill="transparent")

setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#ggsave(filename = "spatial_autocorrelation.tiff", grid.arrange(B, UB, nrow = 2), 
#       height = 20, width = 24, units = "cm")


# alternative plots
box_B <- ggplot(df, aes(x = distance, y = temp_distance, group = cut_width(distance, 1))) + 
  geom_boxplot() + 
  xlab("Distance (m)") +
  ylab(expression(atop("Pairwise absolute difference", "in soil temperature " ( degree~C))))+
  theme(text = element_text(size=15))+
  theme(legend.position="none")
box_UB <- ggplot(df2, aes(x = distance, y = temp_distance, group = cut_width(distance, 1))) + 
  geom_boxplot() + 
    xlab("Distance (m)") +
  ylab(expression(atop("Pairwise absolute difference", "in soil temperature " ( degree~C))))+
  theme(text = element_text(size=15))+
  theme(legend.position="none")
grid.arrange(box_B, box_UB, nrow = 2)






#*************************************************####

# Creating SEM dataset ####

#*************************************************##

# get dataset (downloaded from SustHerb server 19.12.2017). Mosses and trees removed.
# The values are point intercept averagee frequencies after 16 pins in a 50x50 cm frame.

setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER")
PInt <- read_csv("PI_data.csv")

names(PInt)
#str(PInt)


# many absent species show as characters. Turning these into numeric so that it's easier to exlude them
# based on colSums:
to_be_numbers <- c(9:ncol(PInt))
PInt[,to_be_numbers] <- as.numeric(as.character(unlist(PInt[,to_be_numbers])))
rowSums(PInt[,9:ncol(PInt)], na.rm = T)
# some zeros.
#View(PInt[rowSums(PInt[,9:ncol(PInt)], na.rm = T) == 0,])
# Bjøllåa B plot 5 and Slindsvann UB plot 7 were not found in 2016 so data exists
# removing those rows:
PInt <- PInt[rowSums(PInt[,9:ncol(PInt)], na.rm = T) > 0,]


# Balance?
table(PInt$Treatment, PInt$LocalityName2)
# good


# remove some species
colSums(PInt[,9:ncol(PInt)], na.rm=T)
PInt <- PInt[,  c(rep(TRUE, times = 8),    # keep the first 8 columns 
                       colSums(PInt[,9:ncol(PInt)], na.rm=T) > 0)]     # remove comlumns with no values

colnames(PInt)

BLS_taxa <- c("Vaccinium myrtillus",
              "Vaccinium vitis-idaea")
NLS_taxa <- c("Calluna vulgaris",
              "Empetrum nigrum")
TH_taxa <- c("Aqonitium lycoctonium",
             "Athyrium filix-femina",
             "Blechnum spicant",                  #check
             "Dryopteris expansa",
             "Epilobium angustifolium",
             "Filipendula ulmaria",
             "Geranium sylvaticum",
             "Geum rivale",
             "Phegopteris connectilis",       # check
             "Ranunculus repens",              # very variable size
             "Rubus idaeus",
             "Veronica officinalis")          # small, but quite woody
SH_taxa <- c("Alchemilla sp_",
             "Anemone nemorosa",
             "Chamaepericlymenum suecicum",
             "Epilobium sp_",
             "Goodyera repens",
             "Gymnocarpium dryopteris",
             "Linnaea borealis",
             "Maianthemum bifolium",
             "Melampyrum pratense",
             "Melampyrum sylvaticum",
             "Oxalis acetosella",
             "Potentilla erecta",
             "Rubus saxatilis",
             "Taraxacum officinale",
             "Trientalis europaea",
             "Viola sp_")
BLG_taxa <- c("Agrostis capillaris" ,
              "Anthoxanthum odoratum",
              "Deschampsia cespitosa",
              "Festuca sp",
              "Gras 1",
              "Luzula pilosa",
              "Poaceae"              )
NLG_taxa <- c("Avenella flexuosa")
                      
                       
PI_BM <- PInt   
# check for zeros. They should be NAs:
PI_BM[PI_BM == 0] <- NA
PI_BM[,9:ncol(PI_BM)] <- PI_BM[,9:ncol(PI_BM)]/16   # standardising the intercept frequency

# calculating total intercept frequency
PI_BM$tFREQ <- rowSums(PI_BM[,9:ncol(PI_BM)], na.rm=T)
  
# calculating total biomass
PI_BM[, BLS_taxa] <- PI_BM[, BLS_taxa]  * 74.4101   + 1.2857
PI_BM[, NLS_taxa] <- PI_BM[, NLS_taxa]  * 74.184  + 9.2471
PI_BM[, TH_taxa]  <- PI_BM[, TH_taxa]   * 36.4849 + 4.0373
PI_BM[, SH_taxa]  <- PI_BM[, SH_taxa]   * 15.1209 + 0.7713
PI_BM[, BLG_taxa] <- PI_BM[, BLG_taxa]  * 23.3885   + 0.6384
PI_BM[, NLG_taxa] <- PI_BM[, NLG_taxa]  * 5.9653  + 0.8747
PI_BM$tBiomass <- rowSums(PI_BM[,9:ncol(PI_BM)], na.rm=T)

# calculating biomass for some groups that I aim to include in the SEM
PI_BM$shrubBiomass <- rowSums(PI_BM[,c(BLS_taxa, NLS_taxa)], na.rm=T)
PI_BM$thBiomass <- rowSums(PI_BM[,TH_taxa], na.rm=T)
PI_BM$shBiomass <- rowSums(PI_BM[,SH_taxa], na.rm=T)
PI_BM$graminoidBiomass <- rowSums(PI_BM[,c(NLG_taxa, BLG_taxa)], na.rm=T)


PI_BM$uniquePlot <- paste0(PI_BM$LocalityName2, PI_BM$Treatment, PI_BM$Plot)

SEMvars <- c("tFREQ", "tBiomass", "shrubBiomass", "thBiomass", "shBiomass", "graminoidBiomass")
source("M:/Anders L Kolstad/HIGHSTATS/AllRCode/HighstatLibV10.R")
Mypairs(PI_BM[, SEMvars])
# tFREQ is correlated to grasses (and small herbs) 
# and Biomass is corelated to shrubs (and tall herbs).
# tFREQ and Biomass are only weakly correlated.
# To best describe field layer canopy cover i propose
# a compound index using tFREQ and Biomass:
PI_BM$tFREQ_scaled <- scale(PI_BM$tFREQ)
PI_BM$tBiomass_scaled <- scale(PI_BM$tBiomass)
PI_BM$UnderstoryCoverIndex <- scale(PI_BM$tFREQ_scaled+PI_BM$tBiomass_scaled)

SEMvars2 <- c("UnderstoryCoverIndex", "tBiomass", "tFREQ", "shrubBiomass", "thBiomass", "shBiomass", "graminoidBiomass")
Mypairs(PI_BM[, SEMvars2])
# equal correlation between tFREQ and tBiomass!



# calculating Simpson's index:
library(vegan)
names(PI_BM)
PI_BM[is.na(PI_BM)] <- 0
PI_BM$Simpsons_vasc <- diversity(PI_BM[,9:48], index = "simpson")
summary(PI_BM$Simpsons_vasc) # no one's, that good
# plots with index of 0 are plots with only 1 species
# Obs, this Simpsons index here dont include mosses (which can only have one hit per pin)








# ++++++++++++++++++++++++++++++++++++++ ##

# **get SPECIES RICHNESS: ####
# This dataset includes 15 sites, year 2016 only, 
# species recorded by point intercept,
# and total species per plot (observed)

# ++++++++++++++++++++++++++++++++++++++ ##


SR_dat <- read.csv("speciesRichness_data.csv")

# removing all but the five sites with temperature data:
#table(SR_dat$LocalityName)
#unique(PI_BM$LocalityName)
MySites <- unique(PI_BM$LocalityName)
SR_dat2 <- filter(SR_dat, LocalityName %in% MySites)
SR_dat2$LocalityName <- factor(SR_dat2$LocalityName)
table(SR_dat2$LocalityName)


# remove some species that have no observations
table(colSums(SR_dat2[,7:ncol(SR_dat2)], na.rm=T))  # 41 species
SR_dat2 <- SR_dat2[,  c(rep(TRUE, times = 6),    # keep the first 6 columns 
                  colSums(SR_dat2[,7:ncol(SR_dat2)], na.rm=T) > 0)]     # remove comlumns with no values


# turn species columns into numbers
to_be_numbers <- c(7:ncol(SR_dat2))
SR_dat2[,to_be_numbers] <- as.numeric(as.character(unlist(SR_dat2[,to_be_numbers])))
table(rowSums(SR_dat2[,7:ncol(SR_dat2)], na.rm = T)) # two empty rows
#View(SR_dat2[rowSums(SR_dat2[,7:ncol(SR_dat2)], na.rm = T) == 0,])
# Slindsvann UB 7 and 2VBB 5 was not found in 2016 so thats ok
# removing these rows
SR_dat2 <- SR_dat2[rowSums(SR_dat2[,7:ncol(SR_dat2)], na.rm = T) > 0,]

# make site names comparable to SEMdat dataset
SR_dat2$LocalityName2[SR_dat2$LocalityName ==  "namdalseid_1kub"] <- "Kalddalsbekken" 
SR_dat2$LocalityName2[SR_dat2$LocalityName ==  "verdal_2VB"] <-       "Bjoellaaa"
SR_dat2$LocalityName2[SR_dat2$LocalityName ==  "Selbu_Sl"] <-          "Slindsvann"
SR_dat2$LocalityName2[SR_dat2$LocalityName ==  "Sl_Tydal"] <-       "Seterdalsveien"
SR_dat2$LocalityName2[SR_dat2$LocalityName ==  "Bratsberg"] <-       "Bratsberg"


# remove some columns that are not vascular plant species
SR_dat2 <- select(SR_dat2,
               -Bare_ground, 
               -Bare_ground_branch, 
               #-Bare_ground_stone,
               -Bryophyta,
               -Cow_shit,
               -Lichens,
               -Litter,
               #-No_occurrence,
               -Sphagnum_sp)
               #-Stone)



# summing species per row (ncluding trees:
names(SR_dat2) # end with Viola_sp_
SR_dat2$total_SpeciesRichness <- rowSums(SR_dat2[,7:88] > 0, na.rm=T)

# getting seprate SR for mooses and vascular plants:
names(SR_dat2)
mosses <- c("Cirriphyllum_piliferum",
                "Dicranum_sp",
               "Hylocomiastrum_umbratum",
               "Hylocomium_splendens",
               "Marchantiophyta",
               "Plagiochila_asplenioides",
               "Plagiomnium_ellipticum",
               "Plagiomnium_undulatum",
               "Plagiothecium_laetum_curvifolium",
               "Plagiothecium_undulatum",
               "Pleurozium_schreberi",
               "Polytrichum_astrum",
               "Ptilidium_ciliare",
               "Ptilium_crista_castrensis",
               "Rhodobryum_roseum",
               "Rhytidiadelphus_loreus",
               "Rhytidiadelphus_squarrosus_subpinnatus",
               "Rhytidiadelphus_triquetrus",
               "Sciuro_hypnum_reflexum",
               "Sciuro_hypnum_starkei")

SR_dat2$moss_SpeciesRichness <- rowSums(SR_dat2[,mosses] > 0, na.rm=T)
SR_dat2$vasc_SpeciesRichness <- SR_dat2$total_SpeciesRichness-SR_dat2$moss_SpeciesRichness
SR_dat2[is.na(SR_dat2)] <- 0  # NA need to be zero for 'diversity' to work
SR_dat2$Simpsons_moss <- diversity(SR_dat2[,mosses], index = "simpson")
plot(SR_dat2$Simpsons_moss, SR_dat2$moss_SpeciesRichness)
# by artifact, if there are no moss species, the simpsons index = 1, which is wrong.
SR_dat2$Simpsons_moss[SR_dat2$Simpsons_moss == 1] <- 0
summary(SR_dat2$Simpsons_moss)

View(SR_dat2[SR_dat2$LocalityCode == "1KB" & SR_dat2$Plot == 2,])
# apparantly point intercept of vascular plants and mosses appear on seperate lines.


SR_dat2$uniquePlot <- as.factor(paste0(SR_dat2$LocalityName2, SR_dat2$Treatment, SR_dat2$Plot))
names(SR_dat2)

SR_dat3 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "total_SpeciesRichness", 
                 fun.aggregate = sum)
SR_dat4 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "moss_SpeciesRichness", 
                 fun.aggregate = sum)
SR_dat5 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "vasc_SpeciesRichness", 
                 fun.aggregate = sum)
SR_dat6 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "Simpsons_moss", 
                 fun.aggregate = sum)

SR_dat3$total_SR <- SR_dat3$Observed+SR_dat3$Point_Intercept
SR_dat3$moss_SR <- SR_dat4$Observed+SR_dat4$Point_Intercept
SR_dat3$vasc_SR <- SR_dat5$Observed+SR_dat5$Point_Intercept
SR_dat3$Simpsons_moss <- SR_dat6$Observed+SR_dat6$Point_Intercept


# SEM DATASET ##
SEMdat <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                     sheet = "summer2017")
SEMdat$uniquePlot <- paste0(SEMdat$LocalityName2, SEMdat$TRT, SEMdat$Subplot)
SEMdat$UCI <- PI_BM$UnderstoryCoverIndex[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$simpsons_vasc <- PI_BM$Simpsons_vasc[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$simpsons_moss <- SR_dat3$Simpsons_moss[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]

SEMdat$total_SR <- SR_dat3$total_SR[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$moss_SR <- SR_dat3$moss_SR[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$vasc_SR <- SR_dat3$vasc_SR[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$shrubBM <- PI_BM$shrubBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$thBM <- PI_BM$thBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$shBM <- PI_BM$shBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$gramBM <- PI_BM$graminoidBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$avenellaBM <- PI_BM$`Avenella flexuosa`[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]

setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER")
write.csv(SEMdat, file = "SEMdat.csv", row.names = F)



#*************************************************####

# SEM START ####

#*************************************************##
setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER")
SEMdat <- read.csv("SEMdat.csv")

#housekeeping
#str(SEMdat)
SEMdat$Treatment <- factor(SEMdat$TRT)
levels(SEMdat$Treatment) <- c("Open plots", "Exclosed")
SEMdat$CCI <- as.numeric(SEMdat$avg_CCI)
SEMdat$Moss_depth <- as.numeric(SEMdat$avg_moss_depth_cm)
SEMdat$Soil_temp <- as.numeric(SEMdat$avg_temp)
SEMdat$Soil_temp_range <- as.numeric(SEMdat$avg_daily_temp_range)
#SEMdat$Biomass <- SEMdat$biomass
#SEMdat$Vegetation_height <- SEMdat$vegHeight


# Exploration
summary(SEMdat$UCI)
hist(SEMdat$UCI)
plot(SEMdat$UCI)
Boxplot(SEMdat$UCI~ SEMdat$TRT)  

summary(SEMdat$simpsons_vasc)
plot(SEMdat$simpsons_vasc)
hist(SEMdat$simpsons_vasc)
Boxplot(SEMdat$simpsons_vasc~SEMdat$TRT)
t.test(SEMdat$simpsons_vasc~SEMdat$TRT)  # Open plots less diverse in vascular plants

summary(SEMdat$simpsons_moss)
plot(SEMdat$simpsons_moss)
hist(SEMdat$simpsons_moss)
Boxplot(SEMdat$simpsons_moss~SEMdat$TRT)
t.test(SEMdat$simpsons_moss~SEMdat$TRT)  

summary(SEMdat$total_SR)
plot(SEMdat$total_SR)
hist(SEMdat$total_SR)
Boxplot(SEMdat$total_SR~SEMdat$TRT)
t.test(SEMdat$total_SR~SEMdat$TRT)

summary(SEMdat$moss_SR)
plot(SEMdat$moss_SR)
hist(SEMdat$moss_SR)
Boxplot(SEMdat$moss_SR~SEMdat$TRT)
t.test(SEMdat$moss_SR~SEMdat$TRT)

summary(SEMdat$vasc_SR)
plot(SEMdat$vasc_SR)
hist(SEMdat$vasc_SR)
Boxplot(SEMdat$vasc_SR~SEMdat$TRT)
t.test(SEMdat$vasc_SR~SEMdat$TRT)

summary(SEMdat$shrubBM)
plot(SEMdat$shrubBM)
hist(SEMdat$shrubBM)
Boxplot(SEMdat$shrubBM~SEMdat$TRT)


summary(SEMdat$thBM)
plot(SEMdat$thBM)
hist(SEMdat$thBM)
Boxplot(SEMdat$thBM~SEMdat$TRT)
# big difference

summary(SEMdat$shBM)
plot(SEMdat$shBM)
hist(SEMdat$shBM)
Boxplot(SEMdat$shBM~SEMdat$TRT)

summary(SEMdat$gramBM)
plot(SEMdat$gramBM)
hist(SEMdat$gramBM)
Boxplot(SEMdat$gramBM~SEMdat$Treatment)
# more grasses in the Open plots

summary(SEMdat$avenellaBM)
plot(SEMdat$avenellaBM)  # most plots have it
hist(SEMdat$avenellaBM)
Boxplot(SEMdat$avenellaBM~SEMdat$Treatment)


# PAIRS ####
source("M:/Anders L Kolstad/HIGHSTATS/AllRCode/HighstatLibV10.R")  
names(SEMdat)
MyVars <- c("CCI", "Moss_depth", 
            "Soil_temp", 
            "UCI", "Treatment")
Mypairs(SEMdat[,MyVars])

MyVars2 <- c("simpsons_vasc", "simpsons_moss", 
             "total_SR", "moss_SR", "vasc_SR")
Mypairs(SEMdat[,MyVars2])

MyVars3 <- c("moss_SR", "vasc_SR", "total_SR",
             "gramBM", "avenellaBM", "shrubBM", "thBM", "shBM")
Mypairs(SEMdat[,MyVars3])

MyVars4 <- c("simpsons_moss", "simpsons_vasc",
             "gramBM", "avenellaBM", "shrubBM", "thBM", "shBM")
Mypairs(SEMdat[,MyVars4])
# moss diversity is not explained. 
# vasc diversity differs between richness and simpsons, 
# but is negatively correlated with shrubs and positivly correlated with short herbs. 
# Not affected by grasses as hypothesised (but looks non-linear). Neither all grasses or Avenella alone.
# All grasses and Avenella work differently (different signs)


MyVars5 <- c("CCI", "Moss_depth", "Soil_temp", "UCI",
               "gramBM", "avenellaBM", "shrubBM", "thBM", "shBM")
Mypairs(SEMdat[,MyVars5])
# Avenella like warm soil and no moss (non-linear).
# Problem about correation between UCI and vegetation biomass

MyVars6 <- c("CCI", "Moss_depth", "Soil_temp", "UCI",
             "simpsons_vasc", "simpsons_moss", "total_SR", "moss_SR", "vasc_SR")
Mypairs(SEMdat[,MyVars6])
# nothing

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
#tiff("plots/pairPlot.tiff", height = 35, width = 35, units = "cm", res=300)
#Mypairs(SEMdat[,MyVars])
#dev.off()
# Treatment is strongest on CCI, temp, vegetation heigh. 
# Not present on moss depth or biomass
# CCI and temp are negatively correlated, quite linear (beta, zero-inlated)





# Conseptual model ####
library(lavaan)
library(semPlot)
names(SEMdat)
conMod <-   'CCI ~ Treatment
            Moss_depth ~ Treatment
            Biomass ~ Treatment
            Vegetation_height ~ Treatment 
            Soil_temp ~ Vegetation_height+CCI+Biomass+Moss_depth'
conMod_fit <- sem(conMod, SEMdat)   # dont care about scaling

# QUICK LOOK
#semPaths(conMod_fit)

# FIND THE ORDER OF THE NODES TO ALLOW CUSTUM LAYOUT
#semPaths(conMod_fit,what="std",nodeLabels=letters[1:6],edgeLabels=1:12,edge.label.cex=1.5,fade=FALSE)

vert <- matrix(c(-1,  0,    # CCI
               -0.4 , 0,       # Moss
                1,  0,       # Biomass
                0.4,  0,    # Veg heigth
                0,   -0.5,     # temperature
                0,    0.5      # treatment
               ),ncol=2,byrow=TRUE)

hor <- matrix(c(0,  1,    # CCI
               0 , 0.35,       # Moss
               0,  -1,       # Biomass
               0,  -0.35,    # Veg heigth
               1,   0,     # temperature
               -1,    0      # treatment
),ncol=2,byrow=TRUE)


#semPaths(conMod_fit, layout = ly)                       # new layout
#semPaths(conMod_fit, layout = ly, residuals = FALSE, what = "std")    # don't draw errors

# add custom labels

labels <- c("Canopy\nCover","Moss\ndepth","Biomass",
            "Vegetation\nheigth","Soil\ntemperature","Herbivore\nexclusion")
#semPaths(conMod_fit, layout = ly, residuals = FALSE, nodeLabels = labels)

setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/figures")
#tiff("conseptualSEM.tiff", units = "cm", res = 300, height = 30, width = 30)
semPaths(conMod_fit, layout = hor, residuals = F, nodeLabels = labels,
         sizeMan = 21,  sizeMan2 = 12,          # width/heigth of manifest nodes
         edge.color =  "black",   # edge (arrow) colour
         edge.width = 3,          # thicker edges
         label.cex = c(rep(2.5,6)), label.scale = FALSE, border.width = 3  #equal text size
         )   
#dev.off()

# piecewiseSEM
library(piecewiseSEM)
library(glmmTMB)
library(nlme)

# remove rows woth NAs,
MyVars <- c("Treatment", "Moss_depth", "CCI", "vegHeight", "Biomass", "Soil_temp", "LocalityName3")
SEMdat2 <- SEMdat[,MyVars]
SEMdat2 <- na.omit(SEMdat2)


# Individual model validation ####
# **MOSS####

#setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/figures")
#tiff(filename = "moss_trt.tiff", 
#     height = 10, width = 10, units = "cm", res = 300)
(Moss_TRT_gg <- ggplot(data = SEMdat2,
                      aes(x = Treatment,
                          y = Moss_depth))+
  geom_boxplot()+
    theme_classic()+
    xlab("Treatment") +
    ylab("Moss depth (cm)")
)
#dev.off()


Moss_depth <- lme(Moss_depth ~ Treatment, random = ~ 1 | LocalityName3, data = SEMdat2)
plot(Moss_depth)
qqnorm(resid(Moss_depth))
qqline(resid(Moss_depth))
summary(Moss_depth)


# **TEMP####
(Temp__CCI_gg <- ggplot(data = SEMdat2,
                       aes(x = CCI,
                           y = Soil_temp))+
    geom_point()+
        theme_classic()+
    xlab("Canopy Cover Index") +
    ylab(expression(atop("Soil temperature",( degree~C))))+
   geom_abline(intercept = 11.857381003, slope = -0.014856518)
)   # linear, negative

(Temp__moss_gg <- ggplot(data = SEMdat2,
                        aes(x = Moss_depth,
                            y = Soil_temp))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab(expression(atop("Soil temperature",( degree~C))))
)   # linear, weak

(Temp__veg_gg <- ggplot(data = SEMdat2,
                         aes(x = vegHeight,
                             y = Soil_temp))+
    geom_point()+
    theme_classic()+
    xlab("Vegetation heigth (cm)") +
    ylab(expression(atop("Soil temperature",( degree~C))))
)   # linear, weak

(Temp__biom_gg <- ggplot(data = SEMdat2,
                        aes(x = Biomass,
                            y = Soil_temp))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste("Field layer biomass (g m"^"2", ")")))+
    ylab(expression(atop("Soil temperature",( degree~C))))
)   # linear, weak

(Temp__trt_gg <- ggplot(data = SEMdat2,
                         aes(x = Treatment,
                             y = Soil_temp))+
    geom_boxplot()+
    theme_classic()+
    xlab("Treatment") +
    ylab(expression(atop("Soil temperature",( degree~C))))
)   # Treatment effect, direct or indirect. Plot residuals



Soil_temp = lme(Soil_temp ~ Moss_depth + CCI + vegHeight + Biomass + Treatment, 
                random = ~ 1 | LocalityName3, data = SEMdat2)

plot(Soil_temp)
resSoil <- resid(Soil_temp)
qqnorm(resSoil)
plot(SEMdat2$Treatment, resSoil)
plot(SEMdat2$Moss_depth, resSoil)
plot(SEMdat2$vegHeight, resSoil)
plot(SEMdat2$Biomass, resSoil)
plot(SEMdat2$CCI, resSoil)       # lots of values with low CCI, but still looks good

# **CCI####
(CCI__trt_gg <- ggplot(data = SEMdat2,
                        aes(x = Treatment,
                            y = CCI))+
   geom_boxplot()+
   theme_classic()+
   xlab("Treatment") +
   ylab("Canopy cover (%)")
)   # Strong treatment effect

CCI <- lme(CCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat2)
plot(CCI)
resCCI <- resid(CCI)
qqnorm(resCCI)
plot(SEMdat2$Treatment, resCCI)
# good

library(glmmTMB)
SEMdat2$CCI_prop <- SEMdat2$CCI/100
SEMdat2$CCI_prop1 <- ( SEMdat2$CCI_prop * (nrow(SEMdat2) - 1) + 0.5 ) / nrow(SEMdat2)  
#  see highstats EX726A or: Beta regression in R. Cibrari-Neto & Zeileis. 
#  Journal of Statistical Software. April 2010, Volume 34, Issue 2.
summary(SEMdat2$CCI_prop)
summary(SEMdat2$CCI_prop1) # 0 < y <1
tapply(SEMdat2$CCI_prop1, SEMdat2$Treatment, FUN = mean)
tapply(SEMdat2$CCI, SEMdat2$Treatment, FUN = mean)

CCI_beta = glmmTMB(CCI_prop1 ~ Treatment + (1 | LocalityName3), data = SEMdat2, 
                   family= list(family = "beta", link = "logit"))
summary(CCI_beta)
summary(CCI)

# Model validation
E1 <- resid(CCI_beta)
F1 <- fitted(CCI_beta)
phi  <- summary(CCI_beta)$sigma

VarY <- F1 *  (1 - F1) / (1 + phi)  
E1p  <- (SEMdat2$CCI_prop1 - F1) / sqrt(VarY) # Pearson residuals


# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = F1, 
     y = E1p,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)



# Plot Pearson residuals vs each covariate
p <- ggplot()+
     geom_boxplot(data = SEMdat2, 
                      aes(y = E1p, x = Treatment))+
     xlab("Treatment") + ylab("Pearson residuals")

# OK



# **vegetation####
(veg__trt_gg <- ggplot(data = SEMdat2,
                       aes(x = Treatment,
                           y = vegHeight))+
   geom_boxplot()+
   theme_classic()+
   xlab("Treatment") +
   ylab("Height of field layer vegetation (cm)")
)   # Treatment effect

(veg__moss_gg <- ggplot(data = SEMdat2,
                       aes(x = Moss_depth,
                           y = vegHeight))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab("Height of field layer vegetation (cm)")+
    geom_abline(intercept = 21.231533837, slope = -0.656314721)
)   # Negative. Looks linear


vegHeight = lme(vegHeight ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat2)
plot(vegHeight)   # funnel? no negative fitted values


# summary(vegHeight) # confirmes no negative fitted
resVeg <- resid(vegHeight)
qqnorm(resVeg)
qqline(resVeg)  # not too good
plot(SEMdat2$Treatment, resVeg) # not too bad

vegHeight2 <-  lme(vegHeight ~ Treatment + Moss_depth , random = ~ 1 | LocalityName3, data = SEMdat2)
plot(vegHeight2)   # funnel!
summary(SEMdat2$vegHeight) # no zeros
vegHeight2.2 <-  lme(log(vegHeight) ~ Treatment + Moss_depth , random = ~ 1 | LocalityName3, data = SEMdat2)
plot(vegHeight2.2)   # problem fixed

vegHeight3 <- glmer(vegHeight ~ Treatment + Moss_depth + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat2)
plot(vegHeight3)   # decent
resVeg <- resid(vegHeight3)
qqnorm(resVeg)
qqline(resVeg)  # good
plot(SEMdat2$Treatment, resVeg) # not too bad
vegHeight4 <- glmer(vegHeight ~ Treatment + Moss_depth + I(Moss_depth^2) + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat2)
summary(vegHeight4)  # only the linear relationship makes sense
summary(vegHeight3)

plot(SEMdat2$Treatment, SEMdat2$vegHeight) # good
plot(SEMdat2$Biomass, SEMdat2$vegHeight)
plot(SEMdat2$Treatment, SEMdat2$Biomass)
library(lme4)
#vegHeight5 = glmer(vegHeight ~ Treatment + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat2)
#summary(vegHeight5) # looks good after re-running validation plots 

# **biomass####
(biom__trt_gg <- ggplot(data = SEMdat2,
                        aes(x = Treatment,
                            y = Biomass))+
   geom_boxplot()+
   theme_classic()+
   xlab("Treatment") +
   ylab(expression(paste("Biomass (g m"^"2", ")")))
)   # No treatment effect

(biom__vegHeight_gg <- ggplot(data = SEMdat2,
                        aes(x = vegHeight,
                            y = Biomass))+
    geom_point()+
    theme_classic()+
    xlab("Height of field layer (cm)") +
    ylab(expression(paste("Biomass (g m"^"2", ")")))+
    geom_abline(intercept = 74.016590910, slope = 1.829152352)
)   # Positive. Looks linear




Biomass = lme(Biomass ~ Treatment + vegHeight, random = ~ 1 | LocalityName3, data = SEMdat2)
plot(Biomass) # funnel?
resBio <- resid(Biomass)
qqnorm(resBio)
qqline(resBio)
plot(SEMdat2$Treatment, resBio)
plot(SEMdat2$vegHeight, resBio)
summary(Biomass) # no negative values. Small correlation between Int and slope


# All plots ####
setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/figures")

tiff("all_arcs.tiff", units = "cm", res = 300, height = 40, width = 25)
all_arcs <- grid.arrange(CCI__trt_gg,
             Moss_TRT_gg,
             veg__trt_gg,
             veg__moss_gg,
             biom__trt_gg,
             biom__vegHeight_gg,
             Temp__CCI_gg,
             Temp__moss_gg,
             Temp__veg_gg,
             Temp__biom_gg,
             Temp__trt_gg,
             nrow = 6)

dev.off()
# Model list ####
temp_pSEM_randomList = list(
  
  # Predicting soil temperature (mediated model)
  # Soil_temp = lme(Soil_temp ~ Moss_depth + CCI + vegHeight + Biomass, 
  #                random = ~ 1 | LocalityName3, data = SEMdat2),
  # Adding treatment as explanatory node after running pSEMfit:
  Soil_temp = lme(Soil_temp ~ Moss_depth + CCI + vegHeight + Biomass +Treatment, 
                 random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predicting Moss depth
  Moss_depth = lme(Moss_depth ~ Treatment, random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predicting Canopy Cover
  CCI = lme(CCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predict vegetation heitgh
  # vegHeight = glmer(vegHeight ~ Treatment + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat2),
  # Added moss depth as explanatory node
  vegHeight = glmer(vegHeight ~ Treatment +Moss_depth + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat2),
  
  
  # Predict field layer biomass:
  # Biomass = lme(Biomass ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat2)
  # Added vegetation hieght as explanatory node:
  Biomass = lme(Biomass ~ Treatment +vegHeight , random = ~ 1 | LocalityName3, data = SEMdat2)
)

(pSEMfit <- sem.fit(temp_pSEM_randomList, SEMdat2, conditional = F))
# AIC 56
(coef.table <- sem.coefs(temp_pSEM_randomList, SEMdat2, standardize = "none", intercept = F))
#write.csv(coef.table, "coef.table.csv", row.names = F)
mod_fit <- sem.model.fits(temp_pSEM_randomList)
write.csv(mod_fit, "mod_fit.csv", row.names = T)

# Compare with beta model for CCI: ####
# Here I'm confirming that using beta regressin for CCI doesn't change anything. 
# Jump from here ... ####

# Model list #2 ##
temp_pSEM_randomList_2 = list(
  
  # Predicting soil temperature (mediated model)
  Soil_temp = lme(Soil_temp ~ Moss_depth + CCI_prop1 + vegHeight + Biomass +Treatment, 
                  random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predicting Moss depth
  Moss_depth = lme(Moss_depth ~ Treatment, random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predicting Canopy Cover
  CCI_beta = glmmTMB(CCI_prop1 ~ Treatment + (1 | LocalityName3), data = SEMdat2, 
                     family= list(family = "beta", link = "logit")),  
  
  # Predict vegetation heitgh
  vegHeight = glmer(vegHeight ~ Treatment +Moss_depth + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat2),
  
  
  # Predict field layer biomass:
  Biomass = lme(Biomass ~ Treatment +vegHeight , random = ~ 1 | LocalityName3, data = SEMdat2)
)


(pSEMfit_2 <- sem.fit(temp_pSEM_randomList_2, SEMdat2, conditional = F))
# AIC 55 (slightly better)
(coef.table_2 <- sem.coefs(temp_pSEM_randomList_2, SEMdat2, standardize = "none"))
#(coef.table.std <- sem.coefs(temp_pSEM_randomList, SEMdat2, standardize = "scale"))
# The results are almost identidal, so I'm skipping the beta regression to keep all variables in raw scale

## To here ####



# Compare without the gamma model, using log tansformation insted : #
# If the results are comparable I prefer the lme models as they allow the sem-model-fit calcuations.
# Jump from here ... ####
SEMdat2$l_vegHeight <- log(SEMdat2$vegHeight)

# Model list #3 ##
temp_pSEM_randomList_3 = list(
  
  # Predicting soil temperature (mediated model)
  Soil_temp = lme(Soil_temp ~ Moss_depth + CCI + l_vegHeight + Biomass +Treatment, 
                  random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predicting Moss depth
  Moss_depth = lme(Moss_depth ~ Treatment, random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predicting Canopy Cover
  CCI = lme(CCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat2),
  
  # Predict vegetation heitgh
  vegHeight = lme(l_vegHeight ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat2),
  
  
  # Predict field layer biomass:
  Biomass = lme(Biomass ~ Treatment +l_vegHeight , random = ~ 1 | LocalityName3, data = SEMdat2)
)


(pSEMfit_3 <- sem.fit(temp_pSEM_randomList_3, SEMdat2, conditional = F))
# AIC 55 (slightly better)
(coef.table_3 <- sem.coefs(temp_pSEM_randomList_3, SEMdat2, standardize = "none"))
coef.table
# The results are not identidal.
sem.model.fits(temp_pSEM_randomList_3)
# I can perhaps use this R2 with a caveat: vegHeight  Marginal R2: 0.0775025378   Conditional R2:  0.4134856

## To here ####


# This is one option for plotting relationships, but dont work with gamma or with categorical nodes
partial.resid(Soil_temp ~ CCI, 
              temp_pSEM_randomList, SEMdat2,
              list(lmeControl(opt = "optim")), return.data.frame = F)

partial.resid( Biomass ~ vegHeight, 
               temp_pSEM_randomList, SEMdat2,
               list(lmeControl(opt = "optim")), return.data.frame = F)




sem.plot(coef.table = coef.table, corr.errors = NULL,
         show.nonsig = TRUE, scaling = 10, alpha = 0.05)


# FIGURE ####
Mod <-   'CCI ~ Treatment
          Moss_depth ~ Treatment
          Biomass ~ Treatment+Vegetation_height
          Vegetation_height ~ Treatment + Moss_depth
          Soil_temp ~ Vegetation_height+CCI+Biomass+Moss_depth+Treatment'


Mod_fit <- sem(Mod, SEMdat)   # dont care about scaling
summary(Mod_fit)
# FIND THE ORDER OF THE NODES TO ALLOW CUSTUM LAYOUT
semPaths(Mod_fit,what="std",nodeLabels=letters[1:6],edgeLabels=1:11,edge.label.cex=1.5,fade=FALSE, residuals = F)
semPaths(Mod_fit)

# node order   # CCI, Moss, Biomass, Veg heigth, temperature treatment
# edge order  
# 1) TRT-CCI
# 2)  - MOSS
# 3)  -BIOMASS
# 4)  vegetation - biomass
# 5)  TRT - veg
# 6) moss - veg
# 7) veg - temp
# 8) CCI - temp
# 9) Biomass - temp
# 10) Moss - temp
# 11) TRT - temp

colour <- c("green",  # 1) 
            "grey",   # 2)  
            "grey",   # 3)  
            "green",  # 4)  
            "green",  # 5)  
            "red",    # 6) 
            "grey",  # 7) 
            "red",    # 8) 
            "grey",   # 9) 
            "grey",   # 10) 
            "red")    

edge_widths <-  
  c(5,   # 1) 
    1,   # 2)  
    1,   # 3)  
    5,   # 4)  
    5,   # 5)  
    5,   # 6) 
    1,   # 7) 
    5,   # 8) 
    1,   # 9) 
    1,   # 10) 
    5)    

mod_fit
labels <- c(expression(atop("Canopy Cover (%)", paste("R"^"2","=0.25"))),
            expression(atop("Moss depth (cm)", paste("R"^"2","=0.16"))),
            expression(atop("Biomass (g m"^"-2"*")", paste("R"^"2","=0.46"))),
            "Vegetation\nheigth (cm)",
            expression(atop("Soil temperature " ( degree~C), paste("R"^"2","=0.65"))),
            "Herbivore\nexclusion")

edge_labels <- c("19.3***",  # 1) TRT-CCI
                 "",         # 2)  - MOSS
                 "",         # 3)  -BIOMASS
                 "1.8***",   # 4)  vegetation - biomass
                 "7.8***",   # 5)  TRT - veg
                 "-0.7*",    # 6) moss - veg
                 "",         # 7) veg - temp
                 "-0.015***",# 8) CCI - temp
                 "",         # 9) Biomass - temp
                 "",         # 10) Moss - temp
                 "-0.23*")   # 11) TRT - temp

#write.csv(coef.table, "pSEMoutput.csv", row.names = F)
setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/figures")

tiff("pSEM.tiff", units = "cm", res = 300, height = 30, width = 30)
semPaths(Mod_fit, layout = hor, residuals = F, nodeLabels = labels,
         sizeMan = 21, sizeMan2 = 7,            # width/height of manifest nodes
         edge.color =  colour,    # edge (arrow) colour
         edge.width = edge_widths,          # thicker edges
         label.cex = c(rep(1.5,6)), label.scale = F,  #equal text size
         esize = 1, asize = 2,edgeLabels = edge_labels, edge.label.cex = 1.2,
         edge.label.bg = "white", edge.label.position = 0.3, 
         edge.label.color = "black",         border.width = 3)   
dev.off()
rep(2,6)



#),ncol=2,byrow=TRUE)

# EXTENDED SEM ####
names(SEMdat)
SEMdat$SpeciesRichness <- rnorm(nrow(SEMdat), 1,2)
SEMdat$Grasses <- rnorm(nrow(SEMdat))
SEMdat$Herbs <- rnorm(nrow(SEMdat))
SEMdat$LargeFerns <- rnorm(nrow(SEMdat))
SEMdat$DwarfShrubs <- rnorm(nrow(SEMdat))

conMod2 <-  'CCI ~ Treatment
            Moss_depth ~ Treatment
            Biomass ~ Treatment
            Vegetation_height ~ Treatment 
            Soil_temp ~ Treatment
            SpeciesRichness ~ Grasses+Herbs+LargeFerns+DwarfShrubs
            Grasses ~ CCI+Moss_depth+Soil_temp
            Herbs ~ CCI+Moss_depth+Soil_temp
            LargeFerns ~ CCI+Moss_depth+Soil_temp
            DwarfShrubs ~ CCI+Moss_depth+Soil_temp'
conMod_fit2 <- sem(conMod2, SEMdat)   # dont care about scaling

semPaths(conMod_fit2,layout =hor2, what="std",nodeLabels=letters[1:11],edgeLabels=1:22,edge.label.cex=1.5,fade=FALSE)
semPaths(conMod_fit2)
hor2 <- matrix(c(-0.4,  -0.3,          # CCI
                 -0.4 , 0.3,       # Moss
                 -1,  1,         # Biomass
                 -0.7,  0.65,      # Veg heigth
                 -0.4,   -0.9,         # temperature
                1,   0,        # SR
                0.5, 1,        #Grasses
                0.5,  0.3 ,        # Herbs
                0.5,  -0.3,       #Large ferns
                0.5,   -1,       #Dwarf shrubs
                -1,-1             # Treatment
),ncol=2,byrow=TRUE)

labels3 <- c("Canopy\nCover",
             "Moss\ndepth",
             "Biomass",
            "Vegetation\nheigth",
            "Soil\ntemperature",
            "Species\nrichness",
            "Grasses",
            "Herbs",
            "Large ferns",
            "Dwarf shrubs", 
            "Herbivore\nexclusion")

svg("conMod2.svg")
semPaths(conMod_fit2, layout = hor2, residuals = F, nodeLabels = labels3,
         sizeMan = 12,  sizeMan2 = 7,          # width/heigth of manifest nodes
         edge.color =  "black",   # edge (arrow) colour
         edge.width = 3,exoVar = F,          # thicker edges
         label.cex = c(rep(1,6)), label.scale = FALSE, border.width = 3  #equal text size
) # BOTTOM OF PAGE ####
dev.off()
