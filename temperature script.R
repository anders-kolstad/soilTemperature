


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
#library(geoR) #variog
library(plotly) 
#library(lavaan)
#library(semPlot)
library(piecewiseSEM)
library(glmmTMB)
library(nlme)
library(lme4)
library(gtable)

source("M:/Anders L Kolstad/HIGHSTATS/AllRCode/HighstatLibV10.R")
source("M:/Anders L Kolstad/HIGHSTATS/AllRCode/R2glmms.R")
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

# get number of missing loggers
#t2$unique <- paste0(t2$TID, t2$trt, t2$subplot)
#table(t2$unique)
#length(unique(t2$unique))
# 7 loggers did not complete the 344 dayas (but 3 of these had > 328)


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
library(scales)
plot1dat <- aggregate(data = t3.2, 
                cbind(Mean_daily_temperature = Mean_daily_temperature,
                      daily_range = daily_range) ~ 
                                    Date, 
                FUN = function(x) c(mn = mean(x), length = length(x), SD = sd(x) ) )
plot1dat <- do.call(data.frame, plot1dat)


p <-  ggplot(data = plot1dat, 
             aes(x = Date, 
                 y = Mean_daily_temperature.mn)) 
                # group = trt,
                 #linetype = trt)) 
            
p <- p+   geom_line(size=2)   
p <- p + xlab("") 
p <- p + ylab(expression(atop(paste("Soil temp."), ( degree~C))))

p <- p + theme_classic()+theme(text = element_text(size=15))
#p <- p + scale_linetype_discrete(name="",
#                                 breaks=c("B", "UB"),
#                                 labels=c("Open plots", "Exclosure"))
p <- p + theme(plot.margin=unit(c(0.1,1.5,0.1,1),"cm"))
p <- p + theme(axis.title =  element_text(hjust = 0.5))  #default
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.line.x = element_blank())

#               legend.justification=c(1,1), 
#               legend.position=c(1,1),
#               legend.background = element_rect(fill=NA),
#               plot.title = element_text(hjust = 0.5, size=22),
#               legend.key.size = unit(1,"line"),
#               legend.text=element_text(size=15))
p <- p + ylim(0,15)
p <- p + scale_x_date(date_breaks = "1 month", labels=date_format("%b-%Y"),
                      limits = as.Date(c("2016-05-19","2017-04-27")))
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
#dev.off()

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
#p2 <- p2 + geom_ribbon(data = t7, 
 #                      aes(x = Date, 
  #                         ymax = diff_mean.mn+diff_mean.SD, 
   #                        ymin = diff_mean.mn-diff_mean.SD), alpha = 0.2)
p2 <- p2 + geom_ribbon(data = t7, 
                       aes(x = Date, 
                          ymax = diff_mean.mn+1.96*SE_mean_temp, 
                         ymin = diff_mean.mn-1.96*SE_mean_temp),alpha = 0.5)
#p2 <- p2 +   geom_line(size=1, colour = "white")   
p2 <- p2 + xlab("") 
p2 <- p2 + ylab(expression(atop(paste(Delta, " Soil temperature "), ( degree~C))))
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
                         name="Site productivity",
                         breaks=c("High", "Low", "High + Thinned"),
                         labels=c("High (n=5)", "Low (n=7)","High +\nThinned (n=3)"))
p2.3 <- p2.3 + theme_classic()+theme(plot.margin=unit(c(0.1,1,0.1,1),"cm"))+
  theme(text = element_text(size=15),
        legend.justification=c(0,1), 
        legend.position=c(0,1),
        legend.background = element_rect(fill=NA),
        plot.title = element_text(hjust = 0.5, size=22),
        legend.key.size = unit(1,"line"),
        legend.text=element_text(size=15)) 
p2.3 <- p2.3 +scale_x_date(date_breaks = "3 month", labels=date_format("%b-%Y"),
                           limits = as.Date(c("2016-05-19","2017-04-27")))
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




setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")

g1<-ggplotGrob(p)
g2<-ggplotGrob(p2.3)
g<-gtable:::rbind_gtable(g1, g2, "first")
panels <- g$layout$t[grep("panel", g$layout$name)]
g$heights[panels] <- unit(c(1,3), "null")
#tiff("full_year_main_results.tiff", height = 15, width = 20, units = "cm", res = 300)
grid.newpage()
grid.draw(g)
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

test <- aggregate(data = dat2, 
               cbind(temp = Temp) ~ 
                 loggerID+
                 Date, 
               FUN =  mean)
table(test$loggerID)
#59 days

# averages are added to this file:
temp_data <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                        sheet = "summer2017")

temp_data$TRT <- factor(temp_data$TRT)
temp_data$TID <- factor(temp_data$TID)
temp_data$avg_CCI <- as.numeric(temp_data$avg_CCI)
temp_data$avg_moss_depth_cm <- as.numeric(temp_data$avg_moss_depth_cm)
temp_data$avg_temp <- as.numeric(temp_data$avg_temp)
temp_data$avg_daily_temp_range <- as.numeric(temp_data$avg_daily_temp_range)

table(temp_data$loggerID)
head(temp_data)
temp_data$Treatment <- ifelse(temp_data$TRT == "B", "Open plots", "Exclosures")



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
flux_box <- ggplot(data = temp_data, aes(x=Treatment, y=avg_daily_temp_range))+
  geom_boxplot()+
  ylab("Average daily\ntemperature fluctuations")+
  xlab("")+
  theme_classic()+
  theme(text = element_text(size=15))
#tiff("temp_fluctuations_box.tiff", height = 10, width = 10, units = "cm", res = 300)
flux_box
#dev.off()


mod_fluc <- glmmTMB(data = temp_data,
                    avg_daily_temp_range~Treatment+(1|TID),
                    family = Gamma(link = "identity") )
summary(mod_fluc)
res_fluc <- resid(mod_fluc, "pearson")
plot(fitted(mod_fluc), res_fluc)

plotdf <- na.omit(temp_data[,c("Treatment", "avg_daily_temp_range")])
plot(plotdf$Treatment, res_fluc)


# Daily trends

dat4 <- aggregate(data = dat2,
                  Temp~Time+f_TRT,
                  FUN = mean)
dat4 <- dcast(data = dat4, Time~f_TRT,
                  value.var = "Temp"  )
dat4$diff <- dat4$UB-dat4$B





p_daily <- ggplot(data = dat4, aes(x=Time, y=B))+
  geom_line(aes(y=B),linetype=2, size = 2)+
  geom_line(aes(y = UB), linetype=1, size = 2)+
  ylab(expression(atop("Mean soil", "temperature " ( degree~C))))+
  scale_x_datetime(date_breaks = "6 hour",
                   date_labels = "%H:%M")+
  theme_bw()+theme(legend.position="none")+
  #scale_colour_discrete(name="",
  #                     breaks=c("B", "UB"),
  #                    labels=c("Open plots", "Exclosure"))+
  theme(text = element_text(size=15))+
  geom_ribbon(data = dat4, aes(x= Time, ymin=UB, ymax=B, alpha=1))
p_daily



p_daily2 <- ggplot(data = dat4, aes(x=Time, y=diff
                                   ))+
  geom_line(size = 2)+
  ylab(expression(atop(paste(Delta," Soil temperature "), ( degree~C))))+
  scale_x_datetime(date_breaks = "6 hour",
                   date_labels = "%H:%M")+
  theme_classic()+theme(legend.position="none")+
  theme(text = element_text(size=15))
  
library(grid)
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")
#tiff("Daily Summer Soil Temp Fluctuations.tiff", height = 15, width = 15, units = "cm", res = 300)
#grid.newpage()
grid.draw(rbind(ggplotGrob(p_daily), ggplotGrob(p_daily2), size = "last"))
dev.off()

#pdf("Daily Summer Soil Temp Fluctuations.pdf") 
#tiff("Daily Summer Soil Temp Fluctuations.tiff", height = 15, width = 15, units = "cm", res = 300)
p_daily
#dev.off()
# no point having error ribbons because there's a temporal trend with 
# larger variation than the daily fluctuations

# would be nice to make treatment effect plot of this






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



# SPAGETTI PLOT ####

dat5.2 <- aggregate(data = temp_data,
                  avg_temp~
                    TID+
                    TRT,
                  FUN = mean)

p_mean <- ggplot(data= dat5.2, aes(x=TRT, y= avg_temp, group = TID))+
  geom_line(size = 2)+
  xlab("") +
  ylab(expression(atop("Mean summer soil", "temperature " ( degree~C))))+
  
  annotate("text", x=2.1, y=c(11.7, 11.2, 11, 10,9.8), 
           label= c("12", "8", "1", "6", "14"))+
  theme_classic()+
  theme(text = element_text(size=15)) +
  scale_x_discrete(labels=c("B" = "Open plots", "UB" = "Exclosure"))
#tiff("spagetti_plot.tiff", height = 10,width = 10, units = "cm", res = 300)
p_mean
dev.off()


# LINE GRAPH ####

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
dat6.4 <- dcast(data = dat6.2, TID+Date~TRT, value.var = "Temp", fun.aggregate = mean)
dat6.4$diff <- dat6.4$UB-dat6.4$B
dat6.5 <- aggregate(data = dat6.4, diff~Date, 
                    FUN = function(x) c(mn = mean(x), sd = sd(x), length = length(x)))
dat6.5 <- do.call(data.frame, dat6.5)
dat6.5$upper <- dat6.5$diff.mn+(dat6.5$diff.sd/dat6.5$diff.length)*1.96
dat6.5$lower <- dat6.5$diff.mn-(dat6.5$diff.sd/dat6.5$diff.length)*1.96

p_line <- ggplot(data= dat6.3, aes(x=Date, y= Temp.mn, group = TRT, colour = TRT))+
  geom_line(size = 2)+
  xlab("Date") +
  ylab(expression(atop("Summer soil", " temperature " ( degree~C))))+
  theme_classic()+
  scale_colour_discrete(name="",
                        breaks=c("B", "UB"),
                        labels=c("Open plots", "Exclosure"))+
  theme(text = element_text(size=15),
        axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.line.x = element_blank(),
        legend.position = "top",
        legend.key.size = unit(2,"line"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.3)) +
  ylim(c(9,14))
p_line

p_line2 <- ggplot(data= dat6.5, aes(x=Date, y= diff.mn))+
  geom_line(size = 2)+
  xlab("Date") +
  ylab(expression(atop(paste(Delta," Summer soil"), 
            paste("temperature  (", degree~C, ")"))))+
  theme_classic()+theme(text = element_text(size=15)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2)+
  ylim(c(-1.1,0))+
  #geom_hline(yintercept = 0)+
  theme(legend.position = "none")
p_line2

#grid.arrange(p_line, p_line2)   # y axis displaced
g3<-ggplotGrob(p_line)
g4<-ggplotGrob(p_line2)
g <- gtable:::rbind_gtable(g3, g4, "first")
panels <- g$layout$t[grep("panel", g$layout$name)]
g$heights[panels] <- unit(c(1,3), "null")
tiff("summer_main_results.tiff", height = 15, width = 20, units = "cm", res = 300)
grid.newpage()
grid.draw(g)
dev.off()
getwd()


setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER\\figures")
#pdf("Mean soil temp summer2017_5.pdf")
#multiplot(p_mean, p_line, p_daily, cols = 2)
tiff("Mean soil temp summer2017.tiff", height = 20, width = 24, units = "cm", res=300)
grid.arrange(p_line, p_mean, p_daily, flux_box,
             ncol =2, nrow=2,
             layout_matrix = rbind(c(1,1,1), 
                                   c(2,3,4)))
dev.off()

lay <- rbind(c(1,1,1,2,3),
             c(1,1,1,4,5),
             c(6,7,8,9,9))








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
# Shannon might be better as it's not bound between 0 and 1 and thus don't require beta regression, 
# and values might be more evenly distributed around 0 (hopefully)
PI_BM$shannon_vasc <- diversity(PI_BM[,9:48], index = "shannon")
#summary(PI_BM$shannon_vasc) # 
#plot(PI_BM$Simpsons_vasc, PI_BM$shannon_vasc)
#par(mfrow=c(2,1))
#hist(PI_BM$Simpsons_vasc)
#hist(PI_BM$shannon_vasc)




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
SR_dat2$shannon_moss <- diversity(SR_dat2[,mosses], index = "shannon")
#View(SR_dat2[SR_dat2$LocalityCode == "1KB" & SR_dat2$Plot == 2,])
# apparantly point intercept of vascular plants and mosses appear on seperate lines.
#View(SR_dat2[SR_dat2$LocalityCode == "BRUB" & SR_dat2$Plot == 2,])


SR_dat2$uniquePlot <- as.factor(paste0(SR_dat2$LocalityName2, SR_dat2$Treatment, SR_dat2$Plot))
names(SR_dat2)

SR_dat3 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "total_SpeciesRichness", 
                 fun.aggregate = sum)
SR_dat3$total_SR <- SR_dat3$Observed+SR_dat3$Point_Intercept

SR_dat4 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "moss_SpeciesRichness", 
                 fun.aggregate = sum)
SR_dat4$moss_SR <- SR_dat4$Point_Intercept
SR_dat3$moss_SR <- SR_dat4$moss_SR[match(SR_dat3$uniquePlot, SR_dat4$uniquePlot)]

SR_dat5 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "vasc_SpeciesRichness", 
                 fun.aggregate = sum)
SR_dat5$vasc_SR <- SR_dat5$Observed+SR_dat5$Point_Intercept
SR_dat3$vasc_SR <- SR_dat5$vasc_SR[match(SR_dat3$uniquePlot, SR_dat5$uniquePlot)]

SR_dat6 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "Simpsons_moss", 
                 fun.aggregate = sum)
SR_dat3$Simpsons_moss <- SR_dat6$Point_Intercept[match(SR_dat3$uniquePlot, SR_dat6$uniquePlot)]

SR_dat7 <- dcast(SR_dat2, uniquePlot~Method, 
                 value.var = "shannon_moss", 
                 fun.aggregate = sum)
SR_dat3$shannon_moss <- SR_dat7$Point_Intercept[match(SR_dat3$uniquePlot, SR_dat7$uniquePlot)]


# SEM DATASET ##
SEMdat <- read_excel("M:/Anders L Kolstad/systherb data/datasets/Soil temperature.xlsx", 
                     sheet = "summer2017")
SEMdat$uniquePlot <- paste0(SEMdat$LocalityName2, SEMdat$TRT, SEMdat$Subplot)
SEMdat$UCI <- PI_BM$UnderstoryCoverIndex[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$simpsons_vasc <- PI_BM$Simpsons_vasc[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$simpsons_moss <- SR_dat3$Simpsons_moss[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$shannon_vasc <- PI_BM$shannon_vasc[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$shannon_moss <- SR_dat3$shannon_moss[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]



SEMdat$total_SR <- SR_dat3$total_SR[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$moss_SR <- SR_dat3$moss_SR[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$vasc_SR <- SR_dat3$vasc_SR[match(SEMdat$uniquePlot, SR_dat3$uniquePlot)]
SEMdat$shrubBM <- PI_BM$shrubBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
#SEMdat$thBM <- PI_BM$thBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
#SEMdat$shBM <- PI_BM$shBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
#SEMdat$gramBM <- PI_BM$graminoidBiomass[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]
SEMdat$avenellaBM <- PI_BM$`Avenella flexuosa`[match(SEMdat$uniquePlot, PI_BM$uniquePlot)]

SEMdat$Treatment <- factor(SEMdat$TRT)
levels(SEMdat$Treatment) <- c("Open plots", "Exclosed")
SEMdat$CCI <- as.numeric(SEMdat$avg_CCI)
SEMdat$Moss_depth <- as.numeric(SEMdat$avg_moss_depth_cm)
SEMdat$Soil_temp <- as.numeric(SEMdat$avg_temp)
#SEMdat$Soil_temp_range <- as.numeric(SEMdat$avg_daily_temp_range)

# remove rows woth NAs,
MyVars <- c( "LocalityName3", "Treatment", "Subplot", 
             "Moss_depth", "CCI", "UCI","Soil_temp",
             "avenellaBM", "shrubBM", "total_SR", "moss_SR", "vasc_SR", 
             "simpsons_moss", "simpsons_vasc", "shannon_vasc", "shannon_moss")
SEMdat2 <- SEMdat[,MyVars]
SEMdat3 <- na.omit(SEMdat2)
# lost five sites (logger failed, plot not found)



setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER")
write.csv(SEMdat3, file = "SEMdat.csv", row.names = F)



#*************************************************####

# SEM START ####

#*************************************************##
setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER")
SEMdat <- read.csv("SEMdat.csv")

#housekeeping
#str(SEMdat)
# all good

SEMdat$uniquePlot <- factor(paste0(SEMdat$LocalityName3, SEMdat$Subplot, SEMdat$Treatment))
SEMdat$fSubplot <- factor(SEMdat$Subplot)

# removing one outlier plot with very deep moss.
plot(SEMdat$Moss_depth)
SEMdat <- SEMdat[-which(SEMdat$Moss_depth == max(SEMdat$Moss_depth)),]
plot(SEMdat$Moss_depth)


# Exploration
plot.design(CCI~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(UCI~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(Soil_temp~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(avenellaBM~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(shrubBM~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(total_SR~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(moss_SR~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(vasc_SR~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(shannon_vasc~uniquePlot+Treatment+LocalityName3, data = SEMdat)
plot.design(shannon_moss~uniquePlot+Treatment+LocalityName3, data = SEMdat)


summary(SEMdat$simpsons_vasc)
plot(SEMdat$simpsons_vasc)
hist(SEMdat$simpsons_vasc)
Boxplot(SEMdat$simpsons_vasc~SEMdat$Treatment)
t.test(SEMdat$simpsons_vasc~SEMdat$Treatment)  # Open plots less diverse in vascular plants

summary(SEMdat$simpsons_moss)
plot(SEMdat$simpsons_moss)
hist(SEMdat$simpsons_moss)
Boxplot(SEMdat$simpsons_moss~SEMdat$Treatment)
t.test(SEMdat$simpsons_moss~SEMdat$Treatment)  

summary(SEMdat$total_SR)
plot(SEMdat$total_SR)
hist(SEMdat$total_SR)
Boxplot(SEMdat$total_SR~SEMdat$Treatment)
t.test(SEMdat$total_SR~SEMdat$Treatment)

summary(SEMdat$moss_SR)
plot(SEMdat$moss_SR)
hist(SEMdat$moss_SR)
Boxplot(SEMdat$moss_SR~SEMdat$Treatment)
t.test(SEMdat$moss_SR~SEMdat$Treatment)

summary(SEMdat$vasc_SR)
plot(SEMdat$vasc_SR)
hist(SEMdat$vasc_SR)
Boxplot(SEMdat$vasc_SR~SEMdat$Treatment)
t.test(SEMdat$vasc_SR~SEMdat$Treatment)

summary(SEMdat$shrubBM)
plot(SEMdat$shrubBM)
hist(SEMdat$shrubBM)
Boxplot(SEMdat$shrubBM~SEMdat$Treatment)

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
             "avenellaBM", "shrubBM")
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
                "avenellaBM", "shrubBM")
Mypairs(SEMdat[,MyVars5])
# Avenella like warm soil and no moss (non-linear).
# Problem about correation between UCI and vegetation biomass

MyVars6 <- c("CCI", "Moss_depth", "Soil_temp", "UCI",
             "simpsons_vasc", "simpsons_moss", "total_SR", "moss_SR", "vasc_SR")
Mypairs(SEMdat[,MyVars6])
# nothing

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")

# Treatment is strongest on CCI, temp, vegetation heigh. 
# Not present on moss depth or biomass
# CCI and temp are negatively correlated, quite linear (beta, zero-inlated)







# piecewiseSEM  ####
library(piecewiseSEM)
library(glmmTMB)
library(nlme)

# I have to centre soil temperature becuse the avenella model used a quadrtaic term of
# soil temperature which becomes almost perfectly correated to the intercept
SEMdat$Soil_temp_C <- as.numeric(scale(SEMdat$Soil_temp, scale = F))
# log transform shrub biomass
SEMdat$Log_shrubBM <- log(SEMdat$shrubBM+1)
#SEMdat$Log_avenellaBM <- log(SEMdat$avenellaBM+1)

# center the rest
#SEMdat$Moss_depth_C <- as.numeric(scale(SEMdat$Moss_depth, scale = F))
#SEMdat$CCI_C <- scale(SEMdat$CCI, scale = F)

# make CCI a proportion
#SEMdat$CCIprop <- SEMdat$CCI/100



#----------------------------------------------------------#
#----------------------------------------------------------#
#
# Exploratory path analysis #### 
#
#----------------------------------------------------------#
#----------------------------------------------------------#

# Local estimation
# Backward selection


# **MOSS####
library(PerformanceAnalytics)
PairsDat <- SEMdat
library(dplyr)
PairsDat <- select(SEMdat,
                   -Subplot,
                   -UCI,
                   -shrubBM,
                   -uniquePlot,
                   -Soil_temp,
                   -fSubplot)
Mypairs(PairsDat)


Moss_depth <- lme(Moss_depth ~ Treatment, random = ~ 1 | LocalityName3, data = SEMdat)
plot(Moss_depth)
qqnorm(resid(Moss_depth))
qqline(resid(Moss_depth))
summary(Moss_depth)


(Moss_TRT_gg <- ggplot(data = SEMdat,
                       aes(x = Treatment,
                           y = Moss_depth))+
    geom_boxplot()+
    theme_classic()+
    xlab("Treatment") +
    ylab("Moss depth (cm)")
)


# **CCI####
(CCI__trt_gg <- ggplot(data = SEMdat,
                       aes(x = Treatment,
                           y = CCI))+
   geom_boxplot()+
   theme_classic()+
   xlab("Treatment") +
   ylab("Canopy cover (%)")
)   # Strong treatment effect

CCI <- lme(CCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat)
plot(CCI)
resCCI <- resid(CCI)
qqnorm(resCCI)
plot(SEMdat$Treatment, resCCI)
# good


# should idealy be analysed with beta-regression, but results are similar so skipping this headache
library(glmmTMB)

# data need to match:   0<y<1
SEMdat$CCI_prop <- SEMdat$CCI/100
SEMdat$CCI_prop1 <- ( SEMdat$CCI_prop * (nrow(SEMdat) - 1) + 0.5 ) / nrow(SEMdat)  
#  see highstats EX726A or: Beta regression in R. Cibrari-Neto & Zeileis. 
#  Journal of Statistical Software. April 2010, Volume 34, Issue 2.
summary(SEMdat$CCI_prop)
summary(SEMdat$CCI_prop1) # 0 < y <1
tapply(SEMdat$CCI_prop1, SEMdat$Treatment, FUN = mean)
tapply(SEMdat$CCI, SEMdat$Treatment, FUN = mean)

CCI_beta = glmmTMB(CCI_prop1 ~ Treatment + (1 | LocalityName3), data = SEMdat, 
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

UCI <- lme(UCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat)
plot(UCI)

# **TEMP####
#temp_and_cci <- partial.resid(Soil_temp_C~CCI, SEM_tSR_1, SEMdat)
#(Temp__CCI_gg <- ggplot(data = temp_and_cci,
#                       aes(x = x.resids,
#                           y = y.resids))+
#    geom_point()+
#        theme_classic()+
#    xlab("Canopy Cover Index (%) | others") +
#    ylab(expression(atop("Soil temperature | others",( degree~C))))+
#   geom_smooth(method = "lm")
#)   # linear, negative

(Temp__moss_gg <- ggplot(data = SEMdat,
                        aes(x = Moss_depth,
                            y = Soil_temp))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab(expression(atop("Soil temperature",( degree~C))))
)   # linear, weak


(Temp__trt_gg <- ggplot(data = SEMdat,
                         aes(x = Treatment,
                             y = Soil_temp))+
    geom_boxplot()+
    theme_classic()+
    xlab("Treatment") +
    ylab(expression(atop("Soil temperature",( degree~C))))
)   # Treatment effect, direct or indirect. Plot residuals



Soil_temp <- lme(Soil_temp_C ~ Treatment+Moss_depth + CCI,
                random = ~ 1 | LocalityName3, data = SEMdat, method = "ML")

plot(Soil_temp)
resSoil <- resid(Soil_temp)
qqnorm(resSoil)
plot(SEMdat$Treatment, resSoil)
plot(SEMdat$Moss_depth, resSoil)
plot(SEMdat$CCI, resSoil)       # lots of values with low CCI, but still looks good
drop1(Soil_temp, test = "Chi")
Soil_temp <- update(Soil_temp, .~. -Moss_depth)
drop1(Soil_temp, test = "Chi")




# **Avenella####
MyVars <- c("avenellaBM", "Treatment",
             "Moss_depth", "CCI", "Soil_temp_C")
Mypairs(SEMdat[,MyVars])

# remove CCi based on bivariate plots
avenella <- glmmTMB(avenellaBM+1 ~ Treatment+Moss_depth+Soil_temp_C+I(Soil_temp_C^2) 
                     + ( 1 | LocalityName3), family = Gamma(link = "log"), 
                     data = SEMdat)
drop1(avenella) # says to drop the q term - strange
avenella <- update(avenella, .~. -Moss_depth)
drop1(avenella)

summary(avenella)

# removing moss depth

avenella2 <- glmmTMB(avenellaBM+1 ~ Treatment+Soil_temp_C+I(Soil_temp_C^2) 
                    + ( 1 | LocalityName3), family = Gamma(link = "log"), 
                    data = SEMdat)
summary(avenella2)


aveRes <- resid(avenella2, type = "pearson")
plot(aveRes, fitted(avenella2))
plot(SEMdat$Treatment, aveRes)
plot(SEMdat$Soil_temp_C, aveRes)


avenella2$fit$par[5]
avenella2$fit$par[6]
(avenella__trt_gg <- ggplot(data = SEMdat,
                        aes(x = Treatment,
                            y = avenellaBM))+
   geom_boxplot()+
   theme_classic()+
   xlab("Treatment") +
   ylab(expression(paste(italic(Avenella ), paste(" (g m"^"-2", ")"))))
)    
plot(SEMdat$Treatment, aveRes) #OK



(avenella__moss_gg <- ggplot(data = SEMdat,
                            aes(x = Moss_depth,
                                y = avenellaBM))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth") +
    ylab(expression(paste("Avenella biomass (g m"^"2", ")")))
)   # no signal
plot(SEMdat$Moss_depth, aveRes) # ok

# adding model fit (accounting for covariates in this case:
summary(avenella2)
ave <- lm(scale(avenellaBM, scale = F)~Soil_temp_C+I(Soil_temp_C^2), data = SEMdat)
#avenella2$fit$par
ave$coefficients[]
avenella_temp_line <- data.frame(Soil_temp = 
                                   seq(min(SEMdat$Soil_temp_C), max(SEMdat$Soil_temp_C), 
                                       length = 20))
avenella_temp_line[,2] <- avenella2$fit$par[1]+
  (avenella_temp_line[,1]*avenella2$fit$par[5])+
  (I(avenella_temp_line[,1]^2)*avenella2$fit$par[6])
avenella_temp_line[,3] <- avenella_temp_line$Soil_temp+mean(SEMdat$Soil_temp)
avenella_temp_line2 <- data.frame(Soil_temp = 
                                   seq(min(SEMdat$Soil_temp_C), max(SEMdat$Soil_temp_C), 
                                       length = 20))
avenella_temp_line2[,2] <- ave$coefficients[1]+mean(SEMdat$avenellaBM)+
  (avenella_temp_line2[,1]*ave$coefficients[2])+
  (I(avenella_temp_line2[,1]^2)*ave$coefficients[3])
avenella_temp_line2[,3] <- avenella_temp_line$Soil_temp+mean(SEMdat$Soil_temp)
(avenella__temp_gg <- ggplot(data = SEMdat,
                             aes(x = Soil_temp,
                                 y = avenellaBM))+
    geom_point()+
    theme_classic()+
    xlab(expression(atop("Soil temperature",( degree~C)))) +
    ylab(expression(paste(italic(Avenella ), paste(" (g m"^"-2", ")"))))+
    geom_line(data=avenella_temp_line, aes(x=V3, y = V2))+
    geom_line(data=avenella_temp_line2, aes(x=V3, y = V2), linetype = 2)
)   #

plot(SEMdat$Soil_temp, aveRes) # Some funnel, but no bias
plot(I(SEMdat$Soil_temp^2), aveRes) # 



(avenella__cci_gg <- ggplot(data = SEMdat,
                             aes(x = CCI,
                                 y = avenellaBM))+
    geom_point()+
    theme_classic()+
    xlab("CCI") +
    ylab(expression(paste("Avenella biomass (g m"^"2", ")")))
)   # 
plot(SEMdat$CCI, aveRes) # OK

cor.test(SEMdat$avenellaBM, SEMdat$shrubBM, method = "kendal")
plot(SEMdat$avenellaBM, SEMdat$shrubBM)





# **Shrubs####
shrubs <-     lme(Log_shrubBM ~ Treatment+Soil_temp_C+CCI,
                 random = ~ 1 | LocalityName3, data = SEMdat, method = "ML")
plot(shrubs)
qqnorm(resid(shrubs))
drop1(shrubs, test = "Chi")
shrubs <- update(shrubs, .~. -Soil_temp_C)
drop1(shrubs, test = "Chi")
summary(shrubs)
plot(shrubs)
qqnorm(resid(shrubs))






# what about logistic regression
plot(SEMdat$shrubBM)
SEMdat$shrubClass <- ifelse(SEMdat$shrubBM<median(SEMdat$shrubBM), 0,1)

shrubs_bin <- glmmTMB(shrubClass ~ Treatment+Moss_depth+CCI+Soil_temp_C 
                   + ( 1 | LocalityName3), family = binomial(), data = SEMdat)
par(mfrow = c(2,2), cex.lab = 1.5, mar = c(5,5,2,2))
E1 <- resid(shrubs_bin, type = "pearson")
plot(x = SEMdat$Soil_temp_C,
     y = E1)
abline(h = 0, lty = 2)

plot(x = fitted(shrubs_bin),
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = fitted(shrubs_bin),
     y = resid(shrubs_bin),
     xlab = "Fitted values",
     ylab = "Observed data mimus Pi")
abline(h = 0, lty = 2)
library(arm)
binnedplot(x = fitted(shrubs_bin),
           y = resid(shrubs_bin))
# half the binned residuals are outside the 95% CI - not good enough

# GLMM  
#  gamma model
shrubs_gamma <- glmmTMB(shrubBM+2 ~ Treatment+CCI+Soil_temp_C
                   + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat)
qqnorm(resid(shrubs_gamma, type = "pearson")) # no

# scaling:
shrubs2 <- glmmTMB(shrubBM+2 ~ Treatment+sMoss_depth+CCI+sSoil_temp
                 + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat)
qqnorm(resid(shrubs2,type = "pearson")) # not enough

# log link
shrubs3 <- glmmTMB(shrubBM+2 ~ Treatment+Moss_depth+CCI+Soil_temp_C
                   + ( 1 | LocalityName3), family = Gamma(link = "log"), data = SEMdat)
qqnorm(resid(shrubs3, type = "pearson")) # no

# model reduction:
shrubs4 <- glmmTMB(shrubBM+2 ~ Treatment+Moss_depth
                   + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat)
qqnorm(resid(shrubs4, type = "pearson")) # no

# log transform
shrubs5 <- lme(Log_shrubBM ~ Treatment+CCI+Soil_temp_C,
                   random = ~ 1 | LocalityName3, data = SEMdat)
qqnorm(resid(shrubs5, type = "pearson")) # no
plot(SEMdat$Log_shrubBM)
summary(shrubs5)
plot(shrubs5)


shrubRes <- resid(shrubs5, type = "pearson")
shrubFit <- fitted(shrubs5)
plot(shrubFit, shrubRes) # OK
sum(SEMdat$shrubBM == 0)/nrow(SEMdat)  # 32% zeros

(shrubs__temp_gg <- ggplot(data = SEMdat,
                             aes(x = Soil_temp,
                                 y = shrubBM))+
    geom_point()+
    theme_classic()+
    xlab("Soil temperature") +
    ylab(expression(paste("Shrub biomass (g m"^"2", ")")))
) # nothing
#plot(SEMdat$Soil_temp_C, shrubRes)#OK
plot(SEMdat$Soil_temp_C, resid(shrubs)) #ok

(shrubs__trt_gg <- ggplot(data = SEMdat,
                           aes(x = Treatment,
                               y = shrubBM))+
    geom_boxplot()+
    theme_classic()+
    xlab("Treatment") +
    ylab(expression(paste("Shrub biomass (g m"^"-2", ")")))
)
#plot(SEMdat$Treatment, shrubRes)
plot(SEMdat$Treatment, resid(shrubs)) #OK

shrubs_and_moss <- partial.resid(Log_shrubBM~Moss_depth, SEM_tSR_1, SEMdat)

(shrubs__moss_gg <- ggplot(data = shrubs_and_moss,
                           aes(x = x.resids,
                               y = y.resids))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm) | others") +
    ylab(expression(paste("Log shrub biomass (g m"^"-2", ") | others")))+
    geom_smooth(method = "lm")
) # possibly humped shaped byt we dont have the data

#plot(SEMdat$Moss_depth, shrubRes)
plot(SEMdat$Moss_depth, resid(shrubs)) #OK

(shrubs__cci_gg <- ggplot(data = SEMdat,
                           aes(x = CCI,
                               y = Log_shrubBM))+
    geom_point()+
    theme_classic()+
    xlab("CCI") +
    ylab(expression(paste("Shrub biomass (g m"^"2", ")")))+
    geom_smooth(method = "lm")
)
partial.resid(Log_shrubBM~CCI, SEM_tSR_4, SEMdat)
#plot(SEMdat$CCI, shrubRes)
plot(SEMdat$CCI, resid(shrubs)) #OK

(shrubs__avenella_gg <- ggplot(data = SEMdat,
                          aes(x = avenellaBM,
                              y = shrubBM))+
    geom_point()+
    theme_classic()+
    xlab("Avenella") +
    ylab(expression(paste("Shrub biomass (g m"^"2", ")")))
)
#plot(SEMdat$avenellaBM, shrubRes)
plot(SEMdat$avenellaBM, resid(shrubs)) #OK
#plot(I(SEMdat$avenellaBM^2), shrubRes)


# **Total SR####
tSR    = lme(total_SR  ~ 
               Treatment + 
               Moss_depth+
               CCI+
               Soil_temp_C+
               avenellaBM+
               Log_shrubBM, 
             random = ~1| LocalityName3, data = SEMdat, method = "ML")

plot(tSR) #OK
resTSR <- resid(tSR)
qqnorm(resTSR) #OK
drop1(tSR, test = "Chi")
tSR <- update(tSR, .~. -CCI)
drop1(tSR, test = "Chi")
tSR <- update(tSR, .~. -Soil_temp_C)
drop1(tSR, test = "Chi")
tSR <- update(tSR, .~. -Log_shrubBM)
drop1(tSR, test = "Chi")
tSR <- update(tSR, .~. -Moss_depth)
drop1(tSR, test = "Chi")
plot(tSR) #OK
resTSR <- resid(tSR)
qqnorm(resTSR) #OK
plot(SEMdat$Moss_depth, resTSR)

summary(tSR)
(tSR__moss_gg <- ggplot(data = SEMdat,
                            aes(x = Moss_depth,
                                y = total_SR))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab("Total species richness"))
plot(SEMdat$Moss_depth, resTSR) #OK


(tSR__cci_gg <- ggplot(data = SEMdat,
                        aes(x = CCI,
                            y = total_SR))+
    geom_point()+
    theme_classic()+
    xlab("CCI") +
    ylab("Total species richness"))
plot(SEMdat$CCI, resTSR) #OK

(tSR__temp_gg <- ggplot(data = SEMdat,
                        aes(x = Soil_temp,
                            y = total_SR))+
    geom_point()+
    theme_classic()+
    xlab("Soil temp") +
    ylab("Total species richness"))
plot(SEMdat$Soil_temp, resTSR) #OK



(tSR__avenella_gg <- ggplot(data = SEMdat,
                               aes(x = avenellaBM,
                                   y = total_SR))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste(italic(Avenella ), paste(" biomass (g m"^"-2", ")")))) +
    ylab("Total species richness") +
  geom_abline(intercept = 16.125858, slope =  -0.055992))
plot(SEMdat$avenellaBM, resTSR) #OK

(tSR__shrubs_gg <- ggplot(data = SEMdat,
                            aes(x = shrubBM,
                                y = total_SR))+
    geom_point()+
    theme_classic()+
    xlab("Dwarf shrubs") +
    ylab("Total species richness"))
plot(SEMdat$shrubBM, resTSR) #OK

# **Vasc SR####
vSR    <-  lme(vasc_SR  ~ 
                 Treatment + 
                 Moss_depth+
                 CCI+
                 Soil_temp_C+
                 avenellaBM+
                 Log_shrubBM, 
              random = ~1| LocalityName3, data = SEMdat, method = "ML")
plot(vSR)
qqnorm(resid(vSR))
drop1(vSR, test = "Chi")
vSR <- update(vSR, .~. -Soil_temp_C)
drop1(vSR, test = "Chi")
vSR <- update(vSR, .~. -Log_shrubBM)
drop1(vSR, test = "Chi")
vSR <- update(vSR, .~. -avenellaBM)
drop1(vSR, test = "Chi")
vSR <- update(vSR, .~. -Treatment)
drop1(vSR, test = "Chi")
vSR <- update(vSR, .~. -Moss_depth)
drop1(vSR, test = "Chi")


plot(vSR)
resVSR <- resid(vSR)
qqnorm(resVSR)
plot(SEMdat$CCI, resVSR)

summary(vSR)

qqnorm(resVSR) #OK
summary(vSR)

(vSR__moss_gg <- ggplot(data = SEMdat,
                        aes(x = Moss_depth,
                            y = vasc_SR))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab("Vascular plant\nspecies richness"))
plot(SEMdat$Moss_depth, resVSR) #OK


(vSR__cci_gg <- ggplot(data = SEMdat,
                       aes(x = CCI,
                           y = vasc_SR))+
    geom_point()+
    theme_classic()+
    xlab("CCI") +
    ylab("Vascular plant\nspecies richness"))
plot(SEMdat$CCI, resVSR) #OK

(vSR__temp_gg <- ggplot(data = SEMdat,
                        aes(x = Soil_temp,
                            y = vasc_SR))+
    geom_point()+
    theme_classic()+
    xlab("Soil temp") +
    ylab("Vascular plant\nspecies richness"))
plot(SEMdat$Soil_temp, resVSR) #OK



(vSR__avenella_gg <- ggplot(data = SEMdat,
                            aes(x = avenellaBM,
                                y = vasc_SR))+
    geom_point()+
    theme_classic()+
    xlab("Avenella") +
    ylab("Vascular plant\nspecies richness"))
    
plot(SEMdat$avenellaBM, resVSR) #OK

(vSR__shrubs_gg <- ggplot(data = SEMdat,
                          aes(x = shrubBM,
                              y = vasc_SR))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste("Dwarf shrubs (g m"^"-2", ")"))) +
    ylab("Vascular plant\nspecies richness"))
plot(SEMdat$shrubBM, resTSR) #OK



# **Moss SR####
#mSR    = lme(moss_SR  ~ Moss_depth+CCI+Soil_temp_C+avenellaBM+shrubBM, 
#             random = ~1| LocalityName3, data = SEMdat, method = "ML")

MyVars <- c("moss_SR", "Treatment",
            "Moss_depth", "CCI", "Soil_temp_C",
            "avenellaBM", "Log_shrubBM")
Mypairs(SEMdat[,MyVars])

mSR2    = lme(moss_SR  ~ 
                Treatment + 
                Moss_depth+
                I(Moss_depth^2)+
                CCI+
                Soil_temp_C+
                avenellaBM+
                Log_shrubBM,
             random = ~1| LocalityName3, data = SEMdat, method = "ML")
plot(mSR2)
qqnorm(resid(mSR2))
drop1(mSR2, test = "Chi")
mSR2 <- update(mSR2, .~. -avenellaBM)
drop1(mSR2, test = "Chi")
mSR2 <- update(mSR2, .~. -Log_shrubBM)
drop1(mSR2, test = "Chi")
mSR2 <- update(mSR2, .~. -Soil_temp_C)
drop1(mSR2, test = "Chi")
mSR2 <- update(mSR2, .~. -CCI)
drop1(mSR2, test = "Chi")
plot(mSR2)
qqnorm(resid(mSR2))
plot(SEMdat$Treatment, resid(mSR2))
plot(SEMdat$Moss_depth, resid(mSR2))
plot(SEMdat$CCI, resid(mSR2))
plot(SEMdat$Soil_temp, resid(mSR2))  # slight pattern
#mSR3 <- update(mSR2, .~. +Soil_temp_C)
#drop1(mSR3, test = "Chi")
# no, safe to remove
plot(SEMdat$avenellaBM, resid(mSR2))
plot(SEMdat$Log_shrubBM, resid(mSR2))
summary(mSR2)

mSR2 <- update(mSR2, .~. -CCI)
# small but not significant improvement. 

mSR_glmm    = glmmTMB(moss_SR  ~ Moss_depth+I(Moss_depth^2)+CCI+Soil_temp_C+avenellaBM+shrubBM+ 
             (1| LocalityName3), data = SEMdat)   # similar results
summary(mSR_glmm)

plot(mSR2) #OK
resMSR <- resid(mSR2)
qqnorm(resMSR) #OK
summary(mSR2) # using this one:
mSR    = lme(moss_SR  ~ Moss_depth_C+I(Moss_depth_C^2)+CCIprop+Soil_temp_C+avenellaBM+Log_shrubBM, 
             random = ~1| LocalityName3, data = SEMdat, method = "REML")
summary(mSR) # moss is centered to reduce colinearity

mSRX    = lme(moss_SR  ~ Moss_depth+I(Moss_depth^2),
              random = ~1| LocalityName3, data = SEMdat, method = "REML")
summary(mSRX)
mossSR_depth_line <- data.frame(Moss_depth = 
                        seq(min(SEMdat$Moss_depth), max(SEMdat$Moss_depth), 
                            length = 20))
mossSR_depth_line[,2] <- mSRX$coefficients$fixed[1]+
  (mossSR_depth_line[,1]*mSRX$coefficients$fixed[2])+
  (I(mossSR_depth_line[,1]^2)*mSRX$coefficients$fixed[3])
(mSR__moss_gg <- ggplot(data = SEMdat,
                        aes(x = Moss_depth,
                            y = moss_SR))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab("Bryophyte\nspecies richness")+
    geom_line(data=mossSR_depth_line, aes(x=Moss_depth, y = V2)))
plot(SEMdat$Moss_depth, resMSR) #OK


(mSR__cci_gg <- ggplot(data = SEMdat,
                       aes(x = CCI,
                           y = moss_SR))+
    geom_point()+
    theme_classic()+
    xlab("CCI") +
    ylab("Bryophyte\nspecies richness"))
plot(SEMdat$CCI, resMSR) #OK

(mSR__temp_gg <- ggplot(data = SEMdat,
                        aes(x = Soil_temp,
                            y = moss_SR))+
    geom_point()+
    theme_classic()+
    xlab("Soil temp") +
    ylab("Bryophyte\nspecies richness"))
plot(SEMdat$Soil_temp, resMSR) #not perfect
plot(SEMdat$Soil_temp, resid(mSR_glmm, "pearson")) #same


(mSR__avenella_gg <- ggplot(data = SEMdat,
                            aes(x = avenellaBM,
                                y = moss_SR))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste(italic(Avenella ), paste(" biomass (g m"^"-2", ")")))) +
    ylab("Bryophyte\nspecies richness"))
plot(SEMdat$avenellaBM, resMSR) #OK

moss_sr_and_shrubs <- partial.resid(moss_SR~Log_shrubBM, SEM_tSR_1, SEMdat)

(mSR__shrubs_gg <- ggplot(data = moss_sr_and_shrubs,
                          aes(x = x.resids,
                              y = y.resids))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste(atop("Log dwarf shrubs", "(g m"^"-2"*") | others")))) +
    ylab("Bryophyte\nspecies richness")+
    geom_smooth(method = "lm"))

plot(SEMdat$shrubBM, resMSR) #

summary(lm(SEMdat$moss_SR~SEMdat$shrubBM))







# **Vascular Shannon####
vascS    = lme(shannon_vasc  ~ 
                 Treatment + 
                 Moss_depth+
                 CCI+
                 Soil_temp_C+
                 avenellaBM+
                 Log_shrubBM,
             random = ~1| LocalityName3, data = SEMdat, method = "ML")
plot(vascS)
res_vascS <- resid(vascS)
qqnorm(res_vascS)
plot(fitted(vascS), res_vascS)

drop1(vascS, test= "Chi")
vascS <- update(vascS, .~. -Moss_depth)
drop1(vascS, test= "Chi")
vascS <- update(vascS, .~. -Soil_temp_C)
drop1(vascS, test= "Chi")
vascS <- update(vascS, .~. -avenellaBM)
drop1(vascS, test= "Chi")
vascS <- update(vascS, .~. -Log_shrubBM)
drop1(vascS, test= "Chi")
plot(vascS)
res_vascS <- resid(vascS)
qqnorm(res_vascS)
plot(fitted(vascS), res_vascS)
summary(vascS)


# BETA regression  # skipp this ####
summary(SEMdat$simpsons_vasc)
plot(SEMdat$simpsons_vasc) #one zero
View(SEMdat[SEMdat$simpsons_vasc==0,])  # one species only

SEMdat$simpsons_moss2 <- SEMdat$simpsons_moss
SEMdat$simpsons_moss2[SEMdat$simpsons_moss2 == 0] <- NA

SEMdat$simpsons_vasc_x <- ( SEMdat$simpsons_vasc * (nrow(SEMdat) - 1) + 0.5 ) / nrow(SEMdat) 
vascS_3    = glmmTMB(simpsons_vasc_x  ~ Moss_depth+CCI+Soil_temp_C+avenellaBM+shrubBM +
               (1| LocalityName3), data = SEMdat,
               family= list(family = "beta", link = "logit"))
summary(vascS_3)
# Model validation
E1 <- resid(vascS_3)
F1 <- fitted(vascS_3)
phi  <- summary(vascS_3)$sigma

VarY <- F1 *  (1 - F1) / (1 + phi)  
E1p  <- (SEMdat$simpsons_vasc_x - F1) / sqrt(VarY) # Pearson residuals

# end beta regression ####

# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = F1, 
     y = E1p,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

qqnorm(E1)
qqline(E1) # still not great, but not sure its a requirement

# Plot Pearson residuals vs each covariate
grid.arrange(
  #treatment
ggplot()+  geom_boxplot(data = SEMdat, 
               aes(y = res_vascS, x = Treatment))+
  xlab("Treatment") + ylab("Pearson residuals"),
ggplot()+  geom_point(data = SEMdat, 
                        aes(y = res_vascS, x = Moss_depth))+
  xlab("Moss_depth") + ylab("Pearson residuals"),
ggplot()+  geom_point(data = SEMdat, 
                      aes(y = res_vascS, x = Soil_temp_C))+
  xlab("Soil_temp_C") + ylab("Pearson residuals"),
ggplot()+  geom_point(data = SEMdat, 
                      aes(y = res_vascS, x = avenellaBM))+
  xlab("avenellaBM") + ylab("Pearson residuals"),
ggplot()+  geom_point(data = SEMdat, 
                      aes(y = res_vascS, x = shrubBM))+
  xlab("shrubBM") + ylab("Pearson residuals")
)

# OK

summary(vascS)

(vascS__moss_gg <- ggplot(data = SEMdat,
                        aes(x = Moss_depth,
                            y = simpsons_vasc))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm)") +
    ylab("Simpson's (vascular plants)"))
# possibly quadratic
plot(SEMdat$Moss_depth, res_vascS) #ok-ish


####################################
# Visualise the model
  
MyData3 <- expand.grid(CCI = SEMdat$CCI )
head(MyData3)

#Convert the covariate values into an X matrix
X <- model.matrix(~ CCI, data = MyData3)

#Extract parameters and parameter covariance matrix
betas   <- fixef(vascS_3)$cond
CovBeta <- vcov(vascS_3)$cond

#Calculate the fitted values in the predictor scale
MyData3$eta <- X * betas[3]
MyData3$Pi  <- exp(MyData3$eta) / (1 + exp(MyData3$eta))
head(MyData3)
class(MyData3)
MyData4 <- do.call(data.frame,MyData3)

(vascS__cci_gg <- ggplot(data = SEMdat,
                       aes(x = CCI,
                           y = simpsons_vasc))+
    geom_point()+
    theme_classic()+
    xlab("CCI") +
    ylab("Simpson's\n(vascular plants)")+
    geom_line(data = MyData4, aes(x=CCI, y=Pi.CCI)))


plot(SEMdat$CCI, res_vascS) #OK

(vascS__temp_gg <- ggplot(data = SEMdat,
                        aes(x = Soil_temp,
                            y = simpsons_vasc))+
    geom_point()+
    theme_classic()+
    xlab("Soil temp") +
    ylab("Simpson's (vascular plants)"))
plot(SEMdat$Soil_temp, res_vascS) #ok



# Avenella - getting fitted values
MyData5 <- expand.grid(avenella = SEMdat$avenellaBM )
head(MyData5)

#Convert the covariate values into an X matrix
X <- model.matrix(~ avenella, data = MyData5)

#Extract parameters and parameter covariance matrix
# see above

#Calculate the fitted values in the predictor scale
MyData5$eta <- X * betas[5]
MyData5$Pi  <- exp(MyData5$eta) / (1 + exp(MyData5$eta))
head(MyData5)
MyData5 <- do.call(data.frame,MyData5)
(vascS__avenella_gg <- ggplot(data = SEMdat,
                            aes(x = avenellaBM,
                                y = simpsons_vasc))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste(italic(Avenella ), paste(" biomass (g m"^"-2", ")")))) +
    ylab("Simpson's index\n(vascular plants)")+
    geom_line(data=MyData5, aes(x=avenella, y=Pi.avenella)))
plot(SEMdat$avenellaBM, res_vascS) #OK-ish

# Dwarf shrubs - getting fitted values
MyData6 <- expand.grid(shrubs = SEMdat$shrubBM )
head(MyData6)

#Convert the covariate values into an X matrix
X <- model.matrix(~ shrubs, data = MyData6)

#Extract parameters and parameter covariance matrix
# see above

#Calculate the fitted values in the predictor scale
MyData6$eta <- X * betas[6]
MyData6$Pi  <- exp(MyData6$eta) / (1 + exp(MyData6$eta))
head(MyData6)
MyData6 <- do.call(data.frame,MyData6)
(vascS__shrubs_gg <- ggplot(data = SEMdat,
                          aes(x = shrubBM,
                              y = simpsons_vasc))+
    geom_point()+
    theme_classic()+
    xlab(expression(paste("Dwarf shrubs (g m"^"-2", ")"))) +
    ylab("Simpson's index\n (vascular plants)")+
    geom_line(data=MyData6, aes(x=shrubs, y=Pi.shrubs)))
plot(SEMdat$shrubBM, res_vascS) #OK



# **Bryophytes Shannon####
View(SEMdat[SEMdat$shannon_moss==0,])


mossS    = lme(shannon_moss  ~ 
                 Treatment + 
                 Moss_depth+
                 CCI+
                 Soil_temp_C+
                 avenellaBM+
                 Log_shrubBM,
               random = ~1| LocalityName3, data = SEMdat, method = "ML")

plot(mossS)
qqnorm(resid(mossS))
qqline(resid(mossS))
drop1(mossS, test = "Chi")
mossS <- update(mossS, .~. -Log_shrubBM)
drop1(mossS, test = "Chi")
mossS <- update(mossS, .~. -avenellaBM)
drop1(mossS, test = "Chi")
plot(mossS)
qqnorm(resid(mossS))
qqline(resid(mossS))

summary(mossS)

#View(SEMdat[SEMdat$simpsons_moss==0,])
# removing plots with no moss found
#SEMdat$simpsons_moss2 <- SEMdat$simpsons_moss
#SEMdat$simpsons_moss2[SEMdat$simpsons_moss == 0] <- NA
#SEMdat_mossBeta <- na.omit(SEMdat)
#SEMdat_mossBeta$simpsons_moss_x <- ( SEMdat_mossBeta$simpsons_moss2 * (nrow(SEMdat_mossBeta) - 1) + 0.5 ) / nrow(SEMdat_mossBeta) 

#mossS    = glmmTMB(simpsons_moss_x  ~ Moss_depth+CCI+Soil_temp_C+avenellaBM+shrubBM +
#                       (1| LocalityName3), data = SEMdat_mossBeta,
#                     family= list(family = "beta", link = "logit"))




# Model validation
E1 <- resid(mossS)
F1 <- fitted(mossS)
#phi  <- summary(mossS)$sigma

#VarY <- F1 *  (1 - F1) / (1 + phi)  
#E1p  <- (SEMdat_mossBeta$simpsons_moss_x - F1) / sqrt(VarY) # Pearson residuals


# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

qqnorm(E1)
qqline(E1) #  ok

# Plot Pearson residuals vs each covariate
grid.arrange(
  #treatment
  ggplot()+  geom_boxplot(data = SEMdat, 
                          aes(y = E1, x = Treatment))+
    xlab("Treatment") + ylab("Pearson residuals"),
  ggplot()+  geom_point(data = SEMdat, 
                        aes(y = E1, x = Moss_depth))+
    xlab("Moss_depth") + ylab("Pearson residuals"),
  ggplot()+  geom_point(data = SEMdat, 
                        aes(y = E1, x = CCI))+
    xlab("CCI") + ylab("Pearson residuals"),
  ggplot()+  geom_point(data = SEMdat, 
                        aes(y = E1, x = Soil_temp_C))+
    xlab("Soil_temp_C") + ylab("Pearson residuals"),
  ggplot()+  geom_point(data = SEMdat, 
                        aes(y = E1, x = avenellaBM))+
    xlab("avenellaBM") + ylab("Pearson residuals"),
  ggplot()+  geom_point(data = SEMdat, 
                        aes(y = E1, x = shrubBM))+
    xlab("shrubBM") + ylab("Pearson residuals")
)

# OK

mossS_and_moss <- partial.resid(shannon_moss~Moss_depth, SEM_tSR_1, SEMdat)
(mossS__moss_gg <- ggplot(data = mossS_and_moss,
                          aes(x = x.resids,
                              y = y.resids))+
    geom_point()+
    theme_classic()+
    xlab("Moss depth (cm) | others") +
    ylab("Shannon entropy\n(bryophytes) | others")+
    geom_smooth(method= "lm"))

plot(SEMdat_mossBeta$Moss_depth, E1p) #ok


# Visualise the model for moss
#Extract parameters and parameter covariance matrix
betas   <- fixef(mossS)$cond
CovBeta <- vcov(mossS)$cond

start <- min(SEMdat$Moss_depth)*betas[2]+betas[1]
start <- exp(start)/(1+exp(start))
stop <- max(SEMdat$Moss_depth)*betas[2]+betas[1]
stop <- exp(stop)/(1+exp(stop))
mossS__moss_gg <- mossS__moss_gg +
  geom_segment(x=min(SEMdat$Moss_depth),
               xend =max(SEMdat$Moss_depth),
               y=start, yend=stop)


moss_sr_and_cci <- partial.resid(moss_SR~CCI, SEM_tSR_1, SEMdat)

(mossS__cci_gg <- ggplot(data = moss_sr_and_cci,
                          aes(x = x.resids,
                              y = y.resids))+
    geom_point()+
    theme_classic()+
    xlab("CCI % | others") +
    ylab("Shannon entropy\n(bryophytes) |others")+
    geom_smooth(method = "lm"))

# Visualise the model for CCI
start <- min(SEMdat$CCI)*betas[3]+betas[1]
start <- exp(start)/(1+exp(start))
stop <- max(SEMdat$CCI)*betas[3]+betas[1]
stop <- exp(stop)/(1+exp(stop))
mossS__cci_gg <- mossS__cci_gg +
  geom_segment(x=min(SEMdat$CCI),
               xend =max(SEMdat$CCI),
               y=start, yend=stop)
plot(SEMdat_mossBeta$Moss_depth, E1p) #OK

moss_sr_and_temp <- partial.resid(moss_SR~Soil_temp_C, SEM_tSR_1, SEMdat)
(mossS__temp_gg <- ggplot(data = moss_sr_and_temp,
                         aes(x = x.resids,
                             y = y.resids))+
    geom_point()+
    theme_classic()+
    xlab(expression(atop("Soil temperature | others",( degree~C)))) +
    ylab("Shannon entropy (bryophytes)\n| others")+
  geom_smooth(method = "lm"))
(mossS__aven_gg <- ggplot(data = SEMdat_mossBeta,
                          aes(x = avenellaBM,
                              y = simpsons_moss))+
    geom_point()+
    theme_classic()+
    xlab("Avenella") +
    ylab("Simpson's (moss)"))
(mossS__shrubs_gg <- ggplot(data = SEMdat_mossBeta,
                          aes(x = shrubBM,
                              y = simpsons_moss))+
    geom_point()+
    theme_classic()+
    xlab("shrubs") +
    ylab("Simpson's (moss)"))


#----------------------------------------------------------#
#----------------------------------------------------------#
#
# SEM ####
#
#----------------------------------------------------------#
#----------------------------------------------------------#

SEM1 <- list(
  # FOREST STRUCTURE:
  CCI =        lme(CCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat),
  
  # SOIL TEMPERATURE
  Soil_temp =  lme(Soil_temp_C ~ Treatment+CCI,
                   random = ~ 1 | LocalityName3, data = SEMdat),
  
  # COMPETITIVE SPECIES
  glmmTMB(avenellaBM+1 ~ Treatment+Soil_temp_C+I(Soil_temp_C^2) 
          + ( 1 | LocalityName3), family = Gamma(link = "log"), 
          data = SEMdat),
  shrubs =     lme(Log_shrubBM ~ Treatment+CCI,
                   random = ~ 1 | LocalityName3, data = SEMdat),
  
  # DIVERSITY 
  tSR    =   lme(total_SR  ~ Treatment+avenellaBM,
                 random = ~1| LocalityName3, data = SEMdat),
  vSR    =  lme(vasc_SR  ~ CCI, 
                random = ~1| LocalityName3, data = SEMdat),
  vascS    = lme(shannon_vasc  ~ Treatment+CCI, 
                 random = ~1| LocalityName3, data = SEMdat),
  mSR    =   lme(moss_SR  ~ Moss_depth+I(Moss_depth^2)+Treatment, 
                 random = ~1| LocalityName3, data = SEMdat),
  mossS    = lme(shannon_moss  ~ Soil_temp_C+CCI+Treatment+Moss_depth,
                 random = ~1| LocalityName3, data = SEMdat)
 
  )

corrs3 <- c("shannon_vasc ~~ total_SR", 
            "shannon_vasc ~~ vasc_SR",
            "shannon_vasc ~~ moss_SR",
            "shannon_vasc ~~ shannon_moss",
            "vasc_SR ~~ shannon_moss",
            "vasc_SR ~~ total_SR",
            "vasc_SR ~~ moss_SR",
            "moss_SR ~~ shannon_moss",
            "moss_SR ~~ total_SR",
            "shannon_moss ~~ total_SR",
            "CCI ~~ Moss_depth",
            "Log_shrubBM ~~ avenellaBM",
            "Moss_depth ~~ Log_shrubBM")

(SEM1_fit <- sem.fit(SEM1, SEMdat, conditional = F, 
                          corr.errors = corrs3)) 
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
write.csv(SEM1_fit, "SEM1_fit.csv")
(SEM1_indFit <- sem.model.fits(SEM1))  # dont work for gamma
write.csv(SEM1_indFit, "SEM1_indFit.csv")


SEM_tSR_4 <- list(
  
  # FOREST STRUCTURE:
  Moss_depth = lme(Moss_depth ~ Treatment, random = ~ 1 | LocalityName3, data = SEMdat),
  CCI =        lme(CCI ~ Treatment , random = ~ 1 | LocalityName3, data = SEMdat),
  
  # SOIL TEMPERATURE
  Soil_temp =  lme(Soil_temp_C ~ Treatment+CCI,
                   random = ~ 1 | LocalityName3, data = SEMdat),
  
  # COMPETITIVE SPECIES
  avenella =   glmmTMB(avenellaBM+0.1 ~ Treatment+Soil_temp_C+I(Soil_temp_C^2)+Moss_depth
                       + ( 1 | LocalityName3), family = Gamma(link = "identity"), data = SEMdat),
  shrubs =     lme(Log_shrubBM ~ Treatment+CCI,
                   random = ~ 1 | LocalityName3, data = SEMdat),
  
  # DIVERSITY 
  tSR    =   lme(total_SR  ~ Treatment+avenellaBM,
                 random = ~1| LocalityName3, data = SEMdat),
  vascS    = lme(shannon_vasc  ~ CCI, 
                 random = ~1| LocalityName3, data = SEMdat),
  mossS    = lme(shannon_moss  ~ Soil_temp_C+CCI+Treatment+Moss_depth+avenellaBM,
                 random = ~1| LocalityName3, data = SEMdat),
  vSR    =  lme(vasc_SR  ~ Moss_depth, 
                random = ~1| LocalityName3, data = SEMdat),
  mSR    =   lme(moss_SR  ~ Moss_depth+I(Moss_depth^2)+avenellaBM, 
                 random = ~1| LocalityName3, data = SEMdat))


# spesifying correlated errors that should not be avaluated in d-sep test or Fishers C





corrs4 <- c("shannon_vasc ~~ total_SR", 
            "shannon_vasc ~~ vasc_SR",
            "shannon_vasc ~~ moss_SR",
            "shannon_vasc ~~ shannon_moss",
            "vasc_SR ~~ shannon_moss",
            "vasc_SR ~~ total_SR",
            "vasc_SR ~~ moss_SR",
            "moss_SR ~~ shannon_moss",
            "moss_SR ~~ total_SR",
            "shannon_moss ~~ total_SR",
            "CCI ~~ Moss_depth",
            "Log_shrubBM ~~ avenellaBM",
            "Moss_depth ~~ Log_shrubBM")




(SEM_tSR_4_fit <- sem.fit(SEM_tSR_4, SEMdat, conditional = F, 
                          corr.errors = corrs3)) 
setwd("M:\\Anders L Kolstad\\systherb data\\TEMPERATURE PAPER")
#write.csv(SEM_tSR_4_fit, "SEM_tSR_4_modFit.csv")
(SEM_tSR_4_indFit <- sem.model.fits(SEM_tSR_4))  # dont work for gamma
#write.csv(SEM_tSR_4_indFit, "SEM_tSR_4_indFit.csv")




(c.tabl <- sem.coefs(SEM1, SEMdat, standardize = "none", intercept = F,
                     corr.errors = corrs3))
class(c.tabl$response)
c.tabl$response <- as.character(c.tabl$response)
c.tabl$response[c.tabl$response == "Soil_temp_C"]  <- c("Soil temperature")
c.tabl$response[c.tabl$response == "CCI"]  <- c("Canopy cover")
c.tabl$response[c.tabl$response == "Moss_depth"]  <- c("Moss depth")
c.tabl$response[c.tabl$response == "TreatmentOpen plots"]  <- c("Herbivore exclusion") # remember to change signs
#c.tabl$response[c.tabl$response == "UCI"]  <- c("Vegetation density")
c.tabl$response[c.tabl$response == "avenellaBM + 0.1"]  <- c("Avenella flexuosa")
c.tabl$response[c.tabl$response == "Log_shrubBM"]  <- c("Log(shrub biomass)")
c.tabl$response[c.tabl$response == "shannon_vasc"]  <- c("Shannon entropy (vascular plants)")
c.tabl$response[c.tabl$response == "shannon_moss"]  <- c("Shannon entropy (bryophytes)")
c.tabl$response[c.tabl$response == "moss_SR"]  <- c("Species richness (bryophytes)")
c.tabl$response[c.tabl$response == "vasc_SR"]  <- c("Species richness (vascular plants)")
c.tabl$response[c.tabl$response == "total_SR"]  <- c("Species richness (total)")

c.tabl$response[c.tabl$response == "~~ shannon_vasc"]  <- c("Shannon entropy (vascular plants")
c.tabl$response[c.tabl$response == "~~ vasc_SR"]  <- c("Species richness (vascular plants)")
c.tabl$response[c.tabl$response == "~~ moss_SR"]  <- c("Species richness (bryophytes)")
c.tabl$response[c.tabl$response == "~~ shannon_moss"]  <- c("Shannon entropy (bryophytes)")
c.tabl$response[c.tabl$response == "~~ Moss_depth"]  <- c("Moss depth")
c.tabl$response[c.tabl$response == "~~ CCI"]  <- c("Canopy cover")
c.tabl$response[c.tabl$response == "~~ Log_shrubBM"]  <- c("Log(shrub biomass)")

c.tabl$predictor <- as.character(c.tabl$predictor)
c.tabl$predictor[c.tabl$predictor == "~~ total_SR"]  <- c("Species richness (total)")
c.tabl$predictor[c.tabl$predictor == "~~ vasc_SR"]  <- c("Species richness (vascular plants)")
c.tabl$predictor[c.tabl$predictor == "~~ moss_SR"]  <- c("Species richness (bryophytes)")
c.tabl$predictor[c.tabl$predictor == "~~ shannon_moss"]  <- c("Shannon entropy (bryophytes)")
c.tabl$predictor[c.tabl$predictor == "~~ Moss_depth"]  <- c("Moss depth")
c.tabl$predictor[c.tabl$predictor == "~~ CCI"]  <- c("Canopy cover")
c.tabl$predictor[c.tabl$predictor == "~~ Log_shrubBM"]  <- c("Log(shrub biomass)")
c.tabl$predictor[c.tabl$predictor == "~~ avenellaBM"]  <- c("Avenella flexuosa")


c.tabl$predictor[c.tabl$predictor == "Soil_temp_C"]  <- c("Soil temperature")
c.tabl$predictor[c.tabl$predictor == "CCI"]  <- c("Canopy cover")
c.tabl$predictor[c.tabl$predictor == "Moss_depth"]  <- c("Moss depth")
c.tabl$predictor[c.tabl$predictor == "TreatmentOpen plots"]  <- c("Herbivore exclusion") # remember to change signs
#c.tabl$predictor[c.tabl$predictor == "UCI"]  <- c("Vegetation density")
c.tabl$predictor[c.tabl$predictor == "avenellaBM"]  <- c("Avenella flexuosa")
c.tabl$predictor[c.tabl$predictor == "Log_shrubBM"]  <- c("Log(shrub biomass)")
c.tabl$predictor[c.tabl$predictor == "shannon_vasc"]  <- c("Shannon entropy (vascular plants")
c.tabl$predictor[c.tabl$predictor == "shannon_moss"]  <- c("Shannon entropy (bryophytes)")
c.tabl$predictor[c.tabl$predictor == "moss_SR"]  <- c("Species richness (bryophytes)")
c.tabl$predictor[c.tabl$predictor == "vasc_SR"]  <- c("Species richness (vascular plants)")
c.tabl$predictor[c.tabl$predictor == "total_SR"]  <- c("Species richness (total)")
c.tabl$predictor[c.tabl$predictor == "I(Soil_temp_C^2)"]  <- c("Soil temperature ^2")
c.tabl$predictor[c.tabl$predictor == "I(Moss_depth^2)"]  <- c("Moss depth ^2")


write.csv(c.tabl, "SEM_coeff_raw.csv", row.names = F)


#----------------------------------------------------------#
#----------------------------------------------------------#
#
# All plots ####
#
#----------------------------------------------------------#
#----------------------------------------------------------#

#plotting relationships with p>0.01
# need to align the axes somehow...



#1
n1 <-  ggplot(data = SEMdat,
         aes(x = Soil_temp,
             y = avenellaBM))+
    geom_point()+
    theme_classic()+
    xlab(expression(atop("Soil temperature",( degree~C)))) +
    ylab(expression(paste(italic(Avenella ), paste(" (g m"^"-2", ")"))))+
    geom_line(data=avenella_temp_line2, aes(x=V3, y = V2), linetype = 1)
  

#2  
n2 <-   ggplot(data = SEMdat,
         aes(x = Treatment,
             y = avenellaBM))+
    geom_boxplot()+
    theme_classic()+
    xlab("Treatment") +
    ylab(expression(paste(italic(Avenella ), paste(" (g m"^"-2", ")"))))

  
  
#2  
n3 <- ggplot(data = SEMdat,
       aes(x = Treatment,
           y = CCI))+
  geom_boxplot()+
  theme_classic()+
  xlab("Treatment") +
  ylab("Canopy cover (%)")

#3

n4 <- ggplot(data = SEMdat,
       aes(x = CCI,
           y = Soil_temp))+
  geom_point()+
  theme_classic()+
  xlab("Canopy Cover Index (%)") +
  ylab(expression(atop("Soil temperature",( degree~C))))+
  geom_smooth(method = "lm", se = F, colour = "black")+
  xlim(c(0,100))

n5 <- ggplot(data = SEMdat,
       aes(x = Moss_depth,
           y = moss_SR))+
  geom_point()+
  theme_classic()+
  xlab("Moss depth (cm)") +
  ylab("Bryophyte\nspecies richness")+
  geom_line(data=mossSR_depth_line, aes(x=Moss_depth, y = V2))


n6 <- ggplot(data = SEMdat,
       aes(x = Moss_depth,
           y = shannon_moss))+
  geom_point()+
  theme_classic()+
  xlab("Moss depth (cm)") +
  ylab("Shannon entropy\n(bryophytes)")+
  geom_smooth(method= "lm", se=F, colour = "black")

library(grid)
setwd("M:/Anders L Kolstad/systherb data/TEMPERATURE PAPER/figures")
tiff("strong_arcs.tiff", units = "cm", res = 300, height = 40, width = 20)
grid.draw(rbind(ggplotGrob(n1), 
                ggplotGrob(n2), 
                ggplotGrob(n3), 
                ggplotGrob(n4),
                ggplotGrob(n5),
                ggplotGrob(n6),
                  size = "last"))


dev.off()



# other less significant arcs



#3
ggplot(data = SEMdat,
       aes(x = Treatment,
           y = shrubBM))+
  geom_boxplot()+
  theme_classic()+
  xlab("Treatment") +
  ylab(expression(paste("Shrub biomass", paste(" (g m"^"-2", ")"))))
,

ggplot(data = SEMdat,
       aes(x = CCI,
           y = shrubBM))+
  geom_point()+
  theme_classic()+
  xlab("Canopy cover (%)") +
  ylab(expression(paste("Shrub biomass", paste(" (g m"^"-2", ")"))))+
  geom_smooth(method= "lm", se=F, colour = "black")
,

ggplot(data = SEMdat,
       aes(x = Moss_depth,
           y = shannon_moss))+
  geom_point()+
  theme_classic()+
  xlab("Moss depth (cm)") +
  ylab("Shannon entropy\n(bryophytes)")+
  geom_smooth(method= "lm"),


#4
ggplot(data = SEMdat,
       aes(x = CCI,
           y = shannon_moss))+
  geom_point()+
  theme_classic()+
  xlab("Canopy cover (%)") +
  ylab("Shannon entropy\n(bryophytes)")+
  geom_smooth(method = "lm"),

#5
ggplot(data = SEMdat,
       aes(x = CCI,
           y = shannon_vasc))+
  geom_point()+
  theme_classic()+
  xlab("CCI") +
  ylab("Shannon entropy\n(vascular plants)")+
  geom_smooth(method = "lm"),

#6



#7 skip trt effect on temp
#8
ggplot(data = SEMdat,
       aes(x = Moss_depth,
           y = moss_SR))+
  geom_point()+
  theme_classic()+
  xlab("Moss depth (cm)") +
  ylab("Bryophyte\nspecies richness")+
  geom_line(data=mossSR_depth_line, aes(x=Moss_depth, y = V2)),


#9

ggplot(data = SEMdat,
       aes(x = Treatment,
           y = total_SR))+
  geom_boxplot()+
  theme_classic()+
  xlab("Treatment") +
  ylab("Total species richness"),

#10
ggplot(data = SEMdat,
       aes(x = avenellaBM,
           y = total_SR))+
  geom_point()+
  theme_classic()+
  xlab(expression(paste(italic(Avenella ), paste(" biomass (g m"^"-2", ")")))) +
  ylab("Total species richness") +
  geom_abline(intercept = 16.125858, slope =  -0.055992, linetype=2)
ggplot(data = SEMdat,
       aes(x = Treatment,
           y = Soil_temp))+
  geom_boxplot()+
  theme_classic()+
  xlab("Treatment") +
  ylab(expression(atop("Soil temperature",( degree~C))))