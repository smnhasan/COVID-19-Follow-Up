
library(ggplot2)

ct <- c(25.2, 28, 29, 31, 32, 35, 39.24)

cn <- c(23000, 19898.86, 18791.31, 16576.21, 15468.66,
        12146.01, 7450,23000, 19898.86, 18791.31, 16576.21, 15468.66,
        12146.01, 7450)

pn <- c(1,2,5,6,6,4,3)
pt <- c(4,6,8,7,6,5,4)

patient_types <- c("Patient Numbers","Patient Numbers","Patient Numbers","Patient Numbers","Patient Numbers","Patient Numbers","Patient Numbers",
                   "Estimated Patient","Estimated Patient","Estimated Patient","Estimated Patient","Estimated Patient","Estimated Patient","Estimated Patient")

patient_Number <- c(1,2,5,6,6,4,3,4,6,8,7,6,5,4)

day <- c(1,5,10,15,20,25,30)
day <- factor(day)
# day <- c("1st Day","5th Day","10th Day","15th Day","20th Day","25th Day","30th Day")

dat <- data.frame(cn,patient_types,patient_Number)
str(dat)

level_order <- c() #this vector might be useful for other plots/analyses


pred_stats <- read.csv('pred_stats.csv')
require(dplyr)
require(ggplot2)

ggplot() + 
  geom_bar(data = dat, aes(fill=patient_types, x=patient_Number, y=cn), position="stack", stat="identity")+
  labs(title = 'Random Forest Model Predicting on 2020',        
       subtitle = 'Prediction Performance by Current Days Not Flown Period',
       x = 'Current Days Not Flown',
       y = 'Count',
       fill = '') 



# ggplot(dat, aes(x = factor(day, level = level_order), y = pn)) + geom_col()
# 
# p <- ggplot(dat, aes(x = factor(day), group=1))
# p
# p <- p + geom_line(aes(y = cn, colour = "Copy Number / 100 ml"),size = 1)
# p
# 
# p <- p + geom_point(aes(y = cn, colour = "Copy Number / 100 ml"),size = 1.5)
# p
# 
# p <- p + geom_line(aes(y = ct*50, colour = "CT Value"),size = 1)
# p
# 
# p <- p + geom_point(aes(y = ct*50, colour = "CT Value"),size = 1.5)
# p
# 
# p <- p + scale_y_continuous(sec.axis = sec_axis(~./50, name = "CT Value"))
# p
# p <- p + geom_col(aes(y = pn*100),size = 1.5) 
# p
# 
# # modifying colours and theme options
# p <- p + scale_colour_manual(values = c("blue", "red"))
# p <- p + labs(y = "Copy Number / 100 ml",
#               x = "Day",
#               colour = "")
# p <- p + theme(legend.position = c(0.18, 0.88))
# p
# 
# 
# 


# 
# x <- structure(list(Mean = structure(1:7, .Label = c("1st Day","5th Day","10th Day","15th Day","20th Day","25th Day","30th Day"), class = "factor"), 
#                Chronologic = c(1,2,5,6,6,4,3), 
#                Cognitive = c(4,6,8,7,6,5,4), Delta = structure(c(7L, 4L, 5L, 2L, 3L, 6L, 1L), .Label = c("-10%", "-11%", "-12%", "-4%", "-6%", "-8%", "10%"), 
#                                                                                         class = "factor")), class = "data.frame", row.names = c(NA, -7L))
# 
# 
# df <- data.frame(x$Mean, x$Chronologic, x$Cognitive, x$Delta)
# 
# require(tidyr)
# library(ggplot2)
# df.long <- gather(df, variable, value, -Mean)
# ggplot(data = df.long, aes(x = Mean, y = value, fill = variable)) +
#   geom_col(position = position_dodge())
# 
# 
# 
# df.long$value <- as.numeric(gsub("%", "", df.long$value))
# 
# ggplot(data = df.long, aes(x = Mean, y = value)) +
#   geom_col(data = df.long[df.long$variable != "Delta" ,], position = position_dodge(),
#            aes(fill = variable)) +
#   geom_line(data = df.long[df.long$variable == "Delta",], 
#             aes(group = 1, y = value + 20)) +
#   scale_y_continuous(sec.axis = sec_axis(trans = ~.- 20, name = "Delta"))


# packages

library(ggplot2)

# data

coll <- data.frame(
  Day = c(1,5,10,15,20,25,30),
  Temp = c(7450, 12146.01, 18791.31, 23000, 19898.86,  16576.21, 15468.66            ), 
  Categories = c(rep("Patient number from clinical samples", 7, sep = ","), rep("Estimated patient number from wastewater", 7, sep = ",")),
  value = c(1,2,5,6,6,4,3,4,6,8,7,6,5,4),
  sd = c(1,2,5,6,6,4,3,4,6,8,7,6,5,4)/3)




# Create additional variable for modified temperature to scale with the preciptation y axis
# tranformation coefficient
coll$temp_mod <- (coll$Temp/5000) 


# plot

ggplot(coll, aes(Day, value, fill = Categories))+ 
  geom_bar(stat = "identity", position = "dodge")+ 
  scale_fill_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=1.2,
                position=position_dodge(3.5))+
  ylim(0, 25000)+
  geom_smooth(aes(Day, temp_mod, colour = "Copy number/1000mL"),size=3, color="darkslateblue",se = FALSE)+ 
  geom_point(aes(Day, temp_mod, colour = "Copy number/1000mL"),size=6)+
  scale_y_continuous(sec.axis = sec_axis(~. *5000 , name = "Copy Number / 1000 ml"))  + labs(x = "Day", y = "Patient Number", colour = "") +  theme(text = element_text(size = 20),legend.position="bottom")   





# # packages
# 
# library(ggplot2)
# 
# # data
# 
# coll <- data.frame(
#   Date = c("1/1/2019", "2/1/2019", "3/1/2019", "4/1/2019", "5/1/2019", "6/1/2019", "7/1/2019",
#            "8/1/2019", "9/1/2019", "10/1/2019", "11/1/2019", "12/1/2019"),
#   Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#   Temp = c(NA,-3,-6,-13,-12,-6,-5,-1,-4,-7,-8,NA), 
#   variable = c(rep("bar1", 12, sep = ","), rep("bar2", 12, sep = ","), rep("bar3" , 12, sep = ",")),
#   value = rnorm(36,400,100))
# 
# # Data wrangling
# 
# coll$Date <- as.POSIXct(coll$Date, format = "%m/%d/%Y")
# 
# # Create additional variable for modified temperature to scale with the preciptation y axis
# # tranformation coefficient
# coeff <- 50
# 
# coll$temp_mod <- (coll$Temp * coeff) + 700
# 
# 
# 
# 
# 
# # plot
# 
# ggplot(coll, aes(Date, value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   ylab("Precipitation")+
#   xlab("Month")+
#   ylim(0, 900)+
#   geom_line(aes(Date, temp_mod))+
#   geom_point(aes(Date, temp_mod))+
#   scale_y_continuous(sec.axis = sec_axis(~-rev(.) / coeff, name = "Temp", breaks = seq(0, -15, by = -2)))





