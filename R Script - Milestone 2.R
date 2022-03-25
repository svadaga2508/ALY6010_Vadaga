#version.string R version 4.1.2 (2021-11-01)
#Name            - Vadaga, Satyanarayana
#Course title    - ALY6010, Probability Theory and Introductory Statistics
#Module          - 3 & 4

version

#install.packages("FSA")
#install.packages("FSAdata")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("plotrix")
#install.packages("tidyr")
#install.packages("plyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("lubridate")
install.packages("ggpubr")

library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("tidyr")
library("plyr")
library("tidyverse")
library("ggplot2")
library("scales")
library("lubridate")
library("ggpubr")

setwd("C:/Users/MadhuSatya/OneDrive/Documents")

source_df <- read.csv("Hotel_bookings.csv", header = TRUE, sep = ",")


### Data Analysis #####

headtail(source_df)
labels(source_df)
str(source_df)

# Removing Agent ,address(adr) & company columns as data because of insufficient information.
source_df <- subset(source_df, select = -agent)
source_df <- subset(source_df, select = -company)

headtail(source_df)

# Find if any rows with N/A in the children column
nrow(source_df[is.na(source_df$children),])

# Remove the rows with N/A in the children column
source_df <- source_df[!is.na(source_df$children),]

# Update data in is_canceled column(0 to Cancelled, 1 to Confirmed)
source_df$is_canceled <- ifelse(test = source_df$is_canceled =="0", yes = "Confirmed", no = "Cancelled")

# Rename column is_canceled to Booking_Status
colnames(source_df)[2] <- "Booking_Status"

# update data in is_repeated_guest column(1 to Yes, 0 to No)
source_df$is_repeated_guest <- ifelse(test = source_df$is_repeated_guest =="1", yes = "Yes", no = "No")


# Create bins on lead time
lead_time_bin  <- cut(source_df$lead_time , breaks = c(0,30,90,150,210,270,360), right = FALSE)
source_df$lead_time_bin <- lead_time_bin


############# Inferential Statistics ####################

######## Question -1

mean(subset(source_df, lead_time_bin == "[30,90)" & children != 0 & babies != 0)$lead_time) # 56.3

mean(subset(source_df, lead_time_bin == "[30,90)"  & children != 0 & babies != 0)$lead_time) # 59.1

# Create density plot to show lead_time distribution
ggdensity(subset(source_df,(hotel == 'City Hotel' & lead_time_bin == "[30,90)"))$lead_time,
                 fill = "lightgray", color="blue")+
                  labs(  
                  title = "Density plot ", 
                  x = "Lead Time",
                  y = "Density",  
        )

# From density plot, we can infer data is heavily skewed towards right side.

# Create two vectors for holding lead time with and without children

v_with_children <- subset(source_df,
                          ( hotel == "City Hotel" &
                            lead_time_bin == "[30,90)" & 
                              children != 0 & babies != 0))$lead_time

v_without_children <- subset(source_df,
                             ( hotel == "City Hotel" &
                               lead_time_bin == "[30,90)" & 
                                 children == 0 & babies == 0))$lead_time

# Create a boxplot to check the median of with and without children data
boxplot(v_with_children, 
        v_without_children,
        main = "Lead time in hotel bookings with & without children",
        names = c("With Children","Without Children")
)

# Observed median is slightly higher for bookings with children

mean(v_with_children) # 59.17
mean(v_without_children) # 56.4
# Observed mean is slightly higher for bookings with children 

############ Normality Checks ################

hist(v_with_children,
     main = "Lead-time distribution for bookings with children",
     xlab ="Lead time",
     ylab ="Frequency",
     col = "cadetblue",
     col.main = 'cadetblue'
)

hist(v_without_children,
     main = "Lead-time distribution for bookings without children",
     xlab ="Lead time",
     ylab ="Frequency",
     col = "cadetblue",
     col.main = 'cadetblue'
)

v_difference <- v_without_children - v_with_children

hist(v_difference,
     main = "Lead-time distribution for differences in vectors",
     xlab ="Lead time",
     ylab ="Frequency",
     col = "cadetblue",
     col.main = 'cadetblue'
)


# Conduct t-test on lead-time of city hotel by limiting data to  

#H0 - Lead time is not increased with presence of children. u1<=u2
#H1 - Lead time is increased with presence of children. u1>u2


# Two  sample one sided t-test with conf - 95%
t.test(v_with_children, v_without_children,
       alternative ="greater",
       conf.level = 0.95)
# p value : 0.4738. Greater than significance level 0.05.
# Fail to reject null hypothesis


# Two  sample one sided t-test with conf - 95%
t.test(v_without_children, 
       v_with_children,
       alternative ="less",
       conf.level = 0.95)
# p value : 0.4738. Greater than significance level 0.05.
# Fail to reject null hypothesis


# Two sample two-sided at 95%
t.test(v_without_children, 
       v_with_children,
       alternative ="two.sided",
       conf.level = 0.95)
# p value : 0.9477. Greater than significance level 0.05.
# Fail to reject null hypothesis

# Two  sample one sided t-test with conf - 50%
t.test(v_with_children, v_without_children,
       alternative ="greater",
       conf.level = 0.50)
# p value : 0.4738. Less than significance level 0.50.
# Reject null hypothesis

######## Question -2

v_confirmed_bookings<-subset(source_df, 
                      lead_time_bin == "[30,90)"& 
                      Booking_Status == "Confirmed" )$lead_time  
v_cancelled_bookings <-subset(source_df, 
                       lead_time_bin == "[30,90)"& 
                       Booking_Status == "Cancelled" )$lead_time

mean(v_confirmed_bookings)
mean(v_cancelled_bookings)

hist(v_confirmed_bookings,
     main = "Histogram of confirmed bookings",
     xlab = "Lead Time",
     ylab = "Frequency")

hist(v_cancelled_bookings,
     main = "Histogram of Cancelled bookings",
     xlab = "Lead Time",
     ylab = "Frequency")

hist(v_confirmed_bookings - v_cancelled_bookings,
     main = "Histogram showing differences",
     xlab = "Lead Time",
     ylab = "Frequency")

# H0 - Lead time is not increasing cancellations(u1 <= u2)
# H1- Lead time is increasing cancellations

# two sample, one-sided t-test
t.test(v_cancelled_bookings, 
       v_confirmed_bookings,
       alternative = "greater",
       conf.level = 0.95)
#p-value = 6.436e-12. Less than significance level - 0.05.
# Reject null hypothesis

# two sample, one-sided t-test
t.test(v_cancelled_bookings, 
       v_confirmed_bookings,
       alternative = "greater",
       conf.level = 0.99)
#p-value = 6.436e-12. Less than significance level - 0.05.
# Reject null hypothesis

# two sample, two-sided t-test
t.test(v_cancelled_bookings, 
       v_confirmed_bookings,
       alternative = "two.sided",
       conf.level = 0.95)
#p-value = 1.287e-11. Less than significance level - 0.05.
# Reject null hypothesis

############ Question 3

count(source_df$deposit_type)

mean(subset(source_df, 
            lead_time_bin == "[30,90)"& 
              deposit_type =="No Deposit"
)$adr
) # $107.6    

mean(subset(source_df, 
            lead_time_bin == "[30,90)"& 
              deposit_type =="Non Refund"
)$adr
) # $95.1    

mean(subset(source_df, 
            lead_time_bin == "[30,90)"& 
              deposit_type =="Refundable"
)$adr
) # $121.2    


# create a varibale to hold rate of no deposit type
v_no_deposit_rate <- subset(source_df, 
                         lead_time_bin == "[30,90)"& 
                           deposit_type =="No Deposit"
                        )$adr

v_refund_rate <- subset(source_df, 
                        lead_time_bin == "[30,90)"& 
                          deposit_type =="Refundable"
)$adr

v_non_refund_rate <- subset(source_df, 
                        lead_time_bin == "[30,90)"& 
                          deposit_type =="Non Refund"
)$adr


hist(v_no_deposit_rate,
     main = "Histogram of daily rate",
     xlab = "Daily Rate",
     ylab = "Frequency")



# H0 : Average daily rate is 107
# H1 : Average daily rate is not equal to 107

# One sample, two-sided t-test
t.test(v_no_deposit_rate, 
       alternative = "two.sided",
       conf.level = 0.95)
#p-value = 2.2e-16. Less than significance level - 0.05.
# Reject null hypothesis

# One sample, two-sided t-test
t.test(v_no_deposit_rate, 
       alternative = "two.sided",
       conf.level = 0.99)
#p-value = 2.2e-16. Less than significance level - 0.01.
# Reject null hypothesis

#p-value = 2.2e-16. Less than significance level 0.01.
# Reject null hypothesis


