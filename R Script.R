#version.string R version 4.1.2 (2021-11-01)
#Name            - Vadaga, Satyanarayana
#Course title    - ALY6010, Probability Theory and Introductory Statistics
#Module          - 1

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


# Read the data from source file

setwd("C:/Users/MadhuSatya/OneDrive/Documents")

source_df <- read.csv("Highest_Grossing_Movies.csv",
                      header=TRUE, sep =",")


# Remove unwanted/unnamed columns.

source_df <- source_df[-c(1)]


# Examine the file for data cleaning.

lbls <- labels(source_df)
lbls
glimpse(source_df)
summary(source_df)



# Replace . with _ in column names

names(source_df) <- gsub("\\.","_", names(source_df))

names(source_df)[5] <- "Domestic_Sales_in_Million_Dollars"
names(source_df)[6] <- "International_Sales_in_Million_Dollars"
names(source_df)[7] <- "World_Sales_in_Million_Dollars"

# Convert sales into millions
source_df$Domestic_Sales_in_Million_Dollars <- round(source_df$Domestic_Sales_in_Million_Dollars/1000000,2)
source_df$International_Sales_in_Million_Dollars <- round(source_df$International_Sales_in_Million_Dollars /1000000,2)
source_df$World_Sales_in_Million_Dollars <- round(source_df$World_Sales_in_Million_Dollars /1000000,2)

# Extract Year from Release Date
source_df$Release_Date <- as.Date(source_df$Release_Date, format ="%m/%d/%Y" )
source_df$Release_Year <- format(source_df$Release_Date, format ="%Y")


Distributor_Frequency <- source_df %>%
  select(Distributor) %>%
  group_by(Distributor) %>% 
  dplyr::summarize(Movie_count = n())  

Distributor_Frequency

Year_Frequency <- source_df %>%
  select(Release_Year) %>%
  group_by(Release_Year) %>%
  dplyr:: summarize(Movie_count = n())

Year_Frequency

License_Frequency <- source_df %>%
  select(License) %>%
  filter(License !='NA')%>%
  group_by(License) %>%
  dplyr:: summarize(Movie_count = n())

License_Frequency

#Separate genres into different columns 
genre_df <- source_df %>%
  separate(Genre, into = c("genre1","genre2","genre3","genre4","genre5","genre6","genre7","genre8",sep = ","))


genre_frequency_table <-
  genre_df %>%
  select(genre1) %>%
  group_by(genre1) %>% 
  dplyr::summarize(Movie_count = n())  

genre_frequency_table

genre_domestic_sales_statistics_in_millions <-
  genre_df %>%
  select(genre1, Domestic_Sales_in_Million_Dollars)%>%
  group_by(genre1) %>%
  dplyr::summarize(Total_Domestic_Sales = sum(Domestic_Sales_in_Million_Dollars) ,Mean_Domestic_sales = mean(Domestic_Sales_in_Million_Dollars, trim =0, na.rm = FALSE),Sd_Domestic_Sales = sd(Domestic_Sales_in_Million_Dollars, na.rm = FALSE))

tot_sales <-sum(genre_domestic_sales_statistics_in_millions$Total_Domestic_Sales)
tot_mean_sales <- sum(genre_domestic_sales_statistics_in_millions$Mean_Domestic_sales)

genre_domestic_sales_statistics_in_millions$rel_freq_tot_sales <- factor(round((genre_domestic_sales_statistics_in_millions$Total_Domestic_Sales/tot_sales)*100,2))
genre_domestic_sales_statistics_in_millions$rel_freq_mean_sales <- factor(round((genre_domestic_sales_statistics_in_millions$Mean_Domestic_sales/tot_mean_sales)*100,2))

genre_domestic_sales_statistics_in_millions

year_tot_sales <-
  genre_df  %>%
  select(Release_Year, Domestic_Sales_in_Million_Dollars, International_Sales_in_Million_Dollars)%>%
  group_by(Release_Year) %>%
  dplyr::summarize(Tot_Dom_Sales = sum(Domestic_Sales_in_Million_Dollars) ,Tot_Int_Sales = sum(International_Sales_in_Million_Dollars))

year_avg_sales <-
  genre_df  %>%
  select(Release_Year, Domestic_Sales_in_Million_Dollars, International_Sales_in_Million_Dollars)%>%
  group_by(Release_Year) %>%
  dplyr::summarize(Avg_Dom_Sales = mean(Domestic_Sales_in_Million_Dollars) ,Avg_Int_Sales = mean(International_Sales_in_Million_Dollars))




par(las=1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(cex.axis = 0.8, cex.lab =0.8)




plot(Year_Frequency$Release_Year, 
     Year_Frequency$Movie_count,
     type ="p", 
     main = "Movies count by Year",
     xlab = "Release Year",
     ylab = "Count",
     pch = 2,
     cex = 1,
     col = "blue"
)




hist(source_df$Domestic_Sales_in_Million_Dollars, 
     main = "Domestic Sales Distribution",
     xlim = c(0,1000),
     ylim = c(0,500),
     xlab ="Sales in Million Dollars",
     ylab ="Frequency",
     col = "cadetblue",
     col.main = 'cadetblue'
)

hist(source_df$International_Sales_in_Million_Dollars, 
     main = "International Sales Distribution",
     xlim = c(0,1000),
     ylim = c(0,600),
     xlab ="Sales in Million Dollars",
     ylab ="Frequency",
     col = "lightgreen",
     col.main = 'lightgreen'
)



Dist_plot <- ggplot(Distributor_Frequency, aes(x=reorder(Distributor,-Movie_count), y=Movie_count, fill = Distributor ))+
             geom_bar(stat = "identity", show.legend = FALSE)+
             theme(axis.text.x = element_text(angle = 90))+
             labs(title = "Number of Movies by distributor",x = "Distributor", y = "Movie Count")

Dist_plot

license_plot <- ggplot(License_Frequency, aes(x=reorder(License,-Movie_count), y=Movie_count, fill = License ))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Number of Movies by License",x = "License", y = "Movie Count")

license_plot

Genre_plot <- ggplot(genre_frequency_table, aes(x=reorder(genre1,-Movie_count), y=Movie_count, fill = genre1 ))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Number of Movies by Genre",x = "Genre", y = "Movie Count")

Genre_plot

Dom_sales_plot <- ggplot(genre_domestic_sales_statistics_in_millions, aes(x=reorder(genre1,-Total_Domestic_Sales), y=Total_Domestic_Sales/1000, fill = genre1 ))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Domestic Sales by Genre",x = "Genre", y = "Sales in Billions")

Dom_sales_plot

colors <- c("Domestic Sales" = "orange", "International Sales" = "blue")
  
year_tot_sales_plot <- ggplot(year_tot_sales, aes(x=as.numeric(Release_Year)))+
  geom_line(aes(y=Tot_Dom_Sales/1000, color ="Domestic Sales"), size = 1)+
  geom_line(aes(y=Tot_Int_Sales/1000, color = "International Sales"), size = 1)+
  labs(x = "Release Year", title = "Total Sales by Year", y= "Total Sales in Billions ", color ="Legend")+
  scale_color_manual(values = colors)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0,15))

year_tot_sales_plot

year_avg_sales_plot <- ggplot(year_avg_sales, aes(x=as.numeric(Release_Year)))+
  geom_line(aes(y=Avg_Dom_Sales, color ="Domestic Sales"), size = 1)+
  geom_line(aes(y=Avg_Int_Sales, color = "International Sales"), size = 1)+
  labs(x = "Release Year", title = "Average Sales by Year", y= "Average Sales in Millions ", color ="Legend")+
  #scale_color_manual(values = colors)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0,500))

year_avg_sales_plot