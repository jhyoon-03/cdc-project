# Times Series for FluView
# based on page 1-2 of Reich Lab
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
help(read.csv)
data1 <- read.csv(file.choose(), header=TRUE)
data1 
View(data1)
# import FluView CSV file to R
# variables are YEAR, WEEK, X..WEIGHTED.ILI

# Clean data1
library(cdcfluview)
library(dplyr)
data1 <- data1 %>%
  select('YEAR','WEEK','X..WEIGHTED.ILI') # remove X variable (fourth column of data1)
print(head(data1,42)) # adjust number of rows (42)
# Note: View(FluView_LineChart_Data) and View(data1) are different. We use View(data1)


#Time Series
library(ggplot2) 
plot <- ggplot() +
  geom_line(mapping = aes(x = WEEK, y = X..WEIGHTED.ILI),
            size=0.7,
            dat = data1) +
  xlab("WEEK") + ylab("X..WEIGHTED.ILI") +
  coord_cartesian(ylim = c(0, 10)) +
  ggtitle("Fluview Visits for Influenza-like-Illnesses")
print(plot)

# xlba(") if not want x-axis label
# xlab("WEEK") if want x-axis label
