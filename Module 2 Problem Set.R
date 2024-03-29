

#                   Problem Set 1: Create WD
################### Create The Following ################### 

#Create your own working directory (a folder on your computer in which we can
#store our data, as well as our Rscripts)









getwd()
setwd("/Users/stevenfellinger/Desktop")

# install packages
install.packages("plyr")
install.packages("dplyr")
install.packages("devtools")
install.packages("DT")
install.packages("ggrepel")
install.packages("ggthemes")
install.packages("gridExtra")
install.packages("janitor")
install.packages("plotly")
install.packages("stringr")
install.packages("data.table")
install.packages("reactable")

# load packages

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)


# Import data / CSV

TestTrackMan=fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))
TestTrackMan <- subset(TestTrackMan, select = -c(...1))

# Save Data

write.csv(TestTrackMan, "TestTrackMan.csv")

# View Data

  # All Data

view(TestTrackMan)

  # Top X # of Rows

head(TestTrackMan, 1)

  # Column Names

colnames(TestTrackMan)

  # Type of Values Within Column (Numeric, Character, etc.)

is.character(TestTrackMan$Pitcher)

  # Unique Values Within Column

unique(TestTrackMan$Pitcher)

  # Table of Frequency

table(TestTrackMan$Pitcher)





