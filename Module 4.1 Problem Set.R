##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


getwd()
setwd("C:/Users/stevenfellinger/Desktop/R Coding in Sports Class")


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


# Import data / CSV & Remove Extra Column "V1"

TestTrackMan=fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))
TestTrackMan <- subset(TestTrackMan, select = -c(...1))



# What is ggplot?

?ggplot


# Basic ggplot

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point()


# Changing Point Size

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(size = 3)


# Adding Color

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType), size = 3)


# Setting Color Scale

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType), size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))


# Changing Shape

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(shape = TaggedPitchType),size = 3)


# Changing Shape With Color

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(shape = TaggedPitchType, color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))


# Adding Context (StrikeZone + Plate)

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0))



# Removing Background and Setting Plot Limits

ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5)






########## Problem Set ##############

#Create the Following:

# 1) Play around with geom_segment, create a strikezone that is a 5x5 within the zone (as opposed to the current 3x3)
ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))+
  geom_segment(x = (-17.5/12)+.25, y = (60.08/12), xend = (10.5/12)+.25, yend = (60.08/12)) +
  geom_segment(x = (-17.5/12)+.25, y = (.29/12), xend = (10.5/12)+.25, yend = (.29/12)) +
  geom_segment(x = (-17.5/12)+.25, y = (60.08/12), xend = (-11.7/12)+.25, yend = (.29/12)) +
  geom_segment(x = (10.5/12)+.25, y = (60.08/12), xend = (10.5/12)+.25, yend = (.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5)
# 2) Create a background that is neither gray (default) or blank, but rather a color of your choosing
ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))+
  geom_segment(x = (-17.5/12)+.25, y = (60.08/12), xend = (10.5/12)+.25, yend = (60.08/12)) +
  geom_segment(x = (-17.5/12)+.25, y = (.29/12), xend = (10.5/12)+.25, yend = (.29/12)) +
  geom_segment(x = (-17.5/12)+.25, y = (60.08/12), xend = (-11.7/12)+.25, yend = (.29/12)) +
  geom_segment(x = (10.5/12)+.25, y = (60.08/12), xend = (10.5/12)+.25, yend = (.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) +
  theme(panel.background = element_rect(fill = "red"))
