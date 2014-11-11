#----------------------------------------------------------
# LDS Apostolic Succession
#----------------------------------------------------------
# Andrew Heiss (andrew@andrewheiss.com)
#
# Inspired by Sam Orme's post at Modern Mormon Men
# http://www.modernmormonmen.com/2014/11/lds-succession-considered-to-excess.html
#
# Data collected from Wikipedia
#

# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)

# Load and clean data
qof12 <- read.csv("qof12.csv", stringsAsFactors=FALSE) %>%
  mutate(seat = factor(seat, labels=paste("Seat", unique(seat)), ordered=TRUE),
         start_date = ymd(start_date),
         end_date = ymd(end_date),
         mid_date = start_date + floor((end_date-start_date)/2),
         reason_ended = factor(reason_ended),
         short_reason = factor(short_reason),
         short_reason = paste(short_reason, "   "))  # Add padding for legend

# Create fake gridlines since there's no way to get 
# the x axis to show under each facet
faux.gridlines <- data.frame(lines=ymd(c("1850-01-01", "1900-01-01", "1950-01-01", "2000-01-01")))

# Plot this thing
p <- ggplot(qof12, aes(x=start_date, y=1)) 
q12 <- p + 
  geom_rect(aes(xmin=start_date, xmax=end_date, ymin=0, ymax=1, fill=short_reason), 
           colour="#FFFFFF") + 
  geom_vline(data=faux.gridlines, aes(xintercept=as.numeric(lines)), 
             colour="black", size=1, alpha=0.15) +
  geom_text(aes(x=mid_date, y=0.5, label=name), 
            size=2, angle=90, colour="#774F38", fontface="bold") + 
  scale_fill_manual(values=c("#C5E0DC", "#F1D4AF", "#F7B6A5"), name="") + 
  labs(x=NULL, y=NULL, title="LDS Apostolic Succession") + 
  facet_wrap(~ seat, ncol=1) + theme_bw() + 
  theme(legend.position="top", axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.ticks.x=element_blank(), panel.grid=element_blank(),
        strip.text.x=element_text(size=8, face="bold"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"),
        legend.text=element_text(size=8), legend.margin=unit(0,"cm"), 
        legend.key.height=unit(0.5,"lines"), legend.key.width=unit(0.5,"lines"))
q12

# Save plots
ggsave(q12, filename="q12.pdf", width=6, height=20)
ggsave(q12, filename="q12.png", width=6, height=20)
