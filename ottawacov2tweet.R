library(cowplot)
library(gghighlight)
library(tidyverse)
library(dplyr)
library(zoo)
library(ggrepel)

##pull in and convert ALL SARS ww data to average of N1/PMMOV + N2/PMMOV and limit to dates for which there is VOC data
wwopen <- read.csv("https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv") %>%
  mutate(Date = as.Date(sampleDate, format= "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2020-04-07") & Date <= Sys.Date()) %>%
  mutate(N1N2norm = ((covN1_nPMMoV_meanNr + covN2_nPMMoV_meanNr)/2))

#create 7d rolling mean column
wwopen$roll7d = rollmean(wwopen$N1N2norm, 7, na.pad=TRUE)
#get min and max values if weekly averages ignoring NA values
min <- min(wwopen$roll7d, na.rm = TRUE)
med <- mean(wwopen$roll7d, na.rm = TRUE)
max <- max(wwopen$roll7d, na.rm = TRUE)
#define last data point, last weekly average, percent of pandemic median
lastpoint<-tail(wwopen, n=1)
fourthlast<-tail(wwopen, n=4) %>% head(wwopen, n=1)
percentofmedian<-round(fourthlast$roll7d / med *100, digits=0)

#PLOTS -------------------------------------------------------------
ottawaalltime <- ggplot(wwopen, aes(x=Date, y = N1N2norm, alpha = 5/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen, mapping=aes(x=Date, y = roll7d), color="slateblue")+
  #deprecated tidyquant geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=0.2, alpha = 3/10)+
  theme_classic()+
  scale_x_date(date_breaks = "3 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "entire pandemic - Ottawa, Canada")+
  gghighlight(Date >= Sys.Date()-364)+
  ylab("SARS-CoV-2 signal")+
  geom_hline(yintercept = c(min,med,max), color=c("darkgreen", "darkorange", "darkred"))+
  geom_label(aes(x=as.Date("2020-04-07"), y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2, hjust=0)+
  geom_label(aes(x=as.Date("2020-04-07"), y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2, hjust=0)+
  geom_label(aes(x=as.Date("2020-04-07"), y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2, hjust=0)


ottawapastyear <- ggplot(wwopen %>% filter(Date >= Sys.Date()-364), aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen %>% filter(Date >= Sys.Date()-364), mapping=aes(x=Date, y = roll7d), color="slateblue")+
  #geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=1, alpha =3/10)+
  theme_classic()+
  scale_x_date(date_breaks = "month" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "past year")+
  gghighlight(Date >= Sys.Date()-60)+
  ylab("SARS-CoV-2 signal")+
  geom_hline(yintercept = c(min,med,max), color=c("darkgreen", "darkorange", "darkred"))+
  geom_label(aes(x=Sys.Date()-364, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)



ottawapast2months <- ggplot(wwopen %>% filter(Date >= Sys.Date()-60), aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen %>% filter(Date >= Sys.Date()-60), mapping=aes(x=Date, y = roll7d), color="slateblue", linewidth=1.2)+
  #geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1.5, show.legend = T)+
  geom_point()+
  theme_classic()+
  scale_x_date(date_breaks = "week" , date_labels = "%b %d")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "past 2 months", caption="data source: https://github.com/Big-Life-Lab/PHESD")+
  ylab("SARS-CoV-2 signal")+
  geom_hline(yintercept = c(min,med,max,fourthlast$roll7d), color=c("darkgreen", "darkorange", "darkred", "slateblue"))+
  geom_segment(aes(x= as.Date(lastpoint$Date), xend= as.Date(lastpoint$Date), y=-.00013, yend= lastpoint$N1N2norm), color="black", alpha=0.008)+
  geom_label(aes(x=Sys.Date()-60, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-60, y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-60, y=fourthlast$roll7d, label = paste(percentofmedian, "% of pandemic average", sep="")), color="slateblue", fill="white", alpha=1/25, size=3.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-60, y=-.00013, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=lastpoint$Date, y=-.00013, label= paste('latest data:', toString(format(lastpoint$Date, "%B %d")))), color="slategrey", fill="white", alpha=1/25, size=2.5, vjust=0.5, hjust=1
  )

#title <- ggdraw() +
 # draw_label("Wastewater-derived SARS-COV-2 signal at Ottawa wastewater treatment plant")

#ARRANGE and LABEL -----------------------------------------------------------------------------
#dev.new(width = 5, height = 5, unit = "inches")
plot <-plot_grid(ottawaalltime, ottawapastyear, ottawapast2months,ncol = 1, align = 'vh', axis = 'l',labels = c('A', 'B', 'C'), rel_widths=c(1, 1,1), label_size = 16)

# define the plot file to tweet
png <- save_plot("ottawacov2ww.png", plot=plot,base_height=9,base_width=5)

# Tweet alt-text description (1000 character limit)
alttext <- paste(
  "Plots of SARS-CoV-2 signal across time in Ottawa, Canada;",
  sep= "\n")
nchar(alttext)

# Tweet message (280 character limit)
message <- paste(
  "#Ottawa SARS-CoV-2 wastewater trends as of: ", 
  format(lastpoint$Date, "%B %d"), 
  ". (A) Pandemic overview with past year highlighted, (B) Past year with last 2 months highlighted, (C) Past 2 months. Polyline =7 day average normalized signal.",
  sep=""
)
nchar(message)

# 2nd Tweet message (280 character limit)
message2 <- paste(
  "Average of N1 and N2 SARS-CoV-2 genetic markers normalized to Pepper Mild Mottle Virus as a fecal strength indicator; Samples are collected by @ottawacity, tested and analyzed by @RobDelatolla lab; Data i/o by @doug_manuel lab; Plots and tweet bot by @rnaguru.",
  sep= "\n"
)
nchar(message2)

#TWEET it out via pooptweets twitter app --------------------------------------------------------
library(rtweet)

# Create a token containing your Twitter keys
rbot_token <- rtweet::create_token(
  app = "pooptweet2",
  # the name of the Twitter app
  consumer_key = Sys.getenv("RBOT_TWITTER_API_KEY"),
  consumer_secret = Sys.getenv("RBOT_TWITTER_API_SECRET"),
  access_token = Sys.getenv("RBOT_TWITTER_ACCESS_KEY"),
  access_secret = Sys.getenv("RBOT_TWITTER_ACCESS_SECRET"),
  set_renv = FALSE
)

firsttweet <- post_tweet(
  status = message,
  media = png,
  media_alt_text = alttext,
  token = rbot_token
)

reply_id <- ids(firsttweet)

post_tweet(
  status = message2,
  token = rbot_token,
  in_reply_to_status_id = reply_id
)

