library(cowplot)
library(gghighlight)
library(tidyverse)
library(dplyr)
library(tidyquant)
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
med <- median(wwopen$roll7d, na.rm = TRUE)
max <- max(wwopen$roll7d, na.rm = TRUE)
#define last data point
lastpoint<-tail(wwopen, n=1)

#PLOTS -------------------------------------------------------------
ottawaalltime <- ggplot(wwopen, aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1.5, show.legend = T)+
  geom_point()+
  theme_classic()+
  scale_x_date(date_breaks = "3 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "entire pandemic")+
  gghighlight(Date >= Sys.Date()-364)+
  ylab("SARS-CoV-2 signal")+
  geom_hline(yintercept = c(min,med,max), color=c("darkgreen", "orange", "darkred"))+
  geom_label(aes(x=as.Date("2020-04-07"), y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2, hjust=0)+
  geom_label(aes(x=as.Date("2020-04-07"), y=med, label = "pandemic weekly median"), color="orange", fill="white", alpha=1/25, size=2, hjust=0)+
  geom_label(aes(x=as.Date("2020-04-07"), y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2, hjust=0)


ottawapastyear <- ggplot(wwopen %>% filter(Date >= Sys.Date()-364), aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1.5, show.legend = T)+
  geom_point()+
  theme_classic()+
  scale_x_date(date_breaks = "month" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "past year")+
  gghighlight(Date >= Sys.Date()-60)+
  ylab("SARS-CoV-2 signal")+
  geom_hline(yintercept = c(min,med,max), color=c("darkgreen", "orange", "darkred"))+
  geom_label(aes(x=Sys.Date()-364, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=med, label = "pandemic weekly median"), color="orange", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)



ottawapast2months <- ggplot(wwopen %>% filter(Date >= Sys.Date()-60), aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1.5, show.legend = T)+
  geom_point()+
  theme_classic()+
  scale_x_date(date_breaks = "week" , date_labels = "%b %d")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "past 2 months", caption="data source: https://github.com/Big-Life-Lab/PHESD")+
  ylab("SARS-CoV-2 signal")+
  geom_hline(yintercept = c(min,med,max), color=c("darkgreen", "orange", "darkred"))+
  geom_label(aes(x=Sys.Date()-60, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-60, y=med, label = "pandemic weekly median"), color="orange", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=Sys.Date()-60, y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)+
  #geom_label(aes(x=lastpoint$Date, y=lastpoint$N1N2norm, label = lastpoint$Date), color="blue", fill="white", alpha=1/25, size=2, vjust=0.5, hjust=0.5)
  geom_text_repel(
    data = lastpoint, 
    aes(label= paste('lastest data:',toString(format(Date, "%B %d")))),
    segment.curvature = -0.2,
    box.padding = 1.0,
    nudge_x = 0,
    nudge_y = 0.0005,
    force = 1,
  )


#ARRANGE and LABEL -----------------------------------------------------------------------------
#dev.new(width = 5, height = 5, unit = "inches")
plot <-plot_grid(ottawaalltime, ottawapastyear, ottawapast2months,ncol = 1, align = 'vh', axis = 'l',labels = c('A', 'B', 'C'), rel_widths=c(1, 1,1), label_size = 16)

# define the plot file to tweet
png <- save_plot("ottawacov2ww.png", plot=plot,base_height=9,base_width=5)

# Tweet alt-text description
alt_text <- paste(
  "A plot of Ottawa wastewater data."
  )

# Tweet message
message <- paste(
  "This is a tweet test and will be deleted."
  )
nchar(message)

#TWEET it out via pooptweets twitter app --------------------------------------------------------
library(rtweet)

# Create a token containing your Twitter keys
rbot_token <- rtweet::create_token(
  app = "pooptweets",
  # the name of the Twitter app
  consumer_key = Sys.getenv("RBOT_TWITTER_API_KEY"),
  consumer_secret = Sys.getenv("RBOT_TWITTER_API_SECRET"),
  access_token = Sys.getenv("RBOT_TWITTER_ACCESS_KEY"),
  access_secret = Sys.getenv("RBOT_TWITTER_ACCESS_SECRET"),
  set_renv = FALSE
)

post_tweet(
  status = message,
  media = png,
  media_alt_text = alt-text,
  token = rbot_token
)
