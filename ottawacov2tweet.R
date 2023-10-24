library(cowplot)
library(gghighlight)
library(tidyverse)
library(dplyr)
library(zoo)
library(ggrepel)
library(knitr)
library(ern)
library(ggplot2)

#CURATE DATA "wwopen" FOR COV2 PLOTS -------------------------------------------------------------
##pull in and convert ALL SARS ww data to average of N1/PMMOV + N2/PMMOV and limit to dates for which there is VOC data
wwopen <- read.csv("https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv") %>%
  mutate(Date = as.Date(sampleDate, format= "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2020-04-07") & Date <= Sys.Date()) %>%
  mutate(N1N2norm = ((covN1_nPMMoV_meanNr + covN2_nPMMoV_meanNr)/2)*10^3) #transform to *10^-3 to make it easier to read y-axis

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

##pull in and convert ALL SARS ww data to average of N1/PMMOV + N2/PMMOV and limit to dates for which there is VOC data
ww.conc <- wwopen %>%
mutate(Date = as.Date(sampleDate, format= "%Y-%m-%d")) %>%select(c(28,29)) %>%
  rename("date" = "Date", "value" = "N1N2norm") 

lastpointReff<-tail(ww.conc, n=1)

#PLOTS -------------------------------------------------------------
ottawaalltime <- ggplot(wwopen, aes(x=Date, y = N1N2norm, alpha = 7/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen, mapping=aes(x=Date, y = roll7d), color="slateblue")+
  #deprecated tidyquant geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=0.2, alpha = 1/10)+
  ylim(NA,2)+
  theme_classic()+
  scale_x_date(date_breaks = "3 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "entire pandemic - Ottawa, Canada")+
  gghighlight(Date >= Sys.Date()-364)+
  ylab("SARS-CoV-2 signal x10\u00b3")+
  geom_hline(yintercept = c(min,med,max), color=c("darkgreen", "darkorange", "darkred"))+
  geom_label(aes(x=as.Date("2020-04-07"), y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2.5, hjust=0)+
  geom_label(aes(x=as.Date("2020-04-07"), y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2.5, hjust=0)+
  geom_label(aes(x=as.Date("2020-04-07"), y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2.5, hjust=0)


ottawapastyear <- ggplot(wwopen %>% filter(Date >= Sys.Date()-364), aes(x=Date, y = N1N2norm, alpha = 7/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen %>% filter(Date >= Sys.Date()-364), mapping=aes(x=Date, y = roll7d), color="slateblue")+
  #geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=1, alpha =1/10)+
  ylim(NA,2)+
  theme_classic()+
  scale_x_date(date_breaks = "month" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  labs(title= "past year")+
  gghighlight(Date >= Sys.Date()-60)+
  ylab("SARS-CoV-2 signal x10\u00b3")+
  geom_hline(yintercept = c(min,med,max,fourthlast$roll7d), color=c("darkgreen", "darkorange", "darkred","slateblue"))+
  geom_label(aes(x=Sys.Date()-260, y=fourthlast$roll7d, label = paste(percentofmedian, "% of pandemic average", sep="")), color="slateblue", fill="white", alpha=1/25, size=3.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2.5,hjust=0)



ottawapast2months <- ggplot(wwopen %>% filter(Date >= Sys.Date()-60), aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen %>% filter(Date >= Sys.Date()-60), mapping=aes(x=Date, y = roll7d), color="slateblue", linewidth=1.2)+
  #geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1.5, show.legend = T)+
  geom_point(size=1, alpha =3/10)+
  theme_classic()+
  scale_x_date(date_breaks = "2 weeks" , date_labels = "%b %d")+
  ggplot2::labs(title = "past 2 months"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  #labs(title= "past 2 months", caption="data source: https://github.com/Big-Life-Lab/PHESD")+
  ylab("SARS-CoV-2 signal x10\u00b3")+
  #geom_hline(yintercept = c(min,med,max,fourthlast$rll7d), color=c("darkgreen", "darkorange", "darkred", "slateblue"))+
  geom_hline(yintercept = c(min,med), color=c("darkgreen", "darkorange"))+
  geom_segment(aes(x= as.Date(lastpoint$Date), xend= as.Date(lastpoint$Date), y=-.1, yend= lastpoint$N1N2norm), color="black", alpha=0.008)+
  #geom_label(aes(x=Sys.Date()-60, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2,hjust=0)+
  #geom_label(aes(x=Sys.Date()-60, y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2,hjust=0)+
  #geom_label(aes(x=Sys.Date()-60, y=fourthlast$roll7d, label = paste(percentofmedian, "% of \n pandemic average", sep="")), color="slateblue", fill="white", alpha=1/50, size=3,hjust=0)+
  #geom_label(aes(x=Sys.Date()-60, y=-.00013, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=lastpoint$Date, y=-.1, label= paste('latest:', toString(format(lastpoint$Date, "%B %d")))), color="slategrey", fill="white", alpha=1/25, size=3, vjust=0.5, hjust=1, label.size=0
  )

#title <- ggdraw() +
# draw_label("Wastewater-derived SARS-COV-2 signal at Ottawa wastewater treatment plant")

#infer incidence and Reff using Ern package and plot
plot_diagnostic_ww <- function (r.estim, caption = NULL) 
{
  ggplot2::theme_set(ggplot2::theme_bw())
  date.start = min(r.estim$R$date)
  date.end = max(r.estim$R$date)
  date.last2months = max(Sys.Date()-60)
  #min.inc = min(r.estim$inc$mean)
  #max.inc = max(r.estim$inc$mean)
  xsc = ggplot2::scale_x_date(limits = c(lubridate::ymd(date.last2months), 
                                         lubridate::ymd(date.end)), date_breaks = "2 weeks" , date_labels = "%b %d")
  
  g.inc = r.estim$inc %>% ggplot2::ggplot(ggplot2::aes(x = date, y = mean)) + 
    #ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.8, fill="slateblue", outline.type = "both") + 
    ggplot2::geom_line(color="slateblue", linewidth = 1.2, alpha = 8/10) +
    #geom_hline(yintercept = c(min.inc,max.inc), color=c("darkgreen", "darkred"))+
    ggplot2::labs(title = "wastewater-derived \n COVID-19 incidence \n (calibrated to \n BA.2 wave)", y = "new cases/day/million people") + 
    xsc +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.9),
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=8)
    ) +
    ylim(0,NA)
    #geom_label(aes(x=Sys.Date()-60, y=-.00013, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)+
    #geom_label(aes(x=as.Date("2021-12-30"), y=500, label = "BA.1"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0) +
    #geom_label(aes(x=as.Date("2022-04-10"), y=500, label = "BA.2"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0) +
    #geom_label(aes(x=as.Date("2022-07-01"), y=500, label = "BA.4/5"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0) +
    #geom_label(aes(x=as.Date("2022-09-30"), y=500, label = "BF*"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0) +
    #geom_label(aes(x=as.Date("2022-12-30"), y=500, label = "BQ*"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0) +
    #geom_label(aes(x=as.Date("2023-04-01"), y=500, label = "XBB*"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0) +
    #geom_label(aes(x=as.Date("2023-09-15"), y=500, label = "EG*"), color="steelblue", fill="white", alpha=1/25, size=2,label.size=0)
  
  g.r = r.estim$R %>% ggplot2::ggplot(ggplot2::aes(x = date, 
                                                   y = mean)) + 
    ggplot2::geom_hline(yintercept = 1, color = "grey50", linetype = "dashed") + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), fill = "slateblue", alpha = 8/10, outline.type = "both") + 
    #ggplot2::geom_line(color="slateblue", linewidth = 0.8, alpha = 3/10) + 
    xsc + 
    ggplot2::labs(title = "wastewater-derived \n Effective Reproduction \n Number",
                  y = expression(R[e]), caption="data source: https://github.com/Big-Life-Lab/PHESD"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.9),
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      axis.title.y = element_text(size=8),
      axis.title.x = element_blank()
    ) 
 
  # Return both 'g.inc' and 'g.r' as a list
  return(list(g.inc = g.inc, g.r = g.r))
}

#define decay parameters
dist.fec <- def_dist_fecal_shedding(pathogen = "sarscov2", subtype = "")
dist.gi <- def_dist_generation_interval(pathogen = "sarscov2")

R_ww <- estimate_R_ww(
  ww.conc,
  dist.fec,
  dist.gi,
  scaling.factor = 5500, #adjusted based on 10^3 N1N2norm transformation
  prm.smooth = list(window = 14, align = "center", method = "loess", span = 0.1),
  prm.R = list(iter = 10, CI = 0.999, window = 14, config.EpiEstim = NULL),
  silent = FALSE,
  RL.max.iter = 9
)

#plot function
plotReff <- plot_diagnostic_ww(R_ww)
# Access the 'g' plot
g.inc_plot <- plotReff$g.inc
# Access the 'g.r' plot
g.r_plot <- plotReff$g.r

#ARRANGE and LABEL -----------------------------------------------------------------------------

bottomrow <- plot_grid(ottawapast2months, g.inc_plot, g.r_plot, ncol=3, align = 'h', labels = c('C', 'D', 'E'),label_size = 16)
finalplot <-plot_grid(ottawaalltime, ottawapastyear, bottomrow,ncol = 1, align = 'l', axis = 'l',labels = c('A', 'B'), rel_widths=c(1, 1, 4), rel_heights = c(1,1,1), label_size = 16)

# define the plot file to tweet
png <- save_plot("ottawacov2ww.png", plot=finalplot,base_height=9,base_width=5)

# Tweet alt-text description (1000 character limit)
alttext <- paste(
  "Plots of SARS-CoV-2 signal across time in Ottawa, Canada;",
  sep= "\n")
nchar(alttext)

# Tweet message (280 character limit)
message <- paste(
  "#Ottawa SARS-CoV-2 wastewater trends as of: ", 
  format(lastpoint$Date, "%B %d"), 
  ". Pandemic highlighting past year (A), blue line is weekly average, (B) Past year highlighting last 2 months, (C) Past 2 months, (D) inferred case incidence, (E) inferred Reff. D and E determined with phac-nml-phrsd/ern",
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
