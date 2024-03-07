library(cowplot)
library(gghighlight)
library(tidyverse)
library(tidyquant)
library(dplyr)
library(zoo)
library(ggrepel)
library(ern)
library(ggplot2)

#CURATE DATA "wwopen" FOR COV2 PLOTS -------------------------------------------------------------
##pull in and convert ALL SARS ww data to average of N1/PMMOV + N2/PMMOV and limit to dates for which there is VOC data
wwopen <- read.csv("https://raw.githubusercontent.com/Delatolla-lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv") %>%
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
lastpoint <-tail(wwopen, n=1)
fourthlast <-tail(wwopen, n=4) %>% head(wwopen, n=1)
percentofmedian <-round(fourthlast$roll7d / med *100, digits=0)
percentofmax <- round(fourthlast$roll7d / max *100, digits=0)

##pull in and convert ALL SARS ww data to average of N1/PMMOV + N2/PMMOV and limit to dates for which there is VOC data
ww.conc <- wwopen %>%
mutate(Date = as.Date(sampleDate, format= "%Y-%m-%d")) %>%select(c(28,29)) %>%
  rename("date" = "Date", "value" = "N1N2norm") 

lastpointReff<-tail(ww.conc, n=1)

#CURATE DATA "wwopen2" FOR ALL OTHER VIRUS PLOTS -------------------------------------------------------------
#create a dataset to be used for the non-COV virus data
wwopen2 <- wwopen %>%
  rename(
    INFA = InfA_copies_per_pep_copies_avg,
    INFB = InfB_copies_per_pep_copies_avg,
    RSV  = RSV_copies_per_pep_copies_avg,
    MPOX = MPOX_copies_per_pep_copies_avg,
  ) %>%
  filter(Date >= as.Date("2022-02-01") & Date <= Sys.Date())

#change any instance of "Not tested" to "NA"
#transform Y values to be more easily read
xformy <- function(x) ifelse(x != "Not tested", as.numeric(x) * 10^3, x)
columns_to_transform <- c("INFA", "INFB", "RSV", "MPOX")
wwopen2 <- wwopen2 %>%
  mutate(across(all_of(columns_to_transform), ~xformy(.)))

#create 7d rolling mean column
#wwopen2$INFA_roll7d = rollmean(as.numeric(wwopen2$INFA), k=3, na.pad=TRUE, align = "right", min_obs = 1)
#wwopen2$INFB_roll7d = rollmean(as.numeric(wwopen2$INFB), 7, na.pad=TRUE, align = "right", min_obs = 1)
#wwopen2$RSV_roll7d = rollmean(as.numeric(wwopen2$RSV), 7, na.pad=TRUE, align = "right", min_obs = 1)
#wwopen2$MPOX_roll7d = rollmean(as.numeric(wwopen2$MPOX), 7, na.pad=TRUE, align = "right", min_obs = 1)
#define last data point, last weekly average, percent of pandemic median
lastpoint_INFA<-wwopen2 %>%
  filter(!is.na(INFA), INFA >= 0, INFA != "Not tested") %>%
  tail(n = 1)
lastpoint_INFB<-wwopen2 %>%
  filter(!is.na(INFB), INFB >= 0, INFB != "Not tested") %>%
  tail(n = 1)
lastpoint_RSV<-wwopen2 %>%
  filter(!is.na(RSV), RSV >= 0, RSV != "Not tested") %>%
  tail(n = 1)
lastpoint_MPOX <- wwopen2 %>%
  filter(!is.na(MPOX), MPOX >= 0, MPOX != "Not tested") %>%
  tail(n = 1)
fourthlast<-tail(wwopen, n=4) %>% head(wwopen, n=1)

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
  ylab(bquote('SARS-CoV-2 signal'~x10^-3))+
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
  ylab(bquote('SARS-CoV-2 signal'~x10^-3))+
  geom_hline(yintercept = c(min,med,max,fourthlast$roll7d), color=c("darkgreen", "darkorange", "darkred","slateblue"))+
  geom_label(aes(x=Sys.Date()-364, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-364, y=-.00020, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2.5,hjust=0)+
  geom_label(aes(x=Sys.Date()-300, y=fourthlast$roll7d, label = paste(percentofmax,"% of pandemic weekly maximum \n",percentofmedian, "% of pandemic weekly average ",  sep="")), color="slateblue", fill="white", alpha=1/400, size=3.2,hjust=0)
  
  ottawapast2months <- ggplot(wwopen %>% filter(Date >= Sys.Date()-60), aes(x=Date, y = N1N2norm, alpha = 3/10)) +
  # geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = input$N1N2spanslider,color = "#FC4E07",fill = "#FC4E07")+
  geom_line(wwopen %>% filter(Date >= Sys.Date()-60), mapping=aes(x=Date, y = roll7d), color="slateblue", linewidth=1.2)+
  #geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1.5, show.legend = T)+
  geom_point(size=1, alpha =3/10)+
  theme_classic()+
  ylim(NA,2)+
  scale_x_date(date_breaks = "2 weeks" , date_labels = "%b %d")+
  ggplot2::labs(title = "past 2 months"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title=element_text(hjust=0.01, size=10),
        axis.title.x=element_blank())+
  #labs(title= "past 2 months", caption="data source: https://github.com/Big-Life-Lab/PHESD")+
  ylab(bquote('SARS-CoV-2 signal'~x10^-3))+
  #geom_hline(yintercept = c(min,med,max,fourthlast$rll7d), color=c("darkgreen", "darkorange", "darkred", "slateblue"))+
  geom_hline(yintercept = c(min,med, max), color=c("darkgreen", "darkorange", "darkred"))+
  geom_segment(aes(x= as.Date(lastpoint$Date), xend= as.Date(lastpoint$Date), y=-.1, yend= lastpoint$N1N2norm), color="black", alpha=0.008)+
  #geom_label(aes(x=Sys.Date()-60, y=max, label = "pandemic weekly maximum"), color="darkred", fill="white", alpha=1/25, size=2,hjust=0)+
  #geom_label(aes(x=Sys.Date()-60, y=med, label = "pandemic weekly average"), color="darkorange", fill="white", alpha=1/25, size=2,hjust=0)+
  #geom_label(aes(x=Sys.Date()-60, y=fourthlast$roll7d, label = paste(percentofmedian, "% of \n pandemic average", sep="")), color="slateblue", fill="white", alpha=1/50, size=3,hjust=0)+
  #geom_label(aes(x=Sys.Date()-60, y=-.00013, label = "pandemic weekly minimum"), color="darkgreen", fill="white", alpha=1/25, size=2,hjust=0)+
  geom_label(aes(x=lastpoint$Date, y=-.1, label= paste('latest:', toString(format(lastpoint$Date, "%b %d")))), color="slategrey", fill="white", alpha=1/25, size=3, vjust=0.5, hjust=0.95, label.size=0
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
    ggplot2::geom_line(color="darkslategray", linewidth = 1.2, alpha = 8/10) +
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
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), fill = "darkslategray", alpha = 8/10, outline.type = "both") + 
    #ggplot2::geom_line(color="slateblue", linewidth = 0.8, alpha = 3/10) + 
    xsc + 
    ggplot2::labs(title = "wastewater-derived \n Effective Reproduction \n Number",
                  y = expression(R[e]), caption="data source: https://github.com/Delatolla-lab/PHESD"
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

#calculate incidence from last 60 days and proportion of population infect
incidence_last60days <- plotReff$g.inc$data %>%
  filter(date >= as.Date(max(Sys.Date() - 60)) & date <= as.Date(max(date)))

# calculate cumualtive incidence in last 60 days and percent of Ottawa population infected
cumulative_incidence_last60day <- sum(incidence_last60days$mean)
percent_infected_last60day <- signif((cumulative_incidence_last60day / 1000000) * 100, digits = 2) #assumes 1M captured in Ottawa

# add to incidence plot
g.inc_plot <- g.inc_plot + annotate("text", x = Sys.Date() -30, y = 2000, size = 2.4, label = paste("An estimated \n",percent_infected_last60day,"% of Ottawans \n have been infected in \n the past 2 months."))

#limit Reff y-axis
g.r_plot <- g.r_plot + ylim(0.1,1.5)

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
  ". Pandemic highlighting past year (A), blue line is weekly average; Past year highlighting last 2 months (B); Past 2 months (C), inferred case incidence (D), and Reff (E) determined with github.com/phac-nml-phrsd/ern",
  sep=""
)
nchar(message)

# 2nd Tweet message (280 character limit)
replymessage <- paste(
  "Average of N1 and N2 SARS-CoV-2 genetic markers normalized to Pepper Mild Mottle Virus as a fecal strength indicator; Samples are collected by @ottawacity, tested and analyzed by @RobDelatolla lab; Data i/o by @doug_manuel lab; Plots and tweet bot by @rnaguru.",
  sep= "\n"
)
nchar(replymessage)

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
  status = replymessage,
  token = rbot_token,
  in_reply_to_status_id = reply_id
)

#OTHER VIRUS PLOTS -------------------------------------------------------------
ottawaalltime_INFA <- ggplot(wwopen2, aes(x=Date, y = as.numeric(INFA), alpha = 7/10)) +
  #geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = 0.2, color= "lightpink", fill = "lightpink", linewidth = 0.1, alpha = 4/10)+
  #geom_line(wwopen2, mapping=aes(x=Date, y = as.numeric(INFA_roll7d)), color="slateblue")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=1, alpha = 1/10, na.rm = TRUE)+
  #geom_point(aes(x=as.Date(lastpoint_INFA$Date), as.numeric(lastpoint_INFA$INFA)),size=0.2, alpha = 1/10, color= "blue")+
  theme_classic()+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.01, size = 8, margin = margin(b = -10)),
        axis.title.x=element_blank())+
  labs(title= "Influenza A virus RNA - Ottawa, Canada")+
  ylab(bquote('IAV signal'~x10^-3))+
  geom_label(aes(x=lastpoint_INFA$Date, y=-0.04, label= paste('latest data:', toString(format(lastpoint_INFA$Date, "%B %d")))), color="slategrey", fill="white", alpha=1/25, size=2.5, vjust=0.5, hjust=1
  )+
  #limit y-axis and add comment to plot to deal with an outlier on decmeber 7, 2023
  ylim(-0.04,0.3)+
  annotate("text", x = Sys.Date() -250, y = 0.25, size = 2.2, color="darkblue", label = paste("To improve visual clarity, a single-day, high-signal outlier \n reported December 7, 2023 has been removed from this plot. \n The weekly average (blue line) still includes this outlier.")
  )


ottawaalltime_RSV <- ggplot(wwopen2, aes(x=Date, y = as.numeric(RSV), alpha = 7/10)) +
  #geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = 0.2, color= "lightpink", fill = "lightpink", linewidth = 0.1, alpha = 4/10)+
  #geom_line(wwopen2, mapping=aes(x=Date, y = RSV_roll7d, na.rm = TRUE), color="slateblue")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=1, alpha = 1/10, na.rm = TRUE)+
  #geom_point(aes(x=as.Date(lastpoint_INFA$Date), as.numeric(lastpoint_INFA$INFA)),size=0.2, alpha = 1/10, color= "blue")+
  theme_classic()+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.01, size = 8, margin = margin(b = -10)),
        axis.title.x=element_blank())+
  labs(title= "Respiratory Syncytial Virus RNA - Ottawa, Canada")+
  ylab(bquote('RSV signal'~x10^-3))+
  geom_label(aes(x=lastpoint_RSV$Date, y=-0.04, label= paste('latest data:', toString(format(lastpoint_RSV$Date, "%B %d")))), color="slategrey", fill="white", alpha=1/25, size=2.5, vjust=0.5, hjust=1
  )

ottawaalltime_INFB <- ggplot(wwopen2, aes(x=Date, y = as.numeric(INFB), alpha = 7/10)) +
  #geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = 0.2, color= "lightpink", fill = "lightpink", linewidth = 0.1, alpha = 4/10)+
  #geom_line(wwopen2, mapping=aes(x=Date, y = INFB_roll7d, na.rm = TRUE), color="slateblue")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=1, alpha = 1/10, na.rm = TRUE)+
  #geom_point(aes(x=as.Date(lastpoint_INFA$Date), as.numeric(lastpoint_INFA$INFA)),size=0.2, alpha = 1/10, color= "blue")+
  theme_classic()+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.01, size = 8, margin = margin(b = -10)),
        axis.title.x=element_blank(),
        plot.margin = margin(0, 0, 0, 0))+
  labs(title= "Influenza B virus RNA - Ottawa, Canada")+
  ylab(bquote('IBV signal'~x10^-3))+
  geom_label(aes(x=lastpoint_INFB$Date, y=-0.0003, label= paste('latest data:', toString(format(lastpoint_INFB$Date, "%B %d")))), color="slategrey", fill="white", alpha=1/25, size=2.5, vjust=0.5, hjust=1
  )

ottawaalltime_MPOX <- ggplot(wwopen2, aes(x=Date, y = as.numeric(MPOX), alpha = 7/10)) +
  #geom_smooth(method = "loess", se = TRUE, show.legend = FALSE, span = 0.2, color= "lightpink", fill = "lightpink", linewidth = 0.1, alpha = 4/10)+
  #geom_line(wwopen2, mapping=aes(x=Date, y = MPOX_roll7d, na.rm = TRUE), color="slateblue")+
  geom_ma(ma_fun = SMA, n=7, linetype= 1, size=1, show.legend = T)+
  geom_point(size=1, alpha = 1/10, na.rm = TRUE)+
  #geom_point(aes(x=as.Date(lastpoint_INFA$Date), as.numeric(lastpoint_INFA$INFA)),size=0.2, alpha = 1/10, color= "blue")+
  theme_classic()+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b %Y")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust=0.9, hjust=0.9),
        plot.title = element_text(hjust = 0.01, size = 8, margin = margin(b = -10)),
        axis.title.x=element_blank())+
  labs(title= "MPOX Virus DNA - Ottawa, Canada", caption="data source: https://github.com/Big-Life-Lab/PHESD")+
  ylab(bquote('MPOX signal'~x10^-3))+
  geom_label(aes(x=lastpoint_MPOX$Date, y=-0.005, label= paste('latest data:', toString(format(lastpoint_MPOX$Date, "%B %d")))), color="slategrey", fill="white", alpha=1/25, size=2.5, vjust=0.5, hjust=1
  ) 

#ARRANGE and LABEL -----------------------------------------------------------------------------
#dev.new(width = 5, height = 5, unit = "inches")
plot2 <-plot_grid(ottawaalltime_INFA, NULL, ottawaalltime_RSV, NULL, ottawaalltime_INFB, NULL, ottawaalltime_MPOX,ncol = 1, align = 'vh', axis = 'l',labels = c('A', '1', 'B', '2', 'C', '3', 'D'), rel_widths = c(1, 1, 1, 1, 1, 1, 1), rel_heights = c(1,-0.3,1,-0.3,1,-0.3,1), label_size = 16)

# define the plot file to tweet
png2 <- save_plot("ottawavirusww.png", plot=plot2,base_height=8,base_width=5)

# Tweet alt-text description (1000 character limit)
alttext2 <- paste(
  "Plots of IAV, RSV, IBV, MPOX wastewater signal across time in Ottawa, Canada;",
  sep= "\n")
nchar(alttext)

# Tweet message (280 character limit)
message2 <- paste(
  "#Ottawa virus wastewater trends as of: ", 
  format(lastpoint_INFA$Date, "%B %d"), 
  ". Polyline = 7 day average normalized signal.",
  sep=""
)
nchar(message2)

# 2nd Tweet message (280 character limit)
replymessage2 <- paste(
  "Average of genetic markers for each viral target normalized to Pepper Mild Mottle Virus as a fecal strength indicator; Samples are collected by @ottawacity, tested and analyzed by @RobDelatolla lab; Data i/o by @doug_manuel lab; Plots and tweet bot by @rnaguru.",
  sep= "\n"
)
nchar(replymessage2)

#TWEET out 2nd post via pooptweets twitter app --------------------------------------------------------
secondtweet <- post_tweet(
  status = message2,
  media = png2,
  media_alt_text = alttext2,
  token = rbot_token
)

reply_id2 <- ids(secondtweet)

post_tweet(
  status = replymessage2,
  token = rbot_token,
  in_reply_to_status_id = reply_id2
)
