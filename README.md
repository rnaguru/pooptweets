[![pooptweets](https://github.com/rnaguru/pooptweets/actions/workflows/pooptweets.yml/badge.svg?event=schedule)](https://github.com/rnaguru/pooptweets/actions/workflows/pooptweets.yml)
### An application to gather Ottawa wastewater data from PHESD github repo, assemble plots, and tweet the plots @rnaguru whenever there is a new commit.

**Overview of Pipeline:** 


* YAML file `pooptweets.yml` defines env for Github Actions, location of secrets, R packages.
* In `ottawacov2tweet.R` pull Ottawa daily N1/PMMoV and N2/PMMoV, normalized copies from [PHESD](https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv);
* compute average normalized copies [(N1+N2)/PMMoV];
* compute 7d rolling mean across pandemic;
* compute min, median, and max 7-day valued across pandemic;
* pull out row corresponding to last day of sampling;
* create the following plots (using ggplot, ggrepel, gghighlight):
  * pandemic-wide overview showing min/median/max, datpoints as normalized copies and computer 7d average using the SMA function of ggplot (on-the-fly)
  * past year (as above, highlighted portion in 'pandemic-wide' plot)
  * past 2 months (as above, highlighted portion in 'past year' plot)
  * wastewater-based incidence and Reff (uses a modified Ern package; see detailed pipeline below);
* arrange and label panels using cowplot;
* save a png file;
* tweet to @rnaguru using rtweet with WebAuth 1.0 authentication via Github secrets;
* tweet again but with RSV, IAV, IBV plots, with a similar method as above. This code is incorporated into `pooptweets.yml`


**Detailed pipeline**
1. Pull in Ottawa viral RNA/DNA data from [PHESD](https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv).
2. transform to make y-axis units easier to read and create min, mean, max and other landmarks such as percent of pandemic mean and lastest datapoints.
3. Clean up inputs (remove NAs and other non-numeric entries).
4. Generate pandemic, annual, 2 month plots of SARS-CoV-2 signal.
5. Use the Ern package forked from the following repo and called in `pooptweets.yml`:
```
Champredon D, Papst I, Yusuf W (2023). _ern: An R Package to Estimate
the Effective Reproduction Number Using Clinical and Wastewater
Surveillance Data_. National Microbiology Laboratory, Public Health
Agency of Canada, Government of Canada.
<https://github.com/phac-nml-phrsd/ern>.
```
6. Use the default ern "sarscov2" incubation period and generation interval distribution as defined in `distributions.R` of the Ern package
```
mean     = 3.49,
      mean_sd  = 0.1477,
      shape    = 8.5,
      shape_sd = 1.8945,
      max      = 8
```
```
mean     = 6.84,
      mean_sd  = 0.7486,
      shape    = 2.39,
      shape_sd = 0.3573,
      max      = 15)
```

7. Run a modified plot_diagnostic_ww function (customizes look of plot)
8. calculate incidence metrics as follows:
```
#calculate incidence from last 60 days and proportion of population infected
incidence_last60days <- plotReff$g.inc$data %>%
  filter(date >= as.Date(max(Sys.Date() - 60)) & date <= as.Date(max(date)))

# calculate cumulative incidence in last 60 days and percent of Ottawa population infected
cumulative_incidence_last60day <- sum(incidence_last60days$mean) #sum daily incidence over last 60 days
percent_infected_last60day <- signif((cumulative_incidence_last60day / 1000000) * 100, digits = 2) #calculate percent infected assuming 1M captured in Ottawa
```
> [!IMPORTANT]
> The estimate_R_ww function defines scaling factor (you must calibrate incidence based on a known clinical benchmark), smoothing parameters, iterations of model. In this case, the estimated peak daily cases during the BA.2 wave in Ottawa in Spring 2022 based on estimated daily cases in the province of Ontario (https://www.cbc.ca/news/canada/toronto/wastewater-data-ontario-1.6410892)
```
estimate_R_ww(
  ww.conc,
  dist.fec,
  dist.gi,
  scaling.factor = 5500, #adjusted based on 10^3 N1N2norm transformation
  prm.smooth = list(window = 14, align = "center", method = "loess", span = 0.1),
  prm.R = list(iter = 10, CI = 0.999, window = 14, config.EpiEstim = NULL),
  silent = FALSE,
  RL.max.iter = 9
```

> [!CAUTION]
> The Reff and incidence curves are generated based on the entire pandemic period, but only the last two months are shown via the tweet .
