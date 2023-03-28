### An application to gather Ottawa wastewater data from PHESD github repo, assemble plots, and tweet the plots @rnaguru whenever there is a new commit.

Pipeline:

In R:
* pull Ottawa daily N1/PMMoV and N2/PMMoV, normalized copies from PHESD
* compute average normlized copies
* compute 7d rolling mean across pandemic
* compute min, median, and max 7-day valued across pandemic
* pull out row corresponding to last day of sampling
* create the following plots (ggplot, ggrepel, gghihglight):
  * pandemic-wide overview showing min/median/max, datpoints as normalized copies and computer 7d average using the SMA function of ggplot (on-the-fly)
  * past year (as above, highlighted portion in 'pandemic-wide' plot)
  * past 2 months (as above, highlighted portion in 'past year' plot)
* arrange and label panels using cowplot
* save a png file
* tweet to @rnaguru using rtweet with WebAuth 1.0 authentication via Github secrets

YAML
* defines env for Github Actions, location of secrets, R packages.
-
