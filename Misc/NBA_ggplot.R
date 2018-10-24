
library(tidyverse)
require(cowplot)

sc <- read_csv("steph_curry_shot_data.csv")
lj <- read_csv("lebron_james_shot_data.csv")
kd <- read_csv("kevin_durant_shot_data.csv")


# CDF comparing Shot distance for Shots made
sc_shot_made <- filter(sc, shot_made==1)
lj_shot_made <- filter(lj, shot_made==1)
kd_shot_made <- filter(kd, shot_made==1)

ggplot(sc_shot_made, aes(shot_distance))+
  stat_ecdf(data=sc_shot_made, aes(shot_distance), color="darkblue" )+
  stat_ecdf(data=lj_shot_made, aes(shot_distance),  color="darkred" )+
  stat_ecdf(data=kd_shot_made, aes(shot_distance),  color="purple" )+
  xlab("Distance to basket for shot made, CDF")+
  ylab("")
ggsave("Comparisonbetweenplayers.pdf")

# alternatively we can first append the three data sets, and then plot

threeplayers<-bind_rows(list( "Stephen Curry"=sc, "Lebron James"=lj, "Kevin Durrant"=kd) , .id="player")
threeplayers_shots_made <- filter(threeplayers, shot_made==1)

ggplot(threeplayers_shots_made, aes(shot_distance, colour=player))+
  stat_ecdf()+
  xlab("Distance to basket for shot made, CDF")+
  ylab("")
ggsave("Comparisonbetweenplayers_onedataset.pdf")

# The combined data set also allows to do a comparative boxplot
ggplot(threeplayers_shots_made)+
  geom_boxplot(
    mapping=aes( 
      x = reorder(player, shot_distance, FUN = median),
      y = shot_distance
    )
  )+
  coord_flip()+
  xlab("")+
  ylab("Boxplots of distance of shot made, by player")+
  theme_minimal()
  

  ggsave("boxplot.pdf")



# two ways density for steve Curry

SC1 <- ggplot(sc, aes(distance_from_midline_feet,distance_from_baseline_feet))+
  geom_bin2d()+
  ylab("from baseline")



SC2 <- ggplot(sc, aes(distance_from_midline_feet,distance_from_baseline_feet))+
  geom_density2d()+
  ylab("from baseline")

plot_grid(SC1, SC2, ncol = 1)
ggsave("stevecurry2d.pdf")



