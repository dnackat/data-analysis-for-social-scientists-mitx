# set the library with the packages we use
library(ggplot2)
library(tidyverse)
require(cowplot)

#load in the data 
bihar_data<-read_csv("data/Bihar_sample_data.csv")

#have a look
print(bihar_data)

#keep only females
bihar_adult_females <-filter(bihar_data, adult==1,female==1) 

#have a look 

print(bihar_adult_females)

# default histogram in ggplot

ggplot(bihar_adult_females, aes(height_cm))+
  geom_histogram()
ggsave("output/bihar_raw.pdf")

#some people look like they are very small: filtering
bihar_adult_females_trunc <-filter(bihar_adult_females, height_cm>120, height_cm<200)

#Plotting again, with a nicer label, and some color 
ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue")+
  xlab("Height in centimeters, Bihar Females")
ggsave("output/bihar_better.pdf")

# playing with the bins 

#Playing with bins 
bihar1 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 5)+
  xlab("bin width=5")+
  ylab("")


bihar2 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 10)+
  xlab("bin width=10")+
  ylab("")


bihar3 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 20)+
  xlab("bin width=20")+
  ylab("")

bihar4 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 50)+
  xlab("bin width=50")+
  ylab("")

plot_grid(bihar1, bihar2, bihar3, bihar4, labels="Female Height in Bihar", hjust=-1, vjust=0.2)
ggsave("output/bihargrid.pdf")


#US Data from National Health and Nutrition Examination Survey
us_data <- read_csv("data/US_sample_data.csv")

print(us_data)

# Subset the data for adult females
us_adult_females_trunc <- filter(us_data,female==1 , adult==1, height_cm>120 , height_cm<200)

print(us_adult_females_trunc)


#Plotting the US women in one go
ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="red", color="darkred")+
  xlab("Height in centimeters, US females")
ggsave("output/US_better.pdf")




# kernel density estimation

ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm))

ggsave("output/US_kernel.pdf")


#playing with the bandwidth

US1 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=1)+
  xlab("bw=1")+
  ylab("")

US2 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=5)+
  xlab("bw=5")+
  ylab("")

US3 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=10)+
  xlab("bw=10")+
  ylab("")

US4 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=20)+
  xlab("bw=20")+
  ylab("")

plot_grid(US1, US2, US3, US4, labels="Female Height in the US", hjust=-1, vjust=0.2)
ggsave("output/US_kerneltries.pdf")

#combining the two histograms
ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=bihar_adult_females_trunc, aes(height_cm),fill="blue", color="darkblue" )+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm), fill="red", color="darkred" )


#makes no sense to do it as count! 

ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=bihar_adult_females_trunc, aes(height_cm, ..density.. ),fill="blue", color="darkblue")+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="red", color="darkred" )+
  xlab("Height in centimeters")

# more visible as points 

ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_freqpoly(data=bihar_adult_females_trunc, aes(height_cm, ..density.. ), color="darkblue" )+
  geom_freqpoly(data=us_adult_females_trunc, aes(height_cm , ..density..),  color="darkred" )+
  xlab("Height in centimeters")

ggsave("output/comparehistograms.pdf")

# Kernel density

ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_density(data=bihar_adult_females_trunc, aes(height_cm), color="darkblue" )+
  geom_density(data=us_adult_females_trunc, aes(height_cm),  color="darkred" )+
  xlab("Height in centimeters")

ggsave("output/heightkernel.pdf")

# Representing the CDF

ggplot(bihar_adult_females_trunc, aes(height_cm))+
  stat_ecdf(data=bihar_adult_females_trunc, aes(height_cm), color="darkblue" )+
  stat_ecdf(data=us_adult_females_trunc, aes(height_cm),  color="darkred" )+
  xlab("Height in centimeters")



ggsave("output/heightcdf.pdf")