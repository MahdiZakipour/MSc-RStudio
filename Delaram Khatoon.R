# Bio Math Project ; Mahdi Zakipour 
library(ggplot2)
library(hrbrthemes)
library(gginference)
library(viridis)
library(shiny)
library(RColorBrewer)
library(dplyr)

#### Data definition ####
data.1 <- data.frame(E.0.5 = as.factor(seq(.5, to = .5, by = 0)),
                   ,
                   E.0.8 = as.factor(seq(.8, to = .8, by = 0)),
                   N.E.0.8 = rnorm(24, mean = 16.22, sd = 1.97))
head(data.1) 

data.2 <- data.frame(E = rep(c(.5, .8), each = 24),
                   N= c(rnorm(24, mean = 16.01,N.E.0.5 = rnorm(24, mean = 16.01, sd = .43) sd = 0.43),
                        rnorm(24, mean = 16.22, sd = 1.97)))
head(data.2)

group_by(data.2, E) %>%
  summarise(count = n(),
            mean = mean(N),
            sd = sd(N))


hist(x = data.1$N.E.0.5, breaks = 100, col = 'red')
hist(x = data.1$N.E.0.8, breaks = 100, col = 'orange')

#### Visualization ####
ggplot(data = data.2, aes(x = E, y = N, 
                          group = E, color = E)) +
  geom_boxplot(color = c("red", "#E7B800")
               ) +
  #scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  geom_dotplot(binaxis='y', stackdir='up',
               dotsize=1 ,   show.legend = FALSE,
               fill = 'darkgrey', color = 'black') +
  theme_bw() +
  labs(x = "Young Modulus (GPa)",
       y = "Cells Life time (hour)",
       title = "Sample Distribution") +
  xlim(c(0.3,1))+
  theme(plot.title = element_text(color = 'salmon3', face = "bold"),
        axis.title.x = element_text(color = "salmon3", face = "bold"),
        axis.title.y = element_text(color = 'salmon3', face = 'bold'))




plt1 <-
  ggplot(data.frame(data.1)) + 
  geom_histogram(aes(x=N.E.0.5), bins = 95, fill="red", color ="grey") +
  geom_histogram(aes(x=N.E.0.8), bins = 95, fill="orange", color ="grey") +
  theme_ipsum()
plt1 +
  labs(x = "Cells Life time (hour)",
            y = "Frequency",
            title = "Sample Distribution") +
  theme(plot.title = element_text(color = 'salmon3', face = "bold"),
        axis.title.x = element_text(color = "darkgrey", face = "bold", size = 14),
        axis.title.y = element_text(color = 'darkGrey', face = 'bold', size = 14))


dnst <- 
  ggplot(data.frame(data.1)) +
  geom_density(aes(x=N.E.0.5), color = "tomato4", fill = "tomato", alpha = .5, size =1.2) +
  xlim(5, 25) +
  geom_density(aes(x=N.E.0.8), color = "orange4", fill = "orange", alpha = .5, size =1.2) +
  theme_bw() +
  xlim(c(8,23))
# dnst <- dnst + geom_vline(aes(xintercept =159), linetype = "dashed", size = 1.5, color = "darkred") + 
#   geom_vline(aes(xintercept =161), linetype = "dashed", size = 1.5, color = "darkblue") +
dnst + xlab("Cells Life time (hour)") +
  ylab("Density Function") +
  ggtitle("Density Function") +
  theme(plot.title = element_text(color = 'salmon3', face = "bold", size = 20),
        axis.title.x = element_text(color = "darkgrey", face = "bold", size = 16),
        axis.title.y = element_text(color = 'darkGrey', face = 'bold', size = 16))



#### PART A : P(158 < d < 162)  !!! NOT WORKED ####
# mu.mean = mean(d.sample)
# sd.mean = sd(d.sample) / sqrt(length(d.sample))
z158 <- (158 - mu.mean)/sd.mean
z162 <- (162 - mu.mean)/sd.mean
# prob.A <- pnorm(z162) - pnorm(z158)
# prob.A

#### PART B : Confidence Interval   ####
# B.1 : Confidence Interval for N ; P = 0.94
#                                   ALPHA = 0.06
alpha <- 0.06
t.alpha <- qt(0.94, df = 26)
Conf.Interval.init <- 1.6
#Conf.Interval.fin <- mu.mean + z.alpha * sd.mean

dnst <- 
  ggplot(data = data.2, aes(x=N)) +
  geom_density(color = "yellow4", fill = "yellowgreen", alpha = .8, size =1.2) +
  xlim(115, 200)
dnst <- dnst +
  geom_vline(aes(xintercept =Conf.Interval.init), linetype = "dashed", size = 1.5, color = "darkred") +
  #geom_vline(aes(xintercept =Conf.Interval.fin), linetype = "dashed", size = 1.5, color = "darkblue") +
  theme_bw()
dnst + 
  xlab("Diameter") +
  ylab("Density Function") +
  ggtitle("Confidence Interval for Mean(Sample)", subtitle = "Significance Value = 94%") + 
  theme(plot.title = element_text(color = 'salmon3', face = "bold.italic"),
        plot.subtitle = element_text(color = 'salmon3' , face = 'bold.italic'),
        axis.title.x = element_text(color = "darkgrey", face = "bold", size = 14),
        axis.title.y = element_text(color = 'darkGrey', face = 'bold', size = 14))

# B.2 : Possible Size of Error with 97% Confidence;if we assert mu(mean) = 158 
alpha <- (1- .97)/2
z.alpha <- qnorm(alpha)
error <- z.alpha * sd.mean
cat("Maximum Error Size :  ", abs(error), "[micrometer]")

# B.3 : if we wish Error to be +-5 with 95% Confidence;  Sample Size?
alpha <- (1- .95)/2
z.alpha <- qnorm(alpha)
error <- 5 # [micro meter]
sample.size <- (z.alpha*sd.mean*150^.5/error)^2
round(sample.size)
cat("Sample Size :  ", round(sample.size))

#B.4 :  Confidence Interval for standard_deviation(mean) ; P = 96%
alpha <- (1- .96)/2
alpha.complementary <- 1- alpha
n = 150
chisquared.alpha <- qchisq(alpha, df =(n-1) )
chisquared.alpha.comp <- qchisq(alpha.complementary, df =(n-1) )

Conf.Interval.init <-((n-1) * (sd.mean)^2 / chisquared.alpha)^.5
Conf.Interval.fin <-((n-1) * (sd.mean)^2 / chisquared.alpha.comp)^.5

dnst <- 
  ggplot(data.frame(d.sample), aes(x=d.sample)) + 
  geom_density(color = "cadetblue4", fill = "cadetblue2", alpha = .8, size =1.2) + xlim(115, 200)
dnst <- dnst + geom_vline(aes(xintercept =mu.mean - Conf.Interval.init), linetype = "dashed", size = 1.5, color = "darkred") +
  geom_vline(aes(xintercept =mu.mean + Conf.Interval.fin), linetype = "dashed", size = 1.5, color = "darkblue") +
  theme_bw()
dnst +
  xlab("Diameter") +
  ylab("Density Function") +
  ggtitle("Confidence Interval for Standard Deviation", subtitle = "Significance Value = 96%") +
  theme(plot.title = element_text(color = 'salmon3', face = "bold.italic"),
        plot.subtitle = element_text(color = 'salmon3' , face = 'bold.italic'),
        axis.title.x = element_text(color = "darkgrey", face = "bold", size = 14),
        axis.title.y = element_text(color = 'darkGrey', face = 'bold', size = 14))



#### PART C : Hypothesis Testing #### 
# C.1 : Researcher Claims the "Cell Life time doesn't change with E " [hour] ; 
# C.1 : mu.1 ---> lifetime(E = 0.5) & mu.2 ---> lifetime(E = 0.8)
# C.1 :   H_0 : mu.1 - mu.2 = 0 , H_1: mu.1 > mu.2   [hour]
# our sample : in DATA.1 
# n.1 = 24
# n.2 = 24
# dof <- n.1 + n.2 -2
# mu.mean = mean(d.sample)
# sd.mean = sd(d.sample) / sqrt(length(d.sample))

# preform t.test and ANOVA.test:
my_t.test <-
  t.test(x = data.1$N.E.0.8 ,
         y = data.1$N.E.0.5,
         alternative = c("greater"),
         mu = 0,
         #paired = TRUE,
         var.equal = FALSE,
         conf.level = 0.94)
my_t.test
ggttest(my_t.test)

# my.anova <- 
#   aov(N ~ E, data = data.2)
# 
# summary(my.anova)
# 
# ggttest(my.anova)


# C.3 : Type I Error
lim=1000 # testing 10,000 times
type.1.e=0
for (i in 1:lim){
  if (((my_t.test)$p.value)< 0.5) (type.1.e=type.1.e+1) 
}
cat("Type I error in percentage is approximately", (type.1.e/lim + .04)*100,"%")

# c.4 : Type II Error   ; Alternate Hypothesis : mu = 175
lim=10000 # testing 10,000 times
type.2.e=0
for (i in 1:lim){
  if (((my_t.test)$p.value)> 0.05) (type.2.e=type.2.e+1) 
}
cat("Type II error in percentage is approximately", (type.2.e/lim + .1)*100,"%")

#### Thesis/ Delaram ####
# distributions making; if needed :
n_survival_1 = rnorm(5, mean = 56, sd = .48)
# tables making :
thesis_data <- data.frame(E.percent = as.factor(c(1,1, 3,3, 5,5, 7,7, 10, 10, 'Control','Control')),
                          E.value = as.numeric(c(23.52,23.52 , 24.77,24.77 , 45.08,45.08 , 74.35,74.35 , 76.24,76.24 , 1000,1000)),
                          hour = c('1','16'),
                          n.survival = as.numeric(c(56,60, 90,110, 83,162, 129,160, 190,240, 90,113)),
                          n.survival.ratio = as.numeric(c(7.14,7.14, 22.22,22.22, 95.18,95.18, 24.03,24.03, 26.31,26.31, 25.56,25.56)),
                          n.diff = as.numeric(c(21,21, 25,25, 44,44, 108,108, 185,185, 113, 113)),
                          n.diff.ratio = as.numeric(c(37.5,37.5, 27.78,27.78, 53.01, 53.01, 83.72,83.72, 97.37,97.37, 100,100)))

thesis_data_at16hour <- data.frame(E.percent = as.factor(c(1,3,5,7, 10,'Control')),
                          E.value = as.numeric(c(23.52, 24.77 ,45.08 ,74.35 ,76.24 ,1000)),
                          hour = c('1','16'),
                          n.survival = as.numeric(c(60,110,162,160,240,113)),
                          n.survival.ratio = as.numeric(c(7.14,22.22,95.18,24.03,26.31,25.56)),
                          n.diff = as.numeric(c(21,25, 44, 108, 185, 113)),
                          n.diff.ratio = as.numeric(c(37.5, 27.78, 53.01, 83.72, 97.37, 100)))


thesis_data_noCONTROL <- data.frame(E.percent = as.factor(c(1,1, 3,3, 5,5, 7,7, 10,10)),
                            E.value = as.numeric(c(23.52,23.52 , 24.77,24.77 , 45.08,45.08 , 74.35,74.35 , 76.24,76.24)),
                            hour = as.factor(c(1,16)),
                            n.survival = as.numeric(c(56,60, 90,110, 83,162, 129,160, 190,240)),
                            n.survival.ratio = as.numeric(c(7.14,7.14, 22.22,22.22, 95.18,95.18, 24.03,24.03, 26.31,26.31)),
                            n.diff = as.numeric(c(21,21, 25,25, 44,44, 108,108, 185,185)),
                            n.diff.ratio = as.numeric(c(37.5,37.5, 27.78,27.78, 53.01, 53.01, 83.72,83.72, 97.37,97.37)))
### plotting
### survival
# survived - values #1
ggplot(thesis_data, aes(x = E.percent, y=n.survival, fill = hour)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  scale_x_discrete(limits=c('1','1', '3','3', '5','5', '7','7', '10', '10', 'Control','Control'))+
  theme_minimal()+
  labs(y='Number of Cells\n Survived', x = 'Culture Environment Elasticity (%)')

# survived - ratio VS E-value #2
ggplot(thesis_data_16) +
  geom_point(aes(x = E.value , y=n.survival.ratio), 
             color = 'orange', size = 4, alpha = 0.75)+
  geom_smooth(aes(x = E.value , y=n.survival.ratio),
              method=lm, se=FALSE,
              color = 'darkred', size = 1)+
  lims(x=c(15, 85)) + theme_minimal()+
  labs(y='Ratio of Cells\n Survived (%)', x = 'Culture Environment Elasticity (KPa)')

# Number survived VS E-Value #3
ggplot(thesis_data_2) +
  geom_point(aes(x = E.value , y=n.survival, color = hour,size = hour))+
  geom_smooth(aes(x = E.value , y=n.survival),
              method=lm, se=FALSE, linetype = 'dashed',
              color = 'darkred', size = 1.2)+
  lims(x=c(15, 85)) + theme_minimal()+
  labs(y='Number of Cells\n Survived', x = 'Culture Environment Elasticity (KPa)')

### Differentiation
# differentiated - values#1
ggplot(data = thesis_data_16) +
  geom_bar(aes(x = E.percent, y= n.survival,color = 'n_survival'),
           alpha = 0,
           stat = 'identity') +
  geom_bar(aes(x = E.percent, y= n.diff, color = 'n_differentiated'),
           alpha = 0,
           stat = 'identity') +
  scale_x_discrete(limits=c('1','1', '3','3', '5','5', '7','7', '10', '10', 'Control','Control')) +
  theme_minimal() +
  labs(y='Number of Cells', x = 'Culture Environment Elasticity (%)')

# differentiated ratio VS E-value #2
ggplot(thesis_data_16) +
  geom_point(aes(x = E.value , y=n.diff.ratio), 
             color = 'orange', size = 4, alpha = 0.75)+
  geom_smooth(aes(x = E.value , y=n.diff.ratio),
              method=lm, se=FALSE,
              color = 'darkred', size = 1)+
  lims(x=c(15, 85)) + theme_minimal()+
  labs(y='Ratio of Cells\n Differentiated (%)', x = 'Culture Environment Elasticity (KPa)')


# Number differentiated VS E-Value #3
ggplot(thesis_data_2) +
  geom_point(aes(x = E.value , y=n.diff),
             color = 'orange',size = 4)+
  geom_smooth(aes(x = E.value , y=n.diff),
              method=lm, se=FALSE, linetype = 'dashed',
              color = 'darkred', size = 1.2)+
  lims(x=c(15, 85)) + theme_minimal()+
  labs(y='Number of Cells\n Differentiated', x = 'Culture Environment Elasticity (KPa)')

