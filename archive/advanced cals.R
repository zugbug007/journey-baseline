

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        COMPUTE INDEPENDENT SAMPLE T-TEST                 ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paired t-tests: Compare the means of two sets of paired samples, taken from two populations with unknown variance
# The two-sample unpaired t-test is a commonly used test that compares the means of two samples.
# Appropriate data
# •  Two-sample data.  That is, one measurement variable in two groups or samples
# •  Dependent variable is interval/ratio, and is continuous
# •  Independent variable is a factor with two levels.  That is, two groups
# •  Data for each population are normally distributed
# •  For Student's t-test, the two samples need to have the same variance.  However, Welch’s t-test, which is used by default in R, does not assume equal variances.
# •  Observations between groups are independent.  That is, not paired or repeated measures data
# •  Moderate skewness is permissible if the data distribution is unimodal without outliers

# Hypotheses
# •  Null hypothesis:  The means of the populations from which the data were sampled for each group are equal.
# •  Alternative hypothesis (two-sided): The means of the populations from which the data were sampled for each group are not equal.

# Interpretation
# Reporting significant results as “Mean of variable Y for group A was different than that for group B.” is acceptable.

library(ggpubr)
library(rstatix)
journey_name_Stat <- "Commercial: Shop Checkout Steps 1-4"
pre <- journey_data %>% select(journey_name, journey_type, Day, Visits) %>% filter(journey_name == journey_name_Stat & journey_type =="pre") %>% 
  group_by(journey_type) %>% arrange(Day) #%>% pull(Visits)
post <- journey_data %>% select(journey_name, journey_type, Day, Visits) %>% filter(journey_name == journey_name_Stat & journey_type =="post") %>% 
  group_by(journey_type) %>% arrange(Day) #%>%  pull(Visits)

df1 <- journey_data %>% select(journey_name, journey_type, Day, Visits) %>% filter(journey_name == journey_name_Stat) %>% 
  group_by(journey_type) %>% arrange(Day)

# Load the data
data("genderweight", package = "datarium")
# Show a sample of the data by group
set.seed(123)
genderweight %>% sample_n_by(group, size = 2)

genderweight %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")

# Compute t-test
res <- t.test(weight ~ group, data = genderweight)
res

# https://uc-r.github.io/t_test
ggplot(df1, aes(journey_type, Visits)) + geom_boxplot()

t.test(Visits ~ journey_type, data = df1)

# https://gist.github.com/psychemedia/1ee9214becf289eb3e0818f5fe7f0a58
library(ggplot2)
g=ggplot(economics_long, aes(date, value01, colour = variable)) + geom_line()+ggtitle('dummy title')
g=p2

#The label values may not be the limts
txt=paste('The chart titled"', g$labels$title,'"',
          'with x-axis', g$labels$x,'labeled from',ggplot_build(g)$panel$ranges[[1]]$x.labels[1],'to',tail(ggplot_build(g)$panel$ranges[[1]]$x.labels,n=1),
          'and y-axis', g$labels$y,'labeled from',ggplot_build(g)$panel$ranges[[1]]$y.labels[1],'to',tail(ggplot_build(g)$panel$ranges[[1]]$y.labels,n=1),sep=' ')
if ('colour' %in% attributes(g$labels)$names){
  txt=paste(txt,'\nColour is used to represent',g$labels$colour)
  
  if ( class(g$data[[g$labels$colour]]) =='factor') {
    txt=paste(txt,', a factor with levels: ',
              paste(levels(g$data[[g$labels$colour]]), collapse=', '), '.', sep='')
  }
}

txt
#"The chart titled "dummy title" with x-axis date labeled from 1970 to 2010 and y-axis value01 labeled from 0.00 to 1.00
#Colour is used to represent variable, a factor with levels: pce, pop, psavert, uempmed, unemploy."

# https://www.marinedatascience.co/blog/2019/09/28/comparison-of-change-point-detection-methods/#take-home-message
library(changepoint)
library(changepoint.np)
library(bcp)
library(strucchange)
library(segmented)
library(tree)

# Simulate
set.seed(42)  # I always use 42; no fiddling
df = data.frame(
  x = 1:100,
  y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1))
)


shop_cp <- journey_data %>% select(Day, journey_name, Visits) %>% filter(journey_name == "Commercial: Shop Checkout Steps 1-4") %>% arrange((Day)) %>% 
  select(-journey_name) 

fit_changepoint = cpt.mean(shop_cp$Visits)

# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)


------------------------------------------------------

ggplot(melt(data.frame(time=as.numeric(time(var.Bu.Dis@data.set)), var.Bu.Dis@data.set), id.vars="time"), aes(time, value)) + 
  geom_line() +
  geom_vline(xintercept = cpts.ts(var.Bu.Dis), colour="red", linetype = "dotdash")
