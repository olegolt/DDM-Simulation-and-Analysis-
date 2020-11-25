# -------------------------------------------------------------------------------------------------------------------------------------
# 
#                                                Script for analysing all data 
# 
# -------------------------------------------------------------------------------------------------------------------------------------

# In this script we will analyse all relevant data (percentage of orange responses and DDM parameters)

# libraries
library(plyr)
library(ez)
library(compute.es)
library(ggplot2)
library(Hmisc)
library(reshape)


# -------------------------------------------------------------------------------------------------------------------------------------
#      Functions                                                                                                                   
# -------------------------------------------------------------------------------------------------------------------------------------

# ---------------------
#  summarySE function
# ---------------------

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, 
## and confidence interval (default 95%).
##     data: a data frame.
##     measurevar: the name of a column that contains the variable to be summariezed
##     groupvars: a vector containing names of columns that contain grouping variables
##     na.rm: a boolean that indicates whether to ignore NA's
##     conf.interval: the percent range of the confidence interval (default is 95%)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# -------------------------------------------------------------------------------------------------------------------------------------
#      Analyse of demographic data and suspicion check (not relevant for simulated data)
# -------------------------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------------------------
#      Data pre-processing 
# -------------------------------------------------------------------------------------------------------------------------------------

# load data
data_trial <- read.csv("data_trial.csv")

# check for trials with no responses and exclude them
n_trials_all = nrow(data_trial)
data_trial <- data_trial[!is.na(data_trial$resp),]
n_trials_ex1 = nrow(data_trial)
percentage_no_response = ((n_trials_all-n_trials_ex1)/n_trials_all)*100
print("% trials without response")
print(percentage_no_response)

# check for outlier 
data_trial_iqr <- ddply(data_trial,.(participant,norm,stimulus),
                        function(x) c(iqr = IQR(x$rt),
                                      first_quartile = as.numeric(quantile(x$rt, 0.25)),
                                      third_quartile = as.numeric(quantile(x$rt, 0.75))))
data_trial_iqr$lower_limit = data_trial_iqr$first_quartile - data_trial_iqr$iqr * 1.5
data_trial_iqr$upper_limit = data_trial_iqr$third_quartile + data_trial_iqr$iqr * 1.5
data_trial <- merge(data_trial,data_trial_iqr)
data_trial$outlier = ifelse(data_trial$lower_limit < data_trial$rt & data_trial$upper_limit > data_trial$rt,0,1)
outlier <- data_trial[data_trial$outlier == 1,]
percentage_outlier =((nrow(data_trial)-(nrow(data_trial)-nrow(outlier)))/nrow(data_trial))*100
print("% trials with outlier")
print(percentage_outlier)
# remove outlier
data_trial <- data_trial[data_trial$outlier != 1,]

# avarage data over trials within stimulus conditions
data_rp <- ddply(data_trial,.(participant,norm,stimulus),
                 function(x) c(resp = mean(x$resp)*100,
                               rt = mean(x$rt)))

# merge with questionnaire data thereby excluding participants (see above) --> not relevant for simulated data
# data_rp <- merge(quest,data_rp)



# -------------------------------------------------------------------------------------------------------------------------------------
#      Response probabilities 
# -------------------------------------------------------------------------------------------------------------------------------------

# ---------------------
#  ANOVA
# ---------------------

data_rp$stimulus = as.factor(data_rp$stimulus)
anova <-ezANOVA(data_rp,
                dv=resp,
                wid=participant,
                between = .(norm),
                within = .(stimulus),
                within_full = .(stimulus),
                detailed=F,
                return_aov =T,
                type=3)
# show results
anova$ANOVA


# ---------------------
#  Means, SEs and CIs
# ---------------------

means <- ddply(data_rp,.(participant,norm),
               function(x) c(resp = mean(x$resp)))

means_table <- summarySE(data = means,measurevar = "resp", groupvars = c("norm"))
means_table$lci = means_table$resp - means_table$ci
means_table$uci = means_table$resp + means_table$ci
means_table


# ---------------------
#  2-way interactions
# ---------------------

n <- table(data_rp$norm)/3

# t-test 47.5% orange
print("stimulus: 47.5%")
t <- t.test(resp~norm, data = data_rp[data_rp$stimulus == "47.5",])
t
#effect sizes
tes(t$statistic, n.1 = n[1], n.2 = n[2])

# t-test 50% orange
print("stimulus: 50%")
t <- t.test(resp~norm, data = data_rp[data_rp$stimulus == "50",])
t
#effect sizes
tes(t$statistic, n.1 = n[1], n.2 = n[2])

# t-test 52.5% orange
print("stimulus: 52.5%")
t <- t.test(resp~norm, data = data_rp[data_rp$stimulus == "52.5",])
t
#effect sizes
tes(t$statistic, n.1 = n[1], n.2 = n[2])


# ---------------------
#  plot results
# ---------------------

A<-ggplot(data_rp, aes(stimulus,resp, fill = norm, color = norm, shape = norm)) + 
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line",size = 1, aes(group = norm)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1, size = 1) +
  scale_color_manual(values=c("#007fbf","#ff8040")) +
  ylab("orange responses (%)") +
  xlab("proportion of orange pixels (%)") +
  coord_cartesian(ylim=c(0, 100)) 
A


# -------------------------------------------------------------------------------------------------------------------------------------
#      Drift rates 
# -------------------------------------------------------------------------------------------------------------------------------------

# read ddm data
ddm <- read.csv("data_DDM.csv")

# rename first column to participant 
names(ddm)[1] <- "participant"

# build dataframe with 3 rows for every participant (one v value for every stimulus condition)
ddm_long <- melt(ddm, id.vars = c("participant","a","zr","t0","st0","fit"),
                 measure.vars = c("v_1","v_2","v_3"),
                 variable_name = "stimulus")
ddm_long$stimulus <- as.factor(as.numeric(gsub("v_","",ddm_long$stimulus)))
names(ddm_long)[8] <- "v"

# merging data, thereby excluding participants with outliers / suspicion check (see above, not relevant for simulated data)
#data_ddm <- merge(data_rp,ddm_long)

# these two lines are only relevant for the simulated data
ddm_long <- ddm_long[order(ddm_long$participant),] 
data_ddm<-cbind(data_rp,ddm_long[2:6],ddm_long[8])

# find and exclude participants with a bad fit (not relevant for simulated data)
#print("p(crit)")
#data_ddm$p_crit[1]
#print("number of models with bad fit")
#nrow(data_ddm[data_ddm$fit < data_ddm$p_crit,])/3
## excluding  participants with bad fit (4 participants)
#data_ddm <- data_ddm[data_ddm$fit > data_ddm$p_crit,]
## indexing participants with good fit in extinction phase
#index <- data.frame(participant = unique(data_ddm$participant))


# ---------------------
#  ANOVA
# ---------------------

anova <-ezANOVA(data_ddm,
                dv=v,
                wid=participant,
                between = .(norm),
                within = .(stimulus),
                within_full = .(stimulus),
                detailed=F,
                type=3)
anova$ANOVA


# ---------------------
#  Means, SEs and CIs
# ---------------------

means <- ddply(data_ddm,.(participant,norm),
               function(x) c(v = mean(x$v)))

means_table <- summarySE(data = means,measurevar = "v", groupvars = c("norm"))
means_table$lci = means_table$v - means_table$ci
means_table$uci = means_table$v + means_table$ci
means_table


# ---------------------
#  2-way interactions, because interaction is significant in simulated data
# ---------------------

n <- table(data_ddm$norm)/3

# t-test 47.5% orange
print("stimulus: 47.5%")
t <- t.test(resp~norm, data = data_ddm[data_ddm$stimulus == "47.5",])
t
#effect sizes
tes(t$statistic, n.1 = n[1], n.2 = n[2])

# t-test 50% orange
print("stimulus: 50%")
t <- t.test(resp~norm, data = data_ddm[data_ddm$stimulus == "50",])
t
#effect sizes
tes(t$statistic, n.1 = n[1], n.2 = n[2])

# t-test 52.5% orange
print("stimulus: 52.5%")
t <- t.test(resp~norm, data = data_ddm[data_ddm$stimulus == "52.5",])
t
#effect sizes
tes(t$statistic, n.1 = n[1], n.2 = n[2])

# ---------------------
#  plot results
# ---------------------

B <- ggplot(data_ddm, aes(stimulus,v, fill = norm, color = norm, shape = norm)) + 
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line",size = 1, aes(group = norm)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .1, size = 1) +
  scale_color_manual(values=c("#007fbf","#ff8040")) +
  ylab("drift rate (v)") +
  xlab("proportion of orange pixels (%)") +
  coord_cartesian(ylim=c(-5, 5)) 
B















