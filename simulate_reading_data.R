# ------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------- Script for simulating data and reading it in ------------------------------------------- 
# ------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------

# In this script we will first simulate data using a DDM with parameters from Germar & Mojzisch (2019). Afterwards this simulated data 
# will be read in and saved as a .csv file. For analyses see the script "DDM_analyses" and "Experiment_analyses".


# ------------------------------------------------------------------------------------------------------------------------------------
# ---------- Simulate data using the fast-dm (Voss & Voss, 2007) procedure -----------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------

# for simulating data the construct-samples.exe file is used
# we will simulate two different data sets corresponding to the two conditions (blue and orange norm) in Germar & Mojzisch (2019)
# we use the parameters calculated in the analysis of the Germar & Mojzisch (2019) paper to obtain plausible data 
# for the blue norm condititon we use the drift rate values [-3.06 (47.5% orange), -0.43 (50% orange), 2.52 (52.5% orange)]
# for the orange norm condititon we use the drift rate values [-2.57 (47.5% orange), 0.43 (50% orange), 3.66 (52.5% orange)]

# ----- Simulate data in blue norm condition 

for (sub in 1:50) {
  system("construct-samples.exe -a 1 -z 0.5 -v -3.067352 -t 0.5 -d 0 -Z 0 -V 0 -T 0 -r -N 1 -n 36 -p 5 -o temp1.lst")
  system("construct-samples.exe -a 1 -z 0.5 -v -0.4386587 -t 0.5 -d 0 -Z 0 -V 0 -T 0 -r -N 1 -n 36 -p 5 -o temp2.lst")
  system("construct-samples.exe -a 1 -z 0.5 -v  2.525409 -t 0.5 -d 0 -Z 0 -V 0 -T 0 -r -N 1 -n 36 -p 5 -o temp3.lst")
  
  
  S1 = read.table("temp1.lst", header = FALSE)
  S2 = read.table("temp2.lst", header = FALSE)
  S3 = read.table("temp3.lst", header = FALSE)
  
  
  S = rbind(S1,S2,S3)
  C = c(rep(1,36), rep(2,36), rep(3,36))
  S = cbind(C, S)
  
  fn = sprintf("%d.dat", sub)
  write.table(S, fn, col.names=FALSE, row.names=FALSE)
}

unlink("temp1.lst")
unlink("temp2.lst")
unlink("temp3.lst")

# empty environment
rm(list=ls())

# ----- Simulate data in orange norm condition 

for (sub in 51:100) {
  system("construct-samples.exe -a 1 -z 0.5 -v -2.566767 -t 0.5 -d 0 -Z 0 -V 0 -T 0 -r -N 1 -n 36 -p 5 -o temp1.lst")
  system("construct-samples.exe -a 1 -z 0.5 -v 0.4281289 -t 0.5 -d 0 -Z 0 -V 0 -T 0 -r -N 1 -n 36 -p 5 -o temp2.lst")
  system("construct-samples.exe -a 1 -z 0.5 -v  3.655644 -t 0.5 -d 0 -Z 0 -V 0 -T 0 -r -N 1 -n 36 -p 5 -o temp3.lst")
  
  S1 = read.table("temp1.lst", header = FALSE)
  S2 = read.table("temp2.lst", header = FALSE)
  S3 = read.table("temp3.lst", header = FALSE)
  
  S = rbind(S1,S2,S3)
  C = c(rep(1,36), rep(2,36), rep(3,36))
  S = cbind(C, S)
  
  fn = sprintf("%d.dat", sub)
  write.table(S, fn, col.names=FALSE, row.names=FALSE)
}

unlink("temp1.lst")
unlink("temp2.lst")
unlink("temp3.lst")

# empty environment
rm(list=ls())

# ------------------------------------------------------------------------------------------------------------------------------------
# ---------------------- load simulated data and preprocess it -----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------

# -----
# --- Read in the first data file [here we selected the first simulated particpant of the orange norm condition]
# -----

# save the data of the first simulated particiapant in df
df <- read.delim("51.dat", header = FALSE, sep="")
# per participant we have 3 trial conditions (47,5% orange, 50%, 52.5%); here we rename the 3 conditions to the corresponding orange values
df[,1] <- ifelse(df[,1] == "1", 47.5, ifelse(df[,1] == "2", 50, 52.5))
# create a column to indicate particpant number
C = c(rep(51,108))
# create a column to indicate norm condition (here orange)
N = c(rep("orange",108))
# create a column to indicate trial number, because order of the trial conditions were randomized we sample the order of the rows 
rows <- sample(nrow(df))
df <- df[rows, ]
T = c(1:108)
# add participant number, norm condition and trial number to df and name them 
df = cbind(C,N,T, df)
colnames(df) <- c("participant","norm","trial","stimulus","resp","rt")


# -----
# --- read in all participant of the orange norm condition and add them to df
# -----

for (sub in 52:100) {
  fn = sprintf("%d.dat", sub)
  data <- read.delim(fn, header = FALSE, sep="")
  data[,1] <- ifelse(data[,1] == "1", 47.5, ifelse(data[,1] == "2", 50, 52.5))
  C = c(rep(sub,108))
  N = c(rep("orange",108))
  rows <- sample(nrow(data))
  data <- data[rows, ]
  T = c(1:108)
  data = cbind(C,N,T, data)
  colnames(data) <- c("participant","norm","trial","stimulus","resp","rt")
  df=rbind(df,data)
}


# -----
# --- read in all participant of the blue norm condition and add them to df
# -----

for (sub in 1:50) {
  fn = sprintf("%d.dat", sub)
  data <- read.delim(fn, header = FALSE, sep="")
  data[,1] <- ifelse(data[,1] == "1", 47.5, ifelse(data[,1] == "2", 50, 52.5))
  C = c(rep(sub,108))
  N = c(rep("blue",108))
  rows <- sample(nrow(data))
  data <- data[rows, ]
  T = c(1:108)
  data = cbind(C,N,T, data)
  colnames(data) <- c("participant","norm","trial","stimulus","resp","rt")
  df=rbind(df,data)
}

# -----
# --- order df regarding participant number and save df as .csv file
# -----

df <- df[order(df$participant),] 


write.csv(df,"data_trial.csv", row.names = F)
data_trial <- read.csv("data_trial.csv")
