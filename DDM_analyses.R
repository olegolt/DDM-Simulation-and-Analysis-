# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------ Script for DDM analyses ------------------------------------------------------ 
# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# In this script we will analyse the data of each participant using a Drift-Diffusion model and prepare data for further analysis

# -------------------------------------------------------------------------------------------------------------------------------------
# ---------- Analysing data using the drift diffusion model ---------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# For the DDM analyses we first have to specify our DDM in a ".CTL" file
# Here, just click on the "experiment.file" in RStudio to open it. Then modify the settings according to your analysis plan

# Run the DDM analyses 
system("fast-dm.exe")

data_DDM <- read.table("data_DDM.log", header = TRUE)

# In the simulated data the information which condition the participant was, is missing.
# We therefore order the data first and assign the condition (participant 1:50 = blue, 51:100 = orange)
data_DDM <- data_DDM[order(data_DDM$dataset),] 
condition <- c(rep("blue",50),rep("orange",50))
data_DDM <- cbind(data_DDM,condition)

# save this file as .csv
write.csv(data_DDM,"data_DDM.csv", row.names = F)



