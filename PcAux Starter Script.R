
# Installing PcAux

# This is meant to be a starter guide to installing and running PcAux
# Please adjust the settings to fit your data
# More information on PcAux can be found at https://github.com/PcAux-Package/PcAux
# The current user guide can be found at https://github.com/PcAux-Package/PcAux/blob/master/documentation/PcAux_Field_Guide.pdf

#######################
# Installing Packages #
#######################

# In order to install PcAux, you need to first install "devtools"
install.packages("devtools")

# Next, you install the latest version of PcAux directly from github
devtools::install_github("PcAux-Package/PcAux/source/PcAux", ref = "master", force = TRUE)

# We will also use part of the Tidyverse package
install.packages("tidyverse")

# We will also use the miceadds package to get the data ready for Mplus
install.packages("miceadds")

###################
# Reading in Data #
###################

# Read in your data
dat <- read.csv(file.choose())

# If you have any data with Missing Data Codes, replace those values with "NA"
dat[][dat[]==-999]<-NA  # Recode "missing code" data to NA
# !!!WARNING!!! if your missing code is also an ID number, you'll have to reassign the ID number

#################
# Prepare PcAux #
#################

# List your variables in individual quotations - c("variable1","variable2")
myNoms   <- c()   # list all of your nominal variables here
myOrds   <- c()   # list all of your ordinal variables here
myIds    <- c()   # list your id variable here
myTrash  <- c()   # list your variables that you are dropping from imputation here
myMods   <- c()   # list any moderators you plan on using in your analysis model here

#####################
# Impute with PcAux #
#####################

######## STEP 1 ########
library(PcAux)

# The first stage of PcAux is prep data
pd <- prepData(rawData    = dat,    # Use what ever name you gave your data set.
               moderators = myMods,
               nomVars    = myNoms,
               ordVars    = myOrds,
               idVars     = myIds,
               dropVars   = myTrash)

# After this stage is complete, you'll want to assign the copy part of the object to another object
frozenPcAuxData1 <- pd$copy() 

# You can also save this copy to an .rds in case you need to call on it again without having to run prep data
saveRDS(frozenPcAuxData1, "frozenPcAuxData1.RDS")

# Loading the .rds from your working directory
pcAuxDat <- readRDS("frozenPcAuxData1.RDS")

# Loading from the global environment
pcAuxDat <- frozenPcAuxData1


######## STEP 1 EXAMPLE ########

## Load the data:
data(iris2)

#################
# Prepare PcAux #
#################

## Prepare the data:
myNoms   <- c("Species")   
myOrds   <- c("Petal.Width")   
myTrash  <- c("Junk")   

#####################
# Impute with PcAux #
#####################

pd <- prepData(rawData   = iris2,
               nomVars   = myNoms,
               ordVars   = myOrds,
               idVars    = "ID",
               dropVars  = myTrash,
               groupVars = "Species")

frozenPcAuxData1 <- pd$copy() 

saveRDS(frozenPcAuxData1, "frozenPcAuxData1.RDS")


######## STEP 2 ########

# The next part of PcAux is creating the principle components.
# First, PcAux will impute the missing data using just one iteration with mice 
# Then it will calculate the principle components
cpa<- createPcAux(pcAuxData = pcAuxDat,
                  nComps =c(Inf,Inf) ,
                  interactType = 2, # Refer to the PcAux manual regarding interaction types that would best fit your analysis
                  maxPolyPow = 1,
                  control= list(minPredCor=0.2))

# As before, we want to save the copy part of this object
frozen_cpa <- cpa$copy()

# Saving it as an .rds object into your working directory
saveRDS(frozen_cpa, "frozen_cpa.RDS")

# Loading it from the .rds. in your working directory
pcAuxDat <- readRDS("frozen_cpa.RDS")

# We can use rSquared plots to see how many principle components we want to use in our analysis
# Generally we look for where we get diminishing returns

# The first one shows us the linear principle components
plot(pcAuxDat$rSquared[[1]])
# The second one shows us the nonlinear principle components
plot(pcAuxDat$rSquared[[2]])

# We can get an exact number of components up to a certain rsquared with the following method
lvvec <- pcAuxDat$rSquared[[1]] # A vector of the rsquared for linear PCs
nlvvec <- pcAuxDat$rSquared[[2]] # A vector of the rsquared for nonlinear PCs

# Choose the thresholds based on your data and estimated need of variance explained
sum(lvvec < .6)  # This gives us the number of linear PCs needed to capture 60% of the variance
sum(nlvvec < .5) # This gives us the number of nonlinear PCs need to capture 50% of the variance

######## STEP 2 EXAMPLE ########

cpa<- createPcAux(pcAuxData = pd,
                  nComps =c(Inf,Inf) ,
                  interactType = 2, 
                  control= list("minPredCor"=0.2))

frozen_cpa <- cpa$copy()
saveRDS(frozen_cpa, "frozen_cpa.RDS")

plot(cpa$rSquared[[1]])
plot(cpa$rSquared[[2]])


lvvec <- cpa$rSquared[[1]] 
nlvvec <- cpa$rSquared[[2]] 

sum(lvvec < .9)  
sum(nlvvec < .6) 

######## STEP 3 ########

# Run miWithPcAuX to impute the data using the principle components.
midat <- miWithPcAux(rawData = dat,        # Use what ever name you gave your data set.
                     pcAuxData = cpa,
                     nComps = c(.6,.5),    # This is where you identify the number of PCs to use, lin fist, then nonlin.
                     # This template currently has 60% variance explained by lin, 50% explained by nonlin
                     # Adjust as suits your data
                     nImps = 100)          # This tells it to return us 100 imputed data sets

# Save the copy part of this object for reference
frozen_midat <- midat$copy()

# Save the copy object as a .rds for future reference so you don't have to rerun the analysis.
saveRDS(frozen_midat,"frozen_midat.rds")

######## STEP 3 EXAMPLE ########

midat <- miWithPcAux(rawData   = iris2,
                     pcAuxData = cpa,
                     nComps = c(2,4),
                     nImps     = 5)

frozen_midat <- midat$copy()
saveRDS(frozen_midat,"frozen_midat.rds")

##################################
# Export Imputation for Analysis #
##################################
library(tidyverse)
dat <- iris2
# Now we can create our imputed data files

# First, we can create a .csv that is the original unimputed data with the PC's attached
jointComponents <- purrr::reduce(midat$pcAux, left_join, by =  "ID") # Use whatever main ID variable is in your data
full_dat <- left_join(dat,jointComponents,by="ID")
write.csv(full_dat,"Raw_Data_with_PCs.csv", row.names = F, col.names = T)

# Next, we can write out all the individual data sets to .csv files used by Mplus
miceObject <- midat$miceObject

library(miceadds)
write.datlist(datlist = miceObject,name = "implist",Mplus = TRUE)

# Finally, we can create the grand mean dataset which averages all of the imputed datasets
# First, we create a stack of the 100 imputed data sets
miData <- midat$miDatasets
impstack <- miData[[1]]
for(i in 2:length(miData)) {
  impstack <- rbind(impstack, miData[[i]])
}

# Next, we load the grand mean function
devtools::source_url("https://raw.github.com/ppanko/grandMean/master/grandMeanFunction3.R")

# Then we identify our continuous variables
contVars <- setdiff(names(impstack), myTrash)
contVars <- setdiff(contVars, myNoms)
contVars <- setdiff(contVars, myOrds)
contVars <- setdiff(contVars, "ID") # Use your data's ID variable

# Finally, calculate the grand mean dataset
dat_gm <- grandMean(impstack,
                    idName = "ID", # Use your data's ID variable
                    discNames = unique(c(myTrash, myNoms, myOrds)),
                    contNames = unique(contVars))

# Save your grand mean dataset
write.csv(dat_gm, "GrandMean.csv", row.names = F)

##################################
# Parceling on Multiple Datasets #
##################################

# First, we take the output from PcAux and take out the list of imputed data sets
impList <- midat$miDatasets

# Next, we create vectors of the columns we want to parcel together
parcel_1 <- c("var1", "var2")

# Then we loop through the list of imputed data sets to make new columns 
for (i in 1:length(impList)) {
  impList[[i]]$parcel_1 <- rowMeans(impList[[i]][,parcel_1]) # change your new varable name for each parcel
}

# We repeat the above step for all of your parcels