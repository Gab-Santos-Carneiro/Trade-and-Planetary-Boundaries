### Rethinking trade for the ecological transition: Quantifying the trade drivers of planetary boundaries

# READ ME: The authors run the model using an online server. The data downloaded from the GLORIA website is also hosted in a cloud. 
# This is an adapted script and, unfortunately, we were not able to test it as our personal computers are not able to handle such amount of data
# The files of the GLORIA database can be downloaded from: https://ielab.info/labs/ielab-gloria  You need to register and then
# download the "GLORIA_ReadMe_xxx.xlsx" file and the "GLORIA_MRIOs_59_2021.zip" file which is located inside the folder "GLORIA_MRIO_Loop059_part_I_MRIOdatabase"
# inside the google drive.

# 0 - Loading data and basic matrices --------------------------------------------------------

# Cleaning memory
rm(list = ls())
year = 2021
gc()

# Setting timeout option to 10 minutes
options(timeout = 60*10)

# Setting the list of packages to be installed and attached
packages <- c('plyr','dplyr', 'ggplot2', 'data.table', 'ARTofR', 'readxl','reshape2','tidyverse', 'openxlsx')
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)
lapply(packages, require, character.only=TRUE)


# Creating labels

file_path <- file.path(getwd(), "GLORIA_ReadMe_057.xlsx") ## We indicate the GLORIA document 

# Read each sheet from the Excel file into separate data frames.
Regions    <- read.xlsx(file_path, sheet = "Regions")
Sectors    <- read.xlsx(file_path, sheet = "Sectors")
VA_FD      <- read.xlsx(file_path, sheet = "Value added and final demand")
Satellites <- read.xlsx(file_path, sheet = "Satellites")


label_IO <- matrix(rep(NA,(120*164)*3),ncol=3)
for (i in 1:164) {
  label_IO[((i-1)*120+1):(i*120),1] <- as.matrix(Regions)[i,2]
  label_IO[((i-1)*120+1):(i*120),2] <- as.matrix(Regions)[i,3]
  label_IO[((i-1)*120+1):(i*120),3] <- as.matrix(Sectors)[,2]
}

label_FD <- matrix(rep(NA,(6*164)*3),ncol=3)
for (i in 1:164) {
  label_FD[((i-1)*6+1):(i*6),1] <- as.matrix(Regions)[i,2]
  label_FD[((i-1)*6+1):(i*6),2] <- as.matrix(Regions)[i,3]
  label_FD[((i-1)*6+1):(i*6),3] <- as.matrix(VA_FD)[,3]
}

label_Q <- as.matrix(Satellites)[,2:4]     


# Loading data

set_wd <- getwd()

V <- read.csv(file.path(set_wd, paste0("20230310_120secMother_AllCountries_002_V-Results_", 
                                       year, "_057_Markup001(full).csv")),
              fileEncoding = "UTF-8")

Y <- read.csv(file.path(set_wd, paste0("20230315_120secMother_AllCountries_002_Y-Results_", 
                                       year, "_057_Markup001(full).csv")),
              fileEncoding = "UTF-8")

Q <- read.csv(file.path(set_wd, paste0("20230310_120secMother_AllCountries_002_TQ-Results_", 
                                       year, "_057_Markup001(full).csv")),
              fileEncoding = "UTF-8")

QY <- read.csv(file.path(set_wd, paste0("20230310_120secMother_AllCountries_002_YQ-Results_", 
                                        year, "_057_Markup001(full).csv")),
               fileEncoding = "UTF-8")

T <- read.csv(file.path(set_wd, paste0("20230315_120secMother_AllCountries_002_T-Results_", 
                                       year, "_057_Markup001(full).csv")),
              fileEncoding = "UTF-8")

# Creating matrices

# Creating output vector
x <- as.vector(rep(NA,(120*164)))
for (i in 1:164) {
  x[(i*120-119):(i*120)] <- apply(T[,(i*240-119):(i*240)],2,FUN=sum)
}

# Creating output vector
x2 <- as.vector(rep(NA,(120*164)))
for (i in 1:164) {
  x2[(i*120-119):(i*120)] <- apply(T[(i*240-239):(i*240-120),],1,FUN=sum)
}

#Testing if they are the same
sum((x-x2)^2) #[1] 0

# Creating the IO table 
IO <- matrix(rep(NA,(120*164)^2),ncol=(120*164))
T <- as.matrix(T)
for (i in 1:164) {
  for (j in 1:164) {
    IO[(i*120-119):(i*120),(j*120-119):(j*120)] <- T[(i*240-119):(i*240),((j-1)*240+1):(j*240-120)]
  }
}

# Creating VA table
VA <- matrix(rep(NA,(120*164)*6),ncol=(120*164))
V <- as.matrix(V)
for (i in 1:164) {
  VA[1:6,(i*120-119):(i*120)] <- V[((i-1)*6+1):(i*6),((i-1)*240+1):(i*240-120)]
}

# Creating FD table
FD <- matrix(rep(NA,(120*164)*(6*164)),ncol=(6*164))
Y <- as.matrix(Y)
for (i in 1:164) {
  for (j in 1:164) {
    FD[(i*120-119):(i*120),(j*6-5):(j*6)] <- Y[(i*240-119):(i*240),((j-1)*6+1):(j*6)]
  }
}

# Creadting Q table
QT <- matrix(rep(NA,(120*164)*nrow(Q)),ncol=(120*164))
Q <- as.matrix(Q)
for (i in 1:164) {
  QT[,(i*120-119):(i*120)] <- Q[,((i-1)*240+1):(i*240-120)]
}
QY <- as.matrix(QY)


# Domestic dummies
ID <- matrix(rep(0,(120*164)^2),ncol=(120*164))
for (i in 1:164) {
  ID[((i-1)*120+1):(i*120),((i-1)*120+1):(i*120)] <- 1
}

IF <- matrix(rep(0,(120*164)*(6*164)),ncol=(6*164))
for (i in 1:164) {
  IF[((i-1)*120+1):(i*120),((i-1)*6+1):(i*6)] <- 1
}

# Coefficient matrix and Leontief
A <- t(t(IO)/(xin+10^(-6)))
L <- solve(diag(ncol(A))-A)


# 1 - Calculating direct footprints ---------------------------------------



# Creating footprint intensity, direct
Q2 <- as.matrix(Q[2864,])/(x+0.00001) # GHG Emissions (EDGAR)
Q2 <- cbind(Q2,apply(Q[68:73,],2,FUN=sum)/(x+0.00001)) # Land use
Q2 <- cbind(Q2,apply(Q[80:85,],2,FUN=sum)/(x+0.00001)) # Biodiversity loss
Q2 <- cbind(Q2,apply(Q[86:87,],2,FUN=sum)/(x+0.00001)) # Water stress
Q2 <- cbind(Q2,apply(Q[88:89,],2,FUN=sum)/(x+0.00001)) # Blue water consumption
Q2 <- cbind(Q2,apply(Q[1:62,],2,FUN=sum)/(x+0.00001)) # Material use, total
Q2 <- cbind(Q2,apply(Q[74:79,],2,FUN=sum)/(x+0.00001)) # Energy, total

colnames(Q2)[1:2] <- c("GHG_Emissions","Land_use")
colnames(Q2)[3:7] <- as.matrix(label_Q[c(80,86,88,1,74),1])

# Use of fertilizer minerals in agriculture
Fertilizers <- as.vector(Q[43,]/(x+0.00001))*L # Embodied fertilzers matrix
Fertilizers <- as.vector(apply(Fertilizers,2,FUN=sum))

for (j in 1:164) {
  Fertilizers[(22+(j-1)*120):(120+(j-1)*120)] <- 0 } # Keeping only agriculture

Q2 <- cbind(Q2,Fertilizers) # Fertilizers

# Chemical use of non-energy material
L_nonen <- L
for (j in 1:164) {
  L_nonen[(93+(j-1)*120),] <- 0 } # Excluding material embodied in energy

Chemicals <- apply(Q2[6]*L_nonen,2,FUN=sum) # Embodied non-energy materials matrix
for (j in 1:164) {
  Chemicals[(1+(j-1)*120):(63+(j-1)*120)] <- 0
  Chemicals[(73+(j-1)*120):(120+(j-1)*120)] <- 0 } # Keeping only chemicals

Q2 <- cbind(Q2,Chemicals) # Chemicals (all materials, excluding embodied in energy)


# 2 - Calculating the footprints embodied in trade ------------------------

Footprints_inTrade <- matrix(NA,ncol(Q2),ncol=4)
for (k in 1:ncol(Q2)) {
  
  # Embodied footprints
  e <- as.matrix(Q2[,k]) # direct emissions per output
  eL <- t(L) %*% e # total emissions per output
  
  E_bysec <- cbind(as.data.frame(label_IO),e,eL) # Organising it
  
  df <- join(E_bysec,ddply(E_bysec, .(V3), summarize, median=median(eL)))
  df %>%  filter(median>quantile(median,0.7,na.rm = T)) %>% 
    ggplot(aes(x=eL*10^3,y=reorder(V3,median)))+
    geom_boxplot(notch=F,outlier.shape=NA,fill='#FF8080', color="black")+
    labs(x=colnames(Q2)[k],y="")+ 
    theme_bw()+coord_cartesian(xlim=c(0, max(df$median*3*10^3)))+scale_x_sqrt()+
    stat_summary(fun=median, geom="point", shape=18, size=3, fill="#63416A", color="#63416A")
  
  # Emissions by country
  eLY <- t(t(eL) %*% FD) # Emissions embodied in final demand (consumption-based emissions)
  
  aux <- cbind(as.data.frame(label_IO),e*x)
  colnames(aux)[4] <- "E"
  E_byctr <- ddply(aux, .(V1), summarize, Prod=sum(E))
  
  aux <- cbind(as.data.frame(label_FD),eLY)
  aux <- ddply(aux, .(V1), summarize, Cons=sum(eLY))
  E_byctr <- join(E_byctr,aux,by="V1",match="first") # Footprints by country, consumption and productio-based
  
  # Footprints embodied in trade (by consumer country)
  eLM <- t(L*(1-ID)) %*% e # total emissions per output, excluding domestic
  eLMYM <- t(eLM) %*% (FD*(1-IF)) # Footprints of j embodied in imported FD of j 
  eLMY <- t(eLM) %*% FD - eLMYM # Footprints of j embodied in imported FD of others
  eLYM <- t(eL) %*% (FD*(1-IF)) - eLMYM # Footprints of j embodied in domestic FD of others (inputs)
  
  sum(eLMY+eLYM+eLMYM)/sum(eLY) # Total trade related footprints
  sum(eLMY)/sum(eLY) # Embodied in imported final demand, excluding domestic footprints
  sum(eLYM)/sum(eLY) # Embodied in domestic final demand (embodied in inputs)
  
  # Footprints embodied in trade (by origin and destination)
  eLY_ <- diag(as.vector(e)) %*% L %*% FD
  eLMYM_ <- diag(as.vector(e)) %*% (L*(1-ID)) %*% (FD*(1-IF)) # Footprints of j embodied in imported FD of j 
  eLMY_ <- diag(as.vector(e)) %*% (L*(1-ID)) %*% FD - eLMYM_ # Footprints of j embodied in imported FD of others
  eLYM_ <- diag(as.vector(e)) %*% L %*% (FD*(1-IF)) - eLMYM_ # Footprints of j embodied in domestic FD of others (inputs)
  
  sum(eLMY_+eLYM_+eLMYM_)/sum(eLY_) # Total trade related footprints
  sum(eLMY_)/sum(eLY_) # Embodied in imported final demand, excluding domestic footprints
  sum(eLYM_)/sum(eLY_) # Embodied in domestic final demand (embodied in inputs)
  Footprints_inTrade[k,] <- c(sum(eLMY_+eLYM_+eLMYM_)/sum(eLY_)*100,sum(eLMY_)/sum(eLY_)*100,sum(eLYM_)/sum(eLY_)*100,sum(eLMYM_)/sum(eLY_)*100)
  
  # Origin-destination of the footprint
  byctrFD <- matrix(0,6*164,ncol=164)
  for (i in 0:163) {
    byctrFD[(i*6+1):((i+1)*6),i+1] <- 1
  }
  
  byctr <- matrix(0,120*164,ncol=164)
  for (i in 0:163) {
    byctr[(i*120+1):((i+1)*120),i+1] <- 1
  }
  
  eTr <- as.data.frame(t(byctr) %*% (eLMY_+eLYM_) %*% byctrFD)
  colnames(eTr) <- unique(label_IO$V1)
  rownames(eTr) <- unique(label_IO$V1)
  
  
  # Sector of origin-destination of the footprint
  bysec <- matrix(0,120*164,ncol=120)
  for (i in 0:163) {
    bysec[(i*120+1):((i+1)*120),1:120] <- diag(120)
  }
  
  eTr_sec <- as.data.frame(t(bysec) %*% (eLMY_+eLYM_) %*% byctrFD)
  colnames(eTr_sec) <- unique(label_IO$V1)
  
  # Saving
  saveRDS(eTr, file = file.path(getwd(), paste0(colnames(Q2)[k], "_inTrade_byctr.rds")))
  saveRDS(eTr_sec, file = file.path(getwd(), paste0(colnames(Q2)[k], "_inTrade_bysec.rds")))
}
rownames(Footprints_inTrade) <- colnames((Q2))
colnames(Footprints_inTrade) <- c("Total in trade","Embodied in imported FD, excl. domestic","Embodied in domestic FD","Embodied in imported FD, domestic")
saveRDS(Footprints_inTrade, file = file.path(getwd(), "Footprints_inTrade.rds"))

# By sector
y <- apply(FD,1,FUN=sum)
yM <- apply((FD*(1-IF)),1,FUN=sum)

bysec <- matrix(0,120*164,ncol=120)
for (i in 0:163) {
  bysec[(i*120+1):((i+1)*120),1:120] <- diag(120)
}

for (k in 1:ncol(Q2)) {
  
  # Embodied footprints
  e <- as.matrix(Q2[,k]) # direct emissions per output
  
  # Footprints embodied in trade (by origin and destination)
  eL <- diag(as.vector(e)) %*% L # total emissions per output
  eLM <- diag(as.vector(e)) %*% (L*(1-ID)) 
  
  eLy_ <- eL %*% diag(y)
  eLMyM_ <- eLM %*% diag(yM) # Footprints of j embodied in imported FD of j 
  eLMy_ <- eLM %*% diag(y) - eLMyM_ # Footprints of j embodied in imported FD of others
  eLyM_ <- eL %*% diag(yM) - eLMyM_ # Footprints of j embodied in domestic FD of others (inputs)
  
  # Origin-destination of the footprint
  eTr <- as.data.frame(t(bysec) %*% (eLMy_+eLyM_) %*% bysec)
  rownames(eTr) <- unique(label_IO$V3)
  
  # Saving
  saveRDS(eTr, file = file.path(getwd(), paste0(colnames(Q2)[k], "_inTrade_secbysec.rds")))
}


# 3 - Analysis and preparing data for visualization -----------------------

# We employed the Flourish online tool in order to create the charts in the paper. The tool is free and can be accessed in https://flourish.studio
# Here we show the steps to prepare the data to be plugged in Flourish

# 3.1 - Figure 1: Share of Pressure ---------------------------------------

## Figure 1: Share of pressure by intercountry traded goods
View(Footprints_inTrade)

# 3.2 - Figure 2: Income groups -------------------------------------------


## Figure 2: Trade pressure on the planetary boundaries exerted by different income groups of countries

# Pre-defined indices dividing high, middle and low-income countries: 

high_indices <- unique(c(8,11,12,15,20,21,27,31,32,33,42,43,44,47,
                         53,54,56,57,59,65,68,69,71,74,77,78,79,82,
                         87,88,94,95,96,103,114,115,117,118,121,125,
                         127,129,130,133,136,140,142,144,156,157))
middle_indices <- unique(c(7,9,10,13,19,22,23,24,26,29,34,35,39,40,
                           41,48,50,58,60,64,66,67,72,73,76,80,83,92,
                           98,100,101,109,110,122,128,131,138,148,150,
                           152,162))
low_indices <- unique(c(1,2,3,4,5,6,14,16,17,18,25,28,30,36,37,38,
                        45,46,49,51,52,55,61,62,63,70,75,81,84,85,
                        86,89,90,91,93,97,99,102,104,105,106,107,108,
                        111,112,113,116,119,120,123,124,126,132,134,
                        135,137,139,141,145,146,147,149,151,153,154,
                        155,158,159,160,161,163,164))


### Biogeochemical flows (Fertilizers)
options(scipen = 999)
Fert <- as.matrix(apply(Fertilizers_inTrade_byctr, 2, sum))
Fert2 <- as.matrix(apply(Fertilizers_inTrade_byctr, 1, sum))
Fert <- as.data.frame(cbind(Fert, Fert2))

Fert$ImpShare <- Fert[,1] / sum(Fert[,1]) * 100
Fert$ExpShare <- Fert[,2] / sum(Fert[,2]) * 100
Fert$indice   <- 1:nrow(Fert)

# Compute sums by income group using pre-defined indices:
sum_high_Fert   <- sum(Fert[high_indices, 3])
sum_middle_Fert <- sum(Fert[middle_indices, 3])
sum_low_Fert    <- sum(Fert[low_indices, 3])

cat("Biogeochemical flows - Fertilizers\n")
cat("  High Income:   ", sum_high_Fert, "\n")
cat("  Middle Income: ", sum_middle_Fert, "\n")
cat("  Low Income:    ", sum_low_Fert, "\n\n")


### Change in biosphere integrity (Biodiversity loss)
Bio <- as.matrix(apply(Biodiversity_loss_inTrade_byctr, 2, sum))
Bio2 <- as.matrix(apply(Biodiversity_loss_inTrade_byctr, 1, sum))
Bio <- as.data.frame(cbind(Bio, Bio2))

Bio$ImpShare <- Bio[,1] / sum(Bio[,1]) * 100
Bio$ExpShare <- Bio[,2] / sum(Bio[,2]) * 100

sum_high_Bio   <- sum(Bio[high_indices, 3])
sum_middle_Bio <- sum(Bio[middle_indices, 3])
sum_low_Bio    <- sum(Bio[low_indices, 3])

cat("Biosphere Integrity - Biodiversity Loss\n")
cat("  High Income:   ", sum_high_Bio, "\n")
cat("  Middle Income: ", sum_middle_Bio, "\n")
cat("  Low Income:    ", sum_low_Bio, "\n\n")


### Land system change
Land <- as.matrix(apply(Land_use_inTrade_byctr, 2, sum))
Land2 <- as.matrix(apply(Land_use_inTrade_byctr, 1, sum))
Land <- as.data.frame(cbind(Land, Land2))

Land$ImpShare <- Land[,1] / sum(Land[,1]) * 100
Land$ExpShare <- Land[,2] / sum(Land[,2]) * 100

sum_high_Land   <- sum(Land[high_indices, 3])
sum_middle_Land <- sum(Land[middle_indices, 3])
sum_low_Land    <- sum(Land[low_indices, 3])

cat("Land System Change\n")
cat("  High Income:   ", sum_high_Land, "\n")
cat("  Middle Income: ", sum_middle_Land, "\n")
cat("  Low Income:    ", sum_low_Land, "\n\n")


### Blue water consumption
Blue <- as.matrix(apply(Blue_water_consumption_inTrade_byctr, 2, sum))
Blue2 <- as.matrix(apply(Blue_water_consumption_inTrade_byctr, 1, sum))
Blue <- as.data.frame(cbind(Blue, Blue2))

Blue$ImpShare <- Blue[,1] / sum(Blue[,1]) * 100
Blue$ExpShare <- Blue[,2] / sum(Blue[,2]) * 100

sum_high_Blue   <- sum(Blue[high_indices, 3])
sum_middle_Blue <- sum(Blue[middle_indices, 3])
sum_low_Blue    <- sum(Blue[low_indices, 3])

cat("Blue Water Consumption\n")
cat("  High Income:   ", sum_high_Blue, "\n")
cat("  Middle Income: ", sum_middle_Blue, "\n")
cat("  Low Income:    ", sum_low_Blue, "\n\n")


### Water stress
Stress <- as.matrix(apply(Water_stress_inTrade_byctr, 2, sum))
Stress2 <- as.matrix(apply(Water_stress_inTrade_byctr, 1, sum))
Stress <- as.data.frame(cbind(Stress, Stress2))

Stress$ImpShare <- Stress[,1] / sum(Stress[,1]) * 100
Stress$ExpShare <- Stress[,2] / sum(Stress[,2]) * 100

sum_high_Stress   <- sum(Stress[high_indices, 3])
sum_middle_Stress <- sum(Stress[middle_indices, 3])
sum_low_Stress    <- sum(Stress[low_indices, 3])

cat("Water Stress\n")
cat("  High Income:   ", sum_high_Stress, "\n")
cat("  Middle Income: ", sum_middle_Stress, "\n")
cat("  Low Income:    ", sum_low_Stress, "\n\n")


### Climate Change (GHG Emissions)
GHG <- as.matrix(apply(GHG_Emissions_inTrade_byctr, 2, sum))
GHG2 <- as.matrix(apply(GHG_Emissions_inTrade_byctr, 1, sum))
GHG <- as.data.frame(cbind(GHG, GHG2))

GHG$ImpShare <- GHG[,1] / sum(GHG[,1]) * 100
GHG$ExpShare <- GHG[,2] / sum(GHG[,2]) * 100

sum_high_GHG   <- sum(GHG[high_indices, 3])
sum_middle_GHG <- sum(GHG[middle_indices, 3])
sum_low_GHG    <- sum(GHG[low_indices, 3])

cat("Climate Change (GHG Emissions)\n")
cat("  High Income:   ", sum_high_GHG, "\n")
cat("  Middle Income: ", sum_middle_GHG, "\n")
cat("  Low Income:    ", sum_low_GHG, "\n\n")


### Novel entities
Novel <- as.matrix(apply(Chemicals_inTrade_byctr, 2, sum))
Novel2 <- as.matrix(apply(Chemicals_inTrade_byctr, 1, sum))
Novel <- as.data.frame(cbind(Novel, Novel2))

Novel$ImpShare <- Novel[,1] / sum(Novel[,1]) * 100
Novel$ExpShare <- Novel[,2] / sum(Novel[,2]) * 100

sum_high_Novel   <- sum(Novel[high_indices, 3])
sum_middle_Novel <- sum(Novel[middle_indices, 3])
sum_low_Novel    <- sum(Novel[low_indices, 3])

cat("Novel Entities\n")
cat("  High Income:   ", sum_high_Novel, "\n")
cat("  Middle Income: ", sum_middle_Novel, "\n")
cat("  Low Income:    ", sum_low_Novel, "\n")


# 3.3 - Figure 3: Sankeys -------------------------------------------------------

## Figure 3: Sankey diagram of global trade's pressure over the planetary boundaries 
# Sankeys were completely generated in the Flourish tool. Here we just show which data we used as input in Flourish

# Indices in order to separate the countries according to World Bank's regions:

# East Asia and Pacific
HighEAP <- c(11,27,42,68,82,87,117,136)
MidEAP <- c(34,72,109,148)
LowEAP <- c(4,86,89,104,105,123,124,126,160)

# Europe and Central Asia
HighECA <- c(12,15,32,42,43,44,47,53,54,56,57,59,65,69,71,74,77,79,94,95,96,103,114,115,125,127,130,140,142,143,144)
MidECA <- c(2,7,10,13,19,22,23,60,83,98,101,131,150,152)
LowECA <- c(85,149,155,158)

# Latin America and the Caribbean
HighLAC <- c(21,33,121,156)
MidLAC <- c(1,9,24,26,39,40,41,48,50,66,67,80,100,122,128,138)
LowLAC <- c(25,113,159)

# Middle East and North Africa
HighMENA <- c(8,20,78,88,118,128,133)
MidMENA <- c(76,92)
LowMENA <- c(46,49,51,70,75,81,90,97,120,145,151,161)

# North America
HighNA <- c(31,157)

# South Asia
MidSA <- c(73)
LowSA <- c(5,18,28,93,116,119)

# Sub-Saharan Africa
MidSSA <- c(29,35,58,64,110,162)
LowSSA <- c(3,6,14,16,17,30,36,37,38,45,52,55,61,62,63,84,91,99,102,106,107,108,111,112,132,134,135,137,139,141,146,147,153,154,163,164)

# Organize the regions into a list
regions <- list(
  "EAP"  = list(high = HighEAP,  midlow = c(MidEAP, LowEAP)),
  "ECA"  = list(high = HighECA,  midlow = c(MidECA, LowECA)),
  "LAC"  = list(high = HighLAC,  midlow = c(MidLAC, LowLAC)),
  "MENA" = list(high = HighMENA, midlow = c(MidMENA, LowMENA)),
  "NA"   = list(high = HighNA),
  "SA"   = list(midlow = c(MidSA, LowSA)),
  "SSA"  = list(midlow = c(MidSSA, LowSSA))
)

# Define the block (column‐group) definitions
blocks <- list(
  "High EAP"           = HighEAP,
  "Mid+Low EAP"        = c(MidEAP, LowEAP),
  "High ECA"           = HighECA,
  "Mid+Low ECA"        = c(LowECA, MidECA),
  "High LAC"           = HighLAC,
  "Mid+Low LAC"        = c(LowLAC, MidLAC),
  "High MENA"          = HighMENA,
  "Mid+Low MENA"       = c(MidMENA, LowMENA),
  "North America"      = HighNA,
  "Mid+Low SA"         = c(MidSA, LowSA),
  "Mid+Low SSA"        = c(MidSSA, LowSSA)
)

# We use a modified helper function to compute sums with source/destination
computeSums <- function(col_group, regions, M, destName) {
  # M is the indicator matrix (e.g. Fertilizers_inTrade_byctr)
  # destName is the destination label (i.e. the block name)
  out <- data.frame(source = character(),
                    destination = character(),
                    value = numeric(),
                    stringsAsFactors = FALSE)
  
  # Loop over regions in a fixed order:
  for(reg in c("EAP", "ECA", "LAC", "MENA", "NA", "SA", "SSA")) {
    if (!is.null(regions[[reg]]$high)) {
      sumVal <- sum(M[regions[[reg]]$high, col_group])
      out <- rbind(out,
                   data.frame(source = paste(reg, "(high)"),
                              destination = destName,
                              value = sumVal,
                              stringsAsFactors = FALSE))
    }
    if (!is.null(regions[[reg]]$midlow)) {
      sumVal <- sum(M[regions[[reg]]$midlow, col_group])
      out <- rbind(out,
                   data.frame(source = paste(reg, "(mid+low)"),
                              destination = destName,
                              value = sumVal,
                              stringsAsFactors = FALSE))
    }
  }
  return(out)
}

# We use a modified wrapper function to process an indicator
processIndicator <- function(M, indicatorName) {
  cat("\n=== Indicator:", indicatorName, "===\n")
  outList <- list()
  for(blockName in names(blocks)) {
    col_grp <- blocks[[blockName]]
    df <- computeSums(col_grp, regions, M, blockName)
    cat("Block:", blockName, "\n")
    print(df)
    outList[[blockName]] <- df
  }
  # Combine results for all blocks into one data frame:
  result <- do.call(rbind, outList)
  return(result)
}

# We repeat the task for the other boundaries 
# Replace the matrix names below with your actual matrices
res_Fert <- processIndicator(Fertilizers_inTrade_byctr, "Fertilizers in Trade")
res_GHG  <- processIndicator(GHG_Emissions_inTrade_byctr, "GHG Emissions in Trade")
res_Land <- processIndicator(Land_use_inTrade_byctr, "Land Use in Trade")
res_Blue <- processIndicator(Blue_water_consumption_inTrade_byctr, "Blue Water Consumption in Trade")
res_Stress <- processIndicator(Water_stress_inTrade_byctr, "Water Stress in Trade")





# 3.4 - Figure 4: Radar Charts --------------------------------------------------

# Define the region indices
# (For LAC, we combine High, Mid, and Low; for SSA, SA, etc., we combine Mid and Low only)
LAC    <- c(21,33,121,156, 1,9,24,26,39,40,41,48,50,66,67,80,100,122,128,138, 25,113,159)
SSA    <- c(MidSSA, LowSSA)  # previously defined MidSSA and LowSSA
SA     <- c(MidSA, LowSA)    # previously defined MidSA and LowSA
MENA_H <- HighMENA           # previously defined HighMENA
MENA_M <- c(MidMENA, LowMENA) # previously defined MidMENA and LowMENA
EAP_H  <- HighEAP            # previously defined HighEAP
EAP_M  <- c(MidEAP, LowEAP)   # previously defined MidEAP and LowEAP
ECA_H  <- HighECA            # previously defined HighECA
ECA_M  <- c(MidECA, LowECA)   # previously defined MidECA and LowECA
NA_reg <- HighNA             # previously defined HighNA

regions_radar <- list(
  LAC         = LAC,
  SSA         = SSA,
  SA          = SA,
  NorthAmerica = NA_reg,
  MENA_H      = MENA_H,
  MENA_M      = MENA_M,
  EAP_H       = EAP_H,
  EAP_M       = EAP_M,
  ECA_H       = ECA_H,
  ECA_M       = ECA_M
)

# Define the indicator matrices
indicators <- list(
  "Fertilizers"   = Fertilizers_inTrade_byctr,
  "Biodiversity"  = Biodiversity_loss_inTrade_byctr,
  "Land Use"      = Land_use_inTrade_byctr,
  "Blue Water"    = Blue_water_consumption_inTrade_byctr,
  "Water Stress"  = Water_stress_inTrade_byctr,
  "GHG"           = GHG_Emissions_inTrade_byctr,
  "Chemicals"     = Chemicals_inTrade_byctr
)

# 3. Loop over each region and indicator to compute shares
# For each indicator, we compute the percentage share of the global total 
# coming from the rows (Exports) and from the columns (Imports)

# Create an empty list to hold the results for each region:
radar_results <- list()

for (regName in names(regions_radar)) {
  reg_idx <- regions_radar[[regName]]
  # Create a data frame to hold one radar chart’s inputs:
  df <- data.frame(
    Indicator = character(),
    Exports   = numeric(),  # row share
    Imports   = numeric(),  # column share
    stringsAsFactors = FALSE
  )
  
  for (indName in names(indicators)) {
    M <- indicators[[indName]]
    globalSum <- sum(M)
    # Compute the share of the total coming from the selected rows (Exports)
    # and from the selected columns (Imports):
    exportShare <- sum(M[reg_idx, ]) / globalSum * 100
    importShare <- sum(M[, reg_idx]) / globalSum * 100
    df <- rbind(df, data.frame(Indicator = indName,
                               Exports   = exportShare,
                               Imports   = importShare,
                               stringsAsFactors = FALSE))
  }
  radar_results[[regName]] <- df
}

# 4. Print the results for each region
for (regName in names(radar_results)) {
  cat("\n=== Region:", regName, "===\n")
  print(radar_results[[regName]])
}


# Annex Calculations ------------------------------------------------------

# Reorganize data

Fert <- as.data.frame(apply(Fertilizers_inTrade_bysec, 1,sum))
Fert$Share <- Fert[,1]/sum(Fert[,1])*100
Fert2 <- as.data.frame(apply(Fertilizers_inTrade_secbysec, 2,sum))
Fert2$Share <- Fert2[,1]/sum(Fert2[,1])*100

Bio <- as.data.frame(apply(Biodiversity_loss_inTrade_bysec, 1,sum))
Bio$Share <- Bio[,1]/sum(Bio[,1])*100
Bio2 <- as.data.frame(apply(Biodiversity_loss_inTrade_secbysec, 2,sum))
Bio2$Share <- Bio2[,1]/sum(Bio2[,1])*100

Land <- as.data.frame(apply(Land_use_inTrade_bysec, 1,sum))
Land$Share <- Land[,1]/sum(Land[,1])*100
Land2 <- as.data.frame(apply(Land_use_inTrade_secbysec, 2,sum))
Land2$Share <- Land2[,1]/sum(Land2[,1])*100

Blue <- as.data.frame(apply(Blue_water_consumption_inTrade_bysec, 1,sum))
Blue$Share <- Blue[,1]/sum(Blue[,1])*100
Blue2 <- as.data.frame(apply(Blue_water_consumption_inTrade_secbysec, 2,sum))
Blue2$Share <- Blue2[,1]/sum(Blue2[,1])*100

Stress <- as.data.frame(apply(Water_stress_inTrade_bysec, 1,sum))
Stress$Share <- Stress[,1]/sum(Stress[,1])*100
Stress2 <- as.data.frame(apply(Water_stress_inTrade_secbysec, 2,sum))
Stress2$Share <- Stress2[,1]/sum(Stress2[,1])*100

GHG <- as.data.frame(apply(GHG_Emissions_inTrade_bysec, 1,sum))
GHG$Share <- GHG[,1]/sum(GHG[,1])*100
GHG2 <- as.data.frame(apply(GHG_Emissions_inTrade_secbysec, 2,sum))
GHG2$Share <- GHG2[,1]/sum(GHG2[,1])*100

Chem <- as.data.frame(apply(Chemicals_inTrade_bysec, 1,sum))
Chem$Share <- Chem[,1]/sum(Chem[,1])*100
Chem2 <- as.data.frame(apply(Chemicals_inTrade_secbysec, 2,sum))
Chem2$Share <- Chem2[,1]/sum(Chem2[,1])*100

options(scipen=999)

install.packages("factoextra")
install.packages("GGally")
library(factoextra)
library(GGally)

## Production Perspective

Clusterdata <- cbind(Fert$Share,Bio$Share,Land$Share,Blue$Share,Stress$Share,GHG$Share,Chem$Share)
Labels <- as.data.frame(label_IO[c(1:120),3])
rownames(Clusterdata) <- as.character(Labels[,1])
colnames(Clusterdata) <- c("Biogeochemical","Biosphere","Land system","Blue water", "Water stress","Climate change", "Novel entities")

distance <- dist(Clusterdata, method = "euclidean") #Calculating distance between observations
distance <- distance^2 #squaring distances

cluster <- hclust(distance, method = "ward.D") #Change for single, average and Ward.D2
cbind(cluster$merge,cluster$height) #merging results of the clusters
plot(cluster) #plot of raw dendogram

n=nrow(Clusterdata) #code for creating the elbow and decide on the number of clusters
g=seq(1,n-1,1)
g=sort(g, decreasing=TRUE)
plot(cluster$height,g,type="l", ylim = c(1, 30))
axis(2, at = seq(0, 100, by = 1))

Result <- cutree(cluster,6) #Choose the number of clusters. Single = 3, Average = 7, Ward.D = 6, Ward.D2 = 6
fviz_cluster(list(data = Clusterdata, cluster = Result)) # Plot cluster

# PCA analysis underlining the cluster
pca_result <- prcomp(Clusterdata, scale = TRUE)
summary(pca_result)
pca_result$rotation
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)

# Code for biplot
biplot(pca_result, choices = 1:2, xlim = c(-7, 7), ylim = c(-7, 7), scale = 0,
       col = c("transparent", "black"), cex = c(0, 1))

# Correlation Analysis
ggcorr(Clusterdata, method = c("everything", "spearman"), label = T, label_round = 3)
ggcorr(Clusterdata, method = c("everything", "kendall"), label = T, label_round = 3)


## Consumption Perspective

Clusterdata2 <- cbind(Fert2$Share,Bio2$Share,Land2$Share,Blue2$Share,Stress2$Share,GHG2$Share,Chem2$Share)
Labels <- as.data.frame(label_IO[c(1:120),3])
rownames(Clusterdata2) <- as.character(Labels[,1])
colnames(Clusterdata2) <- c("Biogeochemical","Biosphere","Land system","Blue water", "Water stress","Climate change", "Novel entities")

distance <- dist(Clusterdata2, method = "euclidean") #Calculating distance between observations
distance <- distance^2 #squaring distances

cluster <- hclust(distance, method = "ward.D2") #Change for single, average and Ward.D
cbind(cluster$merge,cluster$height) #merging results of the clusters
plot(cluster) #plot of raw dendogram

n=nrow(Clusterdata2) #code for creating the elbow and decide on the number of clusters
g=seq(1,n-1,1)
g=sort(g, decreasing=TRUE)
plot(cluster$height,g,type="l", ylim = c(1, 30))
axis(2, at = seq(0, 100, by = 1))

Result <- cutree(cluster,3) #Choose the number of clusters. Single = 5, Average = 8, Ward.D = 6, Ward.D2 = 3
fviz_cluster(list(data = Clusterdata2, cluster = Result)) # Plot nice cluster

# PCA analysis underlining the cluster
pca_result <- prcomp(Clusterdata2, scale = TRUE)
summary(pca_result)
pca_result$rotation
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)

# Assume pca_result is the result of your PCA analysis
biplot(pca_result, choices = 1:2, xlim = c(-7, 7), ylim = c(-7, 7), scale = 0,
       col = c("transparent", "black"), cex = c(0, 1))


# Correlation Analysis
ggcorr(Clusterdata2, method = c("everything", "spearman"), label = T, label_round = 3)
ggcorr(Clusterdata2, method = c("everything", "kendall"), label = T,label_round = 3)

