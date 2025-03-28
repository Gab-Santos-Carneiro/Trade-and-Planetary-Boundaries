### Rethinking trade for the ecological transition: Quantifying the trade drivers of planetary boundaries - CODE 1

# READ ME: This is the original code employed by the authors using Onyxia and Amazon cloud servers. 
# The files of the GLORIA database can be downloaded from: https://ielab.info/labs/ielab-gloria  You need to register and then
# download the "GLORIA_ReadMe_xxx.xlsx" file and the "GLORIA_MRIOs_59_2021.zip" file which is located inside the folder "GLORIA_MRIO_Loop059_part_I_MRIOdatabase"
# inside the google drive. This is the first code, use it to generate and save the matrices needed for the Input-Output analysis

# Cleaning memory
rm(list = ls())
gc()

# Setting timeout option to 10 minutes
options(timeout = 60*10)

# Setting the list of packages to be installed and attached
packages <- c('dplyr', 'ggplot2', 'data.table', 'aws.s3', 'ARTofR', 'readxl')
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)
lapply(packages, require, character.only=TRUE)

# Set Sys.setenv

bucketlist(region="") # Check if it's alright

########################
### 0) Loading files ###
########################

### Organizing inital data
rm(list=ls())
year = 2021
bucket <- "projet-esteem"
set_wd <- "Gloria/data/057"

# Loading matrices 
V <- s3read_using(FUN = data.table::fread,encoding = "UTF-8",
                  object = paste(set_wd,"/20230320_120secMother_AllCountries_002_V-Results_",year,"_057_Markup001(full).csv",sep=""),
                  bucket = bucket, opts = list("region" = ""))

Y <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                  object = paste(set_wd,"/20230320_120secMother_AllCountries_002_Y-Results_",year,"_057_Markup001(full).csv",sep=""),
                  bucket = bucket, opts = list("region" = ""))

Q <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                  object = paste(set_wd,"/20230310_120secMother_AllCountries_002_TQ-Results_",year,"_057_Markup001(full).csv",sep=""),
                  bucket = bucket, opts = list("region" = ""))

QY <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                   object = paste(set_wd,"/20230310_120secMother_AllCountries_002_YQ-Results_",year,"_057_Markup001(full).csv",sep=""),
                   bucket = bucket, opts = list("region" = ""))

T <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                  object = paste(set_wd,"/20230320_120secMother_AllCountries_002_T-Results_",year,"_057_Markup001(full).csv",sep=""),
                  bucket = bucket, opts = list("region" = ""))

# Loading labels
Regions <- s3read_using(FUN = read_excel, sheet = "Regions",
                        object = paste(set_wd,"/GLORIA_ReadMe_057.xlsx",sep=""),
                        bucket = bucket, opts = list("region" = ""))

Sectors <- s3read_using(FUN = read_excel, sheet = "Sectors",
                        object = paste(set_wd,"/GLORIA_ReadMe_057.xlsx",sep=""),
                        bucket = bucket, opts = list("region" = ""))

VA_FD <- s3read_using(FUN = read_excel, sheet = "Value added and final demand",
                      object = paste(set_wd,"/GLORIA_ReadMe_057.xlsx",sep=""),
                      bucket = bucket, opts = list("region" = ""))

Satellites <- s3read_using(FUN = read_excel, sheet = "Satellites",
                           object = paste(set_wd,"/GLORIA_ReadMe_057.xlsx",sep=""), 
                           bucket = bucket, opts = list("region" = ""))


############################
### 1) Creating matrices ###
############################

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

# Creadting FD table
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

#################################
### 2) Writing basic matrices ###
#################################

# Writing datasets in my bucket
s3write_using(x = as.data.frame(x), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/x_",year,".rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(FD), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/FD_",year,".rds",sep=""), 
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(IO), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/IO_",year,".rds",sep=""), 
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(QT), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/QT_",year,".rds",sep=""), 
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(QY), FUN = data.table::fwrite, na = "",
              object = paste("Gloria/matrices/QY_",year,".rds",sep=""), 
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(VA), FUN = data.table::fwrite, na = "",
              object = paste("Gloria/matrices/VA_",year,".rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

##########################
### 3) Creating labels ###
##########################

label_IO <- matrix(rep(NA,(120*164)*3),ncol=3)
for (i in 1:164) {
  label_IO[((i-1)*120+1):(i*120),1] <- as.matrix(Regions)[i,2]
  label_IO[((i-1)*120+1):(i*120),2] <- as.matrix(Regions)[i,3]
  label_IO[((i-1)*120+1):(i*120),3] <- as.matrix(Sectors)[,2]
}

label_VA <- as.matrix(VA_FD[,2])

label_FD <- matrix(rep(NA,(6*164)*3),ncol=3)
for (i in 1:164) {
  label_FD[((i-1)*6+1):(i*6),1] <- as.matrix(Regions)[i,2]
  label_FD[((i-1)*6+1):(i*6),2] <- as.matrix(Regions)[i,3]
  label_FD[((i-1)*6+1):(i*6),3] <- as.matrix(VA_FD)[,3]
}

label_Q <- as.matrix(Satellites)[,2:4]           

s3write_using(x = as.data.frame(label_IO), FUN = data.table::fwrite, na = "",
              object = "Gloria/labels/label_IO.rds",
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(matrix(c(rep(NA,6),label_VA),ncol=2)), FUN = data.table::fwrite, na = "",
              object = "Gloria/labels/label_VA.rds", 
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(label_FD), FUN = data.table::fwrite, na = "",
              object = "Gloria/labels/label_FD.rds",
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(label_Q), FUN = data.table::fwrite, na = "",
              object = "Gloria/labels/label_Q.rds", 
              bucket = bucket, opts = list("region" = ""))

#########################
### 4) Other matrices ###
#########################

# Obtaining x from output and input (and adding at least 10% of margin and 2% of FD sales)
iIO <- apply(IO,2,FUN=sum)
xin <- iIO+apply(VA,2,FUN=sum)
for (i in 1:nrow(IO)) {
  xin[i] <- max(x[i],xin[i],iIO[i]/0.9)
}

IOi <- apply(IO,1,FUN=sum)
xout <- IOi+apply(FD,1,FUN=sum)
for (i in 1:nrow(IO)) {
  xout[i] <- max(x[i],xout[i],IOi[i]/0.98)
}

# Coefficient matrices
A <- t(t(IO)/(xin+10^(-6)))
B <- IO/(xout+10^(-6))

# Leontief and Ghosh
L <- solve(diag(ncol(A))-A) # 15 minutes (AFD server: 2 min, my comp: no chance of running)
G <- solve(diag(ncol(B))-B) 

# Saving files
s3write_using(x = as.data.frame(A), FUN = data.table::fwrite, na = "",
              object = paste("Gloria/matrices/A_",year,".rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(B), FUN = data.table::fwrite, na = "",
              object = paste("Gloria/matrices/B_",year,".rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(L), FUN = data.table::fwrite, na = "",
              object = paste("Gloria/matrices/L_",year,".rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.frame(G), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/G_",year,".rds",sep=""), 
              bucket = bucket, opts = list("region" = ""))

