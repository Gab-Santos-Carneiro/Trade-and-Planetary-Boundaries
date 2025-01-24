### Planetary boundaries - Consumption and production

###########################
### 0) Reading matrices ###
###########################

# Cleaning memory
rm(list = ls())
gc()

# Setting timeout option to 10 minutes
options(timeout = 60*10)

# Setting the list of packages to be installed and attached
packages <- c('plyr','dplyr', 'ggplot2', 'data.table', 'aws.s3', 'ARTofR', 'readxl','reshape2','tidyverse')
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)
lapply(packages, require, character.only=TRUE)

# Set the Sys.setenv (in SSP Cloud, click in My account > Connect to storage > Select "R (aws.S3)", copy the code and paste it here)

rm(list=ls())
year = 2021
bucket <- "projet-esteem"

install.packages("aws.s3", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "M5LPHUZJ9LCF1P6P63OS",
           "AWS_SECRET_ACCESS_KEY" = "jveTsEOJc59VRPip7x5wPRRh3CYzAoQvi3xa9cg+",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJNNUxQSFVaSjlMQ0YxUDZQNjNPUyIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNzM3MDUzMTAyLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6ImdhYnJpZWwuc2FudG9zLWNhcm5laXJvQGV0dS51LXBhcmlzLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTczODMzMzMwOSwiZmFtaWx5X25hbWUiOiJDYXJuZWlybyIsImdpdmVuX25hbWUiOiJHYWJyaWVsIiwiZ3JvdXBzIjpbIlVTRVJfT05ZWElBIiwiZXN0ZWVtIl0sImlhdCI6MTczNzcyODUwOSwiaXNzIjoiaHR0cHM6Ly9hdXRoLmxhYi5zc3BjbG91ZC5mci9hdXRoL3JlYWxtcy9zc3BjbG91ZCIsImp0aSI6ImFhNWQ2ZDI4LTQ2MDUtNDJhNy1iOTUwLTU5Y2Y0N2JlN2IwZiIsIm5hbWUiOiJHYWJyaWVsIENhcm5laXJvIiwicG9saWN5Ijoic3Rzb25seSIsInByZWZlcnJlZF91c2VybmFtZSI6ImJpbHNjYXJuZWlybyIsInJlYWxtX2FjY2VzcyI6eyJyb2xlcyI6WyJvZmZsaW5lX2FjY2VzcyIsInVtYV9hdXRob3JpemF0aW9uIiwiZGVmYXVsdC1yb2xlcy1zc3BjbG91ZCJdfSwicmVzb3VyY2VfYWNjZXNzIjp7ImFjY291bnQiOnsicm9sZXMiOlsibWFuYWdlLWFjY291bnQiLCJtYW5hZ2UtYWNjb3VudC1saW5rcyIsInZpZXctcHJvZmlsZSJdfX0sInJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLXNzcGNsb3VkIl0sInNjb3BlIjoib3BlbmlkIHByb2ZpbGUgZ3JvdXBzIGVtYWlsIiwic2lkIjoiYTgwM2IzOWMtMDllNS00NzM5LWI5MjItYWI4M2FhMjRhMjRmIiwic3ViIjoiZjc5ZDZjMDItNjU1NC00ZWRiLTk1N2UtZTFkODZkMjdkODlkIiwidHlwIjoiQmVhcmVyIn0.oh3AwuFIOV_GLaTbTpMIwRRE9Hl1X5diRle0g35yAlLFE3ndhnMQ-EOup031YQNqLV2kJOUTGycYLo3ZYUKXoA",
           "AWS_S3_ENDPOINT"= "minio.lab.sspcloud.fr")

library("aws.s3")


# Loading labels
label_Q <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                        object = "Gloria/labels/label_Q.rds",
                        bucket = bucket, opts = list("region" = ""))

label_IO <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                         object = "Gloria/labels/label_IO.rds",
                         bucket = bucket, opts = list("region" = ""))

label_FD <- s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                         object = "Gloria/labels/label_FD.rds",
                         bucket = bucket, opts = list("region" = ""))

# Loading data

x <- as.matrix(s3read_using(FUN = data.table::fread,
                            encoding = "UTF-8",
                            #Reading arguments
                            object = paste("Gloria/matrices/x_",year,".rds",sep=""), #the path and name of the file to read
                            bucket = bucket, #The name of the bucket (identifiant IDEP in "Mon compte")
                            opts = list("region" = "") #Purely technical option, but source of error if "region" = "" not specified
))


FD <- as.matrix(s3read_using(FUN = data.table::fread,
                             encoding = "UTF-8",
                             #Reading arguments
                             object = paste("Gloria/matrices/FD_",year,".rds",sep=""), #the path and name of the file to read
                             bucket = bucket, #The name of the bucket (identifiant IDEP in "Mon compte")
                             opts = list("region" = "") #Purely technical option, but source of error if "region" = "" not specified
))

Q <- as.matrix(s3read_using(FUN = data.table::fread,
                            encoding = "UTF-8",
                            #Reading arguments
                            object = paste("Gloria/matrices/QT_",year,".rds",sep=""), #the path and name of the file to read
                            bucket = bucket, #The name of the bucket (identifiant IDEP in "Mon compte")
                            opts = list("region" = "") #Purely technical option, but source of error if "region" = "" not specified
))

A <- as.matrix(s3read_using(FUN = data.table::fread,
                            encoding = "UTF-8",
                            #Reading arguments
                            object = paste("Gloria/matrices/A_",year,".rds",sep=""), #the path and name of the file to read
                            bucket = bucket, #The name of the bucket (identifiant IDEP in "Mon compte")
                            opts = list("region" = "") #Purely technical option, but source of error if "region" = "" not specified
))

# Domestic dummies
ID <- matrix(rep(0,(120*164)^2),ncol=(120*164))
for (i in 1:164) {
  ID[((i-1)*120+1):(i*120),((i-1)*120+1):(i*120)] <- 1
}

IF <- matrix(rep(0,(120*164)*(6*164)),ncol=(6*164))
for (i in 1:164) {
  IF[((i-1)*120+1):(i*120),((i-1)*6+1):(i*6)] <- 1
}

# Leontief
L <- solve(diag(ncol(A))-A)

#############################
### 1) Footprints, direct ###
#############################

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

