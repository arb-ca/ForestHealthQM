#install.packages("\\\\FileServ1\\shares\\STIShare\\EducationTraining\\Technical\\Statistics\\R Trainings\\STIIR_Current_Version\\STIIR_0.1.tar.gz", repos=NULL, type="source")

#The purpose of this script is the sum up all areas for each LEMMA stand ID
#Output is a table with unique LEMMA stand IDs, and the total area for each stand ID


library(openair)
library(lubridate)
library(clifro)
library(ggplot2)
library(reshape2)
library(dplyr)
library(openair)
library(Metrics)

setwd("C:/Users/kwelch/Documents/2022 GHG calcs/2021-22 solicitation/LNU-005/Impact Area/IFTDSS")


# FOR IMPACT AREA

csvfiles = list.files(getwd(), pattern=".csv", all.files=FALSE, full.names=FALSE)
projectid = c("21_FH_LNU_005_intersect_impact", "21_FH_LNU_005_intersect_trt")
#projectid = c("6681","6704")  ## Make sure project ID list corresponds with csv files 
Nprojects = length(projectid)


Variables = c("Sum Area","Baseline Flame Length > 8 Area","Treatment Flame Length > 8 Area",
              "Baseline Flame Length > 8 %","Treatment Flame Length > 8 %",         
              "Full Area Weighted Baseline CBP",
              "Full Area Weighted Treatment CBP","Full Area CBP Ratio",
              "High Severity Area Weighted Baseline CBP",
              "High Severity Area Weighted Treatment CBP","High Severity Area CBP Ratio",
              "Project ID" )

for(i in 1:Nprojects)
{
  dat3 = read.csv(csvfiles[i],header=TRUE)
  
  #The following two lines of code assume that the variables in the arcmap output correspond accordingly
  dat1 = dat3 %>% select(Area2,gridcode,gridcode_1,gridcode_2,gridcode_3)
  names(dat1) = c("Area","baseline_CBP","baseline_flame_length","treatment_CBP","treatment_flame_length")
  dim(dat1)
  dat1[,2:5] = dat1[,2:5]/10000    #Correct for 10000 factor applied to rasters in ArcMap
  dat1$baseline_CBP[which(dat1$baseline_CBP==-1)]=0     #set cells with missing values to zero probability
  dat1$treatment_CBP[which(dat1$treatment_CBP==-1)]=0   #set cells with missing values to zero probability
  dat1$baseline_CBP_Area = dat1$Area*dat1$baseline_CBP     #assign area weighted value
  dat1$treatment_CBP_Area = dat1$Area*dat1$treatment_CBP   #assign area weighted value
  dat1$baseline_FL_8 = 0    #create new variable, flame length greater or equal to 8
  dat1$treatment_FL_8 = 0   #create new variable, flame length greater or equal to 8
  dat1$baseline_FL_8[which(dat1$baseline_flame_length >=8)]=1       #assign value of one based on flame length >=8
  dat1$treatment_FL_8[which(dat1$treatment_flame_length >=8)]=1     #assign value of one based on flame length >=8
  dat1$baseline_FL_8 = dat1$baseline_FL_8*dat1$Area       #multiply 1 x area
  dat1$treatment_FL_8 = dat1$treatment_FL_8*dat1$Area      #multiply 1 x area
  
  mat <- matrix(0,ncol=2, nrow=12)
  outdat = as.data.frame((mat))
  outdat[,1] = Variables #based on variables
  
  outdat[1,2] = sum(dat1$Area)
  outdat[2,2] = sum(dat1$baseline_FL_8)
  outdat[3,2] = sum(dat1$treatment_FL_8)
  outdat[4,2] = 100*sum(dat1$baseline_FL_8)/sum(dat1$Area)   #multiply by 100 percent
  outdat[5,2] = 100*sum(dat1$treatment_FL_8)/sum(dat1$Area)  #multiply by 100 percent
  outdat[6,2] = sum(dat1$baseline_CBP_Area)/sum(dat1$Area)   
  outdat[7,2] = sum(dat1$treatment_CBP_Area)/sum(dat1$Area)
  outdat[8,2] = sum(dat1$treatment_CBP_Area)/sum(dat1$baseline_CBP_Area)   #CFP ratio for full area
  
  dat1$baseline_CBP_Area[which(dat1$baseline_flame_length <8)]=0    # restrict to baseline severe 
  dat1$treatment_CBP_Area[which(dat1$baseline_flame_length <8)]=0   # restrict to baseline severe 
  dat1$Area[which(dat1$baseline_flame_length <8)]=0                 # restrict to baseline severe 
  
  outdat[9,2] = sum(dat1$baseline_CBP_Area)/sum(dat1$Area)
  outdat[10,2] = sum(dat1$treatment_CBP_Area)/sum(dat1$Area)
  outdat[11,2] = sum(dat1$treatment_CBP_Area)/sum(dat1$baseline_CBP_Area)
  outdat[12,2] = projectid[i]
  
  names(outdat) = c("Variable" , "Value")
  fileoutnamae = paste0("Output/IFTDSS_impact_area_output_",as.character(projectid[i]),".csv")
  write.csv(outdat,fileoutnamae,row.names=F)
  
}

