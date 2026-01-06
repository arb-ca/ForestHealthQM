#################################################################################
# Carmen's updated EQ8 
#################################################################################
library(ggplot2)
library(tidyverse)
library(terra)
library(sf)
library(exactextractr)


################################################################################
# Functions for QM
################################################################################

EQ8b <- function(APFO_FR = 0.01, 
                 EP = 10){
  1 - (1 - APFO_FR)^EP
}


#################################################################################
#################################################################################

####################
# Parameters
####################
buff_amount <-  round(2^(5:12) / 30) * 30
FL_ft       <- 8

####################
# Project files
####################
project_shp_whole     <- vect("../Desktop/Auto_Randig/sb63_reproj/sb63_reproj.shp")
proj_data             <- data.frame(project_shp_whole)

proj_names            <- proj_data$Grant_ID

base_randig_file      <- paste0("../Desktop/Auto_Randig/TestRandig/SampleData/Outputs/baseline_90m/",
                                proj_names, "/_RandigOutputs.tif")
treatment_randig_file <- paste0("../Desktop/Auto_Randig/TestRandig/SampleData/Outputs/treatment_thinning_90m/",
                                proj_names, "/_RandigOutputs.tif")
baseline_C            <- rast("../Desktop/TreeMap_FVS_statewide/Statewide/TreeMap_FVS_baseline_2035_AbovegroundTotalLiveC_v1.tif")
withfire_C            <- rast("../Desktop/TreeMap_FVS_statewide/Statewide/TreeMap_FVS_withfire_2035_AbovegroundTotalLiveC_v1.tif")


################################################################################
# Caclulate QM per each project
################################################################################
QM_store <- list()
for(wProj in 1:(length(base_randig_file))){
  
  base_randig    <- rast(base_randig_file[wProj])
  treated_randig <- rast(treatment_randig_file[wProj])
  Project_agg    <- project_shp_whole[wProj]
  
  crs(base_randig)    <- crs(Project_agg) 
  crs(treated_randig) <- crs(Project_agg)
  
  buff_set <- do.call(rbind, lapply(buff_amount, function(x) buffer(Project_agg, x) - Project_agg ))
  
  NAflag(base_randig$`Conditional Flame Length`)    <- -1
  NAflag(treated_randig$`Conditional Flame Length`) <- -1
  
  FL_BP <- rast(list(base_randig$`Conditional Flame Length`,
                     treated_randig$`Conditional Flame Length`,
                     base_randig$`Burn Probability`,
                     treated_randig$`Burn Probability`))
  
  # Crops to extent
  ext_poly      <- vect(ext(FL_BP))
  crs(ext_poly) <- crs(FL_BP) 
  ext_poly      <- project(ext_poly, baseline_C) 
  
  baseline_C_crop <- crop(baseline_C, ext_poly)
  withfire_C_crop <- crop(withfire_C, ext_poly)
  baseline_C_crop <- project(baseline_C_crop, FL_BP)
  withfire_C_crop <- project(withfire_C_crop, FL_BP)
  
  FL_BP        <- rast(list(FL_BP, baseline_C_crop, withfire_C_crop))
  names(FL_BP) <- c("Base_FL_Severe", 
                    "Treat_FL_Severe",
                    "Base_CBP", 
                    "Treat_CBP",
                    "Baseline_AbovegroundLiveCarbon", 
                    "WithFire_AbovegroundLiveCarbon")
  FL_BP        <- mask(FL_BP, Project_agg, inverse = T)
  
  
  remove_C_NA <- is.na(FL_BP$Baseline_AbovegroundLiveCarbon) & is.na(FL_BP$WithFire_AbovegroundLiveCarbon)
  
  
  ##############################################################################
  # This section does not have raster operation for PHS
  # Considering severity influence on C is not immediately clear how to solve
  # For now this term I will avoid using raster operation and use the aggregation
  #################
  
  PHS_I_NTrast <- FL_BP$Base_FL_Severe
  PHS_I_NTrast[remove_C_NA] <- NA
  
  PHS_I_TRrast              <- FL_BP$Treat_FL_Severe
  PHS_I_TRrast[remove_C_NA] <- NA
  
  base_C  <- FL_BP$Baseline_AbovegroundLiveCarbon*(30^2/ (4046.85642))
  wFire_C <- FL_BP$WithFire_AbovegroundLiveCarbon*(30^2/ (4046.85642))
  
  CBP_NTrast <- FL_BP$Base_CBP
  CBP_TRrast <- FL_BP$Treat_CBP
  
  CBP_NTrast[remove_C_NA] <- NA
  CBP_TRrast[remove_C_NA] <- NA
  
  C_and_CBP <- c(base_C, wFire_C, CBP_NTrast, CBP_TRrast, PHS_I_NTrast, PHS_I_TRrast)
  P_WF <- EQ8b()
  
  benefit                  <- sapply(seq(length(buff_set)), function(x){

    only_buffer <- mask(C_and_CBP, buff_set[x])
    names(only_buffer) <- c("Base_C","wFire_C", "CBP_I_NT", "CBP_I_TR", "Base_S", "Treat_S")

    PHS_I_NT_buff <- 1
    
    PHS_I_TR_buff <- 1
    
    EQ8e <- only_buffer$Base_C - (only_buffer$Base_C - 
               only_buffer$wFire_C) * 
      ( (PHS_I_TR_buff * only_buffer$CBP_I_TR) / (PHS_I_NT_buff*only_buffer$CBP_I_NT) )
    
    # toss the inf, these are problematic CBP ratios where orginal CBP was 0 and treated CBP was > 0, thus leading to inf
    #
    EQ8e[is.infinite(EQ8e)] <- NA
    
    EQ8a <- only_buffer$Base_C
    
    EQ8c <- EQ8a - EQ8e 
    
    
    EQ8 <- (EQ8a - P_WF * EQ8c) * 3.67
    
    EQ9 <- (EQ8a - P_WF*(EQ8a - only_buffer$wFire_C))  * 3.67
    
    EQ7 <- EQ8 - EQ9
    ghg_co2eq <- sum(values(EQ7), na.rm=T)
    return(ghg_co2eq)
    
  })
  
  QM_vars <- data.frame(buffer_radius = buff_amount,
                        GHG_BENEFIT_FR = benefit)
  
  QM_store[[paste0("Grant_ID ", proj_names[wProj])]] <- QM_vars %>% mutate(Grant_ID = proj_names[wProj],
                                                                           Area2 = proj_data$Area2[wProj])
  print(proj_names[wProj])
}
QM_df <- do.call(rbind, QM_store)
rownames(QM_df) <- NULL

QM_df %>% 
  group_by(buffer_radius) %>%
  summarise(GHG_mean = mean(GHG_BENEFIT_FR,#GHG_FRP,
                            na.rm=T),
            GHG_se = sd(GHG_BENEFIT_FR,#GHG_FRP,
                        na.rm=T)/sqrt(sum(!is.na(GHG_BENEFIT_FR#GHG_FRP
                        ) )),
            
            lower = GHG_mean - GHG_se * 1.96,
            upper = GHG_mean + GHG_se * 1.96,
            .groups='drop'
  ) %>%
  ggplot(aes(x = buffer_radius, y = GHG_mean)) + 
  geom_line( color = 'darkgreen', linewidth = 2) +
  geom_point() +
  geom_ribbon(aes(x = buffer_radius, ymin = lower, ymax = upper), alpha = 0.25, fill = 'green4') +
  xlab("Buffer Length (m)") +
  ylab("Metric-tons (mean ± 1.96*SE)") + 
  ggtitle("EQ7 raster calc updated QM, -NO HIGH SEVERITY BENEFIT INCLUDED-") + 
  theme_light(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5))
