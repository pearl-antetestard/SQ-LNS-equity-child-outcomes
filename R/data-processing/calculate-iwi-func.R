## author: Pearl Ante-Testard
## date: August 9, 2024
## This script is to calculate the IWI


# Clean the environment
rm(list = ls(all = TRUE))

# Load packages
source(here::here("R", "0-config.R"))

# Read in the data
df_analysis <- readRDS(file = here::here("data", "1-final",
                                         "df_analysis_assets_joined.rds")) 


df_analysis <- df_analysis %>%
  distinct(study_id, .keep_all = TRUE)


# Recode NAs as -9 for all variables
df_analysis <- df_analysis %>%
  mutate(tv_iwi = ifelse(is.na(tv_iwi), -9, tv_iwi),
         ref_iwi = ifelse(is.na(ref_iwi), -9, ref_iwi),
         phone_iwi = ifelse(is.na(phone_iwi), -9, phone_iwi),
         car_iwi = ifelse(is.na(car_iwi), -9, car_iwi),
         bike_iwi = ifelse(is.na(bike_iwi), -9, bike_iwi),
         elec_iwi = ifelse(is.na(elec_iwi), -9, elec_iwi),
         watersource_iwi = ifelse(is.na(watersource_iwi), -9, watersource_iwi),
         floormat_iwi = ifelse(is.na(floormat_iwi), -9, floormat_iwi),
         toiletfac_iwi = ifelse(is.na(toiletfac_iwi), -9, toiletfac_iwi),
         rooms_iwi = ifelse(is.na(rooms_iwi), -9, rooms_iwi),
         water1 = ifelse(is.na(water1), -9, water1),
         water2 = ifelse(is.na(water2), -9, water2),
         water3 = ifelse(is.na(water3), -9, water3),
         toilet1 = ifelse(is.na(toilet1), -9, toilet1),
         toilet2 = ifelse(is.na(toilet2), -9, toilet2),
         toilet3 = ifelse(is.na(toilet3), -9, toilet3),
         floor1 = ifelse(is.na(floor1), -9, floor1),
         floor2 = ifelse(is.na(floor2), -9, floor2),
         floor3 = ifelse(is.na(floor3), -9, floor3),
         cheap_iwi = ifelse(is.na(cheap_iwi), -9, cheap_iwi),
         expensive_iwi = ifelse(is.na(expensive_iwi), -9, expensive_iwi),
         sleepr1 = ifelse(is.na(sleepr1), -9, sleepr1),
         sleepr2 = ifelse(is.na(sleepr2), -9, sleepr2),
         sleepr3 = ifelse(is.na(sleepr3), -9, sleepr3))


# Recode variables
df_analysis <- df_analysis %>%
  mutate(tv_iwi = case_when(tv_iwi == 2 ~ 1,
                            tv_iwi == 1 ~ 0,
                            tv_iwi == -9 ~ -9,
                            .default = as.numeric(tv_iwi)),
         ref_iwi = case_when(ref_iwi == 2 ~ 1,
                             ref_iwi == 1 ~ 0,
                             ref_iwi == -9 ~ -9,
                             .default = as.numeric(ref_iwi)),
         phone_iwi = case_when(phone_iwi == 2 ~ 1,
                               phone_iwi == 1 ~ 0,
                               phone_iwi == -9 ~ -9,
                               .default = as.numeric(phone_iwi)),
         car_iwi = case_when(car_iwi == 2 ~ 1,
                             car_iwi == 1 ~ 0,
                             car_iwi == -9 ~ -9,
                             .default = as.numeric(car_iwi)),
         bike_iwi = case_when(bike_iwi == 2 ~ 1,
                              bike_iwi == 1 ~ 0,
                              bike_iwi == -9 ~ -9,
                              .default = as.numeric(bike_iwi)),
         cheap_iwi = case_when(cheap_iwi == 2 ~ 1,
                               cheap_iwi == 1 ~ 0,
                               cheap_iwi == -9 ~ -9,
                               .default = as.numeric(cheap_iwi)),
         expensive_iwi = case_when(expensive_iwi == 2 ~ 1,
                                   expensive_iwi == 1 ~ 0,
                                   expensive_iwi == -9 ~ -9,
                                   .default = as.numeric(expensive_iwi)),
         elec_iwi = case_when(elec_iwi == 2 ~ 1,
                              elec_iwi == 1 ~ 0,
                              elec_iwi == -9 ~ -9,
                              .default = as.numeric(elec_iwi)),
         #watersource_iwi = factor(watersource_iwi, levels=c(1,2,3)),
         #floormat_iwi = factor(floormat_iwi, levels=c(1,2,3)),
         #toiletfac_iwi = factor(toiletfac_iwi, levels=c(1,2,3)),
         #rooms_iwi = factor(rooms_iwi, levels=c(1,2,3)),
         water1 = case_when(water1 == 2 ~ 1,
                            water1 == 1 ~ 0,
                            water1 == -9 ~ -9,
                            .default = as.numeric(water1)),
         water2 = case_when(water2 == 2 ~ 1,
                            water2 == 1 ~ 0,
                            water2 == -9 ~ -9,
                            .default = as.numeric(water2)),
         water3 = case_when(water3 == 2 ~ 1,
                            water3 == 1 ~ 0,
                            water3 == -9 ~ -9,
                            .default = as.numeric(water3)),
         toilet1 = case_when(toilet1 == 2 ~ 1,
                             toilet1 == 1 ~ 0,
                             toilet1 == -9 ~ -9,
                             .default = as.numeric(toilet1)),
         toilet2 = case_when(toilet2 == 2 ~ 1,
                             toilet2 == 1 ~ 0,
                             toilet2 == -9 ~ -9,
                             .default = as.numeric(toilet2)),
         toilet3 = case_when(toilet3 == 2 ~ 1,
                             toilet3 == 1 ~ 0,
                             toilet3 == -9 ~ -9,
                             .default = as.numeric(toilet3)),
         floor1 = case_when(floor1 == 2 ~ 1,
                            floor1 == 1 ~ 0,
                            floor1 == -9 ~ -9,
                            .default = as.numeric(floor1)),
         floor2 = case_when(floor2 == 2 ~ 1,
                            floor2 == 1 ~ 0,
                            floor2 == -9 ~ -9,
                            .default = as.numeric(floor2)),
         floor3 = case_when(floor3 == 2 ~ 1,
                            floor3 == 1 ~ 0,
                            floor3 == -9 ~ -9,
                            .default = as.numeric(floor3)),
         sleepr1 = case_when(sleepr1 == 2 ~ 1,
                             sleepr1 == 1 ~ 0,
                             sleepr1 == -9 ~ -9,
                             .default = as.numeric(sleepr1)),
         sleepr2 = case_when(sleepr2 == 2 ~ 1,
                             sleepr2 == 1 ~ 0,
                             sleepr2 == -9 ~ -9,
                             .default = as.numeric(sleepr2)),
         sleepr3 = case_when(sleepr3 == 2 ~ 1,
                             sleepr3 == 1 ~ 0,
                             sleepr3 == -9 ~ -9,
                             .default = as.numeric(sleepr3)))



# Calculate the IWI
df_analysis <- df_analysis %>%
  mutate(
    iwi = case_when(
      # all assets are present
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9 ~
        ###!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.00447 - 6.306477 * water1 - 2.302023 * water2 + 7.952443 * water3 - 
        7.439841 * toilet1 - 1.090393 * toilet2 + 8.140637 * toilet3 - 
        7.558471 * floor1 + 1.227531 * floor2 + 6.107428 * floor3 + 
        8.612657 * tv_iwi + 8.429076 * ref_iwi + 7.127699 * phone_iwi + 
        8.056664 * elec_iwi + 4.651382 * car_iwi + 1.84686 * bike_iwi + 
        4.118394 * cheap_iwi + 6.507283 * expensive_iwi - 
        3.699681 * sleepr1 + 0.38405 * sleepr2 + 3.445009 * sleepr3,
      
      # at least 1 asset is missing
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9 ~
        ##is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.920605 + 0 * water1 + 0 * water2 + 0 * water3 - 8.489335 * toilet1 - 
        1.000078 * toilet2 + 9.101241 * toilet3 - 8.792053 * floor1 + 1.405088 * 
        floor2 + 7.126645 * floor3 + 9.940496 * tv_iwi + 9.532776 * ref_iwi + 8.306407 * 
        phone_iwi + 9.209894 * elec_iwi + 5.449126 * car_iwi + 2.53495 * bike_iwi + 4.886433 * 
        cheap_iwi + 7.720215 * expensive_iwi - 4.639218 * sleepr1 + 0.530116 * sleepr2 + 4.271212 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        ##!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        20.887068 - 7.328661 * water1 - 2.412003 * water2 + 9.01139 * water3 +
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 8.818877 * floor1 + 1.482174 * floor2 +
        7.076643 * floor3 + 10.136374 * tv_iwi + 9.701948 * ref_iwi + 8.471279 * phone_iwi +
        9.402895 * elec_iwi + 5.575885 * car_iwi + 2.502624 * bike_iwi + 4.925904 * cheap_iwi +
        7.831473 * expensive_iwi - 4.73953 * sleepr1 + 0.428914 * sleepr2 + 4.476517 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.920605 + 0 * water1 + 0 * water2 + 0 * water3 - 8.489335 * toilet1 - 
        1.000078 * toilet2 + 9.101241 * toilet3 - 8.792053 * floor1 + 1.405088 * 
        floor2 + 7.126645 * floor3 + 9.940496 * tv_iwi + 9.532776 * ref_iwi + 8.306407 * 
        phone_iwi + 9.209894 * elec_iwi + 5.449126 * car_iwi + 2.53495 * bike_iwi + 4.886433 * 
        cheap_iwi + 7.720215 * expensive_iwi - 4.639218 * sleepr1 + 0.530116 * sleepr2 + 4.271212 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9 ~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.385089 - 6.905966 * water1 - 2.612944 * water2 + 8.788897 * water3 - 
        8.184232 * toilet1 - 1.285094 * toilet2 + 9.020976 * toilet3 - 
        8.218712 * floor1 + 1.20911 * floor2 + 6.764747 * floor3 + 
        0 * tv_iwi + 9.183385 * ref_iwi + 7.779777 * phone_iwi + 8.507725 * elec_iwi + 
        5.195496 * car_iwi + 1.950995 * bike_iwi + 4.52176 * cheap_iwi + 
        7.076601 * expensive_iwi - 4.076179 * sleepr1 + 0.394245 * sleepr2 + 
        3.824551 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.457875 - 6.910841 * water1 - 2.362792 * water2 + 8.574823 * water3 - 
        8.120884 * toilet1 - 1.086753 * toilet2 + 8.806269 * toilet3 - 
        8.344341 * floor1 + 1.377425 * floor2 + 6.720488 * floor3 + 
        9.339074 * tv_iwi + 0 * ref_iwi + 7.668854 * phone_iwi + 8.832101 * elec_iwi + 
        4.949752 * car_iwi + 2.184696 * bike_iwi + 4.603353 * cheap_iwi + 
        7.025409 * expensive_iwi - 4.081809 * sleepr1 + 0.387337 * sleepr2 + 
        3.837305 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.064434 - 6.850516 * water1 - 2.48532 * water2 + 8.625108 * water3 - 
        8.082506 * toilet1 - 1.174313 * toilet2 + 8.835939 * toilet3 - 
        8.171393 * floor1 + 1.267707 * floor2 + 6.66119 * floor3 + 
        9.243248 * tv_iwi + 8.988852 * ref_iwi + 0 * phone_iwi + 8.732721 * elec_iwi + 
        4.938465 * car_iwi + 1.967107 * bike_iwi + 4.429965 * cheap_iwi + 
        6.858054 * expensive_iwi - 3.960019 * sleepr1 + 0.443503 * sleepr2 + 
        3.654917 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.028375 - 6.755254 * water1 - 2.630739 * water2 + 8.662495 * water3 - 
        8.035364 * toilet1 - 1.32195 * toilet2 + 8.903208 * toilet3 - 
        8.09294 * floor1 + 1.171606 * floor2 + 6.679951 * floor3 + 
        9.061101 * tv_iwi + 9.194051 * ref_iwi + 7.813931 * phone_iwi + 0 * elec_iwi + 
        5.212346 * car_iwi + 2.003531 * bike_iwi + 4.446956 * cheap_iwi + 
        7.104959 * expensive_iwi - 4.144817 * sleepr1 + 0.400742 * sleepr2 + 
        3.889095 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.334816 - 6.65873 * water1 - 2.38544 * water2 + 8.357154 * water3 - 
        7.840124 * toilet1 - 1.130903 * toilet2 + 8.564661 * toilet3 - 
        7.989144 * floor1 + 1.367741 * floor2 + 6.386174 * floor3 + 
        9.065771 * tv_iwi + 8.775876 * ref_iwi + 7.429358 * phone_iwi + 8.519543 * elec_iwi + 
        0 * car_iwi + 1.901476 * bike_iwi + 4.322058 * cheap_iwi + 
        6.791118 * expensive_iwi - 3.846817 * sleepr1 + 0.42927 * sleepr2 + 
        3.551996 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.512578 - 6.449505 * water1 - 2.374913 * water2 + 8.150878 * water3 - 
        7.614421 * toilet1 - 1.107247 * toilet2 + 8.324944 * toilet3 - 
        7.71274 * floor1 + 1.282779 * floor2 + 6.202325 * floor3 + 
        8.76738 * tv_iwi + 8.619843 * ref_iwi + 7.261226 * phone_iwi + 8.211868 * elec_iwi + 
        4.72906 * car_iwi + 0 * bike_iwi + 4.157438 * cheap_iwi + 
        6.592156 * expensive_iwi - 3.735912 * sleepr1 + 0.396234 * sleepr2 + 
        3.470303 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.097175 - 6.601002 * water1 - 2.422247 * water2 + 8.334952 * water3 - 
        7.775982 * toilet1 - 1.156992 * toilet2 + 8.52177 * toilet3 - 
        7.909069 * floor1 + 1.299258 * floor2 + 6.376146 * floor3 + 
        8.983673 * tv_iwi + 8.83294 * ref_iwi + 7.431439 * phone_iwi + 8.390534 * elec_iwi + 
        4.856322 * car_iwi + 1.829117 * bike_iwi + 0 * cheap_iwi + 
        6.801875 * expensive_iwi - 3.811123 * sleepr1 + 0.400329 * sleepr2 + 
        3.544057 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.95864 - 6.824804 * water1 - 2.510211 * water2 + 8.622648 * water3 - 
        8.080768 * toilet1 - 1.108413 * toilet2 + 8.783553 * toilet3 - 
        8.137996 * floor1 + 1.344529 * floor2 + 6.55315 * floor3 + 
        9.175138 * tv_iwi + 8.966675 * ref_iwi + 7.496656 * phone_iwi + 8.646894 * elec_iwi + 
        4.934158 * car_iwi + 1.804154 * bike_iwi + 4.433763 * cheap_iwi + 
        0 * expensive_iwi - 3.915071 * sleepr1 + 0.427357 * sleepr2 + 
        3.624572 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        23.13399 - 6.873579 * water1 - 2.519801 * water2 + 8.676972 * water3 - 
        8.109378 * toilet1 - 1.211954 * toilet2 + 8.89126 * toilet3 - 
        8.151034 * floor1 + 1.372303 * floor2 + 6.538399 * floor3 + 
        9.263933 * tv_iwi + 9.085458 * ref_iwi + 7.621934 * phone_iwi + 8.740713 * elec_iwi + 
        4.933088 * car_iwi + 1.833687 * bike_iwi + 4.342606 * cheap_iwi + 
        6.93796 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      # at least 2 assets are missing
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9 ~
        ##is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.527151 + 0 * water1 + 0 * water2 + 0 * water3 +
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.348075 * floor1 + 1.689063 * floor2 + 8.353131 * floor3 + 
        11.815886 * tv_iwi + 11.004256 * ref_iwi + 10.017382 * phone_iwi + 
        10.807495 * elec_iwi + 6.654149 * car_iwi + 3.59502 * bike_iwi + 
        5.965202 * cheap_iwi + 9.482285 * expensive_iwi - 
        6.179077 * sleepr1 + 0.617179 * sleepr2 + 5.778042 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        15.552366 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.881388 * toilet1 - 1.308829 * toilet2 + 10.70496 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.653127 * tv_iwi + 11.3664 * ref_iwi + 9.966646 * phone_iwi + 
        10.667625 * elec_iwi + 6.652943 * car_iwi + 3.062859 * bike_iwi + 
        5.915962 * cheap_iwi + 9.286552 * expensive_iwi - 
        5.670978 * sleepr1 + 0.698455 * sleepr2 + 5.17056 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.409632 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.452792 * toilet1 - 1.187772 * toilet2 + 10.191203 * toilet3 - 
        9.685797 * floor1 + 1.355397 * floor2 + 8.04083 * floor3 + 
        0 * tv_iwi + 10.479588 * ref_iwi + 9.200251 * phone_iwi + 
        9.765508 * elec_iwi + 6.212403 * car_iwi + 2.800402 * bike_iwi + 
        5.470988 * cheap_iwi + 8.541723 * expensive_iwi - 
        5.271043 * sleepr1 + 0.567847 * sleepr2 + 4.887472 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.381701 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.331545 * toilet1 - 0.916868 * toilet2 + 9.863865 * toilet3 - 
        9.813299 * floor1 + 1.588637 * floor2 + 7.934399 * floor3 + 
        10.860355 * tv_iwi + 0 * ref_iwi + 9.006405 * phone_iwi + 
        10.17537 * elec_iwi + 5.840475 * car_iwi + 3.09053 * bike_iwi + 
        5.553167 * cheap_iwi + 8.426143 * expensive_iwi - 
        5.236858 * sleepr1 + 0.552378 * sleepr2 + 4.86759 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.077255 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.35238 * toilet1 - 1.056413 * toilet2 + 9.99163 * toilet3 - 
        9.645623 * floor1 + 1.450349 * floor2 + 7.908361 * floor3 + 
        10.802501 * tv_iwi + 10.254659 * ref_iwi + 0 * phone_iwi + 
        10.118562 * elec_iwi + 5.855625 * car_iwi + 2.787755 * bike_iwi + 
        5.343507 * cheap_iwi + 8.237451 * expensive_iwi - 
        5.079251 * sleepr1 + 0.633908 * sleepr2 + 4.622695 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.031149 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.219652 * toilet1 - 1.247688 * toilet2 + 10.008457 * toilet3 - 
        9.481142 * floor1 + 1.305722 * floor2 + 7.891663 * floor3 + 
        10.493423 * tv_iwi + 10.470611 * ref_iwi + 9.214954 * phone_iwi + 
        0 * elec_iwi + 6.20944 * car_iwi + 2.845989 * bike_iwi + 
        5.343384 * cheap_iwi + 8.545747 * expensive_iwi - 
        5.330355 * sleepr1 + 0.571528 * sleepr2 + 4.945184 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.304646 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.031711 * toilet1 - 1.026958 * toilet2 + 9.654246 * toilet3 - 
        9.393397 * floor1 + 1.601593 * floor2 + 7.515133 * floor3 + 
        10.564076 * tv_iwi + 9.988979 * ref_iwi + 8.727782 * phone_iwi + 
        9.838684 * elec_iwi + 0 * car_iwi + 2.65011 * bike_iwi + 
        5.180453 * cheap_iwi + 8.129963 * expensive_iwi - 
        4.879538 * sleepr1 + 0.604003 * sleepr2 + 4.445926 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.544328 + 0 * water1 + 0 * water2 + 0 * water3 - 
        8.776231 * toilet1 - 1.016025 * toilet2 + 9.395089 * toilet3 - 
        9.053179 * floor1 + 1.498664 * floor2 + 7.287214 * floor3 + 
        10.199237 * tv_iwi + 9.84095 * ref_iwi + 8.532103 * phone_iwi + 
        9.465729 * elec_iwi + 5.581513 * car_iwi + 0 * bike_iwi + 
        4.957676 * cheap_iwi + 7.871432 * expensive_iwi - 
        4.714918 * sleepr1 + 0.554905 * sleepr2 + 4.324729 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.066861 + 0 * water1 + 0 * water2 + 0 * water3 - 
        8.95092 * toilet1 - 1.06967 * toilet2 + 9.607798 * toilet3 - 
        9.288957 * floor1 + 1.505876 * floor2 + 7.508359 * floor3 + 
        10.458934 * tv_iwi + 10.080334 * ref_iwi + 8.737968 * phone_iwi + 
        9.668816 * elec_iwi + 5.742609 * car_iwi + 2.543595 * bike_iwi + 
        0 * cheap_iwi + 8.150437 * expensive_iwi - 
        4.826984 * sleepr1 + 0.561342 * sleepr2 + 4.43429 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.038011 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.386774 * toilet1 - 0.966845 * toilet2 + 9.956506 * toilet3 - 
        9.63143 * floor1 + 1.572266 * floor2 + 7.774469 * floor3 + 
        10.739516 * tv_iwi + 10.257645 * ref_iwi + 8.842719 * phone_iwi + 
        10.033122 * elec_iwi + 5.863908 * car_iwi + 2.548909 * bike_iwi + 
        5.362119 * cheap_iwi + 0 * expensive_iwi - 
        5.019807 * sleepr1 + 0.612044 * sleepr2 + 4.583077 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.18955 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.488053 * toilet1 - 1.139338 * toilet2 + 10.188563 * toilet3 - 
        9.701498 * floor1 + 1.625041 * floor2 + 7.79029 * floor3 + 
        10.925921 * tv_iwi + 10.492946 * ref_iwi + 9.066533 * phone_iwi + 
        10.230496 * elec_iwi + 5.886624 * car_iwi + 2.578523 * bike_iwi + 
        5.24577 * cheap_iwi + 8.404784 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        14.353748 - 8.584113 * water1 - 3.034577 * water2 + 10.738133 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.846805 * tv_iwi + 11.539794 * ref_iwi + 10.134281 * phone_iwi + 
        10.861044 * elec_iwi + 6.796018 * car_iwi + 2.987828 * bike_iwi + 
        5.933998 * cheap_iwi + 9.379275 * expensive_iwi - 
        5.769635 * sleepr1 + 0.542468 * sleepr2 + 5.429074 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.276783 - 8.141913 * water1 - 2.761531 * water2 + 10.082939 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.720843 * floor1 + 1.445317 * floor2 + 7.98614 * floor3 + 
        0 * tv_iwi + 10.698891 * ref_iwi + 9.419235 * phone_iwi + 
        9.994434 * elec_iwi + 6.391679 * car_iwi + 2.759204 * bike_iwi + 
        5.528501 * cheap_iwi + 8.689296 * expensive_iwi - 
        5.414026 * sleepr1 + 0.430787 * sleepr2 + 5.1729 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.333422 - 8.112875 * water1 - 2.384169 * water2 + 9.725722 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.851357 * floor1 + 1.692118 * floor2 + 7.86926 * floor3 + 
        11.094686 * tv_iwi + 0 * ref_iwi + 9.200625 * phone_iwi + 
        10.411895 * elec_iwi + 5.985841 * car_iwi + 3.072244 * bike_iwi + 
        5.609377 * cheap_iwi + 8.561904 * expensive_iwi - 
        5.36919 * sleepr1 + 0.422269 * sleepr2 + 5.135024 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.986366 - 8.095883 * water1 - 2.604534 * water2 + 9.902346 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.685706 * floor1 + 1.542082 * floor2 + 7.85676 * floor3 + 
        11.042368 * tv_iwi + 10.456119 * ref_iwi + 0 * phone_iwi + 
        10.359944 * elec_iwi + 6.007127 * car_iwi + 2.752275 * bike_iwi + 
        5.394492 * cheap_iwi + 8.36766 * expensive_iwi - 
        5.204777 * sleepr1 + 0.512317 * sleepr2 + 4.874543 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.892758 - 7.899507 * water1 - 2.795223 * water2 + 9.884065 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.514572 * floor1 + 1.388402 * floor2 + 7.842543 * floor3 + 
        10.725654 * tv_iwi + 10.692824 * ref_iwi + 9.436737 * phone_iwi + 
        0 * elec_iwi + 6.389785 * car_iwi + 2.811789 * bike_iwi + 
        5.398769 * cheap_iwi + 8.696631 * expensive_iwi - 
        5.478679 * sleepr1 + 0.442142 * sleepr2 + 5.228447 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.259927 - 7.830984 * water1 - 2.494674 * water2 + 9.556802 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.436108 * floor1 + 1.696869 * floor2 + 7.462584 * floor3 + 
        10.79091 * tv_iwi + 10.17719 * ref_iwi + 8.913215 * phone_iwi + 
        10.064485 * elec_iwi + 0 * car_iwi + 2.61604 * bike_iwi + 
        5.228499 * cheap_iwi + 8.257722 * expensive_iwi - 
        4.992835 * sleepr1 + 0.494869 * sleepr2 + 4.672625 * sleepr3, 
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.453502 - 7.564703 * water1 - 2.522904 * water2 + 9.330664 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.074668 * floor1 + 1.577709 * floor2 + 7.230118 * floor3 + 
        10.396079 * tv_iwi + 10.012485 * ref_iwi + 8.698114 * phone_iwi + 
        9.66005 * elec_iwi + 5.709795 * car_iwi + 0 * bike_iwi + 
        4.99423 * cheap_iwi + 7.981097 * expensive_iwi - 
        4.81413 * sleepr1 + 0.448743 * sleepr2 + 4.533867 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.992769 - 7.744444 * water1 - 2.557892 * water2 + 9.53055 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.316778 * floor1 + 1.591146 * floor2 + 7.451256 * floor3 + 
        10.668783 * tv_iwi + 10.263454 * ref_iwi + 8.914474 * phone_iwi + 
        9.875305 * elec_iwi + 5.878881 * car_iwi + 2.501721 * bike_iwi + 
        0 * cheap_iwi + 8.269106 * expensive_iwi - 
        4.931547 * sleepr1 + 0.450458 * sleepr2 + 4.653701 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.881013 - 8.082301 * water1 - 2.662554 * water2 + 9.94027 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.663372 * floor1 + 1.664103 * floor2 + 7.714885 * floor3 + 
        10.968049 * tv_iwi + 10.45688 * ref_iwi + 9.030877 * phone_iwi + 
        10.260874 * elec_iwi + 6.012536 * car_iwi + 2.499642 * bike_iwi + 
        5.409591 * cheap_iwi + 0 * expensive_iwi - 
        5.13534 * sleepr1 + 0.48965 * sleepr2 + 4.825383 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        18.000339 - 8.233136 * water1 - 2.708447 * water2 + 10.122461 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.767203 * floor1 + 1.732679 * floor2 + 7.747819 * floor3 + 
        11.190655 * tv_iwi + 10.72557 * ref_iwi + 9.283453 * phone_iwi + 
        10.49657 * elec_iwi + 6.0455 * car_iwi + 2.532787 * bike_iwi + 
        5.299661 * cheap_iwi + 8.555186 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.370207 - 8.058787 * water1 - 3.311234 * water2 + 10.485157 * water3 - 
        9.477805 * toilet1 - 1.702653 * toilet2 + 10.611715 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 10.873586 * ref_iwi + 9.211844 * phone_iwi + 
        9.724121 * elec_iwi + 6.264418 * car_iwi + 2.213782 * bike_iwi + 
        5.377631 * cheap_iwi + 8.360589 * expensive_iwi - 
        4.833614 * sleepr1 + 0.495713 * sleepr2 + 4.506949 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.384299 - 8.095943 * water1 - 2.968341 * water2 + 10.220419 * water3 - 
        9.42374 * toilet1 - 1.429711 * toilet2 + 10.348748 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.860575 * tv_iwi + 0 * ref_iwi + 9.087032 * phone_iwi + 
        10.197897 * elec_iwi + 5.948519 * car_iwi + 2.560903 * bike_iwi + 
        5.518551 * cheap_iwi + 8.320725 * expensive_iwi - 
        4.864617 * sleepr1 + 0.482458 * sleepr2 + 4.552332 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.081055 - 8.016163 * water1 - 3.129099 * water2 + 10.285798 * water3 - 
        9.380867 * toilet1 - 1.541603 * toilet2 + 10.392718 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.734947 * tv_iwi + 10.633189 * ref_iwi + 0 * phone_iwi + 
        10.073189 * elec_iwi + 5.920296 * car_iwi + 2.247969 * bike_iwi + 
        5.266253 * cheap_iwi + 8.081003 * expensive_iwi - 
        4.684025 * sleepr1 + 0.564046 * sleepr2 + 4.283582 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.007048 - 7.835872 * water1 - 3.315434 * water2 + 10.278861 * water3 - 
        9.25572 * toilet1 - 1.738949 * toilet2 + 10.421655 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.434405 * tv_iwi + 10.854999 * ref_iwi + 9.231635 * phone_iwi + 
        0 * elec_iwi + 6.263857 * car_iwi + 2.284868 * bike_iwi + 
        5.261352 * cheap_iwi + 8.377114 * expensive_iwi - 
        4.915456 * sleepr1 + 0.503164 * sleepr2 + 4.584205 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.326099 - 7.750317 * water1 - 2.973922 * water2 + 9.899748 * water3 - 
        9.05261 * toilet1 - 1.469669 * toilet2 + 10.01522 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.493832 * tv_iwi + 10.328227 * ref_iwi + 8.740243 * phone_iwi + 
        9.789344 * elec_iwi + 0 * car_iwi + 2.167541 * bike_iwi + 
        5.111584 * cheap_iwi + 7.983291 * expensive_iwi - 
        4.523173 * sleepr1 + 0.536309 * sleepr2 + 4.14487 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        20.542923 - 7.449356 * water1 - 2.939042 * water2 + 9.585779 * water3 - 
        8.731461 * toilet1 - 1.423227 * toilet2 + 9.664302 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.072466 * tv_iwi + 10.086579 * ref_iwi + 8.489434 * phone_iwi + 
        9.360506 * elec_iwi + 5.603647 * car_iwi + 0 * bike_iwi + 
        4.873341 * cheap_iwi + 7.694077 * expensive_iwi - 
        4.362106 * sleepr1 + 0.487614 * sleepr2 + 4.026947 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.095064 - 7.665647 * water1 - 3.01952 * water2 + 9.859854 * water3 - 
        8.959381 * toilet1 - 1.501624 * toilet2 + 9.948292 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.375616 * tv_iwi + 10.397706 * ref_iwi + 8.737326 * phone_iwi + 
        9.608459 * elec_iwi + 5.790849 * car_iwi + 2.065121 * bike_iwi + 
        0 * cheap_iwi + 7.991365 * expensive_iwi - 
        4.470036 * sleepr1 + 0.495925 * sleepr2 + 4.130347 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.963459 - 7.972913 * water1 - 3.1599 * water2 + 10.271984 * water3 - 
        9.374818 * toilet1 - 1.446535 * toilet2 + 10.313671 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.633937 * tv_iwi + 10.593436 * ref_iwi + 8.834577 * phone_iwi + 
        9.949828 * elec_iwi + 5.909214 * car_iwi + 2.023823 * bike_iwi + 
        5.265493 * cheap_iwi + 0 * expensive_iwi - 
        4.615728 * sleepr1 + 0.536417 * sleepr2 + 4.240579 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        17.42966 - 8.028468 * water1 - 3.160736 * water2 + 10.325044 * water3 - 
        9.401192 * toilet1 - 1.583518 * toilet2 + 10.444901 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.742932 * tv_iwi + 10.736712 * ref_iwi + 8.990701 * phone_iwi + 
        10.068038 * elec_iwi + 5.895382 * car_iwi + 2.065931 * bike_iwi + 
        5.13002 * cheap_iwi + 8.170679 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.31666 - 7.626709 * water1 - 2.696748 * water2 + 9.541032 * water3 - 
        9.003162 * toilet1 - 1.295859 * toilet2 + 9.83302 * toilet3 - 
        9.144416 * floor1 + 1.351928 * floor2 + 7.520151 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 8.410993 * phone_iwi + 
        9.35928 * elec_iwi + 5.568078 * car_iwi + 2.349335 * bike_iwi + 
        5.110362 * cheap_iwi + 7.677061 * expensive_iwi - 
        4.542373 * sleepr1 + 0.387404 * sleepr2 + 4.314028 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.857277 - 7.560295 * water1 - 2.852471 * water2 + 9.614598 * water3 - 
        8.965041 * toilet1 - 1.404051 * toilet2 + 9.87881 * toilet3 - 
        8.942532 * floor1 + 1.224869 * floor2 + 7.449929 * floor3 + 
        0 * tv_iwi + 9.843277 * ref_iwi + 0 * phone_iwi + 
        9.265424 * elec_iwi + 5.554053 * car_iwi + 2.07932 * bike_iwi + 
        4.896429 * cheap_iwi + 7.479652 * expensive_iwi - 
        4.389409 * sleepr1 + 0.46166 * sleepr2 + 4.081232 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.600106 - 7.388232 * water1 - 3.012764 * water2 + 9.592648 * water3 - 
        8.841448 * toilet1 - 1.576446 * toilet2 + 9.890083 * toilet3 - 
        8.783935 * floor1 + 1.097313 * floor2 + 7.422105 * floor3 + 
        0 * tv_iwi + 10.027259 * ref_iwi + 8.541028 * phone_iwi + 
        0 * elec_iwi + 5.851375 * car_iwi + 2.113087 * bike_iwi + 
        4.885044 * cheap_iwi + 7.733564 * expensive_iwi - 
        4.58649 * sleepr1 + 0.403363 * sleepr2 + 4.343701 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.042938 - 7.345998 * water1 - 2.724356 * water2 + 9.300759 * water3 - 
        8.687163 * toilet1 - 1.343691 * toilet2 + 9.559658 * toilet3 - 
        8.750733 * floor1 + 1.374192 * floor2 + 7.117093 * floor3 + 
        0 * tv_iwi + 9.60852 * ref_iwi + 8.150325 * phone_iwi + 
        9.05483 * elec_iwi + 0 * car_iwi + 2.00928 * bike_iwi + 
        4.775584 * cheap_iwi + 7.422474 * expensive_iwi - 
        4.259044 * sleepr1 + 0.449425 * sleepr2 + 3.95854 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.9726 - 7.072068 * water1 - 2.70104 * water2 + 9.022359 * water3 - 
        8.388392 * toilet1 - 1.306697 * toilet2 + 9.237969 * toilet3 - 
        8.396025 * floor1 + 1.271626 * floor2 + 6.87479 * floor3 + 
        0 * tv_iwi + 9.40365 * ref_iwi + 7.933039 * phone_iwi + 
        8.679725 * elec_iwi + 5.286926 * car_iwi + 0 * bike_iwi + 
        4.565174 * cheap_iwi + 7.171686 * expensive_iwi - 
        4.116115 * sleepr1 + 0.408022 * sleepr2 + 3.852082 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.705162 - 7.262864 * water1 - 2.767754 * water2 + 9.26039 * water3 - 
        8.59438 * toilet1 - 1.374529 * toilet2 + 9.492308 * toilet3 - 
        8.638692 * floor1 + 1.285763 * floor2 + 7.095777 * floor3 + 
        0 * tv_iwi + 9.670194 * ref_iwi + 8.144919 * phone_iwi + 
        8.887511 * elec_iwi + 5.451238 * car_iwi + 1.918704 * bike_iwi + 
        0 * cheap_iwi + 7.428879 * expensive_iwi - 
        4.209226 * sleepr1 + 0.411569 * sleepr2 + 3.944918 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.721589 - 7.527052 * water1 - 2.8801 * water2 + 9.607438 * water3 - 
        8.960953 * toilet1 - 1.317746 * toilet2 + 9.808425 * toilet3 - 
        8.902254 * floor1 + 1.328318 * floor2 + 7.308986 * floor3 + 
        0 * tv_iwi + 9.816206 * ref_iwi + 8.208914 * phone_iwi + 
        9.166629 * elec_iwi + 5.544814 * car_iwi + 1.877385 * bike_iwi + 
        4.899203 * cheap_iwi + 0 * expensive_iwi - 
        4.33133 * sleepr1 + 0.442404 * sleepr2 + 4.040411 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.547661 - 7.601749 * water1 - 2.893772 * water2 + 9.689747 * water3 - 
        9.010746 * toilet1 - 1.447502 * toilet2 + 9.957084 * toilet3 - 
        8.935166 * floor1 + 1.372431 * floor2 + 7.297374 * floor3 + 
        0 * tv_iwi + 9.979964 * ref_iwi + 8.376572 * phone_iwi + 
        9.305855 * elec_iwi + 5.547643 * car_iwi + 1.915484 * bike_iwi + 
        4.791334 * cheap_iwi + 7.591281 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.891306 - 7.553037 * water1 - 2.54821 * water2 + 9.341797 * water3 - 
        8.873334 * toilet1 - 1.16463 * toilet2 + 9.604677 * toilet3 - 
        9.074868 * floor1 + 1.426957 * floor2 + 7.378879 * floor3 + 
        10.061316 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 
        9.627653 * elec_iwi + 5.257074 * car_iwi + 2.351386 * bike_iwi + 
        4.983893 * cheap_iwi + 7.41168 * expensive_iwi - 
        4.390067 * sleepr1 + 0.453255 * sleepr2 + 4.09034 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.931403 - 7.451801 * water1 - 2.731645 * water2 + 9.406788 * water3 - 
        8.835021 * toilet1 - 1.350441 * toilet2 + 9.709969 * toilet3 - 
        9.003298 * floor1 + 1.303352 * floor2 + 7.431409 * floor3 + 
        9.854482 * tv_iwi + 0 * ref_iwi + 8.474964 * phone_iwi + 
        0 * elec_iwi + 5.607324 * car_iwi + 2.418032 * bike_iwi + 
        5.026532 * cheap_iwi + 7.732083 * expensive_iwi - 
        4.641284 * sleepr1 + 0.39679 * sleepr2 + 4.407015 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.004592 - 7.320651 * water1 - 2.442827 * water2 + 9.03079 * water3 - 
        8.582553 * toilet1 - 1.122639 * toilet2 + 9.286987 * toilet3 - 
        8.849856 * floor1 + 1.548149 * floor2 + 7.041614 * floor3 + 
        9.855775 * tv_iwi + 0 * ref_iwi + 8.003463 * phone_iwi + 
        9.36967 * elec_iwi + 0 * car_iwi + 2.258126 * bike_iwi + 
        4.845354 * cheap_iwi + 7.342435 * expensive_iwi - 
        4.251533 * sleepr1 + 0.439023 * sleepr2 + 3.961192 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.136103 - 7.101522 * water1 - 2.454839 * water2 + 8.834889 * water3 - 
        8.352813 * toilet1 - 1.105385 * toilet2 + 9.048232 * toilet3 - 
        8.552114 * floor1 + 1.453205 * floor2 + 6.846945 * floor3 + 
        9.541816 * tv_iwi + 0 * ref_iwi + 7.841538 * phone_iwi + 
        9.039019 * elec_iwi + 5.047741 * car_iwi + 0 * bike_iwi + 
        4.657816 * cheap_iwi + 7.134418 * expensive_iwi - 
        4.129655 * sleepr1 + 0.402653 * sleepr2 + 3.871482 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.829178 - 7.280055 * water1 - 2.499657 * water2 + 9.04223 * water3 - 
        8.539203 * toilet1 - 1.161007 * toilet2 + 9.273946 * toilet3 - 
        8.788514 * floor1 + 1.471003 * floor2 + 7.058259 * floor3 + 
        9.794681 * tv_iwi + 0 * ref_iwi + 8.036443 * phone_iwi + 
        9.249955 * elec_iwi + 5.193097 * car_iwi + 2.173513 * bike_iwi + 
        0 * cheap_iwi + 7.38438 * expensive_iwi - 
        4.221406 * sleepr1 + 0.404794 * sleepr2 + 3.964319 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.789604 - 7.528799 * water1 - 2.585647 * water2 + 9.351692 * water3 - 
        8.88145 * toilet1 - 1.086863 * toilet2 + 9.552837 * toilet3 - 
        9.042056 * floor1 + 1.523953 * floor2 + 7.251524 * floor3 + 
        9.989494 * tv_iwi + 0 * ref_iwi + 8.079361 * phone_iwi + 
        9.532579 * elec_iwi + 5.25935 * car_iwi + 2.149041 * bike_iwi + 
        4.990334 * cheap_iwi + 0 * expensive_iwi - 
        4.337299 * sleepr1 + 0.434828 * sleepr2 + 4.054185 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.639223 - 7.612152 * water1 - 2.610324 * water2 + 9.451774 * water3 - 
        8.944334 * toilet1 - 1.222481 * toilet2 + 9.718852 * toilet3 - 
        9.082737 * floor1 + 1.564714 * floor2 + 7.250736 * floor3 + 
        10.127605 * tv_iwi + 0 * ref_iwi + 8.256066 * phone_iwi + 
        9.675764 * elec_iwi + 5.273206 * car_iwi + 2.181739 * bike_iwi + 
        4.887081 * cheap_iwi + 7.537954 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.491747 - 7.394196 * water1 - 2.881664 * water2 + 9.483666 * water3 - 
        8.804419 * toilet1 - 1.453133 * toilet2 + 9.758907 * toilet3 - 
        8.809524 * floor1 + 1.180818 * floor2 + 7.364581 * floor3 + 
        9.766838 * tv_iwi + 9.876689 * ref_iwi + 0 * phone_iwi + 
        0 * elec_iwi + 5.589318 * car_iwi + 2.147365 * bike_iwi + 
        4.81832 * cheap_iwi + 7.531714 * expensive_iwi - 
        4.483608 * sleepr1 + 0.469536 * sleepr2 + 4.170854 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.587917 - 7.256572 * water1 - 2.579151 * water2 + 9.089599 * water3 - 
        8.543626 * toilet1 - 1.219323 * toilet2 + 9.323134 * toilet3 - 
        8.664316 * floor1 + 1.425387 * floor2 + 6.982982 * floor3 + 
        9.755541 * tv_iwi + 9.374732 * ref_iwi + 0 * phone_iwi + 
        9.263759 * elec_iwi + 0 * car_iwi + 2.025758 * bike_iwi + 
        4.660404 * cheap_iwi + 7.167151 * expensive_iwi - 
        4.123403 * sleepr1 + 0.498397 * sleepr2 + 3.769023 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.651546 - 7.016516 * water1 - 2.569638 * water2 + 8.855171 * water3 - 
        8.285289 * toilet1 - 1.193655 * toilet2 + 9.049842 * toilet3 - 
        8.349618 * floor1 + 1.330484 * floor2 + 6.771858 * floor3 + 
        9.420248 * tv_iwi + 9.205892 * ref_iwi + 0 * phone_iwi + 
        8.912557 * elec_iwi + 5.025662 * car_iwi + 0 * bike_iwi + 
        4.474251 * cheap_iwi + 6.951487 * expensive_iwi - 
        4.000123 * sleepr1 + 0.458411 * sleepr2 + 3.681487 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.343937 - 7.197585 * water1 - 2.626108 * water2 + 9.075085 * water3 - 
        8.478879 * toilet1 - 1.252111 * toilet2 + 9.284802 * toilet3 - 
        8.581339 * floor1 + 1.347403 * floor2 + 6.979508 * floor3 + 
        9.672073 * tv_iwi + 9.452197 * ref_iwi + 0 * phone_iwi + 
        9.124954 * elec_iwi + 5.171129 * car_iwi + 1.943091 * bike_iwi + 
        0 * cheap_iwi + 7.189009 * expensive_iwi - 
        4.086133 * sleepr1 + 0.464709 * sleepr2 + 3.764215 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.265988 - 7.438648 * water1 - 2.719902 * water2 + 9.384134 * water3 - 
        8.810781 * toilet1 - 1.191868 * toilet2 + 9.564229 * toilet3 - 
        8.823119 * floor1 + 1.392684 * floor2 + 7.168943 * floor3 + 
        9.865469 * tv_iwi + 9.576555 * ref_iwi + 0 * phone_iwi + 
        9.398654 * elec_iwi + 5.242386 * car_iwi + 1.909482 * bike_iwi + 
        4.780809 * cheap_iwi + 0 * expensive_iwi - 
        4.193441 * sleepr1 + 0.49658 * sleepr2 + 3.843351 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.218955 - 7.507656 * water1 - 2.73673 * water2 + 9.463844 * water3 - 
        8.858211 * toilet1 - 1.314334 * toilet2 + 9.704962 * toilet3 - 
        8.853087 * floor1 + 1.428008 * floor2 + 7.163143 * floor3 + 
        9.984526 * tv_iwi + 9.730332 * ref_iwi + 0 * phone_iwi + 
        9.522143 * elec_iwi + 5.251106 * car_iwi + 1.945163 * bike_iwi + 
        4.683667 * cheap_iwi + 7.33216 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.673282 - 7.184856 * water1 - 2.748133 * water2 + 9.169762 * water3 - 
        8.529739 * toilet1 - 1.386935 * toilet2 + 9.438405 * toilet3 - 
        8.618104 * floor1 + 1.330988 * floor2 + 7.031275 * floor3 + 
        9.59556 * tv_iwi + 9.629836 * ref_iwi + 8.196453 * phone_iwi + 
        0 * elec_iwi + 0 * car_iwi + 2.070137 * bike_iwi + 
        4.697668 * cheap_iwi + 7.461499 * expensive_iwi - 
        4.340583 * sleepr1 + 0.456238 * sleepr2 + 4.036122 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.626547 - 6.922551 * water1 - 2.721651 * water2 + 8.899544 * water3 - 
        8.241821 * toilet1 - 1.345588 * toilet2 + 9.12402 * toilet3 - 
        8.272806 * floor1 + 1.233975 * floor2 + 6.792609 * floor3 + 
        9.2365 * tv_iwi + 9.421447 * ref_iwi + 7.973633 * phone_iwi + 
        0 * elec_iwi + 5.308071 * car_iwi + 0 * bike_iwi + 
        4.491282 * cheap_iwi + 7.205378 * expensive_iwi - 
        4.18937 * sleepr1 + 0.414955 * sleepr2 + 3.920968 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.298334 - 7.093946 * water1 - 2.783359 * water2 + 9.114925 * water3 - 
        8.426502 * toilet1 - 1.411858 * toilet2 + 9.356246 * toilet3 - 
        8.495681 * floor1 + 1.24376 * floor2 + 6.998727 * floor3 + 
        9.475075 * tv_iwi + 9.671794 * ref_iwi + 8.175076 * phone_iwi + 
        0 * elec_iwi + 5.46456 * car_iwi + 1.978542 * bike_iwi + 
        0 * cheap_iwi + 7.453013 * expensive_iwi - 
        4.282204 * sleepr1 + 0.418312 * sleepr2 + 4.013708 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.351925 - 7.362142 * water1 - 2.906072 * water2 + 9.474809 * water3 - 
        8.800013 * toilet1 - 1.365284 * toilet2 + 9.687024 * toilet3 - 
        8.768872 * floor1 + 1.284117 * floor2 + 7.223423 * floor3 + 
        9.68816 * tv_iwi + 9.844135 * ref_iwi + 8.263677 * phone_iwi + 
        0 * elec_iwi + 5.575868 * car_iwi + 1.944864 * bike_iwi + 
        4.820659 * cheap_iwi + 0 * expensive_iwi - 
        4.420899 * sleepr1 + 0.450067 * sleepr2 + 4.125454 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.132671 - 7.450701 * water1 - 2.923628 * water2 + 9.573571 * water3 - 
        8.866269 * toilet1 - 1.495139 * toilet2 + 9.851916 * toilet3 - 
        8.815702 * floor1 + 1.331178 * floor2 + 7.222379 * floor3 + 
        9.832521 * tv_iwi + 10.0192 * ref_iwi + 8.438689 * phone_iwi + 
        0 * elec_iwi + 5.584036 * car_iwi + 1.98057 * bike_iwi + 
        4.719791 * cheap_iwi + 7.644657 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.882831 - 6.813187 * water1 - 2.462844 * water2 + 8.5703 * water3 - 
        8.028262 * toilet1 - 1.148751 * toilet2 + 8.763041 * toilet3 - 
        8.156235 * floor1 + 1.428759 * floor2 + 6.487796 * floor3 + 
        9.232891 * tv_iwi + 8.97908 * ref_iwi + 7.571804 * phone_iwi + 
        8.687875 * elec_iwi + 0 * car_iwi + 0 * bike_iwi + 
        4.364446 * cheap_iwi + 6.881984 * expensive_iwi - 
        3.885147 * sleepr1 + 0.442958 * sleepr2 + 3.577953 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.549834 - 6.987184 * water1 - 2.515685 * water2 + 8.78038 * water3 - 
        8.214118 * toilet1 - 1.203105 * toilet2 + 8.987256 * toilet3 - 
        8.380685 * floor1 + 1.453077 * floor2 + 6.681115 * floor3 + 
        9.47802 * tv_iwi + 9.216679 * ref_iwi + 7.761558 * phone_iwi + 
        8.893829 * elec_iwi + 0 * car_iwi + 1.880627 * bike_iwi + 
        0 * cheap_iwi + 7.113561 * expensive_iwi - 
        3.967848 * sleepr1 + 0.449366 * sleepr2 + 3.657143 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.4801 - 7.230059 * water1 - 2.607604 * water2 + 9.089498 * water3 - 
        8.543786 * toilet1 - 1.148631 * toilet2 + 9.268927 * toilet3 - 
        8.629731 * floor1 + 1.507555 * floor2 + 6.868523 * floor3 + 
        9.684214 * tv_iwi + 9.354522 * ref_iwi + 7.826762 * phone_iwi + 
        9.172697 * elec_iwi + 0 * car_iwi + 1.850706 * bike_iwi + 
        4.66567 * cheap_iwi + 0 * expensive_iwi - 
        4.076523 * sleepr1 + 0.480553 * sleepr2 + 3.738382 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        24.471336 - 7.273647 * water1 - 2.617057 * water2 + 9.138817 * water3 - 
        8.564269 * toilet1 - 1.260341 * toilet2 + 9.37494 * toilet3 - 
        8.63342 * floor1 + 1.531422 * floor2 + 6.848572 * floor3 + 
        9.770298 * tv_iwi + 9.476376 * ref_iwi + 7.957398 * phone_iwi + 
        9.262475 * elec_iwi + 0 * car_iwi + 1.884064 * bike_iwi + 
        4.564039 * cheap_iwi + 7.251685 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.611255 - 6.746045 * water1 - 2.496416 * water2 + 8.536405 * water3 - 
        7.952918 * toilet1 - 1.174307 * toilet2 + 8.708745 * toilet3 - 
        8.06549 * floor1 + 1.355626 * floor2 + 6.472023 * floor3 + 
        9.140141 * tv_iwi + 9.026479 * ref_iwi + 7.56639 * phone_iwi + 
        8.547361 * elec_iwi + 4.934855 * car_iwi + 0 * bike_iwi + 
        0 * cheap_iwi + 6.887748 * expensive_iwi - 
        3.846801 * sleepr1 + 0.412691 * sleepr2 + 3.568599 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.482264 - 6.972871 * water1 - 2.586276 * water2 + 8.828605 * water3 - 
        8.26217 * toilet1 - 1.124595 * toilet2 + 8.974041 * toilet3 - 
        8.296965 * floor1 + 1.40254 * floor2 + 6.649872 * floor3 + 
        9.332669 * tv_iwi + 9.161458 * ref_iwi + 7.630973 * phone_iwi + 
        8.806241 * elec_iwi + 5.012557 * car_iwi + 0 * bike_iwi + 
        4.473278 * cheap_iwi + 0 * expensive_iwi - 
        3.950259 * sleepr1 + 0.440281 * sleepr2 + 3.648042 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        23.620536 - 7.021352 * water1 - 2.595284 * water2 + 8.882143 * water3 - 
        8.289736 * toilet1 - 1.229779 * toilet2 + 9.081987 * toilet3 - 
        8.309449 * floor1 + 1.429623 * floor2 + 6.635268 * floor3 + 
        9.422529 * tv_iwi + 9.281201 * ref_iwi + 7.758235 * phone_iwi + 
        8.900738 * elec_iwi + 5.011815 * car_iwi + 0 * bike_iwi + 
        4.381417 * cheap_iwi + 7.024131 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.242782 - 7.172829 * water1 - 2.653745 * water2 + 9.075925 * water3 - 
        8.480775 * toilet1 - 1.181173 * toilet2 + 9.232108 * toilet3 - 
        8.548947 * floor1 + 1.430635 * floor2 + 6.866122 * floor3 + 
        9.602921 * tv_iwi + 9.431582 * ref_iwi + 7.839412 * phone_iwi + 
        9.036986 * elec_iwi + 5.168451 * car_iwi + 1.769891 * bike_iwi + 
        0 * cheap_iwi + 0 * expensive_iwi - 
        4.040232 * sleepr1 + 0.447629 * sleepr2 + 3.73382 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        24.233364 - 7.204903 * water1 - 2.655516 * water2 + 9.107684 * water3 - 
        8.487908 * toilet1 - 1.287816 * toilet2 + 9.321122 * toilet3 - 
        8.540553 * floor1 + 1.454701 * floor2 + 6.83428 * floor3 + 
        9.675259 * tv_iwi + 9.533649 * ref_iwi + 7.956205 * phone_iwi + 
        9.115277 * elec_iwi + 5.156142 * car_iwi + 1.806454 * bike_iwi + 
        0 * cheap_iwi + 7.260563 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.124881 - 7.470302 * water1 - 2.760658 * water2 + 9.449576 * water3 - 
        8.846047 * toilet1 - 1.238888 * toilet2 + 9.635002 * toilet3 - 
        8.808532 * floor1 + 1.511305 * floor2 + 7.03792 * floor3 + 
        9.901453 * tv_iwi + 9.697919 * ref_iwi + 8.036793 * phone_iwi + 
        9.41747 * elec_iwi + 5.244444 * car_iwi + 1.767893 * bike_iwi + 
        4.686649 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      # at least 3 assets are missing
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        7.989074 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        14.071748 * tv_iwi + 13.306038 * ref_iwi + 12.330774 * phone_iwi + 
        12.627978 * elec_iwi + 8.403704 * car_iwi + 4.637857 * bike_iwi + 
        7.462431 * cheap_iwi + 11.754889 * expensive_iwi - 
        7.989074 * sleepr1 + 0.852887 * sleepr2 + 7.415506 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        18.881685 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.51067 * floor1 + 1.551035 * floor2 + 9.614643 * floor3 + 
        0 * tv_iwi + 12.176925 * ref_iwi + 11.301477 * phone_iwi + 
        11.441903 * elec_iwi + 7.816534 * car_iwi + 4.207961 * bike_iwi + 
        6.853574 * cheap_iwi + 10.72826 * expensive_iwi - 
        7.371015 * sleepr1 + 0.652029 * sleepr2 + 6.977037 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        18.82346 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.632219 * floor1 + 1.915579 * floor2 + 9.373046 * floor3 + 
        12.968756 * tv_iwi + 0 * ref_iwi + 10.924901 * phone_iwi + 
        11.99176 * elec_iwi + 7.178097 * car_iwi + 4.556757 * bike_iwi + 
        6.906846 * cheap_iwi + 10.462514 * expensive_iwi - 
        7.191241 * sleepr1 + 0.629155 * sleepr2 + 6.813862 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        18.569837 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.551948 * floor1 + 1.732758 * floor2 + 9.475511 * floor3 + 
        13.052566 * tv_iwi + 11.945718 * ref_iwi + 0 * phone_iwi + 
        12.082555 * elec_iwi + 7.279367 * car_iwi + 4.134762 * bike_iwi + 
        6.676971 * cheap_iwi + 10.291829 * expensive_iwi - 
        7.017889 * sleepr1 + 0.77231 * sleepr2 + 6.490885 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        18.544609 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.177192 * floor1 + 1.486995 * floor2 + 9.354923 * floor3 + 
        12.470247 * tv_iwi + 12.142658 * ref_iwi + 11.268061 * phone_iwi + 
        0 * elec_iwi + 7.759099 * car_iwi + 4.207211 * bike_iwi + 
        6.619992 * cheap_iwi + 10.66787 * expensive_iwi - 
        7.367417 * sleepr1 + 0.659991 * sleepr2 + 6.965331 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.852146 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.229673 * floor1 + 1.998124 * floor2 + 8.902002 * floor3 + 
        12.737278 * tv_iwi + 11.629352 * ref_iwi + 10.652099 * phone_iwi + 
        11.721423 * elec_iwi + 0 * car_iwi + 3.840562 * bike_iwi + 
        6.420937 * cheap_iwi + 10.122408 * expensive_iwi - 
        6.622472 * sleepr1 + 0.732155 * sleepr2 + 6.121793 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.167144 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.812501 * floor1 + 1.863424 * floor2 + 8.630896 * floor3 + 
        12.281675 * tv_iwi + 11.536901 * ref_iwi + 10.431837 * phone_iwi + 
        11.258882 * elec_iwi + 6.902389 * car_iwi + 0 * bike_iwi + 
        6.101942 * cheap_iwi + 9.774071 * expensive_iwi - 
        6.354642 * sleepr1 + 0.662595 * sleepr2 + 5.914263 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.597483 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.070979 * floor1 + 1.843832 * floor2 + 8.900431 * floor3 + 
        12.578384 * tv_iwi + 11.777845 * ref_iwi + 10.668609 * phone_iwi + 
        11.466536 * elec_iwi + 7.106373 * car_iwi + 3.662351 * bike_iwi + 
        0 * cheap_iwi + 10.149729 * expensive_iwi - 
        6.526504 * sleepr1 + 0.662515 * sleepr2 + 6.09226 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        18.534393 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.596821 * floor1 + 1.945843 * floor2 + 9.308953 * floor3 + 
        13.019886 * tv_iwi + 12.018855 * ref_iwi + 10.853288 * phone_iwi + 
        12.016165 * elec_iwi + 7.323405 * car_iwi + 3.754326 * bike_iwi + 
        6.733952 * cheap_iwi + 0 * expensive_iwi - 
        6.937572 * sleepr1 + 0.743343 * sleepr2 + 6.436778 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        11.906348 + 0 * water1 + 0 * water2 + 0 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.906348 * floor1 + 2.096063 * floor2 + 9.460554 * floor3 + 
        13.525541 * tv_iwi + 12.587971 * ref_iwi + 11.366657 * phone_iwi + 
        12.545717 * elec_iwi + 7.447126 * car_iwi + 3.80184 * bike_iwi + 
        6.619637 * cheap_iwi + 10.738608 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.817161 + 0 * water1 + 0 * water2 + 0 * water3 - 
        11.16633 * toilet1 - 1.623443 * toilet2 + 12.208059 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 0 * tv_iwi + 
        12.717441 * ref_iwi + 11.272888 * phone_iwi + 11.3483 * elec_iwi + 
        7.806087 * car_iwi + 3.456442 * bike_iwi + 6.78582 * cheap_iwi + 
        10.490772 * expensive_iwi - 6.650832 * sleepr1 + 0.786139 * sleepr2 + 
        6.097031 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.664491 + 0 * water1 + 0 * water2 + 0 * water3 - 
        11.033812 * toilet1 - 1.217215 * toilet2 + 11.765591 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.942859 * tv_iwi + 
        0 * ref_iwi + 11.019596 * phone_iwi + 11.983249 * elec_iwi + 
        7.285045 * car_iwi + 3.924965 * bike_iwi + 6.945946 * cheap_iwi + 
        10.359241 * expensive_iwi - 6.630679 * sleepr1 + 0.753373 * sleepr2 + 
        6.109017 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.49586 + 0 * water1 + 0 * water2 + 0 * water3 - 
        11.11657 * toilet1 - 1.420285 * toilet2 + 12.002985 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.902022 * tv_iwi + 
        12.454431 * ref_iwi + 0 * phone_iwi + 11.954523 * elec_iwi + 
        7.303149 * car_iwi + 3.452105 * bike_iwi + 6.629512 * cheap_iwi + 
        10.087682 * expensive_iwi - 6.37929 * sleepr1 + 0.884081 * sleepr2 + 
        5.717732 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.485772 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.795543 * toilet1 - 1.690915 * toilet2 + 11.896027 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.343316 * tv_iwi + 
        12.643891 * ref_iwi + 11.234613 * phone_iwi + 0 * elec_iwi + 
        7.746636 * car_iwi + 3.503304 * bike_iwi + 6.563595 * cheap_iwi + 
        10.441908 * expensive_iwi - 6.690229 * sleepr1 + 0.783025 * sleepr2 + 
        6.140939 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.71528 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.663848 * toilet1 - 1.368403 * toilet2 + 11.518746 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.560968 * tv_iwi + 
        12.054923 * ref_iwi + 10.606565 * phone_iwi + 11.567318 * elec_iwi + 
        0 * car_iwi + 3.2461 * bike_iwi + 6.371108 * cheap_iwi + 
        9.913521 * expensive_iwi - 6.051431 * sleepr1 + 0.817101 * sleepr2 + 
        5.445472 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.07421 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.28966 * toilet1 - 1.339167 * toilet2 + 11.129004 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.020589 * tv_iwi + 
        11.815226 * ref_iwi + 10.296843 * phone_iwi + 11.025587 * elec_iwi + 
        6.854165 * car_iwi + 0 * bike_iwi + 6.023111 * cheap_iwi + 
        9.510778 * expensive_iwi - 5.784549 * sleepr1 + 0.736005 * sleepr2 + 
        5.250488 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.504306 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.540458 * toilet1 - 1.42815 * toilet2 + 11.443589 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.400893 * tv_iwi + 
        12.179971 * ref_iwi + 10.614792 * phone_iwi + 11.315414 * elec_iwi + 
        7.107297 * car_iwi + 3.079713 * bike_iwi + 0 * cheap_iwi + 
        9.933815 * expensive_iwi - 5.963848 * sleepr1 + 0.751858 * sleepr2 + 
        5.420211 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        17.452601 + 0 * water1 + 0 * water2 + 0 * water3 - 
        11.174467 * toilet1 - 1.283697 * toilet2 + 11.954768 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 12.806247 * tv_iwi + 
        12.456522 * ref_iwi + 10.787141 * phone_iwi + 11.829535 * elec_iwi + 
        7.312912 * car_iwi + 3.092768 * bike_iwi + 6.651042 * cheap_iwi + 
        0 * expensive_iwi - 6.278135 * sleepr1 + 0.84074 * sleepr2 + 
        5.656464 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        11.326667 + 0 * water1 + 0 * water2 + 0 * water3 - 
        11.326667 * toilet1 - 1.545107 * toilet2 + 12.305184 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 13.082677 * tv_iwi + 
        12.799504 * ref_iwi + 11.109044 * phone_iwi + 12.127478 * elec_iwi + 
        7.335237 * car_iwi + 3.129648 * bike_iwi + 6.464602 * cheap_iwi + 
        10.319958 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.414915 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.466603 * toilet1 - 1.087803 * toilet2 + 11.109362 * toilet3 - 
        10.898695 * floor1 + 1.507471 * floor2 + 9.065136 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 10.02177 * phone_iwi + 10.803323 * elec_iwi + 
        6.715324 * car_iwi + 3.505779 * bike_iwi + 6.308069 * cheap_iwi + 
        9.376162 * expensive_iwi - 6.049617 * sleepr1 + 0.581131 * sleepr2 + 
        5.680161 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.093703 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.528397 * toilet1 - 1.272708 * toilet2 + 11.31221 * toilet3 - 
        10.720081 * floor1 + 1.344751 * floor2 + 9.052588 * floor3 + 
        0 * tv_iwi + 11.336974 * ref_iwi + 0 * phone_iwi + 10.789039 * elec_iwi + 
        6.743733 * car_iwi + 3.112872 * bike_iwi + 6.046764 * cheap_iwi + 
        9.158063 * expensive_iwi - 5.845225 * sleepr1 + 0.695352 * sleepr2 + 
        5.354056 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.732819 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.238302 * toilet1 - 1.50588 * toilet2 + 11.206805 * toilet3 - 
        10.395643 * floor1 + 1.148568 * floor2 + 8.931848 * floor3 + 
        0 * tv_iwi + 11.494134 * ref_iwi + 10.208342 * phone_iwi + 0 * elec_iwi + 
        7.118148 * car_iwi + 3.157715 * bike_iwi + 5.983135 * cheap_iwi + 
        9.456964 * expensive_iwi - 6.098874 * sleepr1 + 0.602141 * sleepr2 + 
        5.71009 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.215188 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.160836 * toilet1 - 1.228722 * toilet2 + 10.917628 * toilet3 - 
        10.459537 * floor1 + 1.592924 * floor2 + 8.555783 * floor3 + 
        0 * tv_iwi + 11.053339 * ref_iwi + 9.738923 * phone_iwi + 10.527676 * elec_iwi + 
        0 * car_iwi + 2.944805 * bike_iwi + 5.856959 * cheap_iwi + 
        9.06459 * expensive_iwi - 5.594815 * sleepr1 + 0.665144 * sleepr2 + 
        5.125109 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.183583 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.811724 * toilet1 - 1.20926 * toilet2 + 10.560013 * toilet3 - 
        10.00721 * floor1 + 1.468047 * floor2 + 8.240963 * floor3 + 
        0 * tv_iwi + 10.857946 * ref_iwi + 9.477657 * phone_iwi + 10.064847 * elec_iwi + 
        6.380633 * car_iwi + 0 * bike_iwi + 5.556912 * cheap_iwi + 
        8.724243 * expensive_iwi - 5.364649 * sleepr1 + 0.598942 * sleepr2 + 
        4.953203 * sleepr3,    
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.852817 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.035846 * toilet1 - 1.284931 * toilet2 + 10.838179 * toilet3 - 
        10.303353 * floor1 + 1.462472 * floor2 + 8.533148 * floor3 + 
        0 * tv_iwi + 11.159928 * ref_iwi + 9.739687 * phone_iwi + 10.297075 * elec_iwi + 
        6.59715 * car_iwi + 2.8013 * bike_iwi + 0 * cheap_iwi + 
        9.079333 * expensive_iwi - 5.513618 * sleepr1 + 0.604965 * sleepr2 + 
        5.101382 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        27.054349 + 0 * water1 + 0 * water2 + 0 * water3 + 
        -90.579716 * toilet1 + -9.150049 * toilet2 + 11.268249 * toilet3 + 
        -90.711436 * floor1 + 1.521008 * floor2 + 8.870515 * floor3 + 
        0 * tv_iwi + 11.349224 * ref_iwi + 9.845117 * phone_iwi + 10.696705 * elec_iwi + 
        6.751387 * car_iwi + 2.798265 * bike_iwi + 6.070044 * cheap_iwi + 
        0 * expensive_iwi + -5.763198 * sleepr1 + 0.668414 * sleepr2 + 
        5.296146 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        ##is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        21.604998 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.756377 * toilet1 - 1.38367 * toilet2 + 11.621303 * toilet3 - 
        10.848621 * floor1 + 1.610562 * floor2 + 8.915063 * floor3 + 
        0 * tv_iwi + 11.702009 * ref_iwi + 10.171682 * phone_iwi + 11.007562 * elec_iwi + 
        6.797162 * car_iwi + 2.83582 * bike_iwi + 5.932464 * cheap_iwi + 
        9.411936 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.974029 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.347922 * toilet1 - 0.936915 * toilet2 + 10.876839 * toilet3 - 
        10.84284 * floor1 + 1.63973 * floor2 + 8.880721 * floor3 + 
        11.85085 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 11.250797 * elec_iwi + 
        6.275616 * car_iwi + 3.451145 * bike_iwi + 6.126531 * cheap_iwi + 
        9.001577 * expensive_iwi - 5.783267 * sleepr1 + 0.673428 * sleepr2 + 
        5.311896 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.996104 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.193008 * toilet1 - 1.180348 * toilet2 + 10.912003 * toilet3 - 
        10.659111 * floor1 + 1.441927 * floor2 + 8.897795 * floor3 + 
        11.474739 * tv_iwi + 0 * ref_iwi + 10.074084 * phone_iwi + 0 * elec_iwi + 
        6.740375 * car_iwi + 3.565939 * bike_iwi + 6.156404 * cheap_iwi + 
        9.412153 * expensive_iwi - 6.143985 * sleepr1 + 0.588562 * sleepr2 + 
        5.770404 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.020597 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.963957 * toilet1 - 0.924921 * toilet2 + 10.49076 * toilet3 - 
        10.531089 * floor1 + 1.83339 * floor2 + 8.388067 * floor3 + 
        11.579246 * tv_iwi + 0 * ref_iwi + 9.477558 * phone_iwi + 10.914424 * elec_iwi + 
        0 * car_iwi + 3.249245 * bike_iwi + 5.911159 * cheap_iwi + 
        8.890558 * expensive_iwi - 5.525552 * sleepr1 + 0.640227 * sleepr2 + 
        5.078385 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.261895 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.731451 * toilet1 - 0.928053 * toilet2 + 10.264968 * toilet3 - 
        10.184456 * floor1 + 1.724606 * floor2 + 8.159708 * floor3 + 
        11.215757 * tv_iwi + 0 * ref_iwi + 9.312294 * phone_iwi + 10.533234 * elec_iwi + 
        6.014988 * car_iwi + 0 * bike_iwi + 5.659428 * cheap_iwi + 
        8.631102 * expensive_iwi - 5.345988 * sleepr1 + 0.586232 * sleepr2 + 
        4.946626 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.87122 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.920774 * toilet1 - 0.985941 * toilet2 + 10.495303 * toilet3 - 
        10.46353 * floor1 + 1.724212 * floor2 + 8.430261 * floor3 + 
        11.513542 * tv_iwi + 0 * ref_iwi + 9.542543 * phone_iwi + 10.764662 * elec_iwi + 
        6.198006 * car_iwi + 3.127286 * bike_iwi + 0 * cheap_iwi + 
        8.967632 * expensive_iwi - 5.486917 * sleepr1 + 0.589201 * sleepr2 + 
        5.089545 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.976751 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.415288 * toilet1 - 0.826263 * toilet2 + 10.857863 * toilet3 - 
        10.846792 * floor1 + 1.802378 * floor2 + 8.724255 * floor3 + 
        11.79622 * tv_iwi + 0 * ref_iwi + 9.608366 * phone_iwi + 11.166874 * elec_iwi + 
        6.299606 * car_iwi + 3.147323 * bike_iwi + 6.157166 * cheap_iwi + 
        0 * expensive_iwi - 5.714671 * sleepr1 + 0.648795 * sleepr2 + 
        5.265578 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        21.606377 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.608837 * toilet1 - 1.059774 * toilet2 + 11.227407 * toilet3 - 
        10.99754 * floor1 + 1.886841 * floor2 + 8.78695 * floor3 + 
        12.099161 * tv_iwi + 0 * ref_iwi + 9.943356 * phone_iwi + 11.484063 * elec_iwi + 
        6.359683 * car_iwi + 3.183801 * bike_iwi + 6.032775 * cheap_iwi + 
        9.276427 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.692217 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.262345 * toilet1 - 1.355116 * toilet2 + 11.114459 * toilet3 - 
        10.491395 * floor1 + 1.285722 * floor2 + 8.889377 * floor3 + 
        11.458394 * tv_iwi + 11.357054 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        6.764336 * car_iwi + 3.178685 * bike_iwi + 5.905523 * cheap_iwi + 
        9.19345 * expensive_iwi - 5.938477 * sleepr1 + 0.699431 * sleepr2 + 
        5.446505 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.708984 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.995339 * toilet1 - 1.081488 * toilet2 + 10.641967 * toilet3 - 
        10.354542 * floor1 + 1.676791 * floor2 + 8.371487 * floor3 + 
        11.525771 * tv_iwi + 10.770297 * ref_iwi + 0 * phone_iwi + 10.859986 * elec_iwi + 
        0 * car_iwi + 2.921967 * bike_iwi + 5.687101 * cheap_iwi + 
        8.69473 * expensive_iwi - 5.359103 * sleepr1 + 0.728365 * sleepr2 + 
        4.817711 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.838426 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.704047 * toilet1 - 1.073727 * toilet2 + 10.3501 * toilet3 - 
        9.964401 * floor1 + 1.562147 * floor2 + 8.106784 * floor3 + 
        11.11414 * tv_iwi + 10.621727 * ref_iwi + 0 * phone_iwi + 10.431197 * elec_iwi + 
        6.011704 * car_iwi + 0 * bike_iwi + 5.429451 * cheap_iwi + 
        8.412766 * expensive_iwi - 5.169979 * sleepr1 + 0.66674 * sleepr2 + 
        4.683705 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.469141 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.916515 * toilet1 - 1.136892 * toilet2 + 10.60721 * toilet3 - 
        10.248296 * floor1 + 1.564473 * floor2 + 8.379324 * floor3 + 
        11.421613 * tv_iwi + 10.899958 * ref_iwi + 0 * phone_iwi + 10.676706 * elec_iwi + 
        6.199549 * car_iwi + 2.797321 * bike_iwi + 0 * cheap_iwi + 
        8.736833 * expensive_iwi - 5.304329 * sleepr1 + 0.677157 * sleepr2 + 
        4.812345 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.537163 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.40321 * toilet1 - 1.004088 * toilet2 + 10.982763 * toilet3 - 
        10.618683 * floor1 + 1.630672 * floor2 + 8.672646 * floor3 + 
        11.708556 * tv_iwi + 11.058245 * ref_iwi + 0 * phone_iwi + 11.073218 * elec_iwi + 
        6.311502 * car_iwi + 2.797276 * bike_iwi + 5.891261 * cheap_iwi + 
        0 * expensive_iwi - 5.51527 * sleepr1 + 0.740346 * sleepr2 + 
        4.967369 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        21.29946 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.56054 * toilet1 - 1.217165 * toilet2 + 11.301043 * toilet3 - 
        10.738919 * floor1 + 1.701376 * floor2 + 8.719364 * floor3 + 
        11.970029 * tv_iwi + 11.37666 * ref_iwi + 0 * phone_iwi + 11.346819 * elec_iwi + 
        6.359718 * car_iwi + 2.83567 * bike_iwi + 5.769758 * cheap_iwi + 
        9.02148 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.810606 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.905103 * toilet1 - 1.300609 * toilet2 + 10.721914 * toilet3 - 
        10.234745 * floor1 + 1.531642 * floor2 + 8.398561 * floor3 + 
        11.243027 * tv_iwi + 11.056497 * ref_iwi + 9.767535 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 3.001429 * bike_iwi + 5.719102 * cheap_iwi + 
        9.080308 * expensive_iwi - 5.670758 * sleepr1 + 0.667846 * sleepr2 + 
        5.20102 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.808198 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.575449 * toilet1 - 1.273118 * toilet2 + 10.377222 * toilet3 - 
        9.800959 * floor1 + 1.415767 * floor2 + 8.09282 * floor3 + 
        10.799191 * tv_iwi + 10.855966 * ref_iwi + 9.500034 * phone_iwi + 0 * elec_iwi + 
        6.383059 * car_iwi + 0 * bike_iwi + 5.429168 * cheap_iwi + 
        8.735587 * expensive_iwi - 5.43179 * sleepr1 + 0.602886 * sleepr2 + 
        5.018756 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.402177 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.764928 * toilet1 - 1.347217 * toilet2 + 10.620178 * toilet3 - 
        10.062863 * floor1 + 1.404927 * floor2 + 8.357047 * floor3 + 
        11.078706 * tv_iwi + 11.130608 * ref_iwi + 9.741575 * phone_iwi + 0 * elec_iwi + 
        6.583206 * car_iwi + 2.855907 * bike_iwi + 0 * cheap_iwi + 
        9.069262 * expensive_iwi - 5.574385 * sleepr1 + 0.607915 * sleepr2 + 
        5.161334 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.642407 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.311168 * toilet1 - 1.2319 * toilet2 + 11.06762 * toilet3 - 
        10.480714 * floor1 + 1.458739 * floor2 + 8.708525 * floor3 + 
        11.391109 * tv_iwi + 11.360717 * ref_iwi + 9.886902 * phone_iwi + 0 * elec_iwi + 
        6.765604 * car_iwi + 2.867026 * bike_iwi + 5.9274 * cheap_iwi + 
        0 * expensive_iwi - 5.850525 * sleepr1 + 0.672265 * sleepr2 + 
        5.382689 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        21.150253 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.511512 * toilet1 - 1.462033 * toilet2 + 11.441236 * toilet3 - 
        10.638742 * floor1 + 1.550748 * floor2 + 8.770832 * floor3 + 
        11.702158 * tv_iwi + 11.729157 * ref_iwi + 10.225031 * phone_iwi + 0 * elec_iwi + 
        6.822145 * car_iwi + 2.904736 * bike_iwi + 5.803584 * cheap_iwi + 
        9.450869 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.996027 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.349227 * toilet1 - 1.043418 * toilet2 + 9.978541 * toilet3 - 
        9.684432 * floor1 + 1.708572 * floor2 + 7.691449 * floor3 + 
        10.851318 * tv_iwi + 10.324778 * ref_iwi + 8.974602 * phone_iwi + 
        10.124078 * elec_iwi + 0 * car_iwi + 0 * bike_iwi + 5.260188 * cheap_iwi + 
        8.29632 * expensive_iwi - 4.962368 * sleepr1 + 0.632907 * sleepr2 + 
        4.502697 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.609095 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.557451 * toilet1 - 1.101931 * toilet2 + 10.227909 * toilet3 - 
        9.96231 * floor1 + 1.726547 * floor2 + 7.942741 * floor3 + 
        11.153682 * tv_iwi + 10.597413 * ref_iwi + 9.2095 * phone_iwi + 
        10.36616 * elec_iwi + 0 * car_iwi + 2.658982 * bike_iwi + 0 * cheap_iwi + 
        8.611116 * expensive_iwi - 5.089333 * sleepr1 + 0.643614 * sleepr2 + 
        4.623402 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.680154 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.038962 * toilet1 - 0.983593 * toilet2 + 10.609497 * toilet3 - 
        10.344097 * floor1 + 1.810816 * floor2 + 8.229291 * floor3 + 
        11.462636 * tv_iwi + 10.781071 * ref_iwi + 9.316432 * phone_iwi + 
        10.771525 * elec_iwi + 0 * car_iwi + 2.660997 * bike_iwi + 5.71051 * cheap_iwi + 
        0 * expensive_iwi - 5.297096 * sleepr1 + 0.704057 * sleepr2 + 
        4.777888 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        20.541932 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.135811 * toilet1 - 1.175126 * toilet2 + 10.85185 * toilet3 - 
        10.406121 * floor1 + 1.859112 * floor2 + 8.241738 * floor3 + 
        11.654 * tv_iwi + 11.031887 * ref_iwi + 9.556239 * phone_iwi + 
        10.972654 * elec_iwi + 0 * car_iwi + 2.694292 * bike_iwi + 5.577592 * cheap_iwi + 
        8.877816 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.710843 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.247772 * toilet1 - 1.0864 * toilet2 + 9.91202 * toilet3 - 
        9.559573 * floor1 + 1.603602 * floor2 + 7.674025 * floor3 + 
        10.726118 * tv_iwi + 10.399533 * ref_iwi + 8.970882 * phone_iwi + 
        9.932597 * elec_iwi + 5.87934 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 
        8.307033 * expensive_iwi - 4.903498 * sleepr1 + 0.587166 * sleepr2 + 
        4.48761 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.712833 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.700144 * toilet1 - 0.980014 * toilet2 + 10.274201 * toilet3 - 
        9.914263 * floor1 + 1.67609 * floor2 + 7.945953 * floor3 + 
        11.01518 * tv_iwi + 10.585816 * ref_iwi + 9.07954 * phone_iwi + 
        10.308343 * elec_iwi + 6.003542 * car_iwi + 0 * bike_iwi + 5.438383 * cheap_iwi + 
        0 * expensive_iwi - 5.098426 * sleepr1 + 0.640228 * sleepr2 + 
        4.63621 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.780465 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.798453 * toilet1 - 1.157281 * toilet2 + 10.507014 * toilet3 - 
        9.982013 * floor1 + 1.72685 * floor2 + 7.961516 * floor3 + 
        11.203195 * tv_iwi + 10.822736 * ref_iwi + 9.30691 * phone_iwi + 
        10.506814 * elec_iwi + 6.026266 * car_iwi + 0 * bike_iwi + 5.319645 * cheap_iwi + 
        8.565439 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.447646 + 0 * water1 + 0 * water2 + 0 * water3 - 
        9.962681 * toilet1 - 1.038484 * toilet2 + 10.576842 * toilet3 - 
        10.240997 * floor1 + 1.699938 * floor2 + 8.238755 * floor3 + 
        11.361506 * tv_iwi + 10.910644 * ref_iwi + 9.34755 * phone_iwi + 
        10.592261 * elec_iwi + 6.212941 * car_iwi + 2.538732 * bike_iwi + 
        0 * cheap_iwi + 0 * expensive_iwi - 5.243968 * sleepr1 + 
        0.65395 * sleepr2 + 4.773123 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        20.311932 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.033531 * toilet1 - 1.222323 * toilet2 + 10.787759 * toilet3 - 
        10.278401 * floor1 + 1.747563 * floor2 + 8.228029 * floor3 + 
        11.525816 * tv_iwi + 11.126106 * ref_iwi + 9.561253 * phone_iwi + 
        10.770159 * elec_iwi + 6.217995 * car_iwi + 2.575743 * bike_iwi + 
        0 * cheap_iwi + 8.895209 * expensive_iwi + 0 * sleepr1 + 
        0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~
        #is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        21.293455 + 0 * water1 + 0 * water2 + 0 * water3 - 
        10.583595 * toilet1 - 1.110907 * toilet2 + 11.241955 * toilet3 - 
        10.709859 * floor1 + 1.839335 * floor2 + 8.555271 * floor3 + 
        11.884835 * tv_iwi + 11.367756 * ref_iwi + 9.70528 * phone_iwi + 
        11.233035 * elec_iwi + 6.365683 * car_iwi + 2.564281 * bike_iwi + 
        5.788451 * cheap_iwi + 0 * expensive_iwi + 0 * sleepr1 + 
        0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.468717 - 9.672257 * water1 - 3.596008 * water2 + 12.253838 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 12.94427 * ref_iwi + 11.498854 * phone_iwi + 
        11.571085 * elec_iwi + 8.016282 * car_iwi + 3.349189 * bike_iwi + 
        6.813215 * cheap_iwi + 10.6111 * expensive_iwi - 6.79646 * sleepr1 + 
        0.561049 * sleepr2 + 6.473449 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.438448 - 9.675108 * water1 - 3.029955 * water2 + 11.761716 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        13.167204 * tv_iwi + 0 * ref_iwi + 11.21187 * phone_iwi + 
        12.214953 * elec_iwi + 7.448944 * car_iwi + 3.855364 * bike_iwi + 
        6.972754 * cheap_iwi + 10.467245 * expensive_iwi - 6.76334 * sleepr1 + 
        0.538767 * sleepr2 + 6.461501 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.196379 - 9.691195 * water1 - 3.362832 * water2 + 12.067849 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        13.141765 * tv_iwi + 12.662938 * ref_iwi + 0 * phone_iwi + 
        12.200419 * elec_iwi + 7.478389 * car_iwi + 3.358618 * bike_iwi + 
        6.652692 * cheap_iwi + 10.192935 * expensive_iwi - 6.505184 * sleepr1 + 
        0.68463 * sleepr2 + 6.048017 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.138827 - 9.294047 * water1 - 3.612148 * water2 + 11.911705 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        12.569664 * tv_iwi + 12.875796 * ref_iwi + 11.465661 * phone_iwi + 
        0 * elec_iwi + 7.957373 * car_iwi + 3.410151 * bike_iwi + 
        6.592053 * cheap_iwi + 10.569899 * expensive_iwi - 6.84478 * sleepr1 + 
        0.575613 * sleepr2 + 6.508871 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        15.478946 - 9.313258 * water1 - 3.183835 * water2 + 11.555397 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        12.791794 * tv_iwi + 12.25134 * ref_iwi + 10.799179 * phone_iwi + 
        11.800626 * elec_iwi + 0 * car_iwi + 3.163424 * bike_iwi + 
        6.397032 * cheap_iwi + 10.023927 * expensive_iwi - 6.165688 * sleepr1 + 
        0.642961 * sleepr2 + 5.738334 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        14.793031 - 8.914682 * water1 - 3.205535 * water2 + 11.198942 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        12.208802 * tv_iwi + 11.984744 * ref_iwi + 10.460015 * phone_iwi + 
        11.214381 * elec_iwi + 6.996234 * car_iwi + 0 * bike_iwi + 
        6.033864 * cheap_iwi + 9.596028 * expensive_iwi - 5.878349 * sleepr1 + 
        0.570057 * sleepr2 + 5.513959 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        15.241701 - 9.178477 * water1 - 3.271684 * water2 + 11.505237 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        12.604912 * tv_iwi + 12.364993 * ref_iwi + 10.791229 * phone_iwi + 
        11.519734 * elec_iwi + 7.260162 * car_iwi + 2.985531 * bike_iwi + 
        0 * cheap_iwi + 10.027431 * expensive_iwi - 6.063224 * sleepr1 + 
        0.57632 * sleepr2 + 5.69907 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        16.053148 - 9.665891 * water1 - 3.453725 * water2 + 12.123468 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        13.028818 * tv_iwi + 12.661531 * ref_iwi + 10.976327 * phone_iwi + 
        12.054938 * elec_iwi + 7.48266 * car_iwi + 2.981344 * bike_iwi + 
        6.667295 * cheap_iwi + 0 * expensive_iwi - 6.387257 * sleepr1 + 
        0.640206 * sleepr2 + 5.970471 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        9.8991 - 9.8991 * water1 - 3.517742 * water2 + 12.39909 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        13.36706 * tv_iwi + 13.060668 * ref_iwi + 11.346658 * phone_iwi + 
        12.417294 * elec_iwi + 7.525395 * car_iwi + 3.025907 * bike_iwi + 
        6.497701 * cheap_iwi + 10.461126 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        26.279954 - 9.084794 * water1 - 2.709566 * water2 + 10.925628 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.947292 * floor1 + 1.629931 * floor2 + 8.991496 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 10.282327 * phone_iwi + 
        11.082356 * elec_iwi + 6.926333 * car_iwi + 3.489669 * bike_iwi + 
        6.39363 * cheap_iwi + 9.558724 * expensive_iwi - 
        6.247868 * sleepr1 + 0.397109 * sleepr2 + 6.069884 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.901457 - 9.0958 * water1 - 3.017579 * water2 + 11.20524 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.773873 * floor1 + 1.44987 * floor2 + 9.001066 * floor3 + 
        0 * tv_iwi + 11.602832 * ref_iwi + 0 * phone_iwi + 
        11.080982 * elec_iwi + 6.965447 * car_iwi + 3.067651 * bike_iwi + 
        6.124021 * cheap_iwi + 9.333435 * expensive_iwi - 
        6.031784 * sleepr1 + 0.525102 * sleepr2 + 5.717869 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.474008 - 8.731169 * water1 - 3.234482 * water2 + 11.051391 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.43083 * floor1 + 1.23519 * floor2 + 8.880545 * floor3 + 
        0 * tv_iwi + 11.778416 * ref_iwi + 10.500744 * phone_iwi + 
        0 * elec_iwi + 7.374419 * car_iwi + 3.114107 * bike_iwi + 
        6.061776 * cheap_iwi + 9.654317 * expensive_iwi - 
        6.312009 * sleepr1 + 0.42305 * sleepr2 + 6.110277 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.083717 - 8.801551 * water1 - 2.872594 * water2 + 10.801346 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.521057 * floor1 + 1.71058 * floor2 + 8.499386 * floor3 + 
        0 * tv_iwi + 11.302471 * ref_iwi + 9.991317 * phone_iwi + 
        10.804494 * elec_iwi + 0 * car_iwi + 2.901008 * bike_iwi + 
        5.929707 * cheap_iwi + 9.238909 * expensive_iwi - 
        5.761109 * sleepr1 + 0.515141 * sleepr2 + 5.447644 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.976234 - 8.435211 * water1 - 2.905072 * water2 + 10.484675 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.034866 * floor1 + 1.560868 * floor2 + 8.17626 * floor3 + 
        0 * tv_iwi + 11.081246 * ref_iwi + 9.698762 * phone_iwi + 
        10.295559 * elec_iwi + 6.562679 * car_iwi + 0 * bike_iwi + 
        5.610271 * cheap_iwi + 8.86971 * expensive_iwi - 
        5.506157 * sleepr1 + 0.454399 * sleepr2 + 5.244605 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.671769 - 8.666446 * water1 - 2.956514 * water2 + 10.747446 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.340892 * floor1 + 1.563759 * floor2 + 8.469668 * floor3 + 
        0 * tv_iwi + 11.401389 * ref_iwi + 9.977546 * phone_iwi + 
        10.544499 * elec_iwi + 6.793101 * car_iwi + 2.745104 * bike_iwi + 
        0 * cheap_iwi + 9.239129 * expensive_iwi - 
        5.664431 * sleepr1 + 0.45251 * sleepr2 + 5.410349 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.768763 - 9.082265 * water1 - 3.094822 * water2 + 11.260012 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.753976 * floor1 + 1.629159 * floor2 + 8.805112 * floor3 + 
        0 * tv_iwi + 11.613219 * ref_iwi + 10.099478 * phone_iwi + 
        10.970806 * elec_iwi + 6.967766 * car_iwi + 2.730425 * bike_iwi + 
        6.141508 * cheap_iwi + 0 * expensive_iwi - 
        5.932522 * sleepr1 + 0.497371 * sleepr2 + 5.642912 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        20.280472 - 9.333155 * water1 - 3.168082 * water2 + 11.560368 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.947317 * floor1 + 1.746386 * floor2 + 8.876751 * floor3 + 
        0 * tv_iwi + 12.022792 * ref_iwi + 10.47476 * phone_iwi + 
        11.345152 * elec_iwi + 7.031972 * car_iwi + 2.771207 * bike_iwi + 
        6.014257 * cheap_iwi + 9.622269 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.883349 - 9.032046 * water1 - 2.538956 * water2 + 10.726808 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.898468 * floor1 + 1.764087 * floor2 + 8.805484 * floor3 + 
        12.139309 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 
        11.550682 * elec_iwi + 6.448669 * car_iwi + 3.437332 * bike_iwi + 
        6.200846 * cheap_iwi + 9.160552 * expensive_iwi - 
        5.952834 * sleepr1 + 0.514298 * sleepr2 + 5.64697 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.848068 - 8.790657 * water1 - 2.779599 * water2 + 10.709795 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.705331 * floor1 + 1.552999 * floor2 + 8.833077 * floor3 + 
        11.751533 * tv_iwi + 0 * ref_iwi + 10.341949 * phone_iwi + 
        0 * elec_iwi + 6.956238 * car_iwi + 3.5588 * bike_iwi + 
        6.239264 * cheap_iwi + 9.601574 * expensive_iwi - 
        6.35208 * sleepr1 + 0.415129 * sleepr2 + 6.159701 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.973833 - 8.708169 * water1 - 2.442252 * water2 + 10.337211 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.590215 * floor1 + 1.963338 * floor2 + 8.317239 * floor3 + 
        11.851582 * tv_iwi + 0 * ref_iwi + 9.695557 * phone_iwi + 
        11.192634 * elec_iwi + 0 * car_iwi + 3.231824 * bike_iwi + 
        5.979117 * cheap_iwi + 9.046253 * expensive_iwi - 
        5.675449 * sleepr1 + 0.499397 * sleepr2 + 5.374749 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.143105 - 8.447145 * water1 - 2.526723 * water2 + 10.165188 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.21807 * floor1 + 1.8332 * floor2 + 8.085228 * floor3 + 
        11.455111 * tv_iwi + 0 * ref_iwi + 9.511219 * phone_iwi + 
        10.775557 * elec_iwi + 6.164177 * car_iwi + 0 * bike_iwi + 
        5.712307 * cheap_iwi + 8.766555 * expensive_iwi - 
        5.477889 * sleepr1 + 0.448202 * sleepr2 + 5.221554 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.78349 - 8.651861 * water1 - 2.540666 * water2 + 10.370201 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.504991 * floor1 + 1.841426 * floor2 + 8.354882 * floor3 + 
        11.768322 * tv_iwi + 0 * ref_iwi + 9.753346 * phone_iwi + 
        11.021569 * elec_iwi + 6.356239 * car_iwi + 3.09761 * bike_iwi + 
        0 * cheap_iwi + 9.114894 * expensive_iwi - 
        5.626638 * sleepr1 + 0.444308 * sleepr2 + 5.379448 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.799963 - 9.037399 * water1 - 2.620399 * water2 + 10.809146 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.892906 * floor1 + 1.92877 * floor2 + 8.644335 * floor3 + 
        12.071738 * tv_iwi + 0 * ref_iwi + 9.830051 * phone_iwi + 
        11.449398 * elec_iwi + 6.470523 * car_iwi + 3.111304 * bike_iwi + 
        6.226327 * cheap_iwi + 0 * expensive_iwi - 
        5.869658 * sleepr1 + 0.488013 * sleepr2 + 5.587214 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        20.38966 - 9.291894 * water1 - 2.70952 * water2 + 11.120659 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.097766 * floor1 + 2.040383 * floor2 + 8.732659 * floor3 + 
        12.43387 * tv_iwi + 0 * ref_iwi + 10.211206 * phone_iwi + 
        11.830131 * elec_iwi + 6.547478 * car_iwi + 3.152097 * bike_iwi + 
        6.113171 * cheap_iwi + 9.46907 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        20.38966 - 9.291894 * water1 - 2.70952 * water2 + 11.120659 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        11.097766 * floor1 + 2.040383 * floor2 + 8.732659 * floor3 + 
        12.43387 * tv_iwi + 0 * ref_iwi + 10.211206 * phone_iwi + 
        11.830131 * elec_iwi + 6.547478 * car_iwi + 3.152097 * bike_iwi + 
        6.113171 * cheap_iwi + 9.46907 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.614688 - 8.696306 * water1 - 2.693174 * water2 + 10.545377 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.416592 * floor1 + 1.792627 * floor2 + 8.317397 * floor3 + 
        11.805963 * tv_iwi + 10.995179 * ref_iwi + 0 * phone_iwi + 
        11.145153 * elec_iwi + 0 * car_iwi + 2.884583 * bike_iwi + 
        5.749523 * cheap_iwi + 8.84501 * expensive_iwi - 
        5.50179 * sleepr1 + 0.596997 * sleepr2 + 5.097126 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.678366 - 8.385934 * water1 - 2.738558 * water2 + 10.292705 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.998217 * floor1 + 1.656845 * floor2 + 8.046195 * floor3 + 
        11.356075 * tv_iwi + 10.827106 * ref_iwi + 0 * phone_iwi + 
        10.675227 * elec_iwi + 6.165549 * car_iwi + 0 * bike_iwi + 
        5.476735 * cheap_iwi + 8.541157 * expensive_iwi - 
        5.294215 * sleepr1 + 0.538495 * sleepr2 + 4.940887 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.333552 - 8.606487 * water1 - 2.777919 * water2 + 10.534853 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.291114 * floor1 + 1.667287 * floor2 + 8.31945 * floor3 + 
        11.681024 * tv_iwi + 11.120348 * ref_iwi + 0 * phone_iwi + 
        10.937485 * elec_iwi + 6.36383 * car_iwi + 2.749139 * bike_iwi + 
        0 * cheap_iwi + 8.876872 * expensive_iwi - 
        5.435951 * sleepr1 + 0.542654 * sleepr2 + 5.083447 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.309161 - 8.981958 * water1 - 2.887221 * water2 + 10.984059 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.666626 * floor1 + 1.740867 * floor2 + 8.610461 * floor3 + 
        11.989964 * tv_iwi + 11.296543 * ref_iwi + 0 * phone_iwi + 
        11.360246 * elec_iwi + 6.489788 * car_iwi + 2.740088 * bike_iwi + 
        5.952886 * cheap_iwi + 0 * expensive_iwi - 
        5.660577 * sleepr1 + 0.591714 * sleepr2 + 5.266804 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        20.035905 - 9.20167 * water1 - 2.956403 * water2 + 11.251484 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.834235 * floor1 + 1.834171 * floor2 + 8.680766 * floor3 + 
        12.303168 * tv_iwi + 11.662227 * ref_iwi + 0 * phone_iwi + 
        11.689249 * elec_iwi + 6.552849 * car_iwi + 2.78196 * bike_iwi + 
        5.840491 * cheap_iwi + 9.201901 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.670135 - 8.531802 * water1 - 2.918628 * water2 + 10.587505 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.293737 * floor1 + 1.639807 * floor2 + 8.349072 * floor3 + 
        11.520383 * tv_iwi + 11.310453 * ref_iwi + 10.024556 * phone_iwi + 
        0 * elec_iwi + 0 * car_iwi + 2.966212 * bike_iwi + 
        5.789415 * cheap_iwi + 9.259514 * expensive_iwi - 
        5.844596 * sleepr1 + 0.526428 * sleepr2 + 5.522756 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.596657 - 8.189355 * water1 - 2.942271 * water2 + 10.285617 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.827938 * floor1 + 1.501377 * floor2 + 8.034567 * floor3 + 
        11.03367 * tv_iwi + 11.083343 * ref_iwi + 9.724935 * phone_iwi + 
        0 * elec_iwi + 6.566845 * car_iwi + 0 * bike_iwi + 
        5.480785 * cheap_iwi + 8.885134 * expensive_iwi - 
        5.579364 * sleepr1 + 0.466313 * sleepr2 + 5.308447 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.21578 - 8.386269 * water1 - 2.987478 * water2 + 10.510609 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.098369 * floor1 + 1.497541 * floor2 + 8.300145 * floor3 + 
        11.329624 * tv_iwi + 11.374316 * ref_iwi + 9.981824 * phone_iwi + 
        0 * elec_iwi + 6.779713 * car_iwi + 2.808485 * bike_iwi + 
        0 * cheap_iwi + 9.232274 * expensive_iwi - 
        5.731142 * sleepr1 + 0.464658 * sleepr2 + 5.467232 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.350287 - 8.800314 * water1 - 3.143537 * water2 + 11.037022 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.521423 * floor1 + 1.557381 * floor2 + 8.650721 * floor3 + 
        11.666301 * tv_iwi + 11.629768 * ref_iwi + 10.146631 * phone_iwi + 
        0 * elec_iwi + 6.984846 * car_iwi + 2.809069 * bike_iwi + 
        5.996644 * cheap_iwi + 0 * expensive_iwi - 
        6.02855 * sleepr1 + 0.510948 * sleepr2 + 5.72871 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.804877 - 9.069896 * water1 - 3.222597 * water2 + 11.360054 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.734981 * floor1 + 1.676244 * floor2 + 8.740321 * floor3 + 
        12.037763 * tv_iwi + 12.057036 * ref_iwi + 10.535576 * phone_iwi + 
        0 * elec_iwi + 7.061826 * car_iwi + 2.850515 * bike_iwi + 
        5.8834 * cheap_iwi + 9.668631 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.889607 - 8.093454 * water1 - 2.614467 * water2 + 9.908741 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.721634 * floor1 + 1.806496 * floor2 + 7.630957 * floor3 + 
        11.079748 * tv_iwi + 10.515982 * ref_iwi + 9.161629 * phone_iwi + 
        10.351932 * elec_iwi + 0 * car_iwi + 0 * bike_iwi + 
        5.305065 * cheap_iwi + 8.422536 * expensive_iwi - 
        5.074519 * sleepr1 + 0.518194 * sleepr2 + 4.733803 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.523997 - 8.307876 * water1 - 2.653318 * water2 + 10.144669 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.008137 * floor1 + 1.832814 * floor2 + 7.882377 * floor3 + 
        11.398503 * tv_iwi + 10.802582 * ref_iwi + 9.409304 * phone_iwi + 
        10.609585 * elec_iwi + 0 * car_iwi + 2.613863 * bike_iwi + 
        0 * cheap_iwi + 8.748465 * expensive_iwi - 
        5.207984 * sleepr1 + 0.523497 * sleepr2 + 4.866654 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.510632 - 8.68532 * water1 - 2.760957 * water2 + 10.594281 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.396754 * floor1 + 1.92651 * floor2 + 8.166249 * floor3 + 
        11.730184 * tv_iwi + 11.004394 * ref_iwi + 9.529528 * phone_iwi + 
        11.041203 * elec_iwi + 0 * car_iwi + 2.607431 * bike_iwi + 
        5.769102 * cheap_iwi + 0 * expensive_iwi - 
        5.428558 * sleepr1 + 0.571378 * sleepr2 + 5.046997 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.336097 - 8.839178 * water1 - 2.812438 * water2 + 10.784203 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.49692 * floor1 + 1.9924 * floor2 + 8.198281 * floor3 + 
        11.962287 * tv_iwi + 11.292918 * ref_iwi + 9.8018 * phone_iwi + 
        11.285497 * elec_iwi + 0 * car_iwi + 2.644674 * bike_iwi + 
        5.643011 * cheap_iwi + 9.051232 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.574773 - 7.987809 * water1 - 2.672518 * water2 + 9.859977 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.580516 * floor1 + 1.690533 * floor2 + 7.608628 * floor3 + 
        10.935712 * tv_iwi + 10.583691 * ref_iwi + 9.147489 * phone_iwi + 
        10.139246 * elec_iwi + 6.016551 * car_iwi + 0 * bike_iwi + 
        0 * cheap_iwi + 8.423207 * expensive_iwi - 
        5.006448 * sleepr1 + 0.470919 * sleepr2 + 4.710727 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.48757 - 8.337396 * water1 - 2.782908 * water2 + 10.285754 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        9.938215 * floor1 + 1.769518 * floor2 + 7.877066 * floor3 + 
        11.243027 * tv_iwi + 10.785848 * ref_iwi + 9.267488 * phone_iwi + 
        10.535808 * elec_iwi + 6.153046 * car_iwi + 0 * bike_iwi + 
        5.481909 * cheap_iwi + 0 * expensive_iwi - 
        5.211959 * sleepr1 + 0.511813 * sleepr2 + 4.882483 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        18.529218 - 8.488098 * water1 - 2.827922 * water2 + 10.46705 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.04112 * floor1 + 1.836734 * floor2 + 7.910444 * floor3 + 
        11.468289 * tv_iwi + 11.057104 * ref_iwi + 9.524351 * phone_iwi + 
        10.773722 * elec_iwi + 6.186301 * car_iwi + 0 * bike_iwi + 
        5.369975 * cheap_iwi + 8.713545 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.237026 - 8.598002 * water1 - 2.843671 * water2 + 10.584337 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.274365 * floor1 + 1.802713 * floor2 + 8.169771 * floor3 + 
        11.608316 * tv_iwi + 11.128466 * ref_iwi + 9.550506 * phone_iwi + 
        10.837934 * elec_iwi + 6.374079 * car_iwi + 2.475561 * bike_iwi + 
        0 * cheap_iwi + 0 * expensive_iwi - 
        5.364659 * sleepr1 + 0.518356 * sleepr2 + 5.034003 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi== -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.076732 - 8.727781 * water1 - 2.881522 * water2 + 10.739665 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.348951 * floor1 + 1.867202 * floor2 + 8.178421 * floor3 + 
        11.811122 * tv_iwi + 11.379313 * ref_iwi + 9.794734 * phone_iwi + 
        11.05657 * elec_iwi + 6.38949 * car_iwi + 2.517243 * bike_iwi + 
        0 * cheap_iwi + 9.05671 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi == -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi== -9~
        #!#is.na(watersource_iwi) & #is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.959609 - 9.167317 * water1 - 3.018392 * water2 + 11.273313 * water3 + 
        0 * toilet1 + 0 * toilet2 + 0 * toilet3 - 
        10.792292 * floor1 + 1.971412 * floor2 + 8.504908 * floor3 + 
        12.200421 * tv_iwi + 11.647501 * ref_iwi + 9.957387 * phone_iwi + 
        11.554298 * elec_iwi + 6.554413 * car_iwi + 2.493645 * bike_iwi + 
        5.854506 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        25.22262 - 9.079055 * water1 - 3.506876 * water2 + 11.61718 * water3 - 
        10.611127 * toilet1 - 1.781417 * toilet2 + 11.784625 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 10.145476 * phone_iwi + 
        10.866602 * elec_iwi + 6.854533 * car_iwi + 2.775867 * bike_iwi + 
        6.257162 * cheap_iwi + 9.243303 * expensive_iwi - 
        5.532438 * sleepr1 + 0.49348 * sleepr2 + 5.232632 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.864413 - 8.993956 * water1 - 3.718481 * water2 + 11.721995 * water3 - 
        10.576998 * toilet1 - 1.922275 * toilet2 + 11.859453 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 11.851237 * ref_iwi + 0 * phone_iwi + 
        10.760923 * elec_iwi + 6.816309 * car_iwi + 2.370583 * bike_iwi + 
        5.932395 * cheap_iwi + 8.951815 * expensive_iwi - 
        5.293458 * sleepr1 + 0.607546 * sleepr2 + 4.870878 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.505099 - 8.668717 * water1 - 3.912494 * water2 + 11.585242 * water3 - 
        10.303332 * toilet1 - 2.14847 * toilet2 + 11.764806 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 12.010403 * ref_iwi + 10.241837 * phone_iwi + 
        0 * elec_iwi + 7.175984 * car_iwi + 2.401954 * bike_iwi + 
        5.867194 * cheap_iwi + 9.240857 * expensive_iwi - 
        5.533049 * sleepr1 + 0.520054 * sleepr2 + 5.206624 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.984809 - 8.690316 * water1 - 3.508217 * water2 + 11.252192 * water3 - 
        10.192244 * toilet1 - 1.816668 * toilet2 + 11.400607 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 11.505023 * ref_iwi + 9.757728 * phone_iwi + 
        10.48404 * elec_iwi + 0 * car_iwi + 2.289825 * bike_iwi + 
        5.754767 * cheap_iwi + 8.868643 * expensive_iwi - 
        5.102249 * sleepr1 + 0.578186 * sleepr2 + 4.702367 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.908189 - 8.277259 * water1 - 3.438923 * water2 + 10.802557 * water3 - 
        9.746305 * toilet1 - 1.738718 * toilet2 + 10.902979 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 11.169449 * ref_iwi + 9.416448 * phone_iwi + 
        9.944536 * elec_iwi + 6.390981 * car_iwi + 0 * bike_iwi + 
        5.435794 * cheap_iwi + 8.487399 * expensive_iwi - 
        4.884624 * sleepr1 + 0.513752 * sleepr2 + 4.541671 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.634477 - 8.561299 * water1 - 3.560259 * water2 + 11.176162 * water3 - 
        10.049607 * toilet1 - 1.852829 * toilet2 + 11.28842 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 11.576965 * ref_iwi + 9.739197 * phone_iwi + 
        10.240205 * elec_iwi + 6.64502 * car_iwi + 2.156913 * bike_iwi + 
        0 * cheap_iwi + 8.866059 * expensive_iwi - 
        5.023571 * sleepr1 + 0.522661 * sleepr2 + 4.676581 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi!= -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.697751 - 8.933656 * water1 - 3.750504 * water2 + 11.69319 * water3 - 
        10.564436 * toilet1 - 1.790202 * toilet2 + 11.745554 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 11.797904 * ref_iwi + 9.837577 * phone_iwi + 
        10.61413 * elec_iwi + 6.793606 * car_iwi + 2.083751 * bike_iwi + 
        5.926511 * cheap_iwi + 0 * expensive_iwi - 
        5.19966 * sleepr1 + 0.571388 * sleepr2 + 4.810024 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.646905 - 9.027189 * water1 - 3.752056 * water2 + 11.782647 * water3 - 
        10.619716 * toilet1 - 1.967546 * toilet2 + 11.936194 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        0 * tv_iwi + 12.006697 * ref_iwi + 10.056494 * phone_iwi + 
        10.80294 * elec_iwi + 6.78109 * car_iwi + 2.142014 * bike_iwi + 
        5.759903 * cheap_iwi + 9.085116 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.830021 - 9.020843 * water1 - 3.281476 * water2 + 11.365319 * water3 - 
        10.486197 * toilet1 - 1.57774 * toilet2 + 11.505373 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.880166 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 
        11.308307 * elec_iwi + 6.418082 * car_iwi + 2.792201 * bike_iwi + 
        6.092098 * cheap_iwi + 8.889696 * expensive_iwi - 
        5.322981 * sleepr1 + 0.590294 * sleepr2 + 4.918737 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.810344 - 8.805427 * water1 - 3.533207 * water2 + 11.382462 * water3 - 
        10.345566 * toilet1 - 1.84297 * toilet2 + 11.571317 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.515221 * tv_iwi + 0 * ref_iwi + 10.20392 * phone_iwi + 
        0 * elec_iwi + 6.88218 * car_iwi + 2.871201 * bike_iwi + 
        6.117138 * cheap_iwi + 9.294919 * expensive_iwi - 
        5.659351 * sleepr1 + 0.506167 * sleepr2 + 5.351298 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.87423 - 8.683407 * water1 - 3.108822 * water2 + 10.89656 * water3 - 
        10.076551 * toilet1 - 1.502486 * toilet2 + 11.045438 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.591123 * tv_iwi + 0 * ref_iwi + 9.575726 * phone_iwi + 
        10.950958 * elec_iwi + 0 * car_iwi + 2.667156 * bike_iwi + 
        5.880049 * cheap_iwi + 8.784381 * expensive_iwi - 
        5.114271 * sleepr1 + 0.55867 * sleepr2 + 4.734378 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.029017 - 8.357998 * water1 - 3.10654 * water2 + 10.588057 * water3 - 
        9.741637 * toilet1 - 1.462904 * toilet2 + 10.686286 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.135959 * tv_iwi + 0 * ref_iwi + 9.327017 * phone_iwi + 
        10.476919 * elec_iwi + 6.089183 * car_iwi + 0 * bike_iwi + 
        5.596271 * cheap_iwi + 8.472629 * expensive_iwi - 
        4.929383 * sleepr1 + 0.503123 * sleepr2 + 4.598663 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.710546 - 8.625635 * water1 - 3.189793 * water2 + 10.912923 * water3 - 
        10.015861 * toilet1 - 1.556198 * toilet2 + 11.027178 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.505831 * tv_iwi + 0 * ref_iwi + 9.624769 * phone_iwi + 
        10.782648 * elec_iwi + 6.313705 * car_iwi + 2.541141 * bike_iwi + 
        0 * cheap_iwi + 8.844157 * expensive_iwi - 
        5.06905 * sleepr1 + 0.509256 * sleepr2 + 4.737102 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.709558 - 8.976341 * water1 - 3.333933 * water2 + 11.369256 * water3 - 
        10.495659 * toilet1 - 1.461029 * toilet2 + 11.424904 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.769468 * tv_iwi + 0 * ref_iwi + 9.687184 * phone_iwi + 
        11.166333 * elec_iwi + 6.41679 * car_iwi + 2.499213 * bike_iwi + 
        6.092688 * cheap_iwi + 0 * expensive_iwi - 
        5.237558 * sleepr1 + 0.556079 * sleepr2 + 4.864606 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.660441 - 9.085904 * water1 - 3.357822 * water2 + 11.493337 * water3 - 
        10.574537 * toilet1 - 1.65292 * toilet2 + 11.649893 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.956738 * tv_iwi + 0 * ref_iwi + 9.925208 * phone_iwi + 
        11.363159 * elec_iwi + 6.424347 * car_iwi + 2.54523 * bike_iwi + 
        5.930926 * cheap_iwi + 9.050721 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.477985 - 8.737268 * water1 - 3.735608 * water2 + 11.495188 * water3 - 
        10.325767 * toilet1 - 1.975078 * toilet2 + 11.653481 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.41101 * tv_iwi + 11.859562 * ref_iwi + 0 * phone_iwi + 
        0 * elec_iwi + 6.839793 * car_iwi + 2.467764 * bike_iwi + 
        5.805788 * cheap_iwi + 9.001803 * expensive_iwi - 
        5.414951 * sleepr1 + 0.616548 * sleepr2 + 4.987627 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.54894 - 8.59607 * water1 - 3.291565 * water2 + 10.974038 * water3 - 
        10.032209 * toilet1 - 1.628317 * toilet2 + 11.098686 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.456967 * tv_iwi + 11.199874 * ref_iwi + 0 * phone_iwi + 
        10.814704 * elec_iwi + 0 * car_iwi + 2.326673 * bike_iwi + 
        5.605168 * cheap_iwi + 8.528851 * expensive_iwi - 
        4.920661 * sleepr1 + 0.646295 * sleepr2 + 4.446099 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.6231 - 8.236968 * water1 - 3.251444 * water2 + 10.600724 * water3 - 
        9.650161 * toilet1 - 1.574005 * toilet2 + 10.681944 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.967936 * tv_iwi + 10.926744 * ref_iwi + 0 * phone_iwi + 
        10.308151 * elec_iwi + 6.041271 * car_iwi + 0 * bike_iwi + 
        5.326456 * cheap_iwi + 8.206463 * expensive_iwi - 
        4.735971 * sleepr1 + 0.584144 * sleepr2 + 4.31721 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.307078 - 8.507396 * water1 - 3.353445 * water2 + 10.944606 * water3 - 
        9.935925 * toilet1 - 1.670905 * toilet2 + 11.036937 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.334561 * tv_iwi + 11.301597 * ref_iwi + 0 * phone_iwi + 
        10.615541 * elec_iwi + 6.26373 * car_iwi + 2.205818 * bike_iwi + 
        0 * cheap_iwi + 8.554159 * expensive_iwi - 
        4.863756 * sleepr1 + 0.597634 * sleepr2 + 4.435974 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.259211 - 8.845044 * water1 - 3.508571 * water2 + 11.398243 * water3 - 
        10.398244 * toilet1 - 1.598716 * toilet2 + 11.435179 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.596995 * tv_iwi + 11.485861 * ref_iwi + 0 * phone_iwi + 
        10.985657 * elec_iwi + 6.373774 * car_iwi + 2.147849 * bike_iwi + 
        5.773949 * cheap_iwi + 0 * expensive_iwi - 
        5.015923 * sleepr1 + 0.647727 * sleepr2 + 4.543283 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.378611 - 8.929094 * water1 - 3.518151 * water2 + 11.485785 * water3 - 
        10.449517 * toilet1 - 1.766427 * toilet2 + 11.61448 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.750666 * tv_iwi + 11.680903 * ref_iwi + 0 * phone_iwi + 
        11.147445 * elec_iwi + 6.373021 * car_iwi + 2.200911 * bike_iwi + 
        5.624996 * cheap_iwi + 8.743183 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.598174 - 8.444616 * water1 - 3.519048 * water2 + 11.030232 * water3 - 
        9.949775 * toilet1 - 1.861341 * toilet2 + 11.196983 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.181848 * tv_iwi + 11.497487 * ref_iwi + 9.792249 * phone_iwi + 
        0 * elec_iwi + 0 * car_iwi + 2.374764 * bike_iwi + 
        5.629926 * cheap_iwi + 8.898405 * expensive_iwi - 
        5.203783 * sleepr1 + 0.585713 * sleepr2 + 4.799931 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.556594 - 8.0558 * water1 - 3.447082 * water2 + 10.601093 * water3 - 
        9.527189 * toilet1 - 1.777969 * toilet2 + 10.718106 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.662275 * tv_iwi + 11.161168 * ref_iwi + 9.445723 * phone_iwi + 
        0 * elec_iwi + 6.396711 * car_iwi + 0 * bike_iwi + 
        5.320857 * cheap_iwi + 8.511867 * expensive_iwi - 
        4.973604 * sleepr1 + 0.521911 * sleepr2 + 4.625607 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.210006 - 8.305525 * water1 - 3.556721 * water2 + 10.932152 * water3 - 
        9.793108 * toilet1 - 1.886913 * toilet2 + 11.062882 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.001896 * tv_iwi + 11.537507 * ref_iwi + 9.748009 * phone_iwi + 
        0 * elec_iwi + 6.634607 * car_iwi + 2.241239 * bike_iwi + 
        0 * cheap_iwi + 8.871895 * expensive_iwi - 
        5.111373 * sleepr1 + 0.53031 * sleepr2 + 4.759809 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        24.309192 - 8.680924 * water1 - 3.762402 * water2 + 11.465538 * water3 - 
        10.31433 * toilet1 - 1.841378 * toilet2 + 11.539436 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.290975 * tv_iwi + 11.798561 * ref_iwi + 9.88509 * phone_iwi + 
        0 * elec_iwi + 6.810738 * car_iwi + 2.1812 * bike_iwi + 
        5.800024 * cheap_iwi + 0 * expensive_iwi - 
        5.313938 * sleepr1 + 0.580447 * sleepr2 + 4.919247 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.197019 - 8.798607 * water1 - 3.772704 * water2 + 11.585391 * water3 - 
        10.398412 * toilet1 - 2.019334 * toilet2 + 11.758815 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.486395 * tv_iwi + 12.027511 * ref_iwi + 10.116712 * phone_iwi + 
        0 * elec_iwi + 6.80884 * car_iwi + 2.234096 * bike_iwi + 
        5.646992 * cheap_iwi + 9.138229 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.827066 - 7.954031 * water1 - 3.084599 * water2 + 10.188375 * water3 - 
        9.300221 * toilet1 - 1.499015 * toilet2 + 10.280814 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.71263 * tv_iwi + 10.600696 * ref_iwi + 8.930404 * phone_iwi + 
        10.007869 * elec_iwi + 0 * car_iwi + 0 * bike_iwi + 
        5.168976 * cheap_iwi + 8.105099 * expensive_iwi - 
        4.572815 * sleepr1 + 0.554453 * sleepr2 + 4.178072 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.4739 - 8.209685 * water1 - 3.177456 * water2 + 10.510349 * water3 - 
        9.570198 * toilet1 - 1.587387 * toilet2 + 10.613754 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.065462 * tv_iwi + 10.956436 * ref_iwi + 9.213587 * phone_iwi + 
        10.302238 * elec_iwi + 0 * car_iwi + 2.132344 * bike_iwi + 
        0 * cheap_iwi + 8.441016 * expensive_iwi - 
        4.694017 * sleepr1 + 0.56705 * sleepr2 + 4.290916 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.426793 - 8.550049 * water1 - 3.327981 * water2 + 10.962523 * water3 - 
        10.028508 * toilet1 - 1.524465 * toilet2 + 11.015188 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.349139 * tv_iwi + 11.162045 * ref_iwi + 9.312562 * phone_iwi + 
        10.681286 * elec_iwi + 0 * car_iwi + 2.081967 * bike_iwi + 
        5.605957 * cheap_iwi + 0 * expensive_iwi - 
        4.848237 * sleepr1 + 0.614957 * sleepr2 + 4.40254 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        18.632334 - 8.594161 * water1 - 3.325613 * water2 + 11.002003 * water3 - 
        10.038173 * toilet1 - 1.673597 * toilet2 + 11.139362 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.450741 * tv_iwi + 11.306407 * ref_iwi + 9.474732 * phone_iwi + 
        10.790209 * elec_iwi + 0 * car_iwi + 2.130946 * bike_iwi + 
        5.450603 * cheap_iwi + 8.622664 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        21.558224 - 7.854016 * water1 - 3.124087 * water2 + 10.128688 * water3 - 
        9.188777 * toilet1 - 1.52953 * toilet2 + 10.194901 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.577236 * tv_iwi + 10.653809 * ref_iwi + 8.915001 * phone_iwi + 
        9.808656 * elec_iwi + 5.89742 * car_iwi + 0 * bike_iwi + 
        0 * cheap_iwi + 8.104784 * expensive_iwi - 
        4.51543 * sleepr1 + 0.511943 * sleepr2 + 4.161283 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        22.435075 - 8.165022 * water1 - 3.267537 * water2 + 10.547023 * water3 - 
        9.610052 * toilet1 - 1.472959 * toilet2 + 10.56487 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.836195 * tv_iwi + 10.850635 * ref_iwi + 9.010619 * phone_iwi + 
        10.152777 * elec_iwi + 6.015384 * car_iwi + 0 * bike_iwi + 
        5.317829 * cheap_iwi + 0 * expensive_iwi - 
        4.660001 * sleepr1 + 0.553192 * sleepr2 + 4.269594 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        17.854762 - 8.22007 * water1 - 3.267011 * water2 + 10.598414 * water3 - 
        9.634692 * toilet1 - 1.612168 * toilet2 + 10.69611 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        10.946801 * tv_iwi + 10.995093 * ref_iwi + 9.169643 * phone_iwi + 
        10.272034 * elec_iwi + 6.00186 * car_iwi + 0 * bike_iwi + 
        5.181252 * cheap_iwi + 8.284031 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        23.191974 - 8.464151 * water1 - 3.388255 * water2 + 10.934302 * water3 - 
        9.934721 * toilet1 - 1.566786 * toilet2 + 10.955684 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.230153 * tv_iwi + 11.262605 * ref_iwi + 9.326019 * phone_iwi + 
        10.487166 * elec_iwi + 6.254499 * car_iwi + 1.96482 * bike_iwi + 
        0 * cheap_iwi + 0 * expensive_iwi - 
        4.793102 * sleepr1 + 0.567761 * sleepr2 + 4.392779 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        18.41278 - 8.48969 * water1 - 3.372166 * water2 + 10.944293 * water3 - 
        9.92309 * toilet1 - 1.706525 * toilet2 + 11.051733 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.309702 * tv_iwi + 11.373438 * ref_iwi + 9.464761 * phone_iwi + 
        10.57822 * elec_iwi + 6.21899 * car_iwi + 2.020125 * bike_iwi + 
        0 * cheap_iwi + 8.62596 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        19.289207 - 8.864495 * water1 - 3.545434 * water2 + 11.448787 * water3 - 
        10.424713 * toilet1 - 1.654197 * toilet2 + 11.503825 * toilet3 + 
        0 * floor1 + 0 * floor2 + 0 * floor3 + 
        11.623398 * tv_iwi + 11.621596 * ref_iwi + 9.588425 * phone_iwi + 
        10.991776 * elec_iwi + 6.356696 * car_iwi + 1.954527 * bike_iwi + 
        5.621763 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        33.268443 - 8.407617 * water1 - 2.941903 * water2 + 10.490879 * water3 - 
        9.927563 * toilet1 - 1.41087 * toilet2 + 10.828754 * toilet3 - 
        10.015273 * floor1 + 1.366088 * floor2 + 8.349252 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 
        10.252209 * elec_iwi + 5.953864 * car_iwi + 2.535266 * bike_iwi + 
        5.575772 * cheap_iwi + 8.115931 * expensive_iwi - 
        4.917989 * sleepr1 + 0.460468 * sleepr2 + 4.62963 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        33.024566 - 8.20392 * water1 - 3.151747 * water2 + 10.482447 * water3 - 
        9.789305 * toilet1 - 1.635618 * toilet2 + 10.865895 * toilet3 - 
        9.837919 * floor1 + 1.193344 * floor2 + 8.347802 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 9.302737 * phone_iwi + 
        0 * elec_iwi + 6.34354 * car_iwi + 2.602347 * bike_iwi + 
        5.584485 * cheap_iwi + 8.451596 * expensive_iwi - 
        5.193421 * sleepr1 + 0.380851 * sleepr2 + 4.994585 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.275799 - 8.147347 * water1 - 2.804378 * water2 + 10.125512 * water3 - 
        9.592623 * toilet1 - 1.349706 * toilet2 + 10.452978 * toilet3 - 
        9.779273 * floor1 + 1.55656 * floor2 + 7.933072 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 8.824596 * phone_iwi + 10.001035 * elec_iwi + 
        0 * car_iwi + 2.431369 * bike_iwi + 5.417967 * cheap_iwi + 
        8.06569 * expensive_iwi - 4.756555 * sleepr1 + 0.451017 * sleepr2 + 
        4.471983 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.12453 - 7.854935 * water1 - 2.811449 * water2 + 9.856265 * water3 - 
        9.282848 * toilet1 - 1.320705 * toilet2 + 10.126635 * toilet3 - 
        9.390529 * floor1 + 1.440278 * floor2 + 7.671336 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 8.61401 * phone_iwi + 9.594181 * elec_iwi + 
        5.686388 * car_iwi + 0 * bike_iwi + 5.172986 * cheap_iwi + 
        7.801475 * expensive_iwi - 4.596218 * sleepr1 + 0.404936 * sleepr2 + 
        4.352195 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.010696 - 8.084373 * water1 - 2.877073 * water2 + 10.129741 * water3 - 
        9.5252 * toilet1 - 1.398935 * toilet2 + 10.424662 * toilet3 - 
        9.688569 * floor1 + 1.453009 * floor2 + 7.947315 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 8.859989 * phone_iwi + 9.841381 * elec_iwi + 
        5.878062 * car_iwi + 2.321935 * bike_iwi + 0 * cheap_iwi + 
        8.113303 * expensive_iwi - 4.712554 * sleepr1 + 0.404651 * sleepr2 + 
        4.472915 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        33.139597 - 8.37668 * water1 - 2.986614 * water2 + 10.500823 * water3 - 
        9.937593 * toilet1 - 1.306835 * toilet2 + 10.758591 * toilet3 - 
        9.977124 * floor1 + 1.501252 * floor2 + 8.179114 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 8.886142 * phone_iwi + 10.143784 * elec_iwi + 
        5.953082 * car_iwi + 2.27816 * bike_iwi + 5.581465 * cheap_iwi + 
        0 * expensive_iwi - 4.8482 * sleepr1 + 0.438662 * sleepr2 + 
        4.579243 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        28.608304 - 8.50741 * water1 - 3.022998 * water2 + 10.655764 * water3 - 
        10.043579 * toilet1 - 1.482534 * toilet2 + 10.997733 * toilet3 - 
        10.057315 * floor1 + 1.567785 * floor2 + 8.191175 * floor3 + 
        0 * tv_iwi + 0 * ref_iwi + 9.130386 * phone_iwi + 10.360228 * elec_iwi + 
        5.978981 * car_iwi + 2.320183 * bike_iwi + 5.459842 * cheap_iwi + 
        8.297404 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.53237 - 8.148688 * water1 - 3.34487 * water2 + 10.599238 * water3 - 
        9.769985 * toilet1 - 1.763195 * toilet2 + 10.945044 * toilet3 - 
        9.618924 * floor1 + 1.055308 * floor2 + 8.271834 * floor3 + 
        0 * tv_iwi + 10.826941 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        6.323189 * car_iwi + 2.263955 * bike_iwi + 5.328481 * cheap_iwi + 
        8.218282 * expensive_iwi - 4.994774 * sleepr1 + 0.478877 * sleepr2 + 
        4.690665 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.784121 - 8.075809 * water1 - 2.980547 * water2 + 10.212125 * water3 - 
        9.554156 * toilet1 - 1.471108 * toilet2 + 10.508585 * toilet3 - 
        9.560045 * floor1 + 1.411246 * floor2 + 7.864051 * floor3 + 
        0 * tv_iwi + 10.320914 * ref_iwi + 0 * phone_iwi + 9.900453 * elec_iwi + 
        0 * car_iwi + 2.140168 * bike_iwi + 5.187328 * cheap_iwi + 
        7.857833 * expensive_iwi - 4.594111 * sleepr1 + 0.530213 * sleepr2 + 
        4.22442 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.541456 - 7.754791 * water1 - 2.955678 * water2 + 9.888014 * water3 - 
        9.204488 * toilet1 - 1.429293 * toilet2 + 10.133235 * toilet3 - 
        9.148903 * floor1 + 1.296103 * floor2 + 7.579511 * floor3 + 
        0 * tv_iwi + 10.095443 * ref_iwi + 0 * phone_iwi + 9.465823 * elec_iwi + 
        5.657304 * car_iwi + 0 * bike_iwi + 4.945706 * cheap_iwi + 
        7.584019 * expensive_iwi - 4.433274 * sleepr1 + 0.478769 * sleepr2 + 
        4.10949 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.425656 - 7.98761 * water1 - 3.037849 * water2 + 10.179138 * water3 - 
        9.456876 * toilet1 - 1.511237 * toilet2 + 10.443968 * toilet3 - 
        9.440339 * floor1 + 1.308424 * floor2 + 7.8495 * floor3 + 
        0 * tv_iwi + 10.40868 * ref_iwi + 0 * phone_iwi + 9.716613 * elec_iwi + 
        5.84849 * car_iwi + 2.035024 * bike_iwi + 0 * cheap_iwi + 
        7.878286 * expensive_iwi - 4.540831 * sleepr1 + 0.484945 * sleepr2 + 
        4.214645 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.507992 - 8.270969 * water1 - 3.15824 * water2 + 10.551276 * water3 - 
        9.856341 * toilet1 - 1.438637 * toilet2 + 10.780204 * toilet3 - 
        9.715538 * floor1 + 1.347934 * floor2 + 8.076977 * floor3 + 
        0 * tv_iwi + 10.536641 * ref_iwi + 0 * phone_iwi + 10.011061 * elec_iwi + 
        5.931156 * car_iwi + 1.97982 * bike_iwi + 5.318445 * cheap_iwi + 
        0 * expensive_iwi - 4.665143 * sleepr1 + 0.521762 * sleepr2 + 
        4.306427 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        28.091245 - 8.377973 * water1 - 3.182973 * water2 + 10.673686 * water3 - 
        9.936791 * toilet1 - 1.595236 * toilet2 + 10.979595 * toilet3 - 
        9.776481 * floor1 + 1.404508 * floor2 + 8.080219 * floor3 + 
        0 * tv_iwi + 10.752406 * ref_iwi + 0 * phone_iwi + 10.195909 * elec_iwi + 
        5.948901 * car_iwi + 2.02593 * bike_iwi + 5.203927 * cheap_iwi + 
        8.048184 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.656726 - 7.925826 * water1 - 3.172812 * water2 + 10.23892 * water3 - 
        9.465122 * toilet1 - 1.671744 * toilet2 + 10.575496 * toilet3 - 
        9.433329 * floor1 + 1.280554 * floor2 + 7.87018 * floor3 + 
        0 * tv_iwi + 10.563755 * ref_iwi + 9.013133 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 2.182914 * bike_iwi + 5.198615 * cheap_iwi + 
        8.169581 * expensive_iwi - 4.832449 * sleepr1 + 0.470838 * sleepr2 + 
        4.53068 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.289788 - 7.581933 * water1 - 3.123372 * water2 + 9.871784 * water3 - 
        9.082631 * toilet1 - 1.606979 * toilet2 + 10.150281 * toilet3 - 
        8.989812 * floor1 + 1.167096 * floor2 + 7.552637 * floor3 + 
        0 * tv_iwi + 10.289615 * ref_iwi + 8.724043 * phone_iwi + 0 * elec_iwi + 
        5.964279 * car_iwi + 0 * bike_iwi + 4.933688 * cheap_iwi + 
        7.845501 * expensive_iwi - 4.635412 * sleepr1 + 0.419285 * sleepr2 + 
        4.378385 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.127006 - 7.796357 * water1 - 3.211228 * water2 + 10.150552 * water3 - 
        9.317079 * toilet1 - 1.698382 * toilet2 + 10.450681 * toilet3 - 
        9.263225 * floor1 + 1.169089 * floor2 + 7.815357 * floor3 + 
        0 * tv_iwi + 10.602429 * ref_iwi + 8.974293 * phone_iwi + 0 * elec_iwi + 
        6.167048 * car_iwi + 2.068233 * bike_iwi + 0 * cheap_iwi + 
        8.148718 * expensive_iwi - 4.750345 * sleepr1 + 0.420964 * sleepr2 + 
        4.495683 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.359914 - 8.109395 * water1 - 3.369918 * water2 + 10.584123 * water3 - 
        9.764312 * toilet1 - 1.646055 * toilet2 + 10.849391 * toilet3 - 
        9.572843 * floor1 + 1.198275 * floor2 + 8.086328 * floor3 + 
        0 * tv_iwi + 10.788649 * ref_iwi + 9.059955 * phone_iwi + 0 * elec_iwi + 
        6.301106 * car_iwi + 2.011703 * bike_iwi + 5.32973 * cheap_iwi + 
        0 * expensive_iwi - 4.913363 * sleepr1 + 0.456217 * sleepr2 + 
        4.629102 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.776273 - 8.244435 * water1 - 3.397201 * water2 + 10.735168 * water3 - 
        9.873008 * toilet1 - 1.812238 * toilet2 + 11.083875 * toilet3 - 
        9.65883 * floor1 + 1.270696 * floor2 + 8.098198 * floor3 + 
        0 * tv_iwi + 11.035077 * ref_iwi + 9.299565 * phone_iwi + 0 * elec_iwi + 
        6.321115 * car_iwi + 2.056881 * bike_iwi + 5.212883 * cheap_iwi + 
        8.380964 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.680776 - 7.526633 * water1 - 2.818483 * water2 + 9.553182 * water3 - 
        8.908693 * toilet1 - 1.366762 * toilet2 + 9.794828 * toilet3 - 
        8.944162 * floor1 + 1.443991 * floor2 + 7.235559 * floor3 + 
        0 * tv_iwi + 9.844251 * ref_iwi + 8.314568 * phone_iwi + 9.242598 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 4.823024 * cheap_iwi + 7.524686 * expensive_iwi - 
        4.301289 * sleepr1 + 0.465131 * sleepr2 + 3.986528 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.534048 - 7.750628 * water1 - 2.89446 * water2 + 9.830581 * water3 - 
        9.150948 * toilet1 - 1.442234 * toilet2 + 10.090638 * toilet3 - 
        9.227502 * floor1 + 1.468741 * floor2 + 7.485463 * floor3 + 
        0 * tv_iwi + 10.146464 * ref_iwi + 8.554664 * phone_iwi + 9.487561 * elec_iwi + 
        0 * car_iwi + 1.970773 * bike_iwi + 0 * cheap_iwi + 7.812738 * expensive_iwi - 
        4.40497 * sleepr1 + 0.471905 * sleepr2 + 4.08707 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.642926 - 8.040631 * water1 - 3.012853 * water2 + 10.20723 * water3 - 
        9.551908 * toilet1 - 1.377375 * toilet2 + 10.434295 * toilet3 - 
        9.517489 * floor1 + 1.52299 * floor2 + 7.712729 * floor3 + 
        0 * tv_iwi + 10.296275 * ref_iwi + 8.616747 * phone_iwi + 9.79427 * elec_iwi + 
        0 * car_iwi + 1.920995 * bike_iwi + 5.191682 * cheap_iwi + 0 * expensive_iwi - 
        4.532898 * sleepr1 + 0.508468 * sleepr2 + 4.18285 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.239203 - 8.1092 * water1 - 3.026153 * water2 + 10.283441 * water3 - 
        9.591178 * toilet1 - 1.518692 * toilet2 + 10.581515 * toilet3 - 
        9.538825 * floor1 + 1.562833 * floor2 + 7.694118 * floor3 + 
        0 * tv_iwi + 10.465909 * ref_iwi + 8.793249 * phone_iwi + 9.930879 * elec_iwi + 
        0 * car_iwi + 1.965101 * bike_iwi + 5.069382 * cheap_iwi + 7.977203 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.296075 - 7.430396 * water1 - 2.857033 * water2 + 9.496233 * water3 - 
        8.800206 * toilet1 - 1.396636 * toilet2 + 9.711324 * toilet3 - 
        8.817462 * floor1 + 1.349285 * floor2 + 7.206238 * floor3 + 
        0 * tv_iwi + 9.892447 * ref_iwi + 8.298861 * phone_iwi + 9.060176 * elec_iwi + 
        5.543119 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 7.524399 * expensive_iwi - 
        4.248011 * sleepr1 + 0.425475 * sleepr2 + 3.971129 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.319804 - 7.697112 * water1 - 2.971313 * water2 + 9.847353 * water3 - 
        9.170942 * toilet1 - 1.338348 * toilet2 + 10.03037 * toilet3 - 
        9.082784 * floor1 + 1.393406 * floor2 + 7.419609 * floor3 + 
        0 * tv_iwi + 10.038325 * ref_iwi + 8.360795 * phone_iwi + 9.340794 * elec_iwi + 
        5.635837 * car_iwi + 0 * bike_iwi + 4.942282 * cheap_iwi + 0 * expensive_iwi - 
        4.368965 * sleepr1 + 0.456908 * sleepr2 + 4.064832 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.106974 - 7.771784 * water1 - 2.984313 * water2 + 9.929052 * water3 - 
        9.219768 * toilet1 - 1.47015 * toilet2 + 10.179653 * toilet3 - 
        9.115421 * floor1 + 1.436825 * floor2 + 7.408413 * floor3 + 
        0 * tv_iwi + 10.203763 * ref_iwi + 8.531302 * phone_iwi + 9.48139 * elec_iwi + 
        5.639253 * car_iwi + 0 * bike_iwi + 4.833686 * cheap_iwi + 7.686514 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.295223 - 7.955297 * water1 - 3.068814 * water2 + 10.17577 * water3 - 
        9.457386 * toilet1 - 1.417357 * toilet2 + 10.37227 * toilet3 - 
        9.40126 * floor1 + 1.421524 * floor2 + 7.700207 * floor3 + 
        0 * tv_iwi + 10.38338 * ref_iwi + 8.624444 * phone_iwi + 9.615119 * elec_iwi + 
        5.841014 * car_iwi + 1.81905 * bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi - 
        4.481279 * sleepr1 + 0.464469 * sleepr2 + 4.173523 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.892603 - 8.00801 * water1 - 3.070873 * water2 + 10.227221 * water3 - 
        9.478213 * toilet1 - 1.550769 * toilet2 + 10.495312 * toilet3 - 
        9.40638 * floor1 + 1.462224 * floor2 + 7.665052 * floor3 + 
        0 * tv_iwi + 10.525601 * ref_iwi + 8.781696 * phone_iwi + 9.736583 * elec_iwi + 
        5.828156 * car_iwi + 1.868025 * bike_iwi + 0 * cheap_iwi + 7.979751 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi == -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.96416 - 8.327339 * water1 - 3.207926 * water2 + 10.647804 * water3 - 
        9.916585 * toilet1 - 1.494423 * toilet2 + 10.882234 * toilet3 - 
        9.720236 * floor1 + 1.518195 * floor2 + 7.913729 * floor3 + 
        0 * tv_iwi + 10.710399 * ref_iwi + 8.863765 * phone_iwi + 10.071815 * elec_iwi + 
        5.935531 * car_iwi + 1.805249 * bike_iwi + 5.205313 * cheap_iwi + 
        0 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.891916 - 8.216133 * water1 - 2.995529 * water2 + 10.357394 * water3 - 
        9.749221 * toilet1 - 1.484435 * toilet2 + 10.710287 * toilet3 - 
        9.869857 * floor1 + 1.306786 * floor2 + 8.266921 * floor3 + 
        10.662875 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        6.022501 * car_iwi + 2.627718 * bike_iwi + 5.491451 * cheap_iwi + 
        8.207206 * expensive_iwi - 5.056705 * sleepr1 + 0.471941 * sleepr2 + 
        4.761731 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.668371 - 8.027546 * water1 - 2.63616 * water2 + 9.865624 * water3 - 
        9.407046 * toilet1 - 1.20283 * toilet2 + 10.157883 * toilet3 - 
        9.6557 * floor1 + 1.620469 * floor2 + 7.750461 * floor3 + 
        10.645856 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 10.24639 * elec_iwi + 
        0 * car_iwi + 2.431127 * bike_iwi + 5.258586 * cheap_iwi + 
        7.754426 * expensive_iwi - 4.578079 * sleepr1 + 0.516793 * sleepr2 + 
        4.221277 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.689393 - 7.778575 * water1 - 2.656128 * water2 + 9.648569 * water3 - 
        9.147817 * toilet1 - 1.185922 * toilet2 + 9.890452 * toilet3 - 
        9.319462 * floor1 + 1.514756 * floor2 + 7.529138 * floor3 + 
        10.296883 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 9.87184 * elec_iwi + 
        5.36794 * car_iwi + 0 * bike_iwi + 5.046869 * cheap_iwi + 
        7.532508 * expensive_iwi - 4.443539 * sleepr1 + 0.472493 * sleepr2 + 
        4.126409 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.518863 - 7.994517 * water1 - 2.709184 * water2 + 9.898347 * water3 - 
        9.373556 * toilet1 - 1.251466 * toilet2 + 10.162417 * toilet3 - 
        9.601548 * floor1 + 1.532305 * floor2 + 7.784923 * floor3 + 
        10.59337 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 10.125187 * elec_iwi + 
        5.534243 * car_iwi + 2.334116 * bike_iwi + 0 * cheap_iwi + 
        7.817028 * expensive_iwi - 4.549243 * sleepr1 + 0.476814 * sleepr2 + 
        4.231505 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.525335 - 8.256977 * water1 - 2.797558 * water2 + 10.222813 * water3 - 
        9.740811 * toilet1 - 1.158655 * toilet2 + 10.451496 * toilet3 - 
        9.861834 * floor1 + 1.584298 * floor2 + 7.98566 * floor3 + 
        10.779366 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 10.41963 * elec_iwi + 
        5.584969 * car_iwi + 2.298614 * bike_iwi + 5.415769 * cheap_iwi + 
        0 * expensive_iwi - 4.665713 * sleepr1 + 0.512453 * sleepr2 + 
        4.316349 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        28.145415 - 8.374746 * water1 - 2.834881 * water2 + 10.366366 * water3 - 
        9.837494 * toilet1 - 1.321061 * toilet2 + 10.671285 * toilet3 - 
        9.933176 * floor1 + 1.636885 * floor2 + 8.002901 * floor3 + 
        10.965289 * tv_iwi + 0 * ref_iwi + 0 * phone_iwi + 10.611008 * elec_iwi + 
        5.614777 * car_iwi + 2.339 * bike_iwi + 5.307926 * cheap_iwi + 
        7.976033 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.887048 - 7.961671 * water1 - 2.848847 * water2 + 9.989493 * water3 - 
        9.416937 * toilet1 - 1.413977 * toilet2 + 10.329971 * toilet3 - 
        9.633158 * floor1 + 1.500796 * floor2 + 7.846578 * floor3 + 
        10.471302 * tv_iwi + 0 * ref_iwi + 8.907878 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 2.512317 * bike_iwi + 5.332448 * cheap_iwi + 
        8.138195 * expensive_iwi - 4.875282 * sleepr1 + 0.461114 * sleepr2 + 
        4.584771 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.75673 - 7.681994 * water1 - 2.850965 * water2 + 9.727913 * water3 - 
        9.118497 * toilet1 - 1.378591 * toilet2 + 10.009844 * toilet3 - 
        9.253605 * floor1 + 1.3914 * floor2 + 7.586953 * floor3 + 
        10.089958 * tv_iwi + 0 * ref_iwi + 8.688184 * phone_iwi + 0 * elec_iwi + 
        5.732516 * car_iwi + 0 * bike_iwi + 5.09071 * cheap_iwi + 
        7.864892 * expensive_iwi - 4.702634 * sleepr1 + 0.41497 * sleepr2 + 
        4.4523 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.560955 - 7.884918 * water1 - 2.911103 * water2 + 9.971618 * water3 - 
        9.332001 * toilet1 - 1.455821 * toilet2 + 10.278787 * toilet3 - 
        9.524642 * floor1 + 1.397654 * floor2 + 7.843173 * floor3 + 
        10.369137 * tv_iwi + 0 * ref_iwi + 8.921524 * phone_iwi + 0 * elec_iwi + 
        5.915144 * car_iwi + 2.400781 * bike_iwi + 0 * cheap_iwi + 
        8.165395 * expensive_iwi - 4.819395 * sleepr1 + 0.41466 * sleepr2 + 
        4.573486 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.755495 - 8.186453 * water1 - 3.035944 * water2 + 10.364767 * water3 - 
        9.758612 * toilet1 - 1.377497 * toilet2 + 10.637265 * toilet3 - 
        9.83066 * floor1 + 1.442844 * floor2 + 8.094887 * floor3 + 
        10.581183 * tv_iwi + 0 * ref_iwi + 8.982148 * phone_iwi + 0 * elec_iwi + 
        6.015787 * car_iwi + 2.367937 * bike_iwi + 5.496215 * cheap_iwi + 
        0 * expensive_iwi - 4.979771 * sleepr1 + 0.449768 * sleepr2 + 
        4.704316 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        28.155793 - 8.337031 * water1 - 3.077722 * water2 + 10.543117 * water3 - 
        9.887945 * toilet1 - 1.55444 * toilet2 + 10.900279 * toilet3 - 
        9.930818 * floor1 + 1.51405 * floor2 + 8.121673 * floor3 + 
        10.807677 * tv_iwi + 0 * ref_iwi + 9.238759 * phone_iwi + 0 * elec_iwi + 
        6.049248 * car_iwi + 2.407764 * bike_iwi + 5.383485 * cheap_iwi + 
        8.392206 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.740799 - 7.527966 * water1 - 2.54083 * water2 + 9.311731 * water3 - 
        8.8341 * toilet1 - 1.142243 * toilet2 + 9.548953 * toilet3 - 
        9.076464 * floor1 + 1.632684 * floor2 + 7.177677 * floor3 + 
        10.076099 * tv_iwi + 0 * ref_iwi + 8.188483 * phone_iwi + 9.59561 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 4.904831 * cheap_iwi + 7.459602 * expensive_iwi - 
        4.302269 * sleepr1 + 0.45648 * sleepr2 + 3.996215 * sleepr3, 
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.538372 - 7.735072 * water1 - 2.590775 * water2 + 9.550466 * water3 - 
        9.050741 * toilet1 - 1.202854 * toilet2 + 9.808194 * toilet3 - 
        9.349106 * floor1 + 1.660917 * floor2 + 7.413791 * floor3 + 
        10.364772 * tv_iwi + 0 * ref_iwi + 8.406754 * phone_iwi + 9.841082 * elec_iwi + 
        0 * car_iwi + 2.24387 * bike_iwi + 0 * cheap_iwi + 7.73666 * expensive_iwi - 
        4.403453 * sleepr1 + 0.461391 * sleepr2 + 4.096041 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.56934 - 8.003626 * water1 - 2.678979 * water2 + 9.880523 * water3 - 
        9.419359 * toilet1 - 1.119251 * toilet2 + 10.105693 * toilet3 - 
        9.623025 * floor1 + 1.724833 * floor2 + 7.615976 * floor3 + 
        10.571817 * tv_iwi + 0 * ref_iwi + 8.444298 * phone_iwi + 10.146522 * elec_iwi + 
        0 * car_iwi + 2.213235 * bike_iwi + 5.267513 * cheap_iwi + 0 * expensive_iwi - 
        4.52333 * sleepr1 + 0.49636 * sleepr2 + 4.185083 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.213615 - 8.083294 * water1 - 2.705398 * water2 + 9.978658 * water3 - 
        9.475365 * toilet1 - 1.266669 * toilet2 + 10.274033 * toilet3 - 
        9.654955 * floor1 + 1.761244 * floor2 + 7.611003 * floor3 + 
        10.710445 * tv_iwi + 0 * ref_iwi + 8.63092 * phone_iwi + 10.288327 * elec_iwi + 
        0 * car_iwi + 2.250413 * bike_iwi + 5.152033 * cheap_iwi + 7.890554 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.521615 - 7.475268 * water1 - 2.594159 * water2 + 9.308711 * water3 - 
        8.776449 * toilet1 - 1.180365 * toilet2 + 9.521687 * toilet3 - 
        9.001239 * floor1 + 1.549181 * floor2 + 7.187147 * floor3 + 
        10.001495 * tv_iwi + 0 * ref_iwi + 8.212392 * phone_iwi + 9.460899 * elec_iwi + 
        5.292916 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 7.495613 * expensive_iwi - 
        4.268659 * sleepr1 + 0.420466 * sleepr2 + 3.997526 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.497099 - 7.728745 * water1 - 2.682787 * water2 + 9.624938 * water3 - 
        9.125638 * toilet1 - 1.104235 * toilet2 + 9.805863 * toilet3 - 
        9.258892 * floor1 + 1.604846 * floor2 + 7.381716 * floor3 + 
        10.197942 * tv_iwi + 0 * ref_iwi + 8.254265 * phone_iwi + 9.747527 * elec_iwi + 
        5.358731 * car_iwi + 0 * bike_iwi + 5.046129 * cheap_iwi + 0 * expensive_iwi - 
        4.383824 * sleepr1 + 0.451346 * sleepr2 + 4.08579 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.296489 - 7.811425 * water1 - 2.706677 * water2 + 9.723698 * water3 - 
        9.186676 * toilet1 - 1.242507 * toilet2 + 9.972108 * toilet3 - 
        9.298388 * floor1 + 1.644422 * floor2 + 7.380948 * floor3 + 
        10.337442 * tv_iwi + 0 * ref_iwi + 8.433808 * phone_iwi + 9.891458 * elec_iwi + 
        5.373066 * car_iwi + 0 * bike_iwi + 4.941605 * cheap_iwi + 7.649378 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.426405 - 7.972328 * water1 - 2.751142 * water2 + 9.914123 * water3 - 
        9.387819 * toilet1 - 1.16676 * toilet2 + 10.111274 * toilet3 - 
        9.571004 * floor1 + 1.638783 * floor2 + 7.650419 * floor3 + 
        10.521076 * tv_iwi + 0 * ref_iwi + 8.497106 * phone_iwi + 10.028011 * elec_iwi + 
        5.539422 * car_iwi + 2.116879 * bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi - 
        4.495254 * sleepr1 + 0.457191 * sleepr2 + 4.195286 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.041087 - 8.034362 * water1 - 2.767264 * water2 + 9.986647 * water3 - 
        9.423213 * toilet1 - 1.308566 * toilet2 + 10.255066 * toilet3 - 
        9.583512 * floor1 + 1.67464 * floor2 + 7.62719 * floor3 + 
        10.639548 * tv_iwi + 0 * ref_iwi + 8.665206 * phone_iwi + 10.151935 * elec_iwi + 
        5.540096 * car_iwi + 2.158059 * bike_iwi + 0 * cheap_iwi + 7.935166 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        28.054181 - 8.335441 * water1 - 2.872442 * water2 + 10.362178 * water3 - 
        9.832754 * toilet1 - 1.230913 * toilet2 + 10.597306 * toilet3 - 
        9.885985 * floor1 + 1.74303 * floor2 + 7.852608 * floor3 + 
        10.874784 * tv_iwi + 0 * ref_iwi + 8.72246 * phone_iwi + 10.491491 * elec_iwi + 
        5.615338 * car_iwi + 2.116366 * bike_iwi + 5.313288 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.415125 - 7.899693 * water1 - 3.018707 * water2 + 10.079595 * water3 - 
        9.386537 * toilet1 - 1.529231 * toilet2 + 10.388769 * toilet3 - 
        9.422227 * floor1 + 1.360458 * floor2 + 7.780686 * floor3 + 
        10.37975 * tv_iwi + 10.371 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 2.219409 * bike_iwi + 5.10737 * cheap_iwi + 7.926027 * expensive_iwi - 
        4.706668 * sleepr1 + 0.538865 * sleepr2 + 4.332268 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.193611 - 7.591364 * water1 - 2.989191 * water2 + 9.763374 * water3 - 
        9.048255 * toilet1 - 1.481218 * toilet2 + 10.019827 * toilet3 - 
        9.020329 * floor1 + 1.252107 * floor2 + 7.4984 * floor3 + 
        9.969609 * tv_iwi + 10.139301 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        5.698666 * car_iwi + 0 * bike_iwi + 4.869042 * cheap_iwi + 7.643325 * expensive_iwi - 
        4.533664 * sleepr1 + 0.487319 * sleepr2 + 4.204845 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.003188 - 7.799802 * water1 - 3.06538 * water2 + 10.026305 * water3 - 
        9.273869 * toilet1 - 1.561736 * toilet2 + 10.303185 * toilet3 - 
        9.287416 * floor1 + 1.258861 * floor2 + 7.750303 * floor3 + 
        10.250334 * tv_iwi + 10.433604 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        5.881372 * car_iwi + 2.112701 * bike_iwi + 0 * cheap_iwi + 7.927714 * expensive_iwi - 
        4.642101 * sleepr1 + 0.493113 * sleepr2 + 4.311294 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        32.145323 - 8.091954 * water1 - 3.200457 * water2 + 10.419563 * water3 - 
        9.686886 * toilet1 - 1.502088 * toilet2 + 10.662683 * toilet3 - 
        9.578238 * floor1 + 1.295242 * floor2 + 7.995989 * floor3 + 
        10.463501 * tv_iwi + 10.596159 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        5.987453 * car_iwi + 2.066241 * bike_iwi + 5.238912 * cheap_iwi + 0 * expensive_iwi - 
        4.788245 * sleepr1 + 0.531427 * sleepr2 + 4.424176 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.663376 - 8.217226 * water1 - 3.230549 * water2 + 10.563863 * water3 - 
        9.788889 * toilet1 - 1.660039 * toilet2 + 10.884267 * toilet3 - 
        9.657261 * floor1 + 1.355904 * floor2 + 8.012705 * floor3 + 
        10.658268 * tv_iwi + 10.828139 * ref_iwi + 0 * phone_iwi + 0 * elec_iwi + 
        6.012105 * car_iwi + 2.10957 * bike_iwi + 5.132247 * cheap_iwi + 8.135461 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.222613 - 7.436297 * water1 - 2.668811 * water2 + 9.337259 * water3 - 
        8.76269 * toilet1 - 1.239808 * toilet2 + 9.553895 * toilet3 - 
        8.857854 * floor1 + 1.49503 * floor2 + 7.101706 * floor3 + 
        9.947146 * tv_iwi + 9.606226 * ref_iwi + 0 * phone_iwi + 9.459313 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 4.708582 * cheap_iwi + 7.267236 * expensive_iwi - 
        4.165773 * sleepr1 + 0.515217 * sleepr2 + 3.796024 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.017932 - 7.645761 * water1 - 2.732149 * water2 + 9.589923 * water3 - 
        8.98698 * toilet1 - 1.303896 * toilet2 + 9.823319 * toilet3 - 
        9.124629 * floor1 + 1.521514 * floor2 + 7.333864 * floor3 + 
        10.234347 * tv_iwi + 9.882283 * ref_iwi + 0 * phone_iwi + 9.705877 * elec_iwi + 
        0 * car_iwi + 1.997322 * bike_iwi + 0 * cheap_iwi + 7.530465 * expensive_iwi - 
        4.260561 * sleepr1 + 0.524678 * sleepr2 + 3.884668 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.009849 - 7.906513 * water1 - 2.829501 * water2 + 9.920627 * water3 - 
        9.345079 * toilet1 - 1.235903 * toilet2 + 10.122498 * toilet3 - 
        9.38651 * floor1 + 1.576671 * floor2 + 7.533028 * floor3 + 
        10.440898 * tv_iwi + 10.007471 * ref_iwi + 0 * phone_iwi + 10.00239 * elec_iwi + 
        0 * car_iwi + 1.957205 * bike_iwi + 5.043118 * cheap_iwi + 0 * expensive_iwi - 
        4.371746 * sleepr1 + 0.561438 * sleepr2 + 3.962917 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.762537 - 7.970838 * water1 - 2.846849 * water2 + 9.99638 * water3 - 
        9.384473 * toilet1 - 1.368558 * toilet2 + 10.26318 * toilet3 - 
        9.407227 * floor1 + 1.607763 * floor2 + 7.522441 * floor3 + 
        10.558922 * tv_iwi + 10.166846 * ref_iwi + 0 * phone_iwi + 10.123054 * elec_iwi + 
        0 * car_iwi + 1.997827 * bike_iwi + 4.934552 * cheap_iwi + 7.674261 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.937216 - 7.365764 * water1 - 2.711851 * water2 + 9.308446 * water3 - 
        8.684203 * toilet1 - 1.271982 * toilet2 + 9.501605 * toilet3 - 
        8.761861 * floor1 + 1.411428 * floor2 + 7.091169 * floor3 + 
        9.850822 * tv_iwi + 9.672133 * ref_iwi + 0 * phone_iwi + 9.306449 * elec_iwi + 
        5.259124 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 7.283347 * expensive_iwi - 
        4.125388 * sleepr1 + 0.479835 * sleepr2 + 3.789687 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.867533 - 7.609584 * water1 - 2.807474 * water2 + 9.621691 * water3 - 
        9.020422 * toilet1 - 1.210257 * toilet2 + 9.78413 * toilet3 - 
        9.005736 * floor1 + 1.458347 * floor2 + 7.281019 * floor3 + 
        10.044507 * tv_iwi + 9.79676 * ref_iwi + 0 * phone_iwi + 9.582228 * elec_iwi + 
        5.329655 * car_iwi + 0 * bike_iwi + 4.825225 * cheap_iwi + 0 * expensive_iwi - 
        4.231792 * sleepr1 + 0.512341 * sleepr2 + 3.867252 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.780902 - 7.678551 * water1 - 2.823801 * water2 + 9.700925 * water3 - 
        9.066963 * toilet1 - 1.334737 * toilet2 + 9.92554 * toilet3 - 
        9.035387 * floor1 + 1.492934 * floor2 + 7.275637 * floor3 + 
        10.165187 * tv_iwi + 9.95215 * ref_iwi + 0 * phone_iwi + 9.706842 * elec_iwi + 
        5.339015 * car_iwi + 0 * bike_iwi + 4.727361 * cheap_iwi + 7.426441 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.776196 - 7.850824 * water1 - 2.888932 * water2 + 9.920123 * water3 - 
        9.28502 * toilet1 - 1.276893 * toilet2 + 10.095074 * toilet3 - 
        9.305738 * floor1 + 1.488974 * floor2 + 7.541261 * floor3 + 
        10.361404 * tv_iwi + 10.111471 * ref_iwi + 0 * phone_iwi + 9.858873 * elec_iwi + 
        5.508616 * car_iwi + 1.864114 * bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi - 
        4.334615 * sleepr1 + 0.523143 * sleepr2 + 3.962868 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.516998 - 7.899938 * water1 - 2.896545 * water2 + 9.973039 * water3 - 
        9.306664 * toilet1 - 1.403483 * toilet2 + 10.21367 * toilet3 - 
        9.310396 * floor1 + 1.520199 * floor2 + 7.514997 * floor3 + 
        10.461717 * tv_iwi + 10.246536 * ref_iwi + 0 * phone_iwi + 9.964082 * elec_iwi + 
        5.504822 * car_iwi + 1.908777 * bike_iwi + 0 * cheap_iwi + 7.695361 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.480671 - 8.187381 * water1 - 3.009509 * water2 + 10.342532 * water3 - 
        9.698485 * toilet1 - 1.34192 * toilet2 + 10.550892 * toilet3 - 
        9.594805 * floor1 + 1.577622 * floor2 + 7.733735 * floor3 + 
        10.691644 * tv_iwi + 10.401955 * ref_iwi + 0 * phone_iwi + 10.288264 * elec_iwi + 
        5.58552 * car_iwi + 1.858266 * bike_iwi + 5.066519 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.325553 - 7.367511 * water1 - 2.845775 * water2 + 9.42716 * water3 - 
        8.754682 * toilet1 - 1.412411 * toilet2 + 9.678775 * toilet3 - 
        8.815155 * floor1 + 1.400895 * floor2 + 7.153143 * floor3 + 
        9.786923 * tv_iwi + 9.874383 * ref_iwi + 8.368518 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 4.746341 * cheap_iwi + 7.570097 * expensive_iwi - 
        4.388204 * sleepr1 + 0.47251 * sleepr2 + 4.069108 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.108841 - 7.568898 * water1 - 2.916349 * water2 + 9.67854 * water3 - 
        8.972206 * toilet1 - 1.486463 * toilet2 + 9.949219 * toilet3 - 
        9.075523 * floor1 + 1.419952 * floor2 + 7.386418 * floor3 + 
        10.062444 * tv_iwi + 10.158687 * ref_iwi + 8.597283 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 2.040245 * bike_iwi + 0 * cheap_iwi + 7.847947 * expensive_iwi - 
        4.492214 * sleepr1 + 0.478899 * sleepr2 + 4.170377 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        31.267629 - 7.86545 * water1 - 3.047311 * water2 + 10.072345 * water3 - 
        9.383541 * toilet1 - 1.433411 * toilet2 + 10.312139 * toilet3 - 
        9.378753 * floor1 + 1.472267 * floor2 + 7.628409 * floor3 + 
        10.295885 * tv_iwi + 10.339854 * ref_iwi + 8.688434 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 1.999003 * bike_iwi + 5.111032 * cheap_iwi + 0 * expensive_iwi - 
        4.639885 * sleepr1 + 0.516784 * sleepr2 + 4.28527 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.809961 - 7.950796 * water1 - 3.064828 * water2 + 10.168046 * water3 - 
        9.442432 * toilet1 - 1.574958 * toilet2 + 10.478795 * toilet3 - 
        9.416734 * floor1 + 1.516282 * floor2 + 7.6218 * floor3 + 
        10.441949 * tv_iwi + 10.522701 * ref_iwi + 8.8735 * phone_iwi + 0 * elec_iwi + 
        0 * car_iwi + 2.039997 * bike_iwi + 4.996497 * cheap_iwi + 8.046755 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.902245 - 7.263282 * water1 - 2.875828 * water2 + 9.355255 * water3 - 
        8.635387 * toilet1 - 1.436135 * toilet2 + 9.579933 * toilet3 - 
        8.677693 * floor1 + 1.30733 * floor2 + 7.112278 * floor3 + 
        9.65198 * tv_iwi + 9.902127 * ref_iwi + 8.336157 * phone_iwi + 0 * elec_iwi + 
        5.561175 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 7.554358 * expensive_iwi - 
        4.325883 * sleepr1 + 0.432709 * sleepr2 + 4.044492 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.967542 - 7.535157 * water1 - 3.001296 * water2 + 9.721014 * water3 - 
        9.014587 * toilet1 - 1.388286 * toilet2 + 9.915314 * toilet3 - 
        8.953897 * floor1 + 1.349608 * floor2 + 7.338 * floor3 + 
        9.865863 * tv_iwi + 10.076 * ref_iwi + 8.42384 * phone_iwi + 0 * elec_iwi + 
        5.672423 * car_iwi + 0 * bike_iwi + 4.865189 * cheap_iwi + 0 * expensive_iwi - 
        4.4639 * sleepr1 + 0.465183 * sleepr2 + 4.154816 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.704384 - 7.623855 * water1 - 3.018119 * water2 + 9.819266 * water3 - 
        9.079997 * toilet1 - 1.52028 * toilet2 + 10.081021 * toilet3 - 
        9.000533 * floor1 + 1.396052 * floor2 + 7.337375 * floor3 + 
        10.012256 * tv_iwi + 10.252802 * ref_iwi + 8.601762 * phone_iwi + 0 * elec_iwi + 
        5.681089 * car_iwi + 0 * bike_iwi + 4.763527 * cheap_iwi + 7.746518 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~  
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        30.868092 - 7.768733 * water1 - 3.092778 * water2 + 10.020991 * water3 - 
        9.273908 * toilet1 - 1.466355 * toilet2 + 10.229871 * toilet3 - 
        9.247876 * floor1 + 1.371581 * floor2 + 7.600938 * floor3 + 
        10.170307 * tv_iwi + 10.402359 * ref_iwi + 8.676465 * phone_iwi + 0 * elec_iwi + 
        5.869311 * car_iwi + 1.896435 * bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi - 
        4.577575 * sleepr1 + 0.47243 * sleepr2 + 4.26523 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.42329 - 7.838481 * water1 - 3.099473 * water2 + 10.09254 * water3 - 
        9.314809 * toilet1 - 1.599751 * toilet2 + 10.372601 * toilet3 - 
        9.27 * floor1 + 1.416065 * floor2 + 7.578516 * floor3 + 
        10.297925 * tv_iwi + 10.557948 * ref_iwi + 8.842028 * phone_iwi + 0 * elec_iwi + 
        5.862599 * car_iwi + 1.941612 * bike_iwi + 0 * cheap_iwi + 8.030941 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        27.533545 - 8.166935 * water1 - 3.251193 * water2 + 10.534539 * water3 - 
        9.767173 * toilet1 - 1.556247 * toilet2 + 10.783134 * toilet3 - 
        9.599436 * floor1 + 1.470168 * floor2 + 7.844117 * floor3 + 
        10.558142 * tv_iwi + 10.778085 * ref_iwi + 8.955846 * phone_iwi + 0 * elec_iwi + 
        5.99294 * car_iwi + 1.886799 * bike_iwi + 5.132854 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.104184 - 7.143797 * water1 - 2.594423 * water2 + 8.996724 * water3 - 
        8.40476 * toilet1 - 1.221447 * toilet2 + 9.188473 * toilet3 - 
        8.550094 * floor1 + 1.51534 * floor2 + 6.783755 * floor3 + 
        9.647001 * tv_iwi + 9.42278 * ref_iwi + 7.905472 * phone_iwi + 9.063902 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 7.205484 * expensive_iwi - 
        4.005533 * sleepr1 + 0.463262 * sleepr2 + 3.682225 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.043627 - 7.389636 * water1 - 2.688187 * water2 + 9.310245 * water3 - 
        8.738882 * toilet1 - 1.165665 * toilet2 + 9.47351 * toilet3 - 
        8.801569 * floor1 + 1.571554 * floor2 + 6.971803 * floor3 + 
        9.85398 * tv_iwi + 9.561433 * ref_iwi + 7.969599 * phone_iwi + 9.345172 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 4.708472 * cheap_iwi + 0 * expensive_iwi - 
        4.11354 * sleepr1 + 0.495061 * sleepr2 + 3.762157 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        24.995737 - 7.432887 * water1 - 2.697062 * water2 + 9.358742 * water3 - 
        8.758213 * toilet1 - 1.279189 * toilet2 + 9.579788 * toilet3 - 
        8.804637 * floor1 + 1.59457 * floor2 + 6.95209 * floor3 + 
        9.941235 * tv_iwi + 9.684434 * ref_iwi + 8.10249 * phone_iwi + 9.435655 * elec_iwi + 
        0 * car_iwi + 0 * bike_iwi + 4.606094 * cheap_iwi + 7.343736 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        29.916303 - 7.620555 * water1 - 2.763873 * water2 + 9.593912 * water3 - 
        8.991549 * toilet1 - 1.227578 * toilet2 + 9.769112 * toilet3 - 
        9.091459 * floor1 + 1.610921 * floor2 + 7.213641 * floor3 + 
        10.162166 * tv_iwi + 9.864252 * ref_iwi + 8.203106 * phone_iwi + 9.612674 * elec_iwi + 
        0 * car_iwi + 1.810707 * bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi - 
        4.212741 * sleepr1 + 0.505761 * sleepr2 + 3.854127 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.69894 - 7.643728 * water1 - 2.764403 * water2 + 9.616202 * water3 - 
        8.986075 * toilet1 - 1.342772 * toilet2 + 9.852328 * toilet3 - 
        9.069137 * floor1 + 1.62938 * floor2 + 7.17384 * floor3 + 
        10.228064 * tv_iwi + 9.966359 * ref_iwi + 8.323548 * phone_iwi + 9.68298 * elec_iwi + 
        0 * car_iwi + 1.852455 * bike_iwi + 0 * cheap_iwi + 7.605284 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi == -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.66617 - 7.931839 * water1 - 2.87454 * water2 + 9.983852 * water3 - 
        9.373634 * toilet1 - 1.28752 * toilet2 + 10.190221 * toilet3 - 
        9.360696 * floor1 + 1.696904 * floor2 + 7.389546 * floor3 + 
        10.471627 * tv_iwi + 10.136135 * ref_iwi + 8.40444 * phone_iwi + 10.011686 * elec_iwi + 
        0 * car_iwi + 1.807211 * bike_iwi + 4.939112 * cheap_iwi + 0 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        28.767995 - 7.321722 * water1 - 2.730527 * water2 + 9.283287 * water3 - 
        8.663097 * toilet1 - 1.197665 * toilet2 + 9.423737 * toilet3 - 
        8.708763 * floor1 + 1.489377 * floor2 + 6.962947 * floor3 + 
        9.760827 * tv_iwi + 9.627512 * ref_iwi + 7.973942 * phone_iwi + 9.196606 * 
        elec_iwi + 5.246989 * car_iwi + 0 * bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi - 
        4.074413 * sleepr1 + 0.460643 * sleepr2 + 3.756158 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi == -9 & expensive_iwi != -9 & rooms_iwi == -9~
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        24.723465 - 7.353789 * water1 - 2.731843 * water2 + 9.314643 * water3 - 
        8.669526 * toilet1 - 1.305989 * toilet2 + 9.513352 * toilet3 - 8.700149 * 
        floor1 + 1.512808 * floor2 + 6.931515 * floor3 + 9.834614 * tv_iwi + 9.730978 * 
        ref_iwi + 8.093094 * phone_iwi + 9.275932 * elec_iwi + 5.235236 * car_iwi + 0 * bike_iwi + 0 * 
        cheap_iwi + 7.347172 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi == -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        25.621264 - 7.621079 * water1 - 2.838357 * water2 + 9.659514 * water3 - 9.030765 * 
        toilet1 - 1.25578 * toilet2 + 9.82929 * toilet3 - 8.96942 * floor1 + 1.570662 * 
        floor2 + 7.135174 * floor3 + 10.060563 * tv_iwi + 9.894942 * ref_iwi + 8.171906 * phone_iwi + 
        9.579363 * elec_iwi + 5.322723 * car_iwi + 0 * bike_iwi + 4.72526 * cheap_iwi + 0 * expensive_iwi + 0 * 
        sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi != -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi == -9 & expensive_iwi == -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #!#is.na(floormat_iwi) & #!#is.na(tv_iwi) & #!#is.na(ref_iwi) & #!#is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #is.na(cheap_iwi) & #is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        26.427787 - 7.863175 * water1 - 2.923326 * water2 + 9.961823 * water3 - 
        9.298038 * toilet1 - 1.322226 * toilet2 + 10.142714 * toilet3 - 9.266574 * 
        floor1 + 1.610604 * floor2 + 7.383478 * floor3 + 10.37711 * tv_iwi + 10.215465 * 
        ref_iwi + 8.414873 * phone_iwi + 9.856541 * elec_iwi + 5.499907 * car_iwi + 1.720303 * 
        bike_iwi + 0 * cheap_iwi + 0 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3,
      
      
      # missing: ref, water, room
      watersource_iwi == -9 & toiletfac_iwi != -9 & floormat_iwi != -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~ 
      21.606377 + 0 * water1 + 0 * water2 + 0 * water3 - 10.608837 * toilet1 - 
        1.059774 * toilet2 + 11.227407 * toilet3 - 10.99754 * floor1 + 
        1.886841 * floor2 + 8.78695 * floor3 + 12.099161 * tv_iwi + 0 * ref_iwi + 
        9.943356 * phone_iwi + 11.484063 * elec_iwi + 6.359683 * car_iwi + 
        3.183801 * bike_iwi + 6.032775 * cheap_iwi + 9.276427 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0  * sleepr3,
      
      
      ########
      # missing: ref, phone, bike, floor
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi == -9 & elec_iwi != -9 & car_iwi != -9 & bike_iwi == -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi != -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #is.na(phone_iwi) & #!#is.na(elec_iwi) & #!#is.na(car_iwi) & #is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #!#is.na(rooms_iwi) ~
        #study.x == "s101_GHANA" ~
        24.830021 - 9.020843 * water1 - 3.281476 * water2 + 11.365319 * water3 -
        10.486197 * toilet1 - 1.57774 * toilet2 + 11.505373 * toilet3 +
        0 * floor1  + 0 * floor2 + 0 * floor3 + 11.880166 * tv_iwi +
        0 * phone_iwi + 0 *  bike_iwi + 11.308307 * elec_iwi +
        6.418082 * car_iwi + 0 * ref_iwi + 6.092098 * cheap_iwi +
        8.889696 * expensive_iwi - 5.322981 * sleepr1 +
        0.590294 * sleepr2 + 4.918737 * sleepr3,
      
      
      # missing: ref, electricity, floor, room
      watersource_iwi != -9 & toiletfac_iwi != -9 & floormat_iwi == -9 & tv_iwi != -9 & ref_iwi == -9 & phone_iwi != -9 & elec_iwi == -9 & car_iwi != -9 & bike_iwi != -9 & cheap_iwi != -9 & expensive_iwi != -9 & rooms_iwi == -9~ 
        #!#is.na(watersource_iwi) & #!#is.na(toiletfac_iwi) & #is.na(floormat_iwi) & #!#is.na(tv_iwi) & #is.na(ref_iwi) & #!#is.na(phone_iwi) & #is.na(elec_iwi) & #!#is.na(car_iwi) & #!#is.na(bike_iwi) & #!#is.na(cheap_iwi) & #!#is.na(expensive_iwi) & #is.na(rooms_iwi) ~
        24.810344 - 8.805427 * water1 - 3.533207 * water2 + 11.382462 * water3 -
        10.345566 * toilet1 - 1.84297 * toilet2 + 11.571317 * toilet3 +
        0 * floor1  + 0 * floor2 + 0 * floor3 + 11.515221 * tv_iwi +
        0 * ref_iwi + 10.20392 * phone_iwi + 0 * elec_iwi +
        6.88218 * car_iwi + 2.871201 * bike_iwi + 6.117138 * cheap_iwi +
        9.294919 * expensive_iwi - 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3
      
      
      
    ))


# Summarize IWI by study
summary_by_study <- df_analysis %>%
  group_by(study.x) %>%
  summarize(
    mean_iwi = mean(iwi, na.rm = TRUE),
    median_iwi = median(iwi, na.rm = TRUE),
    sd_iwi = sd(iwi, na.rm = TRUE),
    min_iwi = min(iwi, na.rm = TRUE),
    max_iwi = max(iwi, na.rm = TRUE),
    count = n(),
    missing_iwi = sum(is.na(iwi))
  )

# Print the summary
print(summary_by_study)

# Calculate IWI for s113_WASHK
df_analysis_washk <- df_analysis %>%
  filter(study.x == "s113_WASHK") %>%
  mutate(
    iwi = case_when(
study.x == "s113_WASHK" ~ 21.606377 + 0 * water1 + 0 * water2 + 0 * water3 - 10.608837 * toilet1 - 
  1.059774 * toilet2 + 11.227407 * toilet3 - 10.99754 * floor1 + 
  1.886841 * floor2 + 8.78695 * floor3 + 12.099161 * tv_iwi + 0 * ref_iwi + 
  9.943356 * phone_iwi + 11.484063 * elec_iwi + 6.359683 * car_iwi + 
  3.183801 * bike_iwi + 6.032775 * cheap_iwi + 9.276427 * expensive_iwi + 
  0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3)
) 

# Some datasets have a lot of missing values for IWI, so we will recalculate the IWI for those datasets using the general 
# pattern of asset availability instead of per individual case.

# Calculate IWI for s104_PROMISBF
df_analysis_promisbf <- df_analysis %>%
  filter(study.x == "s104_PROMISBF") %>%
  mutate(
    iwi = case_when(
      study.x == "s104_PROMISBF" ~ 25.639223 - 7.612152 * water1 - 2.610324 * water2 + 9.451774 * water3 - 
        8.944334 * toilet1 - 1.222481 * toilet2 + 9.718852 * toilet3 - 
        9.082737 * floor1 + 1.564714 * floor2 + 7.250736 * floor3 + 
        10.127605 * tv_iwi + 0 * ref_iwi + 8.256066 * phone_iwi + 9.675764 * elec_iwi + 
        5.273206 * car_iwi + 2.181739 * bike_iwi + 4.887081 * cheap_iwi + 
        7.537954 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3)
  ) 

# Calculate IWI for s109_PROMISM
df_analysis_promism <- df_analysis %>%
  filter(study.x == "s109_PROMISM") %>%
  mutate(
    iwi = case_when(
      study.x == "s109_PROMISM" ~ 25.639223 - 7.612152 * water1 - 2.610324 * water2 + 9.451774 * water3 - 
        8.944334 * toilet1 - 1.222481 * toilet2 + 9.718852 * toilet3 - 
        9.082737 * floor1 + 1.564714 * floor2 + 7.250736 * floor3 + 
        10.127605 * tv_iwi + 0 * ref_iwi + 8.256066 * phone_iwi + 9.675764 * elec_iwi + 
        5.273206 * car_iwi + 2.181739 * bike_iwi + 4.887081 * cheap_iwi + 
        7.537954 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3)
  ) 

# Calculate IWI for s108_SHINE_HIV-
df_analysis_shine <- df_analysis %>%
  filter(study.x == "s108_SHINE_HIV-") %>%
  mutate(
    iwi = case_when(
      study.x == "s108_SHINE_HIV-" ~ 25.639223 - 7.612152 * water1 - 2.610324 * water2 + 9.451774 * water3 - 
        8.944334 * toilet1 - 1.222481 * toilet2 + 9.718852 * toilet3 - 
        9.082737 * floor1 + 1.564714 * floor2 + 7.250736 * floor3 + 
        10.127605 * tv_iwi + 0 * ref_iwi + 8.256066 * phone_iwi + 9.675764 * elec_iwi + 
        5.273206 * car_iwi + 2.181739 * bike_iwi + 4.887081 * cheap_iwi + 
        7.537954 * expensive_iwi + 0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3)
  ) 

# Calculate IWI for s112_DOSE
df_analysis_dose <- df_analysis %>%
  filter(study.x == "s112_DOSE") %>%
  mutate(
    iwi = case_when(
      study.x == "s112_DOSE" ~ 22.370207 - 8.058787 * water1 - 3.311234 * water2 + 10.485157 * water3 -
        9.477805 * toilet1 - 1.702653 * toilet2 + 10.611715 * toilet3 +
        0 * tv_iwi + 0 * floor1 + 0 * floor2 + 0 * floor3 +
        10.873586 * ref_iwi + 9.211844 * phone_iwi + 9.724121 * elec_iwi +
        6.264418 * car_iwi + 2.213782 * bike_iwi + 5.377631 * cheap_iwi +
        8.360589 * expensive_iwi - 4.833614 * sleepr1 +
        0.495713 * sleepr2 + 4.506949 * sleepr3)
  ) 

# Calculate IWI for s116_MAHAY
df_analysis_mahay <- df_analysis %>%
  filter(study.x == "s116_MAHAY") %>%
  mutate(
    iwi = case_when(
      study.x == "s116_MAHAY" ~ 23.13399 - 6.873579 * water1 - 2.519801 * water2 + 
        8.676972 * water3 - 8.109378 * toilet1 - 1.211954 * toilet2 + 8.89126 * 
        toilet3 - 8.151034 * floor1 + 1.372303 * floor2 + 
        6.538399 * floor3 + 9.263933 * tv_iwi + 9.085458 * ref_iwi + 7.621934 * 
        phone_iwi + 8.740713 * elec_iwi + 4.933088 * car_iwi + 1.833687 * 
        bike_iwi + 4.342606 * cheap_iwi + 6.93796 * expensive_iwi + 
        0 * sleepr1 + 0 * sleepr2 + 0 * sleepr3)
  ) 


# Combine the datasets
df_analysis <- bind_rows(df_analysis %>% filter(study.x != "s113_WASHK"), df_analysis_washk)
df_analysis <- bind_rows(df_analysis %>% filter(study.x != "s104_PROMISBF"), df_analysis_promisbf)
df_analysis <- bind_rows(df_analysis %>% filter(study.x != "s109_PROMISM"), df_analysis_promism)
df_analysis <- bind_rows(df_analysis %>% filter(study.x != "s108_SHINE_HIV-"), df_analysis_shine)
df_analysis <- bind_rows(df_analysis %>% filter(study.x != "s112_DOSE"), df_analysis_dose)
df_analysis <- bind_rows(df_analysis %>% filter(study.x != "s116_MAHAY"), df_analysis_mahay)


# Summarize IWI by study
summary_by_study <- df_analysis %>%
  group_by(study.x) %>%
  summarize(
    mean_iwi = mean(iwi, na.rm = TRUE),
    median_iwi = median(iwi, na.rm = TRUE),
    sd_iwi = sd(iwi, na.rm = TRUE),
    min_iwi = min(iwi, na.rm = TRUE),
    max_iwi = max(iwi, na.rm = TRUE),
    count = n(),
    missing_iwi = sum(is.na(iwi))
  )
  
# Print the summary
print(summary_by_study)
skim(df_analysis$iwi)
summary(df_analysis$iwi)

# Truncate negative values to zero
df_analysis <- df_analysis %>%
  mutate(iwi = ifelse(iwi < 0, 0, iwi))
# Summarize IWI by study again
summary_by_study <- df_analysis %>%
  group_by(study.x) %>%
  summarize(
    mean_iwi = mean(iwi, na.rm = TRUE),
    median_iwi = median(iwi, na.rm = TRUE),
    sd_iwi = sd(iwi, na.rm = TRUE),
    min_iwi = min(iwi, na.rm = TRUE),
    max_iwi = max(iwi, na.rm = TRUE),
    count = n(),
    missing_iwi = sum(is.na(iwi))
  )
# Print the summary
print(summary_by_study)

# Create region variable
df_analysis <- df_analysis %>%
  mutate(region = case_when(
    study.x %in% c("s101_GHANA", "s102_DYADG", "s103_GHANA", "s103_DYADM", "s104_PROMISBF", "s104_PROMISBF_CS", "s107_ZINC", "s108_SHINE_HIV-", 
                   "s109_PROMISM", "s109_PROMISM_CS", "s112_DOSE", "s113_WASHK", "s116_MAHAY") ~ "Africa",
    study.x %in% c("s105_JiVitA", "s106_RDNS", "s111_WASHB") ~ "South Asia (Bangladesh)",
    study.x %in% c("s110_HAITI") ~ "Carribean (Haiti)"
  ))
table(df_analysis$region, useNA = "ifany")


#### Save the dataset ####

write.csv2(df_analysis, file = here::here("data", "1-final",
                                          "df_analysis_iwi.csv"))
saveRDS(df_analysis, file = here::here("data", "1-final",
                                       "df_analysis_iwi.rds"))
