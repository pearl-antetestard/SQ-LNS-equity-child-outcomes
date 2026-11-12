## author: Pearl Ante-Testard
## date: June 12, 2024

# Clean the environment
rm(list = ls(all = TRUE))

# Load the necessary package
source(here::here("R", "0-config.R"))


df_analysis <- read_dta(file = here::here("data", "0-untouched",
                                          "IPD_full_20240710.dta")) %>%
               mutate(study_id = paste0(study, "_", study_pid))
nrow(df_analysis) # 38407

# Get unique values of study_pid in df_analysis
unique_study_pid <- unique(df_analysis$study_pid) # 35412

# Print the unique values
print(unique_study_pid)

# Get only the rows with unique values of study_pid
#df_analysis_unique <- df_analysis %>%
#  distinct(study_pid, .keep_all = TRUE)
#nrow(df_analysis_unique) # 35387

table(df_analysis$study, useNA = "always")
#s101_GHANA       s102_DYADG       s103_DYADM    s104_PROMISBF 
#194             1113              675             1782 
#s104_PROMISBF_CS      s105_JiVitA        s106_RDNS        s107_ZINC 
#1157             4568             2567             2647 
#s108_SHINE_HIV-     s109_PROMISM  s109_PROMISM_CS       s110_HAITI 
#3679             1013             1927              322 
#s111_WASHB        s112_DOSE       s113_WASHK  s114_SHINE_HIV+ 
#  4824             1018             6815              668 
#s116_MAHAY             <NA> 
#  3438                0 

# Removing 'h' and 'i1' from the study_pid
#df_analysis$study_pid <- gsub("h", "", df_analysis$study_pid) # Remove all 'h'
#df_analysis$study_pid <- gsub("i1", "", df_analysis$study_pid) # Remove all 'i1'
#df_analysis$study_pid <- gsub("i2", "", df_analysis$study_pid) # Remove all 'i2'

#############################################

# Read the data file for each study

# Ghana
s101_ghana_ses <- read_dta(file = here::here("data", "0-untouched",
                                             "s101_GHANA_ses_20240209.dta"))
colnames(s101_ghana_ses)
nrow(s101_ghana_ses) # 394
unique_study_pid_ghana <- unique(s101_ghana_ses$study_pid) # 394
str(unique_study_pid_ghana)


# DYAD-G
s102_dyadg_ses <- read_dta(file = here::here("data", "0-untouched",
                                            "s102_DYADG_ses_20240209.dta"))
colnames(s102_dyadg_ses)
nrow(s102_dyadg_ses) # 1320
unique_study_pid_dyadg <- unique(s102_dyadg_ses$study_pid) # 1320
str(unique_study_pid_dyadg)


# DYAD-M
s103_dyadm_ses <- read_dta(file = here::here("data", "0-untouched",
                                             "s103_DYADM_ses_20240209.dta"))
colnames(s103_dyadm_ses)
nrow(s103_dyadm_ses) # 1391
unique_study_pid_dyadm <- unique(s103_dyadm_ses$study_pid) # 1391
str(unique_study_pid_dyadm)


# PROMIS-BF-CSBL
#**** Note: Charles: this doesn't merge with the IPD_full dataset because cross-sectional baseline wasn't included in the impact evaluation analyses
#**** Will not include PROMISBF-CSBL in the analysis to be consistent with the previous IPD analysis
s104_promisbf_csbl_ses_1 <- read_dta(file = here::here("data", "0-untouched",
                                   "s104_PROMISBF_CSBL_ses_20240209.dta"))
nrow(s104_promisbf_csbl_ses_1) # 2318
unique_study_pid_promisbf_csbl_1 <- unique(s104_promisbf_csbl_ses_1$study_pid) # 2318 # nolint
str(unique_study_pid_promisbf_csbl_1)

# Removing 'h' and 'i1' from the study_pid
#s104_promisbf_csbl_ses_1$study_pid <- gsub("h", "", s104_promisbf_csbl_ses_1$study_pid) # Remove all 'h'
#s104_promisbf_csbl_ses_1$study_pid <- gsub("i1", "", s104_promisbf_csbl_ses_1$study_pid) # Remove all 'i1'
#s104_promisbf_csbl_ses_1$study_pid <- gsub("i2", "", s104_promisbf_csbl_ses_1$study_pid) # Remove all 'i2'
#nrow(s104_promisbf_csbl_ses_1) # 2318 

#s104_promisbf_csbl_ses_2 <- read_dta(file = here::here("data", "0-untouched",
#                                                     "Burkina Baseline20240524.dta"))
#nrow(s104_promisbf_csbl_ses_2) # 4824
#unique_study_pid_promisbf_csbl_2 <- s104_promisbf_csbl_ses_2 %>%
#                                    distinct(hhid, IDP, .keep_all = TRUE)
#nrow(unique_study_pid_promisbf_csbl_2) # 2365

#s104_promisbf_csbl_ses_2 <- unique_study_pid_promisbf_csbl_2 %>%
#                          select(hhid, basset_2, basset_6, basset_7, basset_56, basset_54,
#                                 basset_1, basset_16, basset_36, basset_34, s4_q06, s4_q05, 
#                                 s17_q01, s4_q01, s6_q08, s4_q04) %>%
#                          rename(study_pid = hhid) %>%f
#                          mutate(study_pid = as.character(study_pid))

#s104_promisbf_csbl_ses <- left_join(s104_promisbf_csbl_ses_1, s104_promisbf_csbl_ses_2,
#                                    by = c("study_pid", "basset_2", "basset_6", "basset_7", "basset_56", 
#                                           "basset_54", "basset_1", "basset_16", "basset_36", "basset_34", 
#                                           "s17_q01", "s6_q08"))
#nrow(s104_promisbf_csbl_ses) # 2318
#colnames(s104_promisbf_csbl_ses)


# PROMIS-BF-CSEL
s104_promisbf_csel_ses_1 <- read_dta(file = here::here("data", "0-untouched",
                                   "s104_PROMISBF_CSEL_ses_20240209.dta"))
colnames(s104_promisbf_csel_ses_1)
nrow(s104_promisbf_csel_ses_1) # 2317
unique_study_pid_promisbf_csel_1 <- unique(s104_promisbf_csel_ses_1$study_pid) # 2317 
str(unique_study_pid_promisbf_csel_1)

# Removing 'h' and 'i1' from the study_pid
#s104_promisbf_csel_ses_1$study_pid <- gsub("h", "", s104_promisbf_csel_ses_1$study_pid) # Remove all 'h'
#s104_promisbf_csel_ses_1$study_pid <- gsub("i1", "", s104_promisbf_csel_ses_1$study_pid) # Remove all 'i1'
#s104_promisbf_csel_ses_1$study_pid <- gsub("i2", "", s104_promisbf_csel_ses_1$study_pid) # Remove all 'i2'
#nrow(s104_promisbf_csel_ses_1) # 2317

#s104_promisbf_csel_ses_2 <- read_dta(file = here::here("data", "0-untouched", 
#                                                       "Burkina Endline20240524.dta"))
#nrow(s104_promisbf_csel_ses_2) # 4810
#colnames(s104_promisbf_csel_ses_2)
#unique_study_pid_promisbf_csel_2 <- s104_promisbf_csel_ses_2 %>%
#  distinct(hhid, IDP, .keep_all = TRUE)
#nrow(unique_study_pid_promisbf_csel_2) # 4810

#s104_promisbf_csel_ses_2 <- unique_study_pid_promisbf_csel_2 %>%
#                            select(hhid, basset_2, basset_6, basset_7, basset_56, basset_54,
#                            basset_1, basset_16, basset_36, basset_34, s4_q06, s4_q05, 
#                            s17_q01, s4_q01, s6_q08, s4_q04) %>%
#                            rename(study_pid = hhid) %>%
#                            mutate(study_pid = as.character(study_pid))
#nrow(s104_promisbf_csel_ses_2) # 4810
#colnames(s104_promisbf_csel_ses_2)

#s104_promisbf_csel_ses <- left_join(s104_promisbf_csel_ses_1, s104_promisbf_csel_ses_2,
#                                    by = c("study_pid"))
#nrow(s104_promisbf_csel_ses) # 2317
#s104_promisbf_csel_ses <- s104_promisbf_csel_ses_2 %>%
#  distinct(study_pid, .keep_all = TRUE)
#nrow(s104_promisbf_csel_ses) # 2346


# PROMIS-BF
s104_promisbf_ses <- read_dta(file = here::here("data", "0-untouched",
                              "s104_PROMISBF_ses_20240209.dta"))
colnames(s104_promisbf_ses)
nrow(s104_promisbf_ses) # 2113
unique_study_pid_promisbf <- unique(s104_promisbf_ses$study_pid) # 2113
str(unique_study_pid_promisbf)


# RDNS
s106_rdns_ses <- read_dta(file = here::here("data", "0-untouched",
                                            "s106_RDNS_ses_20240209.dta"))
colnames(s106_rdns_ses)
nrow(s106_rdns_ses) # 4011
unique_study_pid_rdns <- unique(s106_rdns_ses$study_pid) # 4011
str(unique_study_pid_rdns)


# ZINC
s107_zinc_ses <- read_dta(file = here::here("data", "0-untouched",
                                            "s107_ZINC_ses_20240209.dta"))
colnames(s107_zinc_ses)
nrow(s107_zinc_ses) # 3220
unique_study_pid_zinc <- unique(s107_zinc_ses$study_pid) # 3220
str(unique_study_pid_zinc)


# SHINE
s108_shine_ses <- read_dta(file = here::here("data", "0-untouched",
                               "s108_114_SHINE_ses_20240725.dta"))
colnames(s108_shine_ses)
nrow(s108_shine_ses) # 4843
unique_study_pid_shine <- unique(s108_shine_ses$study_pid) # 4843
str(unique_study_pid_shine)


# PROMIS-Mali-CSBL
#**** Note: Charles: this doesn't merge with the IPD_full dataset because cross-sectional baseline wasn't included in the impact evaluation analyses
#**** Will not include PROMISBF-CSBL in the analysis to be consistent with the previous IPD analysis
s109_promism_csbl_ses_1 <- read_dta(file = here::here("data", "0-untouched",
                                  "s109_PROMISM_CSBL_ses_20240209.dta"))
colnames(s109_promism_csbl_ses_1)
nrow(s109_promism_csbl_ses_1) # 2305
unique_study_pid_promism_csbl_1 <- unique(s109_promism_csbl_ses_1$study_pid) # 2305
str(unique_study_pid_promism_csbl_1)

# Removing 'h' and 'i1' from the study_pid
#s109_promism_csbl_ses_1$study_pid <- gsub("h", "", s109_promism_csbl_ses_1$study_pid) # Remove all 'h'
#s109_promism_csbl_ses_1$study_pid <- gsub("i1", "", s109_promism_csbl_ses_1$study_pid) # Remove all 'i1'
#s109_promism_csbl_ses_1$study_pid <- gsub("i2", "", s109_promism_csbl_ses_1$study_pid) # Remove all 'i2'
#nrow(s109_promism_csbl_ses_1) # 2305
#s109_promism_csbl_ses_2 <- read_dta(file = here::here("data", "0-untouched",
                                  #"Mali Baseline20240524.dta"))
#nrow(s109_promism_csbl_ses_2) # 4502
#unique_study_pid_promism_csbl_2 <- s109_promism_csbl_ses_2 %>%
  #distinct(hhid, IDP, .keep_all = TRUE)
#nrow(unique_study_pid_promism_csbl_2) # 4502

#s109_promism_csbl_ses_2 <- unique_study_pid_promism_csbl_2 %>%
                          #select(hhid, basset_2, basset_6, basset_7, basset_56, basset_54,
                                #basset_1, basset_16, basset_36, basset_34, s4_q06, s4_q05, 
                                #s17_q01, s4_q01, s6_q08, s4_q04) %>%
                          #rename(study_pid = hhid) %>%
                          #mutate(study_pid = as.character(study_pid))
#nrow(s109_promism_csbl_ses_2) # 4502

#s109_promism_csbl_ses <- left_join(s109_promism_csbl_ses_1, s109_promism_csbl_ses_2,
#                                  by = c("study_pid", "basset_2", "basset_6", "basset_7", "basset_56", 
#                                         "basset_54", "basset_1", "basset_16", "basset_36", "basset_34", 
#                                         "s17_q01", "s6_q08"))
#nrow(s109_promism_csbl_ses) # 2305


# PROMIS-Mali-CSEL
s109_promism_csel_ses_1 <- read_dta(file = here::here("data", "0-untouched",
                                  "s109_PROMISM_CSEL_ses_20240209.dta")) 
colnames(s109_promism_csel_ses_1)
nrow(s109_promism_csel_ses_1) # 2316
unique_study_pid_promism_csel_1 <- unique(s109_promism_csel_ses_1$study_pid) # 2316
str(unique_study_pid_promism_csel_1)

# Removing 'h' and 'i1' from the study_pid
#s109_promism_csel_ses_1$study_pid <- gsub("h", "", s109_promism_csel_ses_1$study_pid) # Remove all 'h'
#s109_promism_csel_ses_1$study_pid <- gsub("i1", "", s109_promism_csel_ses_1$study_pid) # Remove all 'i1'
#s109_promism_csel_ses_1$study_pid <- gsub("i2", "", s109_promism_csel_ses_1$study_pid) # Remove all 'i2'
#nrow(s109_promism_csel_ses_1) # 2316
#s109_promism_csel_ses_2 <- read_dta(file = here::here("data", "0-untouched",
#                                  "Mali Endline20240524.dta")) %>%
#  rename(study_pid = hhid) %>%
#  mutate(study_pid = as.character(study_pid))

#nrow(s109_promism_csel_ses_2) # 4589
#unique_study_pid_promism_csel_2 <- s109_promism_csel_ses_2 %>%
#  distinct(study_pid, .keep_all = TRUE) 
#nrow(unique_study_pid_promism_csel_2) # 4589


# PROMIS-Mali
s109_promism_ses <- read_dta(file = here::here("data", "0-untouched",
                                            "s109_PROMISM_ses_20240209.dta"))
colnames(s109_promism_ses)
nrow(s109_promism_ses) # 1152
unique_study_pid_promism <- unique(s109_promism_ses$study_pid) # 1152
str(unique_study_pid_promism)


# HAITI
s110_haiti_ses <- read_dta(file = here::here("data", "0-untouched",
                                             "s110_HAITI_ses_20240212.dta"))
colnames(s110_haiti_ses)
nrow(s110_haiti_ses) # 4085
unique_study_pid_haiti <- unique(s110_haiti_ses$study_pid) # 615
str(unique_study_pid_haiti)
# Get only the rows with unique values of study_pid
s110_haiti_ses_unique <- s110_haiti_ses %>%
  distinct(study_pid, .keep_all = TRUE)
nrow(s110_haiti_ses_unique) # 615


# WASHB
s111_washb_ses <- read_dta(file = here::here("data", "0-untouched",
                                             "s111_WASHB_ses_20240209.dta"))
colnames(s111_washb_ses)
nrow(s111_washb_ses) # 10047
unique_study_pid_washb <- unique(s111_washb_ses$study_pid) # 5551
str(unique_study_pid_washb) 
# Get only the rows with unique values of study_pid
s111_washb_ses_unique <- s111_washb_ses %>%
  distinct(study_pid, hhid, .keep_all = TRUE)
nrow(s111_washb_ses_unique) # 5551


# DOSE
s112_dose_ses_1 <- read_dta(file = here::here("data", "0-untouched",
                                            "s112_DOSE_ses_20240209.dta"))
#"iLiNS-DOSE SES Variables 2014-11-01.dta"))
colnames(s112_dose_ses_1)
nrow(s112_dose_ses_1) # 1932
unique_study_pid_dose_1 <- unique(s112_dose_ses_1$study_pid) # 1932
str(unique_study_pid_dose_1)

s112_dose_ses_2 <- read_excel(here::here("data", "0-untouched",
                                         "ExportForm19a_DOSE.xlsx"))

s112_dose_ses_2 <- s112_dose_ses_2 %>%
                   select(Child_Number, SocNumbRefrige, # added these additional variables to the dataset 
                          SocNumbCellPho, SocNumbCar, SocNumbBicycle,
                          SocNumbTable, SocNumbRadio, SocNumbAirCond, SocSpecWater,
                          SocElectricity, SocHouseRooms) %>%
                   rename(study_pid = Child_Number) %>%
                   mutate(study_pid = as.character(study_pid))
nrow(s112_dose_ses_2) # 5121

s112_dose_ses <- left_join(s112_dose_ses_1, s112_dose_ses_2, by = "study_pid")
nrow(s112_dose_ses) # 5244
colnames(s112_dose_ses)


# WASHK
s113_washk_ses <- read_dta(file = here::here("data", "0-untouched",
"s113_WASHK_ses_20240710.dta"))
colnames(s113_washk_ses)
nrow(s113_washk_ses) # 17858
unique_study_pid_washk <- unique(s113_washk_ses$study_pid) # 17858
str(unique_study_pid_washk)


# MAHAY
s116_mahay_ses <- read_dta(file = here::here("data", "0-untouched",
                                             "s116_MAHAY_ses_20240209.dta"))
colnames(s116_mahay_ses)
nrow(s116_mahay_ses) # 4676
unique_study_pid_mahay <- unique(s116_mahay_ses$study_pid) # 4676
str(unique_study_pid_mahay)


# JiViTA
s105_jivita_ses <- read_dta(file = here::here("data", "0-untouched",
                                             "s105_JiVitA_ses_20240402.dta"))
nrow(s105_jivita_ses) # 5449
unique_study_pid_jivita <- unique(s105_jivita_ses$study_pid) # 5449
str(unique_study_pid_jivita)


##############################
##############################

# Clean the data

# Ghana
clean_s101_ghana_ses <- s101_ghana_ses %>%
  select(study_pid, owntel2, ownvehp2, ownmot2, ownrad2, owncas2, liguse2,
         sorwat, sorwatsp, toifac, toifacsp, numrom) %>%
  mutate(study = as.character("s101_GHANA"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(owntel2 == 1 ~ 1,
                                  owntel2 == 0 ~ 0),
         car_iwi = case_when(ownvehp2 == 1 ~ 1,
                                   ownvehp2 == 0 ~ 0),
         bike_iwi = case_when(ownmot2 == 1 ~ 1, # motorcycle
                                    ownmot2 == 0 ~ 0),
         cheap_iwi = case_when(ownrad2 == 1 ~ 1,
                                     ownrad2 == 0 ~ 0),
         expensive_iwi = case_when(ownmot2 == 1 ~ 1, # motorcycle
                                         ownmot2 == 0 ~ 0), 
         elec_iwi = case_when(liguse2 == 1 ~ 1, # kind of lighting used: 1=electricity, 2=kerosene, 3=other
                                    liguse2 == 2 ~ 0,
                                    liguse2 == 3 ~ 0),
         watersource_iwi = case_when(sorwat == 1 ~ 1, # source of water: 1=municipal piped supply (333), 2=specify (53), 3=DK/NR? (2), NA=6
                                           sorwat == 2 & sorwatsp == "Bore hole" ~ 2,
                                           sorwat == 2 & sorwatsp == "Borehole" ~ 2,
                                           sorwat == 2 & sorwatsp == "Rain Water" ~ 1,
                                           sorwat == 2 & sorwatsp == "Rainwater" ~ 1,
                                           sorwat == 2 & sorwatsp == "Sachet Water" ~ 2,
                                           sorwat == 2 & sorwatsp == "Schet water" ~ 2,
                                           sorwat == 2 & sorwatsp == "stream" ~ 1,
                                           sorwat == 2 & sorwatsp == "well" ~ 1,
                                           sorwat == 2 & sorwatsp == "Well" ~ 1,
                                           sorwat == 2 & sorwatsp == "Well water" ~ 1,
                                           sorwat == 2 & sorwatsp == "Well Water" ~ 1),
         watersource_iwi = factor(watersource_iwi, levels = c(1, 2, 3)),
         toiletfac_iwi = case_when(toifac == 1 ~ 3, # 1=flush toilet (91), 2=pan latrine (86), 3=KVIP (public) (178), 4=other specify (34), 8=NR/DK
                                         toifac == 2 ~ 2, 
                                         toifac == 3 ~ 2,
                                         toifac == 4 & toifacsp == "Bush" ~ 1,
                                         toifac == 4 & toifacsp == "Deep  Pit" ~ 1,
                                         toifac == 4 & toifacsp == "deep pit" ~ 1,
                                         toifac == 4 & toifacsp == "Deep pit" ~ 1,
                                         toifac == 4 & toifacsp == "Deep Pit" ~ 1,
                                         toifac == 4 & toifacsp == "KVIP (Private )" ~ 3), # 1 KVIP private
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3)),
         rooms_iwi = case_when(numrom == 1 ~ 1, # 0 or 1
                                     numrom == 2 ~ 2, # 2
                                     numrom >= 3 ~ 3),# 3 or more
         rooms_iwi = factor(rooms_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, car_iwi, cheap_iwi, expensive_iwi, elec_iwi, watersource_iwi,
         toiletfac_iwi, rooms_iwi)


# Recode the 3-category variables into dummies
clean_s101_ghana_ses <- clean_s101_ghana_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 1,
                                  watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 1),
         water3 = factor(water3, levels = c(0, 1)))

clean_s101_ghana_ses <- clean_s101_ghana_ses %>% 
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                   toiletfac_iwi == 2 ~ 0,
                                   toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                   toiletfac_iwi == 2 ~ 1,
                                   toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                   toiletfac_iwi == 2 ~ 0,
                                   toiletfac_iwi == 3 ~ 1))

clean_s101_ghana_ses <- clean_s101_ghana_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                   rooms_iwi == 2 ~ 0,
                                   rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                   rooms_iwi == 2 ~ 1,
                                   rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                   rooms_iwi == 2 ~ 0,
                                   rooms_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil 
clean_s101_ghana_ses <- clean_s101_ghana_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s101_ghana_ses) # 394


# DYAD-G
clean_s102_dyadg_ses <- s102_dyadg_ses %>%
  select(study_pid, bhe8it22tv, bhe8it7ref, bhe8it12phone, bhe8it27car,
         bhe8it25bicycle, bhe8it13radio, bhe8it9aircon, bhe8it5elec,
         bhd7wter, bhd3flor, bhd8tolet, bhd4rm) %>%
  mutate(study = as.character("s102_DYADG"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(bhe8it22tv == 0 ~ 0,
                                  bhe8it22tv >= 1 ~ 1),
         ref_iwi = case_when(bhe8it7ref == 0 ~ 0,
                                   bhe8it7ref >= 1 ~ 1),
         phone_iwi = case_when(bhe8it12phone == 0 ~ 0,
                                     bhe8it12phone >= 1 ~ 1),
         car_iwi = case_when(bhe8it27car == 0 ~ 0,
                                  bhe8it27car >= 1 ~ 1),
         bike_iwi = case_when(bhe8it25bicycle == 0 ~ 0,
                                   bhe8it25bicycle >= 1 ~ 1),
         cheap_iwi = case_when(bhe8it13radio == 0 ~ 0,
                                    bhe8it13radio >= 1 ~ 1),
         expensive_iwi = case_when(bhe8it9aircon == 0 ~ 0,
                                        bhe8it9aircon >= 1 ~ 1),
         elec_iwi = case_when(bhe8it5elec == 0 ~ 0,
                                   bhe8it5elec >= 1 ~ 1),
         watersource_iwi = case_when(bhd7wter == 1 ~ 3, # 1=indoor plumbing, 2=standpipe inside, 3=water truck, 4=water sachet, 5=neighbor pipe, 6=standpipe outside private, 7=standpipe public, 8=borehole, 9=well-protected, 10=well-unprotected, 11=river
                                          bhd7wter == 2 ~ 2,
                                          bhd7wter == 3 ~ 2,
                                          bhd7wter == 4 ~ 2,
                                          bhd7wter == 5 ~ 3,
                                          bhd7wter == 6 ~ 3,
                                          bhd7wter == 7 ~ 2,
                                          bhd7wter == 8 ~ 2,
                                          bhd7wter == 9 ~ 2,
                                          bhd7wter == 10 ~ 1,
                                          bhd7wter == 11 ~ 1),
         floormat_iwi = case_when(bhd3flor == 1 ~ 1, # 1=Earth/mud/mud bricks, 2=Cement, 3=Wood, 4=Stone, 5=Burnt bricks, 6=Ceramic/Marble/Tiles, 7=Vinyl tiles, 8=Terrazzo, 66=NA
                                       bhd3flor == 2 ~ 2,
                                       bhd3flor == 3 ~ 2,
                                       bhd3flor == 4 ~ 2,
                                       bhd3flor == 5 ~ 2,
                                       bhd3flor == 6 ~ 3,
                                       bhd3flor == 7 ~ 3,
                                       bhd3flor == 8 ~ 3),
         toiletfac_iwi = case_when(bhd8tolet == 1 ~ 3,
                                      bhd8tolet == 2 ~ 2,
                                      bhd8tolet == 3 ~ 1,
                                      bhd8tolet == 4 ~ 2,
                                      bhd8tolet == 5 ~ 2,
                                      bhd8tolet == 6 ~ 1),
         rooms_iwi = case_when(bhd4rm == 1 ~ 1, # 0 or 1
                                    bhd4rm == 2 ~ 2, # 2
                                    bhd4rm >= 3 ~ 3)) %>% # 3 or more
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi, rooms_iwi)

# Recode the 3-category variables into dummies
clean_s102_dyadg_ses <- clean_s102_dyadg_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 1,
                                  watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 1),
         water3 = factor(water3, levels = c(0, 1)))

clean_s102_dyadg_ses <- clean_s102_dyadg_ses %>% 
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                   floormat_iwi == 2 ~ 0,
                                   floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                   floormat_iwi == 2 ~ 1,
                                   floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                   floormat_iwi == 2 ~ 0,
                                   floormat_iwi == 3 ~ 1))

clean_s102_dyadg_ses <- clean_s102_dyadg_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                   toiletfac_iwi == 2 ~ 0,
                                   toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                   toiletfac_iwi == 2 ~ 1,
                                   toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                   toiletfac_iwi == 2 ~ 0,
                                   toiletfac_iwi == 3 ~ 1))

clean_s102_dyadg_ses <- clean_s102_dyadg_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                   rooms_iwi == 2 ~ 0,
                                   rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                   rooms_iwi == 2 ~ 1,
                                   rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                   rooms_iwi == 2 ~ 0,
                                   rooms_iwi == 3 ~ 1))
        
# If household has a more expensive utensil, it is assumed to have also a cheaper utensil 
clean_s102_dyadg_ses <- clean_s102_dyadg_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3 |
                                                                                  floormat_iwi == 1) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)
nrow(clean_s102_dyadg_ses) # 1320
        

  
# DYAD-M
clean_s103_dyadm_ses <- s103_dyadm_ses %>%
  select(study_pid, FSenumbtv, FSenumbrefrigerator, FSenumbcellphone,
         FSenumbcar, FSenumbbicycle, FSenumbradio, FSenumbaircon, SocElectricity,
         SocSourceWater, SocSanitaryFac, SocHouseRooms) %>%
  mutate(study = as.character("s103_DYADM"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(FSenumbtv == 0 ~ 0,
                                  FSenumbtv >= 1 ~ 1),
         ref_iwi = case_when(FSenumbrefrigerator == 0 ~ 0,
                                   FSenumbrefrigerator >= 1 ~ 1),
         phone_iwi = case_when(FSenumbcellphone == 0 ~ 0,
                                     FSenumbcellphone >= 1 ~ 1),
         car_iwi = case_when(FSenumbcar == 0 ~ 0,
                                  FSenumbcar >= 1 ~ 1),
         bike_iwi = case_when(FSenumbbicycle == 0 ~ 0,
                                   FSenumbbicycle >= 1 ~ 1),
         cheap_iwi = case_when(FSenumbradio == 0 ~ 0,
                                    FSenumbradio >= 1 ~ 1),
         expensive_iwi = case_when(FSenumbaircon == 0 ~ 0,
                                        FSenumbaircon >= 1 ~ 1),
         elec_iwi = case_when(SocElectricity == 0 ~ 0,
                                   SocElectricity >= 1 ~ 1),
         watersource_iwi = case_when(SocSourceWater == 1 ~ 3, # 1=Piped water, 2=Borehole, 3=Protected well, 4=Unprotected well, 5=Lake, 6=River, pond, 66=Other
                                          SocSourceWater == 2 ~ 2,
                                          SocSourceWater == 3 ~ 2,
                                          SocSourceWater == 4 ~ 1,
                                          SocSourceWater == 5 ~ 1,
                                          SocSourceWater == 6 ~ 1),
         toiletfac_iwi = case_when(SocSanitaryFac == 0 ~ 1, # 0=None, 1=Regular pit latrine, 2=Vent. Impr. Pit latrine, 3=Water closet (WC)
                                      SocSanitaryFac == 1 ~ 1,
                                      SocSanitaryFac == 2 ~ 2,
                                      SocSanitaryFac == 3 ~ 3),
         rooms_iwi = case_when(SocHouseRooms == 0 ~ 1, # 0 or 1
                                     SocHouseRooms == 1 ~ 1, # 0 or 1
                                     SocHouseRooms == 2 ~ 2, # 2
                                     SocHouseRooms >= 3 ~ 3)) %>% # 3 or more
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, toiletfac_iwi, rooms_iwi)
         
         
# Recode the 3-category variables into dummies
clean_s103_dyadm_ses <- clean_s103_dyadm_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 1,
                                  watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 1))

clean_s103_dyadm_ses <- clean_s103_dyadm_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                   toiletfac_iwi == 2 ~ 0,
                                   toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                   toiletfac_iwi == 2 ~ 1,
                                   toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                   toiletfac_iwi == 2 ~ 0,
                                   toiletfac_iwi == 3 ~ 1))

clean_s103_dyadm_ses <- clean_s103_dyadm_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                   rooms_iwi == 2 ~ 0,
                                   rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                   rooms_iwi == 2 ~ 1,
                                   rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                   rooms_iwi == 2 ~ 0,
                                   rooms_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s103_dyadm_ses <- clean_s103_dyadm_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s103_dyadm_ses) # 1391
   

# PROMIS-BF-CSEL
clean_s104_promisbf_csel_ses <- s104_promisbf_csel_ses_1 %>%
#  select(study_pid, basset_2, basset_6, basset_7, basset_56, basset_54,
#         basset_1, basset_16, basset_36, basset_34, s4_q06, s4_q05, 
#         s17_q01, s4_q01, s6_q08, s4_q04) %>%
  mutate(study = as.character("s104_PROMISBF_CS"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(basset_2 == 0 ~ 0,
                            basset_2 == 1 ~ 1),
         phone_iwi = case_when(basset_6 == 0 ~ 0,
                               basset_6 == 1 ~ 1),
         car_iwi = case_when(basset_56 == 0 ~ 0,
                             basset_56 == 1 ~ 1),
         bike_iwi = case_when(basset_54 == 0 ~ 0,
                              basset_54 == 1 ~ 1),
         cheap_iwi = case_when(basset_1 == 0 ~ 0,
                               basset_1 == 1 ~ 1),
         expensive_iwi = case_when(basset_36 == 0 ~ 0,
                                   basset_36 == 1 ~ 1),
         watersource_iwi = case_when(s17_q01 == 11 ~ 3, # 11=Piped water:Tap in the house
                                                  s17_q01 == 12 ~ 3, # 12=Piped water:Tap in the concession
                                                  s17_q01 == 13 ~ 2, # 13=Piped water:Public tap (ONEA)
                                                  s17_q01 == 14 ~ 3, # 14=Piped water:Neighbor's tap
                                                  s17_q01 == 21 ~ 1, # 21=Open well:Unprotected well in the house
                                                  s17_q01 == 22 ~ 1, # 22=Open well:Unprotected well in concession
                                                  s17_q01 == 23 ~ 1, # 23=Open well:Unprotected public well
                                                  s17_q01 == 24 ~ 1, # 24=Open well:Neighbor's unprotected well
                                                  s17_q01 == 31 ~ 2, # 31=Protected well:Covered well in house
                                                  s17_q01 == 32 ~ 2, # 32=Protected well:Covered well in concession
                                                  s17_q01 == 33 ~ 2, # 33=Protected well:Covered public well
                                                  s17_q01 == 34 ~ 2, # 34=Protected well:Neighbor's covered well
                                                  s17_q01 == 41 ~ 1, # 41=Surface water:Spring
                                                  s17_q01 == 42 ~ 1, # 42=Surface water:River/stream
                                                  s17_q01 == 43 ~ 1), # 43=Surface water:Pond/Lakes
         toiletfac_iwi = case_when(s6_q08 == 22 ~ 2, # 22=Pit latrine with slab
                                                s6_q08 == 23 ~ 1, # 23=Pit latrine without slab/Open pit
                                                s6_q08 == 41 ~ 1, # 41=Bucket
                                                s6_q08 == 51 ~ 1, # 51=No facility/Bush/Field
                                                s6_q08 == 52 ~ 1), # 52=Pit manure heap
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, watersource_iwi, toiletfac_iwi)
         

# Recode the 3-category variables into dummies
clean_s104_promisbf_csel_ses <- clean_s104_promisbf_csel_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                         watersource_iwi == 2 ~ 0,
                                         watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                         watersource_iwi == 2 ~ 1,
                                         watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                         watersource_iwi == 2 ~ 0,
                                         watersource_iwi == 3 ~ 1))

clean_s104_promisbf_csel_ses <- clean_s104_promisbf_csel_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                          toiletfac_iwi == 2 ~ 0,
                                          toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                          toiletfac_iwi == 2 ~ 1,
                                          toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                          toiletfac_iwi == 2 ~ 0,
                                          toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s104_promisbf_csel_ses <- clean_s104_promisbf_csel_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s104_promisbf_csel_ses) # 2317



# PROMIS-BF
clean_s104_promisbf_ses <- s104_promisbf_ses %>%
  select(study_pid, s5_q12_48, s5_q12_47, s5_q12_46, s5_q12_9, s5_q12_5, s5_q12_38,
         s5_q12_45, s5_q12_14, s5_q12_42, s4_q06, s4_q05, s17_q01, s4_q01,
         s6_q08) %>%
  mutate(study = as.character("s104_PROMISBF"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(s5_q12_48 == 0 ~ 0,
                            s5_q12_48 == 1 ~ 1),
         phone_iwi = case_when(s5_q12_47 == 0 ~ 0,
                               s5_q12_47 == 1 ~ 1),
         car_iwi = case_when(s5_q12_9 == 0 ~ 0,
                             s5_q12_9 == 1 ~ 1),
         bike_iwi = case_when(s5_q12_5 == 0 ~ 0,
                              s5_q12_5 == 1 ~ 1),
         cheap_iwi = case_when(s5_q12_38 == 0 ~ 0,
                               s5_q12_38 == 1 ~ 1),
         expensive_iwi = case_when(s5_q12_14 == 0 ~ 0,
                                   s5_q12_14 == 1 ~ 1),
         elec_iwi = case_when(s4_q06 == 1 ~ 0, # source of lighting: 1=Firewood
                                       s4_q06 == 2 ~ 0, # 2=Charcoal
                                       s4_q06 == 3 ~ 0, # 3=Petroleum
                                       s4_q06 == 5 ~ 1, # 5=Electricity
                                       s4_q06 == 6 ~ 1, # 6=Generator
                                       s4_q06 == 7 ~ 1, # 7=Solar panels
                                       s4_q06 == 8 ~ 0, # 8=Crop residual
                                       s4_q06 == 10 ~ 1, # 10=Batteries
                                       s4_q06 == -96 ~ 0), # -96= Other
         watersource_iwi = case_when(s17_q01 == 11 ~ 3, # 11=Piped water:Tap in the house
                                              s17_q01 == 12 ~ 3, # 12=Piped water:Tap in the concession
                                              s17_q01 == 13 ~ 2, # 13=Piped water:Public tap (ONEA)
                                              s17_q01 == 14 ~ 3, # 14=Piped water:Neighbor's tap
                                              s17_q01 == 21 ~ 1, # 21=Open well:Unprotected well in the house
                                              s17_q01 == 22 ~ 1, # 22=Open well:Unprotected well in concession
                                              s17_q01 == 23 ~ 1, # 23=Open well:Unprotected public well
                                              s17_q01 == 24 ~ 1, # 24=Open well:Neighbor's unprotected well
                                              s17_q01 == 31 ~ 2, # 31=Protected well:Covered well in house
                                              s17_q01 == 32 ~ 2, # 32=Protected well:Covered well in concession
                                              s17_q01 == 33 ~ 2, # 33=Protected well:Covered public well
                                              s17_q01 == 34 ~ 2, # 34=Protected well:Neighbor's covered well
                                              s17_q01 == 41 ~ 1, # 41=Surface water:Spring
                                              s17_q01 == 42 ~ 1, # 42=Surface water:River/stream
                                              s17_q01 == 43 ~ 1), # 43=Surface water:Pond/Lakes
         floormat_iwi = case_when(s4_q01 == 11 ~ 1, # 11=Earth
                                           s4_q01 == 21 ~ 1, # 21=Banco
                                           s4_q01 == 31 ~ 2, # 31=Baked Bricks
                                           s4_q01 == 32 ~ 3, # 32=linoleum (plastic)
                                           s4_q01 == 33 ~ 3, # 33=Ceramic tiles, parquet
                                           s4_q01 == 34 ~ 2, # 34=Cement
                                           s4_q01 == 35 ~ 3), # 35=Carpet
         toiletfac_iwi = case_when(s6_q08 == 22 ~ 2, # 22=Pit latrine with slab
                                            s6_q08 == 23 ~ 1, # 23=Pit latrine without slab/Open pit
                                            s6_q08 == 41 ~ 1, # 41=Bucket
                                            s6_q08 == 51 ~ 1, # 51=No facility/Bush/Field
                                            s6_q08 == 52 ~ 1), # 52=Pit manure heap
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi)
         
# Recode the 3-category variables into dummies
clean_s104_promisbf_ses <- clean_s104_promisbf_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                     watersource_iwi == 2 ~ 0,
                                     watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                     watersource_iwi == 2 ~ 1,
                                     watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                     watersource_iwi == 2 ~ 0,
                                     watersource_iwi == 3 ~ 1))

clean_s104_promisbf_ses <- clean_s104_promisbf_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                        floormat_iwi == 2 ~ 0,
                                        floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                        floormat_iwi == 2 ~ 1,
                                        floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                        floormat_iwi == 2 ~ 0,
                                        floormat_iwi == 3 ~ 1))

clean_s104_promisbf_ses <- clean_s104_promisbf_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                      toiletfac_iwi == 2 ~ 0,
                                      toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                      toiletfac_iwi == 2 ~ 1,
                                      toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                      toiletfac_iwi == 2 ~ 0,
                                      toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s104_promisbf_ses <- clean_s104_promisbf_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s104_promisbf_ses) # 2113


                       
# RDNS
clean_s106_rdns_ses <- s106_rdns_ses %>%
  select(study_pid, SS233B, SS233E, SS233C, SS233P, SS233M, SS233A, SS233J,
         SS233Q, SS231, SS223, SS223T, SS217, SS217T, SS227, SS227T, SS219,
         SS218) %>%
  mutate(study = as.character("s106_RDNS"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(SS233B == 2 ~ 0, # 2=no
                            SS233B == 1 ~ 1), # 1=yes
         ref_iwi = case_when(SS233E == 2 ~ 0, # 2=no
                             SS233E == 1 ~ 1), # 1=yes
         phone_iwi = case_when(SS233C == 2 ~ 0, # 2=no
                               SS233C == 1 ~ 1), # 1=yes
         car_iwi = case_when(SS233P == 2 ~ 0, # 2=no
                             SS233P == 1 ~ 1), # 1=yes
         bike_iwi = case_when(SS233M == 2 ~ 0, # 2=no
                              SS233M == 1 ~ 1), # 1=yes
         cheap_iwi = case_when(SS233A == 2 ~ 0, # 2=no
                               SS233A == 1 ~ 1), # 1=yes
         expensive_iwi = case_when(SS233Q == 2 ~ 0, # 2=no
                                   SS233Q == 1 ~ 1), # 1=yes
         elec_iwi = case_when(SS231 == 2 ~ 0, # 2=no
                              SS231 == 1 ~ 1), # 1=yes
         watersource_iwi = case_when(SS223 == 1 ~ 3, #1 = Piped into dwelling
                                  SS223 == 2 ~ 3, #2 = Piped to yard/plot	
                                  SS223 == 3 ~ 2, #3 = Public tap/standpipe
                                  SS223 == 4 ~ 2, #4 = Tube well/ Shallow tube well
                                  SS223 == 5 ~ 2, #5 = Directly from irrigation pump
                                  SS223 == 6 ~ 2, #6 = Protected well
                                  SS223 == 7 ~ 1, #7 = Unprotected well
                                  SS223 == 8 ~ 1, #8 = Rainwater
                                  SS223 == 9 ~ 1, #9 = Pond/Dighi/small pond
                                  SS223 == 10 ~ 1, #10 = River/fountain/canal/irrigation canal/beel
                                  SS223 == 11 ~ 3), #11 = Bottled water
         watersource_iwi = factor(watersource_iwi, levels = c(1, 2, 3)),
         floormat_iwi = case_when(SS217 == 66 ~ 2, # 66 (1) SS217T == "Bamboo and soil"
                                       SS217 == 1 ~ 1, # 1=Earth / Sand / Mud
                                       SS217 == 2 ~ 2, # 2=Wood planks
                                       SS217 == 3 ~ 2, # 3=Palm / Bamboo
                                       SS217 == 4 ~ 2, # 4=Brick, cement
                                       SS217 == 5 ~ 2, # 5=Concrete
                                       SS217 == 7 ~ 3, # 7=Parquet or polished wood (This might've been miscoded as 7 instead of 6)
                                       SS217 == 8 ~ 3), # 8=Ceramic tiles / Marble/ Mosaic (This might've been miscoded as 8 instead of 7)
         toiletfac_iwi = case_when(SS227 == 1 ~ 3, # 1 = Flush to piped sewer system
                                        SS227 == 2 ~ 3, # 2 = Flush to septic tank
                                        SS227 == 3 ~ 3, # 3 = Flush to pit latrine
                                        SS227 == 4 ~ 3, # 4 = Flush to somewhere else
                                        SS227 == 5 ~ 3, # 5 = Flush, don’t know where
                                        SS227 == 6 ~ 2, # 6 = Pit latrine with slab and water seal
                                        SS227 == 7 ~ 2, # 7 = Pit latrine with slab, no water seal but has a lid
                                        SS227 == 8 ~ 2, # 8 = Pit latrine with slab, no water seal and no lid
                                        SS227 == 9 ~ 1, # 9 = Hanging toilet / Hanging latrine/open latrine
                                        SS227 == 10 ~ 1, # 10 = No facility/ bush/ field
                                        SS227 == 66 ~ 1), # 66 (1) By making pit in the ground
         rooms_iwi = case_when(SS218 == 1 ~ 1, # 0 or 1
                                   SS218 == 2 ~ 2, # 2
                                   SS218 >= 3 ~ 3)) %>% # 3 or more
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi, rooms_iwi)
                                  
# Recode the 3-category variables into dummies
clean_s106_rdns_ses <- clean_s106_rdns_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 0),
         water1 = factor(water1, levels = c(0, 1)),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 1,
                                 watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 1))

clean_s106_rdns_ses <- clean_s106_rdns_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1))

clean_s106_rdns_ses <- clean_s106_rdns_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))
    
clean_s106_rdns_ses <- clean_s106_rdns_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                  rooms_iwi == 2 ~ 0,
                                  rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                  rooms_iwi == 2 ~ 1,
                                  rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                  rooms_iwi == 2 ~ 0,
                                  rooms_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s106_rdns_ses <- clean_s106_rdns_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s106_rdns_ses) # 4011
         


# ZINC
clean_s107_zinc_ses <- s107_zinc_ses %>%
  select(study_pid, bss6116, bss61121, bss6117, bss6118, bss61125, bss61111,
         bss6115, bss6117, bss61128, bss0067, bss067p, bss0069, bss069p,
         bss0062, bss062p, bss0066, bss066p, bss0064) %>%
  mutate(study = as.character("s107_ZINC"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(bss6116 == 0 ~ 0,
                            bss6116 >= 1 ~ 1),
         ref_iwi = case_when(bss61121 == 0 ~ 0,
                             bss61121 == 1 ~ 1),
         phone_iwi = case_when(bss6117 == 0 ~ 0,
                               bss6117 >= 1 ~ 1),
         car_iwi = case_when(bss61125 == 0 ~ 0,
                             bss61125 == 1 ~ 1),
         bike_iwi = case_when(bss61111 == 0 ~ 0,
                              bss61111 >= 1 ~ 1),
         cheap_iwi = case_when(bss6115 == 0 ~ 0,
                               bss6115 >= 1 ~ 1),
         expensive_iwi = case_when(bss61128 == 0 ~ 0,
                                   bss61128 >= 1 ~ 1),
         elec_iwi = case_when(bss0067 == 0 ~ 0, # 0=Nothing
                                   bss0067 == 1 ~ 1, # 1=Electricity
                                   bss0067 == 2 ~ 1, # 2=Generator
                                   bss0067 == 3 ~ 1, # 3=Battery
                                   bss0067 == 4 ~ 1, # 4=Solar panel
                                   bss0067 == 5 ~ 0, # 5=Kerosine lamp
                                   bss0067 == 6 ~ 0, # 6=Gas lamp
                                   bss0067 == 7 ~ 0, # 7=Wood lamp
                                   bss0067 == 8 ~ 0, # 8=Oil lamp
                                   bss0067 == 9 ~ 0, # 9=Torch
                                   bss0067 == 66 ~ 1), # 66=Other, specify bss067p (bio gas (1))
         watersource_iwi = case_when(bss0069 == 1 ~ 3, # 1=Piped water in or outside the concession
                                          bss0069 == 2 ~ 2, # 2=Covered well
                                          bss0069 == 3 ~ 1, # 3=Non-covered well
                                          bss0069 == 4 ~ 1, # 4=Surface water (river, etc.)
                                          bss0069 == 5 ~ 2 ), # 5=Pump
         floormat_iwi = case_when(bss0062 == 1 ~ 2, # 1=Cement or rock
                                  bss0062 == 2 ~ 1), # 2=Packed dirt,  66=Other, specify
         floormat_iwi = factor(floormat_iwi, levels = c(1, 2, 3)),
         toiletfac_iwi = case_when(bss0066 == 1 ~ 3, # 1=WC with running water
                                        bss0066 == 2 ~ 2, # 2=Improved latrine
                                        bss0066 == 3 ~ 1, # 3=Unimproved latrine
                                        bss0066 == 4 ~ 1), # 4=Nature, 66=Other, specify
         rooms_iwi = case_when(bss0064 == 1 ~ 1, # 0 or 1
                                   bss0064 == 2 ~ 2, # 2
                                   bss0064 >= 3 ~ 3)) %>% # 3 or more
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi, rooms_iwi) 
                          
# Recode the 3-category variables into dummies
clean_s107_zinc_ses <- clean_s107_zinc_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 1,
                                 watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 1))

clean_s107_zinc_ses <- clean_s107_zinc_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1),
         floor3 = factor(floor3, levels = c(0, 1)))
         
clean_s107_zinc_ses <- clean_s107_zinc_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1))

clean_s107_zinc_ses = clean_s107_zinc_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                  rooms_iwi == 2 ~ 0,
                                  rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                  rooms_iwi == 2 ~ 1,
                                  rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                  rooms_iwi == 2 ~ 0,
                                  rooms_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s107_zinc_ses <- clean_s107_zinc_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)


 
# SHINE
clean_s108_shine_ses <- s108_shine_ses %>%
  select(study_pid, r01_tv, r01_phone, r01_car, r01_bicycle, r01_watch, p17_solar, haselec,
         g02, x40_goodfloor, g38, g39) %>%
  mutate(study = as.character("s108_SHINE_HIV-"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(r01_tv == 0 ~ 0,
                            r01_tv == 1 ~ 1),
         phone_iwi = case_when(r01_phone == 0 ~ 0,
                               r01_phone == 1 ~ 1),
         car_iwi = case_when(r01_car == 0 ~ 0,
                             r01_car == 1 ~ 1),
         bike_iwi = case_when(r01_bicycle == 0 ~ 0,
                              r01_bicycle == 1 ~ 1),
         cheap_iwi = case_when(r01_watch == 0 ~ 0,
                               r01_watch == 1 ~ 1),
         expensive_iwi = case_when(p17_solar == 0 ~ 0,
                                   p17_solar == 1 ~ 1),
         elec_iwi = case_when(haselec == 0 ~ 0,
                              haselec == 1 ~ 1),
         watersource_iwi = case_when(g02 == 1 ~ 3, # 1=piped into dwelling
                                          g02 == 2 ~ 3, # 2=piped into yard/plot
                                          g02 == 3 ~ 2, # 3=piped into public tap or standpipe
                                          g02 == 4 ~ 2, # 4=borehole
                                          g02 == 5 ~ 2, # 5=deep well, protected
                                          g02 == 6 ~ 1, # 6=Deep well, unprotected
                                          g02 == 7 ~ 2, # 7=shallow well, protected
                                          g02 == 8 ~ 1, # 8=shallow well, unprotected
                                          g02 == 9 ~ 2, # 9=improvised shallow well
                                          g02 == 10 ~ 2, # 10=protected spring
                                          g02 == 11 ~ 1, # 11=unprotected spring
                                          g02 == 12 ~ 1, # 12=surface water (river/dam/stream/lake)
                                          g02 == 13 ~ 1, # 13=river bank/bed
                                          g02 == 14 ~ 1, # 14=rainwater harvester
                                          g02 == 15 ~ 2, # 15=water trucking/bowser
                                          g02 == 16 ~ NA, # 16=other
                                          g02 == 17 ~ NA), # 17=?
         floormat_iwi = case_when(x40_goodfloor == 1 ~ 2, # assumed that having good quality floor has moderate quality based on the global data lab categories
                                  x40_goodfloor == 0 ~ 1),
         floormat_iwi = factor(floormat_iwi, levels = c(1, 2, 3)),
         toiletfac_iwi = case_when(g38 == 1 & g39 == 1 ~ 3,  # 1=flush toilet
                                  #g38 == 2 & g39 == 2 ~ 2,  # 2=blair latrine(VIP) (commented out)
                                   g38 == 1 & g39 == 3 ~ 2,  # 3=pit latrine with slab (non VIP)
                                   g38 == 1 & g39 == 4 ~ 1,  # 4=Pit latrine with no slab
                                   g38 == 1 & g39 == 5 ~ 2,  # 5=composting toilet
                                   g39 == 2 ~ 2,  # 2=blair latrine(VIP)
                                  g38 == 2 & (is.na(g39) | (g39 != 1 & g39 != 2 & g39 != 3 & g39 != 4 & g39 != 5)) ~ 1)) %>% 
  select(study, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi)
nrow(clean_s108_shine_ses) # 4843

# Recode the 3-category variables into dummies
clean_s108_shine_ses <- clean_s108_shine_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 1,
                                 watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 1))

clean_s108_shine_ses <- clean_s108_shine_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1),
         floor3 = factor(floor3, levels = c(0, 1)))

clean_s108_shine_ses <- clean_s108_shine_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s108_shine_ses <- clean_s108_shine_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

# PROMIS-Mali-CSEL
clean_s109_promism_csel_ses_1 <- s109_promism_csel_ses_1 %>%
  select(study_pid, basset_2, basset_6, basset_7, basset_56, basset_54,
         basset_1, basset_16, basset_36, basset_34, 
         s17_q01, s6_q08) %>%
  mutate(study = as.character("s109_PROMISM_CS"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(basset_2 == 0 ~ 0,
                            basset_2 == 1 ~ 1),
         phone_iwi = case_when(basset_6 == 0 ~ 0,
                               basset_6 == 1 ~ 1),
         car_iwi = case_when(basset_56 == 0 ~ 0,
                             basset_56 == 1 ~ 1),
         bike_iwi = case_when(basset_54 == 0 ~ 0,
                              basset_54 == 1 ~ 1),
         cheap_iwi = case_when(basset_1 == 0 ~ 0,
                               basset_1 == 1 ~ 1),
         expensive_iwi = case_when(basset_36 == 0 ~ 0,
                                   basset_36 == 1 ~ 1),
         watersource_iwi = case_when(s17_q01 == 11 ~ 3, # 11=Piped water:Tap in the house
                                                     s17_q01 == 12 ~ 3, # 12=Piped water:Tap in the concession
                                                     s17_q01 == 13 ~ 2, # 13=Piped water:Public tap (ONEA)
                                                     s17_q01 == 14 ~ 3, # 14=Piped water:Neighbor's tap
                                                     s17_q01 == 21 ~ 1, # 21=Open well:Unprotected well in the house
                                                     s17_q01 == 22 ~ 1, # 22=Open well:Unprotected well in concession
                                                     s17_q01 == 23 ~ 1, # 23=Open well:Unprotected public well
                                                     s17_q01 == 24 ~ 1, # 24=Open well:Neighbor's unprotected well
                                                     s17_q01 == 25 ~ 1, # 25=Unprotected well: borehole
                                                     s17_q01 == 31 ~ 2, # 31=Protected well:Covered well in house
                                                     s17_q01 == 32 ~ 2, # 32=Protected well:Covered well in concession
                                                     s17_q01 == 33 ~ 2, # 33=Protected well:Covered public well
                                                     s17_q01 == 34 ~ 2, # 34=Protected well:Neighbor's covered well
                                                     s17_q01 == 35 ~ 2, # 35=Protected well: well
                                                     s17_q01 == 41 ~ 1, # 41=Surface water:Spring
                                                     s17_q01 == 42 ~ 1, # 42=Surface water:River/stream
                                                     s17_q01 == 43 ~ 1, # 43=Surface water:Pond/Lake
                                                     s17_q01 == 51 ~ 1), # 51=Rainwater
         toiletfac_iwi = case_when(s6_q08 == 22 ~ 2, # 22=Pit latrine with slab
                                                  s6_q08 == 23 ~ 1, # 23=Pit latrine without slab/Open pit
                                                  s6_q08 == 41 ~ 1, # 41=Bucket
                                                  s6_q08 == 51 ~ 1, # 51=No facility/Bush/Field
                                                  s6_q08 == 52 ~ 1, # 52=Pit manure heap
                                                  s6_q08 == 99 ~ 1), # 99= no facility - uses neighbors' facility
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, watersource_iwi, toiletfac_iwi)
        
# Recode the 3-category variables into dummies
clean_s109_promism_csel_ses_1 <- clean_s109_promism_csel_ses_1 %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                          watersource_iwi == 2 ~ 0,
                                          watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                           watersource_iwi == 2 ~ 1,
                                           watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                           watersource_iwi == 2 ~ 0,
                                           watersource_iwi == 3 ~ 1))

clean_s109_promism_csel_ses_1 <- clean_s109_promism_csel_ses_1 %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                           toiletfac_iwi == 2 ~ 0,
                                           toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                           toiletfac_iwi == 2 ~ 1,
                                           toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                           toiletfac_iwi == 2 ~ 0,
                                           toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s109_promism_csel_ses_1 <- clean_s109_promism_csel_ses_1 %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s109_promism_csel_ses_1) # 2316


#clean_s109_promism_csel_ses_2 <- s109_promism_csel_ses_2 %>%
#  select(study_pid, basset_2, basset_6, basset_7, basset_56, basset_54,
#         basset_1, basset_16, basset_36, basset_34, 
#         s17_q01, s6_q08) %>%
#  mutate(tv_iwi_promismalicsel = ifelse(basset_2 == 0, 0,
#                         ifelse(basset_2 == 1, 1, NA)),
#         tv_iwi_promismalicsel = factor(tv_iwi_promismalicsel, levels = c(0, 1), labels = c("0", "1")),
#         phone_iwi_promismalicsel = ifelse(basset_6 == 0, 0,
#                            ifelse(basset_6 == 1, 1, NA)),
#         phone_iwi_promismalicsel = factor(phone_iwi_promismalicsel, levels = c(0, 1), labels = c("0", "1")),
#         car_iwi_promismalicsel = ifelse(basset_56 == 0, 0,
#                          ifelse(basset_56 == 1, 1, NA)),
#         car_iwi_promismalicsel = factor(car_iwi_promismalicsel, levels = c(0, 1), labels = c("0", "1")),
#         bike_iwi_promismalicsel = ifelse(basset_54 == 0, 0,
#                           ifelse(basset_54 == 1, 1, NA)),
#         bike_iwi_promismalicsel = factor(bike_iwi_promismalicsel, levels = c(0, 1), labels = c("0", "1")),
#         cheap_iwi_promismalicsel = ifelse(basset_1 == 0, 0,
#                            ifelse(basset_1 == 1, 1, NA)),
#        cheap_iwi_promismalicsel = factor(cheap_iwi_promismalicsel, levels = c(0, 1), labels = c("0", "1")),
#         expensive_iwi_promismalicsel = ifelse(basset_36 == 0, 0,
#                                ifelse(basset_36 == 1, 1, NA)),
#         expensive_iwi_promismalicsel = factor(expensive_iwi_promismalicsel, levels = c(0, 1), labels = c("0", "1")),
#         watersource_iwi_promismalicsel = ifelse(#11=Piped water:Tap in the house
                                  #12=Piped water:Tap in the concession
                                  #13=Piped water:Public tap (ONEA)
                                  #14=Piped water:Neighbor's tap
                                  #21=Open well:Unprotected well in the house
                                  #22=Open well:Unprotected well in concession
                                  #23=Open well:Unprotected public well
                                  #24=Open well:Neighbor's unprotected well
                                  #25=Unprotected well: borehole
                                  #31=Protected well:Covered well in house
                                  #32=Protected well:Covered well in concession
                                  #33=Protected well:Covered public well
                                  #34=Protected well:Neighbor's covered well
                                  #35=Protected well: well
                                  #41=Surface water:Spring
                                  #42=Surface water:River/stream
                                  #43=Surface water:Pond/Lakes
                                  #51=Rainwater         
#                                  s17_q01 == 11, "high_quality",
#                                    ifelse(s17_q01 == 12, "high_quality",
#                                          ifelse(s17_q01 == 13, "middle_quality",
#                                                ifelse(s17_q01 == 14, "high_quality",
#                                                      ifelse(s17_q01 == 21, "low_quality",
#                                                            ifelse(s17_q01 == 22, "low_quality",
#                                                                  ifelse(s17_q01 == 23, "low_quality",
#                                                                        ifelse(s17_q01 == 24, "low_quality",
#                                                                              ifelse(s17_q01 == 25, "low_quality",
#                                                                                    ifelse(s17_q01 == 31, "middle_quality",
#                                                                                          ifelse(s17_q01 == 32, "middle_quality",
#                                                                                                ifelse(s17_q01 == 33, "middle_quality",
#                                                                                                      ifelse(s17_q01 == 34, "middle_quality",
#                                                                                                            ifelse(s17_q01 == 35, "middle_quality",
#                                                                                                                  ifelse(s17_q01 == 41, "low_quality",
#                                                                                                                        ifelse(s17_q01 == 51, "low_quality", NA)))))))))))))))),
#         watersource_iwi_promismalicsel = factor(watersource_iwi_promismalicsel, levels = c("low_quality", "middle_quality", "high_quality")),
#         toiletfac_iwi_promismalicsel = ifelse(s6_q08 == 22, "middle_quality", # 22=Pit latrine with slab, 23=Pit latrine without slab/Open pit, 41=Bucket, 51=No facility/Bush/Field, 52=Pit manure heap, 99= no facility - uses neighbors' facility
#                                ifelse(s6_q08 == 23, "low_quality",
#                                       ifelse(s6_q08 == 41, "low_quality",
#                                              ifelse(s6_q08 == 51, "low_quality",
#                                                     ifelse(s6_q08 == 52, "low_quality", 
#                                                            ifelse(s6_q08 == 99, "low_quality", NA)))))),
#         toiletfac_iwi_promismalicsel = factor(toiletfac_iwi_promismalicsel, levels = c("low_quality", "middle_quality", "high_quality"))) %>%
#  select(study_pid, tv_iwi_promismalicsel, phone_iwi_promismalicsel, car_iwi_promismalicsel, bike_iwi_promismalicsel, cheap_iwi_promismalicsel,
#         expensive_iwi_promismalicsel, watersource_iwi_promismalicsel, toiletfac_iwi_promismalicsel)
#nrow(s109_promism_csel_ses_2) # 4589

# combine the two datasets
#clean_s109_promism_csel_ses <- left_join(clean_s109_promism_csel_ses_1, clean_s109_promism_csel_ses_2,
#                                   by = c("study_pid", "tv_iwi_promismalicsel", "phone_iwi_promismalicsel", "car_iwi_promismalicsel", "bike_iwi_promismalicsel", "cheap_iwi_promismalicsel",
#                                          "expensive_iwi_promismalicsel", "watersource_iwi_promismalicsel", "toiletfac_iwi_promismalicsel")) %>%
#                         select(study_pid, tv_iwi_promismalicsel, phone_iwi_promismalicsel, car_iwi_promismalicsel, bike_iwi_promismalicsel, cheap_iwi_promismalicsel,
#                                expensive_iwi_promismalicsel, watersource_iwi_promismalicsel, toiletfac_iwi_promismalicsel)
#nrow(clean_s109_promism_csel_ses) # 2316


# PROMIS-Mali
clean_s109_promism_ses <- s109_promism_ses %>%
  select(study_pid, s5_q12_2, s5_q12_6, s5_q12_7, s5_q12_56, s5_q12_54,
         s5_q12_1, s5_q12_16, s5_q12_36, s5_q12_34, s4_q06, s4_q05,
         s17_q01, s4_q01, s6_q08) %>%
  mutate(study = as.character("s109_PROMISM"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(s5_q12_2 == 0 ~ 0,
                            s5_q12_2 == 1 ~ 1),
         phone_iwi = case_when(s5_q12_6 == 0 ~ 0,
                               s5_q12_6 == 1 ~ 1),
         car_iwi = case_when(s5_q12_56 == 0 ~ 0,
                             s5_q12_56 == 1 ~ 1),
         bike_iwi = case_when(s5_q12_54 == 0 ~ 0,
                              s5_q12_54 == 1 ~ 1),
         cheap_iwi = case_when(s5_q12_1 == 0 ~ 0,
                               s5_q12_1 == 1 ~ 1),
         expensive_iwi = case_when(s5_q12_36 == 0 ~ 0,
                                   s5_q12_36 == 1 ~ 1),
         elec_iwi = case_when(s4_q06 == 1 ~ 0, # source of lighting: 1=Firewood
                                         s4_q06 == 2 ~ 0, # 2=Charcoal
                                         s4_q06 == 3 ~ 0, # 3=Petroleum
                                         s4_q06 == 5 ~ 1, # 5=Electricity
                                         s4_q06 == 6 ~ 1, # 6=Generator
                                         s4_q06 == 7 ~ 1, # 7=Solar panels
                                         s4_q06 == 8 ~ 0, # 8=Crop residuals
                                         s4_q06 == 10 ~ 1, # 10=Batteries
                                         s4_q06 == 11 ~ 0, # 11= oil lamp
                                         s4_q06 == -96 ~ 0), # -96= Other
          watersource_iwi = case_when(s17_q01 == 11 ~ 3, # 11=Piped water:Tap in the house
                                                  s17_q01 == 12 ~ 3, # 12=Piped water:Tap in the concession
                                                  s17_q01 == 13 ~ 2, # 13=Piped water:Public tap (ONEA)
                                                  s17_q01 == 14 ~ 3, # 14=Piped water:Neighbor's tap
                                                  s17_q01 == 21 ~ 1, # 21=Open well:Unprotected well in the house
                                                  s17_q01 == 22 ~ 1, # 22=Open well:Unprotected well in concession
                                                  s17_q01 == 23 ~ 1, # 23=Open well:Unprotected public well
                                                  s17_q01 == 24 ~ 1, # 24=Open well:Neighbor's unprotected well
                                                  s17_q01 == 25 ~ 1, # 25=Unprotected well: borehole
                                                  s17_q01 == 31 ~ 2, # 31=Protected well:Covered well in house
                                                  s17_q01 == 32 ~ 2, # 32=Protected well:Covered well in concession
                                                  s17_q01 == 33 ~ 2, # 33=Protected well:Covered public well
                                                  s17_q01 == 34 ~ 2, # 34=Protected well:Neighbor's covered well
                                                  s17_q01 == 35 ~ 2, # 35=Protected well: well
                                                  s17_q01 == 41 ~ 1, # 41=Surface water:Spring
                                                  s17_q01 == 42 ~ 1, # 42=Surface water:River/stream
                                                  s17_q01 == 43 ~ 1, # 43=Surface water:Pond/Lake
                                                  s17_q01 == 51 ~ 1), # 51=Rainwater
          floormat_iwi = case_when(s4_q01 == 11 ~ 1, # 11=Earth
                                              s4_q01 == 21 ~ 1, # 21=Banco
                                              s4_q01 == 31 ~ 2, # 31=Baked Bricks
                                              s4_q01 == 32 ~ 3, # 32=linoleum (plastic)
                                              s4_q01 == 33 ~ 3, # 33=Ceramic tiles, parquet
                                              s4_q01 == 34 ~ 2, # 34=Cement
                                              s4_q01 == 35 ~ 3), # 35=Carpet
          toiletfac_iwi = case_when(s6_q08 == 22 ~ 2, # 22=Pit latrine with slab
                                               s6_q08 == 23 ~ 1, # 23=Pit latrine without slab/Open pit
                                               s6_q08 == 41 ~ 1, # 41=Bucket
                                               s6_q08 == 51 ~ 1, # 51=No facility/Bush/Field
                                               s6_q08 == 52 ~ 1, # 52=Pit manure heap
                                               s6_q08 == 99 ~ 1), # 99= no facility - uses neighbors' facility
          toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi)
                                            
# Recode the 3-category variables into dummies
clean_s109_promism_ses <- clean_s109_promism_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                       watersource_iwi == 2 ~ 0,
                                       watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                       watersource_iwi == 2 ~ 1,
                                       watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                       watersource_iwi == 2 ~ 0,
                                       watersource_iwi == 3 ~ 1))

clean_s109_promism_ses <- clean_s109_promism_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                       floormat_iwi == 2 ~ 0,
                                       floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                       floormat_iwi == 2 ~ 1,
                                       floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                       floormat_iwi == 2 ~ 0,
                                       floormat_iwi == 3 ~ 1))

clean_s109_promism_ses <- clean_s109_promism_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                       toiletfac_iwi == 2 ~ 0,
                                       toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                       toiletfac_iwi == 2 ~ 1,
                                       toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                       toiletfac_iwi == 2 ~ 0,
                                       toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s109_promism_ses <- clean_s109_promism_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s109_promism_ses) # 1152


# Haiti
clean_s110_haiti_ses <- s110_haiti_ses_unique %>%
  select(study_pid, tv, frijide, potab, machin, bisiklet, radyo, delko,
         kouran, ale_dlo, ate, pyes) %>%
  mutate(study = as.character("s110_HAITI"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(tv == "do not have" ~ 0,
                                  tv == "own" ~ 1,
                                  tv == "rent" ~ 1), # rent/borrow/share
         ref_iwi = case_when(frijide == "do not have" ~ 0,
                                   frijide == "own" ~ 1,
                                   frijide == "rent" ~ 1),
         phone_iwi = case_when(potab == "do not have" ~ 0,
                                     potab == "own" ~ 1,
                                     potab == "rent" ~ 1),
         car_iwi = case_when(machin == "do not have" ~ 0,
                                   machin == "own" ~ 1,
                                   machin == "rent" ~ 1),
         bike_iwi = case_when(bisiklet == "do not have" ~ 0,
                                    bisiklet == "own" ~ 1,
                                    bisiklet == "rent" ~ 1),
         cheap_iwi = case_when(radyo == "do not have" ~ 0,
                                     radyo == "own" ~ 1,
                                     radyo == "rent" ~ 1),
         expensive_iwi = case_when(delko == "do not have" ~ 0,
                                        delko == "own" ~ 1,
                                        delko == "rent" ~ 1),
         elec_iwi = case_when(kouran == "never" ~ 0,
                                    kouran == "always" ~ 1,
                                    kouran == "sometimes" ~ 1),
         floormat_iwi = case_when(ate == 1 ~ 1, # 1=Earth/sand/rocks
                                        ate == 2 ~ 2, # 2=Concrete/masonry
                                        ate == 3 ~ 3), # 3=Ceramic/mosaic
         rooms_iwi = case_when(pyes == 1 ~ 1, # 0 or 1
                                     pyes == 2 ~ 2, # 2
                                     pyes >= 3 ~ 3)) %>% # 3 or more
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, floormat_iwi, rooms_iwi)
         
# Recode the 3-category variables into dummies 
clean_s110_haiti_ses <- clean_s110_haiti_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1))

clean_s110_haiti_ses <- clean_s110_haiti_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                rooms_iwi == 2 ~ 0,
                                rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                rooms_iwi == 2 ~ 1,
                                rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                rooms_iwi == 2 ~ 0,
                                rooms_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s110_haiti_ses <- clean_s110_haiti_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1| phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s110_haiti_ses) # 615
         


# WASHB
clean_s111_washb_ses <- s111_washb_ses_unique %>%
  select(study_pid, asset_tvbw, asset_tvcol, asset_tv, asset_refrig, asset_phone,
         asset_mobile, asset_moto, asset_bike, asset_radio, asset_table,
         asset_sewmach, elec, tubewell, storewat, floor, cement, latown, latslab, latseal) %>%
  mutate(study = as.character("s111_WASHB"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(asset_tv == 0 ~ 0,
                            asset_tv == 1 ~ 1),
         ref_iwi = case_when(asset_refrig == 0 ~ 0,
                             asset_refrig == 1 ~ 1),
         phone_iwi = case_when(asset_mobile == 0 ~ 0,
                               asset_mobile == 1 ~ 1),
         bike_iwi = case_when(asset_bike == 0 ~ 0,
                              asset_bike == 1 ~ 1),
         cheap_iwi = case_when(asset_radio == 0 ~ 0,
                               asset_radio == 1 ~ 1),
         expensive_iwi = case_when(asset_moto == 0 ~ 0,
                                   asset_moto == 1 ~ 1),
         elec_iwi = case_when(elec == 0 ~ 0,
                              elec == 1 ~ 1),
         watersource_iwi = case_when(tubewell == 0 ~ 1, # WASHB has communal water source (no private?)
                                     tubewell == 1 ~ 2),
         watersource_iwi = factor(watersource_iwi, levels = c(1, 2, 3)),
         floormat_iwi = case_when(floor == 0 ~ 1, # 0=no
                                  floor == 1 ~ 2), # 1=Improved floor (wood, concrete)
         floormat_iwi = factor(floormat_iwi, levels = c(1, 2, 3)),
         toiletfac_iwi = case_when(latslab == 1 ~ 2, # 1=Latrine has slab
                                   latslab == 0 ~ 1), # 0=Latrine has no slab
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, bike_iwi, cheap_iwi, asset_moto,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi)

### NEXT: CHECK FLOOR and TOILET FOR WASHB
all(clean_s111_washb_ses$floor == clean_s111_washb_ses$cement) # TRUE
all(clean_s111_washb_ses$latseal == clean_s111_washb_ses$latslab) 
# for WASHB, no baseline high quality water source, floor and toilet facility?

# Recode the 3-category variables into dummies
clean_s111_washb_ses <- clean_s111_washb_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 1,
                                  watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                  watersource_iwi == 2 ~ 0,
                                  watersource_iwi == 3 ~ 1),
         water3 = factor(water3, levels = c(0, 1)))

clean_s111_washb_ses <- clean_s111_washb_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1),
         floor3 = factor(floor3, levels = c(0, 1)))

clean_s111_washb_ses <- clean_s111_washb_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s111_washb_ses <- clean_s111_washb_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1| phone_iwi == 1 |
                                                                                  bike_iwi == 1 | expensive_iwi == 1 | toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & asset_moto == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s111_washb_ses) # 5551



# DOSE 
clean_s112_dose_ses <- s112_dose_ses %>%
  select(study_pid, SocNumbRefrige, SocNumbCellPho, SocNumbCar, SocNumbBicycle,
         SocNumbTable, SocNumbRadio, SocNumbAirCond, SocElectricity, SocSourceWater,
         SocSpecWater, SocSanitaryFac, SocHouseRooms) %>%
  mutate(study = as.character("s112_DOSE"),
         study_id = paste0(study, "_", study_pid),
         ref_iwi = case_when(SocNumbRefrige == 0 ~ 0,
                             SocNumbRefrige >= 1 ~ 1),
         phone_iwi = case_when(SocNumbCellPho == 0 ~ 0,
                               SocNumbCellPho >= 1 ~ 1),
         car_iwi = case_when(SocNumbCar == 0 ~ 0,
                             SocNumbCar >= 1 ~ 1),
         bike_iwi = case_when(SocNumbBicycle == 0 ~ 0,
                              SocNumbBicycle >= 1 ~ 1),
         cheap_iwi = case_when(SocNumbRadio == 0 ~ 0,
                               SocNumbRadio >= 1 ~ 1),
         expensive_iwi = case_when(SocNumbAirCond == 0 ~ 0,
                                   SocNumbAirCond >= 1 ~ 1),
         elec_iwi = case_when(SocElectricity == 0 ~ 0, # 0=None
                                   SocElectricity == 1 ~ 1, # 1=Generator
                                   SocElectricity == 2 ~ 1, # 2=Car battery
                                   SocElectricity == 3 ~ 1, # 3=Mains cable
                                   SocElectricity == 4 ~ 1, # 4=Sola
                                   SocElectricity == 66 ~ 1), # 66=Other(# intuitively, 66 is still a source of electricity), 88=probably NR/DK
         watersource_iwi = case_when(SocSourceWater == 1 ~ 3, # 1=Piped water
                                          SocSourceWater == 2 ~ 2, # 2= Borehole
                                          SocSourceWater == 3 ~ 2, # 3=Protected well
                                          SocSourceWater == 4 ~ 1, # 4=Unprotected well
                                          SocSourceWater == 5 ~ 1, # 5=Lake
                                          SocSourceWater == 6 ~ 1), # 6=River, pond; # SocSpecWater (for 66 others specify) is not used because the responses do not make sense
         toiletfac_iwi = case_when(SocSanitaryFac == 0 ~ 1, # 0=None
                                        SocSanitaryFac == 1 ~ 1, # 1=Regular pit latrine
                                        SocSanitaryFac == 2 ~ 2, # 2=Ventilated improved pit latrine
                                        SocSanitaryFac == 3 ~ 3), # 3=Water closet (WC); 88=probably NR/DK
         rooms_iwi = case_when(SocHouseRooms <= 1 ~ 1, # 0 or 1
                                    SocHouseRooms == 2 ~ 2, # 2
                                    SocHouseRooms >= 3 ~ 3)) %>% # 3 or more
  select(study, study_id, study_pid, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, toiletfac_iwi, rooms_iwi)

# Recode the 3-category variables into dummies
clean_s112_dose_ses <- clean_s112_dose_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 1,
                                 watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 1))

clean_s112_dose_ses <- clean_s112_dose_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1))

clean_s112_dose_ses <- clean_s112_dose_ses %>%
  mutate(sleepr1 = case_when(rooms_iwi == 1 ~ 1,
                                  rooms_iwi == 2 ~ 0,
                                  rooms_iwi == 3 ~ 0),
         sleepr2 = case_when(rooms_iwi == 1 ~ 0,
                                  rooms_iwi == 2 ~ 1,
                                  rooms_iwi == 3 ~ 0),
         sleepr3 = case_when(rooms_iwi == 1 ~ 0,
                                  rooms_iwi == 2 ~ 0,
                                  rooms_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s112_dose_ses <- clean_s112_dose_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (ref_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | 
                                                                                  expensive_iwi == 1 | toiletfac_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s112_dose_ses) # 5244



# WASH-K
clean_s113_washk_ses <- s113_washk_ses %>%
  select(study_pid, tv, mobilephone, car, bicycle, radio, motorcycle, elec, 
         prim_drink_ws_bl, floor, imp_lat_bl) %>%
  mutate(study = as.character("s113_WASHK"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(tv == 0 ~ 0,
                            tv == 1 ~ 1), # 9=probably NR/DK
         phone_iwi = case_when(mobilephone == 0 ~ 0,
                               mobilephone == 1 ~ 1), # 9=probably NR/DK
         car_iwi = case_when(car == 0 ~ 0,
                             car == 1 ~ 1), # 9=probably NR/DK
         bike_iwi = case_when(bicycle == 0 ~ 0,
                              bicycle == 1 ~ 1), # 9=probably NR/DK
         cheap_iwi = case_when(radio == 0 ~ 0,
                               radio == 1 ~ 1), # 9=probably NR/DK
         expensive_iwi = case_when(motorcycle == 0 ~ 0,
                                   motorcycle == 1 ~ 1), # 9=probably NR/DK
         elec_iwi = case_when(elec == 0 ~ 0,
                              elec == 1 ~ 1), # 9=probably NR/DK
         # prim_drink_ws_bl: 1=Improved water source, 0=Unimproved water source: not used because it's not possible to distinguish
         # high, middle and low quality water source
         floormat_iwi = case_when(floor == 0 ~ 1, # 0=Earth/dung (https://osf.io/2mht9:)
                                  floor == 1 ~ 2), # 1=Concrete; 9=Missing/DK
         floormat_iwi = factor(floormat_iwi, levels = c(1, 2, 3)),
         # based on the questionnaire, toilet and latrine are used interchangeably, so we will assume that the variable toilet_men/toilet_women refers to 
         # access to a toilet/ latrine, so did not use this variable
         toiletfac_iwi = case_when(imp_lat_bl == 0 ~ 1, # 0=No access to improved latrine, baseline
                                   imp_lat_bl == 1 ~ 2), # 1=Household has access to improved latrine, baseline
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi, expensive_iwi,
         elec_iwi, floormat_iwi, toiletfac_iwi)

# Recode the 3-category variables into dummies
clean_s113_washk_ses <- clean_s113_washk_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1),
         floor3 = factor(floor3, levels = c(0, 1)))

clean_s113_washk_ses <- clean_s113_washk_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s113_washk_ses <- clean_s113_washk_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | expensive_iwi == 1 |
                                                                                  toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s113_washk_ses) # 17858



# MAHAY
clean_s116_mahay_ses <- s116_mahay_ses %>%
  select(study_pid, ppyn_television, ppyn_refrigerator, ppyn_home_mobile_phone,
         ppyn_car, ppyn_bicycle, ppyn_tables, ppyn_mopeds, helectr,
         hwater_drinking, safewater, hfloor, htoilette) %>%
  mutate(study = as.character("s116_MAHAY"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(ppyn_television == 0 ~ 0,
                            ppyn_television == 1 ~ 1),
         ref_iwi = case_when(ppyn_refrigerator == 0 ~ 0,
                             ppyn_refrigerator == 1 ~ 1),
         phone_iwi = case_when(ppyn_home_mobile_phone == 0 ~ 0,
                               ppyn_home_mobile_phone == 1 ~ 1),
         car_iwi = case_when(ppyn_car == 0 ~ 0,
                             ppyn_car == 1 ~ 1),
         bike_iwi = case_when(ppyn_bicycle == 0 ~ 0,
                              ppyn_bicycle == 1 ~ 1),
         cheap_iwi = case_when(ppyn_tables == 0 ~ 0,
                               ppyn_tables == 1 ~ 1),
         expensive_iwi = case_when(ppyn_mopeds == 0 ~ 0,
                                   ppyn_mopeds == 1 ~ 1),
         elec_iwi = case_when(helectr == 1 ~ 1, # 1=Electricity
                                    helectr == 2 ~ 0, # 2=Candles
                                    helectr == 3 ~ 0, # 3=Oil
                                    helectr == 4 ~ 0, # 4=Tallow (Jabora)
                                    helectr == 5 ~ 0, # 5=Grouup
                                    helectr == 6 ~ 1, # 6=Solar panel
                                    helectr == 7 ~ 0), #  7=Battery operated lamp; 8=Other
         watersource_iwi = case_when(hwater_drinking == 1 ~ 3, # 1=Piped into dwelling
                                           hwater_drinking == 2 ~ 2, # 2=Public standpipe
                                           hwater_drinking == 3 ~ 2, # 3=Protected well
                                           hwater_drinking == 4 ~ 1, # 4=Unprotected well
                                           hwater_drinking == 5 ~ 2, # 5=Public well
                                           hwater_drinking == 6 ~ 1, # 6=Stream
                                           hwater_drinking == 7 ~ 1, # 7=River
                                           hwater_drinking == 8 ~ 1, # 8=Pond/lake
                                           hwater_drinking == 9 ~ 1, # 9=Rainwater
                                           hwater_drinking == 10 ~ 2, # 10=Tanker truck
                                           hwater_drinking == 11 ~ 2, # 11=Irrigation cannal
                                           hwater_drinking == 12 ~ 2), # 12=Impluvium
         floormat_iwi = case_when(hfloor == 1 ~ 1, # 1=Nautral floor
                                     hfloor == 2 ~ 1, # 2=Weave
                                     hfloor == 3 ~ 2, # 3=Wood/plank/bamboo
                                     hfloor == 4 ~ 3, # 4=Parquet/polished wood
                                     hfloor == 5 ~ 3), # 5=Ceramic tiles/cement; 8=Other
         floormat_iwi = factor(floormat_iwi, levels = c(1, 2, 3)),
         toiletfac_iwi = case_when(htoilette == 1 ~ 3, # 1=Flush toilet
                                         htoilette == 2 ~ 1, # 2=Open pit
                                         htoilette == 3 ~ 1, # 3=Traditional pit latrine
                                         htoilette == 4 ~ 1, # 4=Bucket toilet
                                         htoilette == 5 ~ 1), # 5=No facility; 8=Other
         toiletfac_iwi = factor(toiletfac_iwi, levels = c(1, 2, 3))) %>%
  select(study, study_id, study_pid, tv_iwi, ref_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi)

# Recode the 3-category variables into dummies
clean_s116_mahay_ses <- clean_s116_mahay_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 1,
                                 watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 1))

clean_s116_mahay_ses <- clean_s116_mahay_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1),
         floor3 = factor(floor3, levels = c(0, 1)))

clean_s116_mahay_ses <- clean_s116_mahay_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet1 = factor(toilet1, levels = c(0, 1)),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = factor(toilet2, levels = c(0, 1)),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1),
         toilet3 = factor(toilet3, levels = c(0, 1)))
         
# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s116_mahay_ses <- clean_s116_mahay_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | ref_iwi == 1 |
                                                                                  phone_iwi == 1 | bike_iwi == 1 | car_iwi == 1 |
                                                                                  expensive_iwi == 1 |
                                                                                  toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s116_mahay_ses) # 4676



# JiVitA-4
clean_s105_jivita_ses <- s105_jivita_ses %>%
  select(study_pid, sstv, ssmobile, ssmotorc, sscycle, ssradios, ssdtable,
         ssmotorc, ssirrpump, sselectric, sstubewell, ssgfloor, sstoilet) %>%
  mutate(study = as.character("s105_JiVitA"),
         study_id = paste0(study, "_", study_pid),
         tv_iwi = case_when(sstv == 0 ~ 0, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                            sstv >= 1 & sstv <= 8 ~ 1),
         phone_iwi = case_when(ssmobile == 0 ~ 0, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                               ssmobile >= 1 & ssmobile <= 8 ~ 1),
         car_iwi = case_when(ssmotorc == 0 ~ 0, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                             ssmotorc >= 1 & ssmotorc <= 8 ~ 1),
         bike_iwi = case_when(sscycle == 0 ~ 0, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                              sscycle >= 1 & sscycle <= 8 ~ 1),
         cheap_iwi = case_when(ssradios == 0 ~ 0, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                               ssradios >= 1 & ssradios <= 8 ~ 1),
         expensive_iwi = case_when(ssmotorc == 0 ~ 0, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                                   ssmotorc >= 1 & ssmotorc <= 8 ~ 1),
         elec_iwi = case_when(sselectric == 0 ~ 0, # 0=No
                              sselectric == 1 ~ 1), # 1=Yes, 9=Don’t know
         watersource_iwi = case_when(sstubewell == 0 ~ 1, # 0=None, 1-7=Number, 8=8 or more, 9=Don’t know
                                     sstubewell >= 1 & sstubewell <= 8 ~ 2),
         watersource_iwi = factor(watersource_iwi, levels = c(1, 2, 3)),
         floormat_iwi = case_when(ssgfloor == 0 ~ 1, # 0=No walls/ Fence
                                         ssgfloor == 1 ~ 1, # 1=Thatch, grass, sticks, branches
                                         ssgfloor == 2 ~ 1, # 2=Katcha
                                         ssgfloor == 3 ~ 2, # 3=Tin/wood plank
                                         ssgfloor == 4 ~ 2), # 4=Pakka; 9=Don’t know
         floormat_iwi = factor(floormat_iwi, levels = c(1, 2, 3)),
         toiletfac_iwi = case_when(sstoilet == 0 ~ 1, # Assumed that 0 means no toilet
                                          sstoilet == 1 ~ 1, # 1=Open latrine
                                          sstoilet == 2 ~ 1, # 2=Pit latrine
                                          sstoilet == 3 ~ 2, # 3=Water sealed/slab
                                          sstoilet == 4 ~ 3)) %>% # 4=Flush toilet; 9=Don’t know
  select(study, study_id, study_pid, tv_iwi, phone_iwi, car_iwi, bike_iwi, cheap_iwi,
         expensive_iwi, elec_iwi, watersource_iwi, floormat_iwi, toiletfac_iwi)

# Recode the 3-category variables into dummies
clean_s105_jivita_ses <- clean_s105_jivita_ses %>%
  mutate(water1 = case_when(watersource_iwi == 1 ~ 1,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 0),
         water2 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 1,
                                 watersource_iwi == 3 ~ 0),
         water3 = case_when(watersource_iwi == 1 ~ 0,
                                 watersource_iwi == 2 ~ 0,
                                 watersource_iwi == 3 ~ 1),
         water3 = factor(water3, levels = c(0, 1)))
  
clean_s105_jivita_ses <- clean_s105_jivita_ses %>%
  mutate(floor1 = case_when(floormat_iwi == 1 ~ 1,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 0),
         floor2 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 1,
                                 floormat_iwi == 3 ~ 0),
         floor3 = case_when(floormat_iwi == 1 ~ 0,
                                 floormat_iwi == 2 ~ 0,
                                 floormat_iwi == 3 ~ 1),
         floor3 = factor(floor3, levels = c(0, 1)))

clean_s105_jivita_ses <- clean_s105_jivita_ses %>%
  mutate(toilet1 = case_when(toiletfac_iwi == 1 ~ 1,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 0),
         toilet2 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 1,
                                  toiletfac_iwi == 3 ~ 0),
         toilet3 = case_when(toiletfac_iwi == 1 ~ 0,
                                  toiletfac_iwi == 2 ~ 0,
                                  toiletfac_iwi == 3 ~ 1))

# If household has a more expensive utensil, it is assumed to have also a cheaper utensil
clean_s105_jivita_ses <- clean_s105_jivita_ses %>%
                        mutate(cheap_iwi = case_when(cheap_iwi == 0 & (tv_iwi == 1 | phone_iwi == 1 |
                                                                                  bike_iwi == 1 | car_iwi == 1 | expensive_iwi == 1 |
                                                                                  toiletfac_iwi == 3 | floormat_iwi == 3) ~ 1,
                                                                                  TRUE ~ cheap_iwi),
                               expensive_iwi = case_when(expensive_iwi == 0 & car_iwi == 1 ~ 1, TRUE ~ expensive_iwi)) %>%
                        mutate_all(as.factor)

nrow(clean_s105_jivita_ses) # 5449


#### create variable for water source with 3 levels (high, moderate and low quality)
### https://globaldatalab.org/iwi/computing/
# water supply      
# high quality is private piped water or bottled water;
# middle quality is public tap or standpipe, tubewell or borehole, protected well or spring, or tanker truck;
# low quality is unprotected well or spring, cart with tank/drum, or surface water.

# floor facility
# high quality is finished floor with parquet, carpet, tiles, linoleum, ceramic etc.;
# middle quality is cement, concrete, wood, bamboo etc.;
# low quality is none, earth, dung etc.

# toilet facility
# high quality is private flush toilet (or flush toilet if private unknown);
# middle quality is public toilet, ventilated/improved pit latrine, pit latrine with slab, or composting toilet;
# low quality is pit latrine without slab, open pit, bucket, hanging toilet or no toilet.
    

#### NEXT STEP: join asset data of each study with df_analysis (previous IPD dataset) and save the dataset


### Join the asset data with the IPD dataset

ghana_assets_joined <- df_analysis %>%
  filter(study == "s101_GHANA") %>%
  left_join(clean_s101_ghana_ses, by = "study_id")

dyadg_assets_joined <- df_analysis %>%
  filter(study == "s102_DYADG") %>%
  left_join(clean_s102_dyadg_ses, by = "study_id")
  
dyadm_assets_joined <- df_analysis %>%
  filter(study == "s103_DYADM") %>%
  left_join(clean_s103_dyadm_ses, by = "study_id")  
  
promisbf_csel_assets_joined <- df_analysis %>%
  filter(study == "s104_PROMISBF_CS") %>%
  left_join(clean_s104_promisbf_csel_ses, by = "study_id")  

promisbf_assets_joined <- df_analysis %>%
  filter(study == "s104_PROMISBF") %>%
  left_join(clean_s104_promisbf_ses, by = "study_id")  

jivita_assets_joined <- df_analysis %>%
  filter(study == "s105_JiVitA") %>%
  left_join(clean_s105_jivita_ses, by = "study_id")  

rdns_assets_joined <- df_analysis %>%
  filter(study == "s106_RDNS") %>%
  left_join(clean_s106_rdns_ses, by = "study_id")  

zinc_assets_joined <- df_analysis %>%
  filter(study == "s107_ZINC") %>%
  left_join(clean_s107_zinc_ses, by = "study_id")

shine_assets_joined <- df_analysis %>%
  filter(study == "s108_SHINE_HIV-") %>%
  left_join(clean_s108_shine_ses, by = "study_id")

promism_csel_assets_joined <- df_analysis %>%
  filter(study == "s109_PROMISM_CS") %>%
  left_join(clean_s109_promism_csel_ses_1, by = "study_id")

promism_assets_joined <- df_analysis %>%
  filter(study == "s109_PROMISM") %>%
  left_join(clean_s109_promism_ses, by = "study_id")

haiti_assets_joined <- df_analysis %>%
  filter(study == "s110_HAITI") %>%
  left_join(clean_s110_haiti_ses, by = "study_id")

washb_assets_joined <- df_analysis %>%
  filter(study == "s111_WASHB") %>%
  left_join(clean_s111_washb_ses, by = "study_id")

dose_assets_joined <- df_analysis %>%
  filter(study == "s112_DOSE") %>%
  left_join(clean_s112_dose_ses, by = "study_id")

washk_assets_joined <- df_analysis %>%
  filter(study == "s113_WASHK") %>%
  left_join(clean_s113_washk_ses, by = "study_id")

mahay_assets_joined <- df_analysis %>%
  filter(study == "s116_MAHAY") %>%
  left_join(clean_s116_mahay_ses, by = "study_id")

mahay_assets_joined <- df_analysis %>%
  filter(study == "s116_MAHAY") %>%
  left_join(clean_s116_mahay_ses, by = "study_id")


# Join all datasets

df_assets_joined <- bind_rows(ghana_assets_joined, dyadg_assets_joined, dyadm_assets_joined,
                              promisbf_csel_assets_joined, promisbf_assets_joined, jivita_assets_joined,
                              rdns_assets_joined, zinc_assets_joined, shine_assets_joined, promism_csel_assets_joined,
                              promism_assets_joined, haiti_assets_joined, washb_assets_joined,
                              dose_assets_joined, washk_assets_joined, mahay_assets_joined)



                            
nrow(df_assets_joined) # 39812 (no PROMISBF_CS and PROMISM_CS baselines)
nrow(df_analysis) # 38407
table(df_assets_joined$study.x) # same with study.y
#s101_GHANA       s102_DYADG       s103_DYADM    s104_PROMISBF s104_PROMISBF_CS      s105_JiVitA        s106_RDNS        s107_ZINC 
#194             1113              675             1782             1157             4568             2567             2647 
#s108_SHINE_HIV-     s109_PROMISM  s109_PROMISM_CS       s110_HAITI       s111_WASHB        s112_DOSE       s113_WASHK       s116_MAHAY 
#3679             1013             1927              322             4824             3091             6815             3438 
table(df_assets_joined$study.y)

######################################
######################################

### Save the dataset
write.csv2(df_assets_joined, file = here::here("data", "1-final",
                           "df_analysis_assets_joined.csv"))
saveRDS(df_assets_joined, file = here::here("data", "1-final",
                           "df_analysis_assets_joined.rds"))

