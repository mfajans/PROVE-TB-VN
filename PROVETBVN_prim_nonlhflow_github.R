################################################
#PROVE-TB VN- Primary Analysis
#Date: March 13th 2023
#REDCap Dataset version: (October 4th 2022) Pull
#Coder: Mark Fajans


#Datasets:
#Overall dataset: REDCap Dataset (October 4th 2022)
#Key Variable Dataset (Created by Lara, updated October 4th )
#NLH FLOW TB Excel file **** ONLY TO BE USED DURING PRELIMINARY ANALYSIS
#MSD Data - Updated version Received 10/3/22 from PATH

################################################



#####################################
#Setup
#####################################
rm(list=ls())

#Loading packages
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(table1)
library(reshape2)
library(lattice)
library(epiR)
library(foreign)
library(skimr)
library(arsenal)
library(gtsummary)
library(epiR)
library(pROC)
library(gridExtra)

#Setting working directory 
setwd("~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/WD")


#############################
#Pulling in Data
#############################

###REDCap Data

#REDCap data housed in individual folder corresponding to date pulled, need to update path file if updating data

#Using August 18th 2022 REDCap Pull, sourcing REDCap generated R script to pull in dataset & labels
#rc<-source("~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/Data/August 3 2022 Pull/PROVETBVietNam_R_2022-08-29_1147.R") 

#Using August 9th 2022 REDCap Pull, sourcing REDCap generated R script to pull in dataset & labels. ~600 FLOW TB result entered
#rc<-source("~Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/Data/REDCap/August 3 2022 Pull/PROVETBVietNam_R_2022-10-04_0945.R") 

#Using October 4th Dataset
rc<-source("C:/Users/fajan/OneDrive/Documents/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/Data/REDCap/October 4 2022 Pull/PROVETBVietNam_R_2022-10-04_0945.r") 

### Lara's Key Variable Dataset (October 4th update)

kv<-read.csv("~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/Data/Keyvars/keyVNa_enroll_221004.csv")



### NLH FLOW TB Results
# Excel document provided by NLH, only to be used for GF presentation in October since data has not been QCd, used in interest
# of short time

nlhraw<-read.csv("~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/Data/NLH/Flowraw.csv")





#### MSD DATA - FA, OA &  Pair
# Received from PATH 10.4.22, contains MSD results from urine samples processed at PATH biorepository lab
# Results for both FIND28/A-194 (FA), S420/A-194 (OA pair) & 5e3/A-194 (n=80 subset)

#Note - issue with formatting with PTIDs, fixed in csv prior to import 

msdraw<-read.csv("~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/Data/MSD/MSD results with Vietnam samples 10-04-22.csv")



############################
#Data Cleaning & Management
############################



#################
###REDCap Dataset
#####
summary(data)
glimpse(data)


#Keeping variables of interest

rc<-data %>% dplyr::select(c(
        
        #Eligibility CRF
        ptid,
        elig_reviewdate,
        elig_loc.factor,
        elig_locoth,
        elig_locotheng,
        elig_hivteststatus.factor,
        elig_hivresult.factor,
        elig_age,
        elig_newpatient.factor,
        elig_tpt.factor,
        elig_tbtx.factor,
        elig_suspecttb.factor,
        elig_consent.factor,
        elig_consentdate,
        elig_consentlang,
        elig_consentversvn,
        elig_consentltstorage,
        
        #Baseline characteristics
        char_dob,
        char_age,
        char_gender.factor,
        char_genderoth,
        char_ethnic.factor,
        char_employ.factor,
        clin_admitdate,
        clin_admitdxeng,
        clin_chcough.factor,
        clin_chfeverchill.factor,
        clin_chnitesweat.factor,
        clin_chwtloss.factor,
        clin_chlossapp.factor,
        clin_chfatigue.factor,
        clin_stcough.factor,
        clin_stfeverchill.factor,
        clin_stnitesweat.factor,
        clin_stwtloss.factor,
        clin_stlossapp.factor,
        clin_stfatigue.factor,
        clin_heightcm,
        clin_weightkg,
        
        #Baseline interview
        
        int_evtbtx.factor,
        int_lvnowtb.factor,
        int_lvnowtboth,
        int_lvnowcough.factor,
        int_lvnowcoughoth,
        int_lv3motb.factor,
        int_lv3motboth,
        int_lv3mocough.factor,
        int_lv3mocoughoth,
        int_evart.factor,
        
        
        #Lab results - NLH
        labcc_crstatus.factor,
        labcc_crresult,
        
        labcc_a1cstatus.factor,
        labcc_a1cresult,
        labcc_cd4status.factor,
        labcc_cd4date,
        labcc_cd4result,
        labcc_afbstatus.factor,
        labcc_afbresult.factor,
        labcc_gxpstatus.factor,
        labcc_gxpdate,
        labcc_gxpspec.factor,
        labcc_gxpspecotheng,
        labcc_gxpresult.factor,
        labcc_gxpresultrif.factor,
        labcc_cultstatus.factor,
        labcc_cultdate,
        labcc_cultspec.factor,
        labcc_cultspecotheng,
        labcc_cultresultdate,
        labcc_cultresult.factor,
        labcc_cultresultpos.factor,
        labcc_cultresultposdays,
        
        #Study specific
        labsto_gxpcoll.factor,
        labsto_gxpspec.factor,
        labsto_gxpspecotheng,
        labsto_cultcoll.factor,
        labsto_cultspecotheng,
        
        
        #Patient chart
        chab_xraystatus.factor,
        chab_xraydate,
        chab_xrayresultstatus.factor,
        chab_xrayresult___1.factor,
        chab_xrayresult___2.factor,
        chab_xrayresult___3.factor,
        chab_xrayresult___4.factor,
        chab_xrayresult___5.factor,
        chab_xrayresult___9.factor,
        chab_xrayresultotheng,
        chab_tbtx.factor,
        chab_tbtxtype.factor,
        chab_tbtxtypeoth,
        chab_evtblad.factor,
        chab_evtbhisto.factor,
        chab_evtbimmuno.factor,
        chab_art.factor,
        pres_tb.factor,
        
        
        #Study results
        
        labstr_rbgresult,
        labstr_a1cresult,
        labstr_cd4result,
        
        labstr_gxpresultdate,
        labstr_gxpresult.factor,
        labstr_gxpresultrif.factor,
        
        labstr_cultresultdate,
        labstr_cultresult.factor,
        labstr_cultresultpos.factor,
        labstr_cultresultposdays,
        
        #POC testing frozen CRF31
        pocfrz_flowstatus.factor,
        pocfrz_flowndrsn.factor,
        pocfrz_flowndrsnotheng,
        pocfrz_flowurnthawdate,
        pocfrz_flowurnthawtime,
        pocfrz_flowtestdate,
        pocfrz_flowtesttime,
        pocfrz_flowresult.factor,
        pocfrz_flowresultpos.factor,
        pocfrz_flowresultrepeat___1.factor,
        pocfrz_flowresultinvrsn.factor,
        pocfrz_flowresultinvrsnotheng,
        pocfrz_flowcolorchng.factor,
        pocfrz_flowcubetest,
        pocfrz_flowcubecontrol,
        pocfrz_flowvolremain,
        pocfrz_commentseng,
        
        #Exit
        
        exit_exitdate,
        exit_type.factor,
        exit_earlyrsn.factor
        
))


summary(rc)
glimpse(rc)
skimr::skim((rc))



##Checking completeness of RC FLOW Results (CRFB031)
rcflo<-rc %>% dplyr::select(c(

pocfrz_flowstatus.factor,
pocfrz_flowndrsn.factor,
pocfrz_flowndrsnotheng,
pocfrz_flowurnthawdate,
pocfrz_flowurnthawtime,
pocfrz_flowtestdate,
pocfrz_flowtesttime,
pocfrz_flowresult.factor,
pocfrz_flowresultpos.factor,
pocfrz_flowresultrepeat___1.factor,
pocfrz_flowresultinvrsn.factor,
pocfrz_flowresultinvrsnotheng,
pocfrz_flowcolorchng.factor,
pocfrz_flowcubetest,
pocfrz_flowcubecontrol,
pocfrz_flowvolremain,
pocfrz_commentseng
))

skim(rcflo)


#Deleting IDs using fresh urine testing (n=72)

rc<-rc[!rc$ptid %in% c(
        'NLH-0111-UR',
        'NLH-0112-UR',
        'NLH-0113-UR',
        'NLH-0114-UR',
        'NLH-0115-UR',
        'NLH-0116-UR',
        'NLH-0117-UR',
        'NLH-0118-UR',
        'NLH-0119-UR',
        'NLH-0120-UR',
        'NLH-0121-UR',
        'NLH-0122-UR',
        'NLH-0499-UR',
        'NLH-0500-UR',
        'NLH-0501-UR',
        'NLH-0502-UR',
        'NLH-0503-UR',
        'NLH-0504-UR',
        'NLH-0505-UR',
        'NLH-0506-UR',
        'NLH-0507-UR',
        'NLH-0508-UR',
        'NLH-0509-UR',
        'NLH-0510-UR',
        'NLH-0511-UR',
        'NLH-0512-UR',
        'NLH-0513-UR',
        'NLH-0514-UR',
        'NLH-0515-UR',
        'NLH-0516-UR',
        'NLH-0517-UR',
        'NLH-0518-UR',
        'NLH-0519-UR',
        'NLH-0520-UR',
        'NLH-0521-UR',
        'NLH-0522-UR',
        'NLH-0523-UR',
        'NLH-0524-UR',
        'NLH-0525-UR',
        'NLH-0526-UR',
        'NLH-0527-UR',
        'NLH-0528-UR',
        'NLH-0529-UR',
        'NLH-0530-UR',
        'NLH-0531-UR',
        'NLH-0532-UR',
        'NLH-0533-UR',
        'NLH-0534-UR',
        'NLH-0535-UR',
        'NLH-0536-UR',
        'NLH-0537-UR',
        'NLH-0538-UR',
        'NLH-0539-UR',
        'NLH-0540-UR',
        'NLH-0541-UR',
        'NLH-0542-UR',
        'NLH-0543-UR',
        'NLH-0544-UR',
        'NLH-0545-UR',
        'NLH-0546-UR',
        'NLH-0547-UR',
        'NLH-0548-UR',
        'NLH-0549-UR',
        'NLH-0550-UR',
        'NLH-0551-UR',
        'NLH-0651-UR',
        'NLH-0745-UR',
        'NLH-0746-UR',
        'NLH-0747-UR',
        'NLH-0748-UR',
        'NLH-0749-UR',
        'NLH-0750-UR'
        
), ]


#Deleting participants with urine collected beyond 48 hour window (n=11)

rc<-rc[!rc$ptid %in% c(
        'NLH-0210-UR',
        'NLH-0230-UR',
        'NLH-0261-UR',
        'NLH-0298-UR',
        'NLH-0369-UR',
        'NLH-0726-UR',
        'NLH-0728-UR',
        'NLH-0733-UR',
        'NLH-0734-UR',
        'NLH-0735-UR',
        'NLH-0741-UR'
),]



##Constructed variables

#Clinical Symptoms - Combination if either study procedure or chart review indicates presence of symptoms

rc<- rc %>% mutate(
        
#Cough
        cough_c = case_when( (clin_chcough.factor=="Yes")|(clin_stcough.factor=="Yes") ~ 1,
                             (clin_chcough.factor=="No") | (clin_stcough.factor=="No") ~ 0,
                             
                             TRUE ~ NaN),
        
#Fever
        feverchill_c = case_when( (clin_chfeverchill.factor=="Yes")|(clin_stfeverchill.factor=="Yes") ~ 1,
                                  (clin_chfeverchill.factor=="No") | (clin_stfeverchill.factor=="No") ~ 0,
                                  
                                  TRUE ~ NaN),

#Nightsweats

        nitesweat_c = case_when( (clin_chnitesweat.factor=="Yes")|(clin_stnitesweat.factor=="Yes") ~ 1,
                                 (clin_chnitesweat.factor=="No") | (clin_stnitesweat.factor=="No") ~ 0,
                                 
                                 TRUE ~ NaN),


#Weightloss
        
        wtloss_c = case_when( (clin_chwtloss.factor=="Yes")|(clin_stwtloss.factor=="Yes") ~ 1,
                              (clin_chwtloss.factor=="No") | (clin_stwtloss.factor=="No") ~ 0,
                              
                              TRUE ~ NaN),
 

#Loss of appetite
        
        lossapp_c = case_when( (clin_chlossapp.factor=="Yes")|(clin_stlossapp.factor=="Yes") ~ 1,
                              (clin_chlossapp.factor=="No") | (clin_stlossapp.factor=="No") ~ 0,
                              
                              TRUE ~ NaN),

#Fatigue

        fatigue_c = case_when( (clin_chfatigue.factor=="Yes")|(clin_stfatigue.factor=="Yes") ~ 1,
                                 (clin_chfatigue.factor=="No") | (clin_stfatigue.factor=="No") ~ 0,
                                 
                                 TRUE ~ NaN),
       


#Rif resistance
        
        rifdet_c = case_when((labcc_gxpresultrif.factor=="RIF resistant") | 
                           (labstr_gxpresultrif.factor=="RIF resistant") ~ 1,
                        
                                TRUE ~ 0),

#A1c value 
                
        
        a1ccomb_c = case_when(!is.na(labstr_a1cresult) ~ labstr_a1cresult,
                               is.na(labstr_a1cresult) ~ labcc_a1cresult),

#A1c Category
        
        a1ccat_c = case_when(a1ccomb_c < 6.5 ~ 1,
                             a1ccomb_c>=6.5 & a1ccomb_c<=7.5 ~2,
                             a1ccomb_c>7.5 ~3)

)


 
#Checking variable creation
table(rc$clin_chcough.factor, rc$clin_stcough.factor , rc$cough_c, useNA="ifany")
table(rc$clin_chfeverchill.factor, rc$clin_stfeverchill.factor , rc$feverchill_c, useNA="ifany")        
table(rc$clin_chnitesweat.factor, rc$clin_stnitesweat.factor , rc$nitesweat_c, useNA="ifany")          
table(rc$clin_chwtloss.factor, rc$clin_stwtloss.factor , rc$wtloss_c, useNA="ifany")         
table(rc$clin_chlossapp.factor, rc$clin_stlossapp.factor , rc$lossapp_c, useNA="ifany")        
table(rc$clin_chfatigue.factor, rc$clin_stfatigue.factor , rc$fatigue_c, useNA="ifany")        
        
table(rc$labcc_gxpresultrif.factor, rc$labstr_gxpresultrif.factor, rc$rifdet_c, useNA = "ifany")


#Creating factor version of each symptom variable
noyes.f<-c("No", "Yes")        

#Cough
rc$cough_c.f<-factor(rc$cough_c, levels = c(0,1),
                                 labels = noyes.f)

#Fever
rc$feverchill_c.f<-factor(rc$feverchill_c, levels = c(0,1),
                     labels = noyes.f)

#Nitesweat
rc$nitesweat_c.f<-factor(rc$nitesweat_c, levels = c(0,1),
                     labels = noyes.f)

#Weight Loss
rc$wtloss_c.f<-factor(rc$wtloss_c, levels = c(0,1),
                     labels = noyes.f)

#Loss of appetite
rc$lossapp_c.f<-factor(rc$lossapp_c, levels = c(0,1),
                     labels = noyes.f)

#Fatigue
rc$fatigue_c.f<-factor(rc$fatigue_c, levels = c(0,1),
                     labels = noyes.f)


#Reformatting Rif detected

rc$rifdet_c.f<-factor(rc$rifdet_c, levels = c(0,1),
                       labels = noyes.f)


#Reformatting AFB status
rc$afbval_c[rc$labcc_afbresult.factor=="Negative"]<-0
        rc$afbval_c[rc$labcc_afbresult.factor=="Scanty   {labcc_afbscantycount}"]<-1
        rc$afbval_c[rc$labcc_afbresult.factor=="1+"]<-2
        rc$afbval_c[rc$labcc_afbresult.factor=="2+"]<-3
        rc$afbval_c[rc$labcc_afbresult.factor=="3+"]<-4
        rc$afbval_c[rc$labcc_afbresult.factor=="Ordered but not tested/no result"]<-5
        rc$afbval_c[rc$labcc_afbresult.factor=="Pending"]<-6
        rc$afbval_c[is.na(rc$labcc_afbresult.factor)]<-6
        
rc$afbval_c.f<-factor(rc$afbval_c, levels=c(0:6),
                      labels=c("Negative",
                               "Scanty",
                               "1+",
                               "2+",
                               "3+",
                               "Ordered but not tested/no result",
                               "Pending/NA"
                              
                      ))     


#Reformatting A1c category

rc$a1ccat_c.f<-factor(rc$a1ccat_c, levels=c(1:3),
                      labels=c("<6.5",
                               "6.5-7.5",
                               ">7.5"))
        
#################
###Keyvar dataset
#####
summary(kv)
head(kv)



#Dropping unnecessary variables
kv<-dplyr::select(kv, -c(1:3))
kv<-dplyr::select(kv, -c(#PTID_num, 
                         keybl_age,
                         keybl_gender,
                         keybl_locoth))
                         
                        

#Creating formatted versions of key variables used 

#Location
table(kv$keybl_loc, useNA = "always")

kv$keybl_loc.f[kv$keybl_loc=="1 = General Internal Medicine"]<-1
kv$keybl_loc.f[kv$keybl_loc=="2 = Clinic & Outpatient"]<-2
kv$keybl_loc.f[kv$keybl_loc=="3 = Respiratory TB"]<-3

kv$keybl_loc.f<-factor(kv$keybl_loc.f, levels=c(1,2,3),
                       labels=c("General Internal Medicine",
                                "Clinic & Outpatient",
                                "Respiratory TB")
                       )

table(kv$keybl_loc, kv$keybl_loc.f)



#Inpatient
table(kv$keybl_inpatient, useNA = "always")

kv$keybl_inpatient.f[kv$keybl_inpatient=="1 = Yes"]<-1
kv$keybl_inpatient.f[kv$keybl_inpatient=="2 = No"]<-2

kv$keybl_inpatient.f<-factor(kv$keybl_inpatient.f, levels = c(1,2),
                             labels=c("Yes", "No"))

table(kv$keybl_inpatient, kv$keybl_inpatient.f)

#Recruitment type

table(kv$keybl_rectype, useNA = "always")

kv$keybl_rectype.f[kv$keybl_rectype=="1 = Extrapulmonary TB"]<-2
kv$keybl_rectype.f[kv$keybl_rectype=="2 = Pulmonary TB"]<-1


kv$keybl_rectype.f<-factor(kv$keybl_rectype.f, levels = c(1,2),
                             labels=c("PTB", "EPTB"))


table(kv$keybl_rectype.f, kv$keybl_rectype)


#MTB Cat
table(kv$keybl_MTBcat, useNA="always")

kv$keybl_MTBcat.f[kv$keybl_MTBcat=="1 = Pulmonary: positive, MTB"]<-1
kv$keybl_MTBcat.f[kv$keybl_MTBcat=="2 = Pulmonary: positive, Mycobacterium species unknown/TBD"]<-2
kv$keybl_MTBcat.f[kv$keybl_MTBcat=="3 = Pulmonary: positive, Mycobacterium non-TB"]<-3


kv$keybl_MTBcat.f[kv$keybl_MTBcat=="11 = Extrapulmonary: positive, MTB"]<-4


kv$keybl_MTBcat.f[kv$keybl_MTBcat=="4 = Pulmonary: negative"]<-7
kv$keybl_MTBcat.f[kv$keybl_MTBcat=="14 = Extrapulmonary: negative"]<-8 

kv$keybl_MTBcat.f[kv$keybl_MTBcat=="99 = Pending/other"]<-9 

kv$keybl_MTBcat.f<-factor(kv$keybl_MTBcat.f, levels = c(1,2,3,4,7,8,9),
                           labels=c("PTB- Positive, MTB",
                                    "PTB- Positive, species unknown/TBD",
                                    "PTB- Positive, non-MTB",
                                    "EPTB- Positive",
                                    "PTB- Negative",
                                    "EPTB- Negative",
                                    "Pending/Other"))

table(kv$keybl_MTBcat, kv$keybl_MTBcat.f)


#Xpert 

table(kv$keybl_gxpcat, useNA = "always")


kv$keybl_gxpcat.f[kv$keybl_gxpcat=="1 = Sputum, positive"]<-1
kv$keybl_gxpcat.f[kv$keybl_gxpcat=="2 = Sputum, negative"]<-3

kv$keybl_gxpcat.f[kv$keybl_gxpcat=="4 = Sputum, specimen inadequate"]<-8

kv$keybl_gxpcat.f[kv$keybl_gxpcat=="6 = Extrapulmonary, positive"]<-2                 
kv$keybl_gxpcat.f[kv$keybl_gxpcat=="7 = Extrapulmonary, negative"]<-4

kv$keybl_gxpcat.f[kv$keybl_gxpcat=="99 = Not ordered"]<-9

kv$keybl_gxpcat.f<-factor(kv$keybl_gxpcat.f, levels = c(1,2,3,4,8,9),
                          labels=c("PTB- Positive",
                                   "EPTB- Positive",
                                   "PTB- Negative",
                                   "EPTB- Negative",
                                   "Specimen inadequate",
                                   "Not ordered"))

table(kv$keybl_gxpcat.f, kv$keybl_gxpcat)

#Culture
table(kv$keybl_cultcat, useNA = "ifany")

kv$keybl_cultcat.f[kv$keybl_cultcat=="1 = Sputum, positive M. tuberculosis"]<-1
kv$keybl_cultcat.f[kv$keybl_cultcat=="3 = Sputum, positive Mycobacterium (missing type)"]<-2
kv$keybl_cultcat.f[kv$keybl_cultcat=="4 = Sputum, positive non-tuberculous mycobacteria"]<-3
kv$keybl_cultcat.f[kv$keybl_cultcat=="11 = Extrapulmonary, positive M. tuberculosis"]<-4

kv$keybl_cultcat.f[kv$keybl_cultcat=="5 = Sputum, negative"]<-6
kv$keybl_cultcat.f[kv$keybl_cultcat=="15 = Extrapulmonary, negative"]<-7
                   
kv$keybl_cultcat.f[kv$keybl_cultcat=="6 = Sputum, pending"]<-8
kv$keybl_cultcat.f[kv$keybl_cultcat=="7 = Sputum, specimen inadequate"]<-9
kv$keybl_cultcat.f[kv$keybl_cultcat=="99 = Not ordered"]<-10


kv$keybl_cultcat.f<-factor(kv$keybl_cultcat.f, levels = c(1,2,3,4,6,7,8,9,10),
                          labels=c("PTB- Positive",
                                   "PTB- Positive, no type",
                                   "PTB- Positve, NTM",
                                   "EPTB- Positive",
                                   "PTB- Negative",
                                   "EPTB- Negative",
                                   "PTB- Pending",
                                   "Specimen inadequate",
                                   "Not ordered"
                                   ))

table(kv$keybl_cultcat.f, kv$keybl_cultcat, useNA = "ifany")



#AFB Cat

table(kv$keybl_afbcat, useNA = "ifany")

kv$keybl_afbcat.f[kv$keybl_afbcat=="0 = Negative"]<-0
kv$keybl_afbcat.f[kv$keybl_afbcat=="1 = Positive"]<-1
kv$keybl_afbcat.f[kv$keybl_afbcat=="2 = Pending"]<-2

kv$keybl_afbcat.f<-factor(kv$keybl_afbcat.f, levels=c(0,1,2),
                          labels=c("Negative",
                                   "Positive",
                                   "Pending"))

#HIV status

table(kv$keybl_hiv, useNA = "ifany")

kv$keybl_hiv.f[kv$keybl_hiv=="0 = Negative"]<-0
kv$keybl_hiv.f[kv$keybl_hiv=="1 = Positive"]<-1

kv$keybl_hiv.f<-factor(kv$keybl_hiv.f, levels=c(0,1),
                       labels=c("Negative",
                                  "Positive"))
#Checking MTB status for NTM    

nonMTB<-kv %>% filter(keybl_MTBcat=="2 = Pulmonary: positive, Mycobacterium species unknown/TBD"|
                              keybl_MTBcat=="3 = Pulmonary: positive, Mycobacterium non-TB")


write.csv(nonMTB, "~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/WD/nonMTB.csv")




## Checking if any  participants need to be reclassified between pulmonary & EPTB 

table(kv$keybl_MTBcat.f, kv$keybl_rectype.f)

#2 participants might need to be reclassied, checking participant IDs
eptb_reclass<-subset(kv, (kv$keybl_rectype.f=="EPTB") & (kv$keybl_MTBcat.f=="PTB- Positive, MTB"))
eptb_reclass

#Participant ID NLH-0776-UR, RECLASSIFY AS PTB

ptb_reclass<-subset(kv, (kv$keybl_rectype.f=="PTB") & (kv$keybl_MTBcat.f=="EPTB- Positive"))

#Participant ID NLH-0519-UR, negative using sputum but positive Xpert using bronchial fluid
#Decision from Paul 8.11 - Keep as PTB (Doesn't actually matter, participant dropped since FLOW only tested on sputum)

#NLH0519<-kv[kv$PTID=="NLH-0519-UR",]



### Variable creation
#Ptb01
#Creating new variable to designate final PTB vs EPTB status

kv <- kv %>% mutate(ptb01 =
                   case_when(keybl_rectype.f=="PTB" ~ 1, 
                             (keybl_rectype.f=="EPTB") & (keybl_MTBcat.f=="PTB- Positive, MTB") ~ 1,
                             (keybl_rectype.f=="EPTB") & (keybl_MTBcat.f=="PTB- Positive, species unknown/TBD") ~ 1,
                                TRUE ~ 0))


table(kv$keybl_MTBcat.f, kv$keybl_rectype.f, kv$ptb01) #Correctly fixed

#Creating format version
kv$ptb01.f<-factor(kv$ptb01, levels = c(0,1),
                             labels = c("EPTB",
                                        "PTB"))

#MRSref
#Creating MRS variable for calculating diagnostic parameters
#Xpert positive or culture positive 
#For NTM, if Xpert positive then considered MRS positive

kv <- kv %>% mutate(MRSref =
                            case_when(
                                ((keybl_MTBcat.f=="PTB- Positive, MTB")|
                                (keybl_MTBcat.f=="EPTB- Positive")) ~ 1, #Xpert positive & ID'd as MTB
                                
                                        (keybl_MTBcat.f=="PTB- Positive, species unknown/TBD") & 
                                       (keybl_gxpcat.f=="PTB- Positive") ~ 1, # unknown species but Xpert positive
                                
                                        (keybl_MTBcat.f=="PTB- Positive, non-MTB") & 
                                        (keybl_gxpcat.f=="PTB- Positive") ~ 1, #NTM but Xpert positive
                                
                                        (keybl_MTBcat.f=="PTB- Positive, non-MTB") & 
                                        (keybl_gxpcat.f=="PTB- Negative") ~ 0, #NTM but Xpert negative
                                
                                        (keybl_MTBcat.f=="PTB- Negative")|
                                        (keybl_MTBcat.f=="EPTB- Negative") ~ 0, #Negative

                                
                            ),
                                


#Xpert alone reference standard

                     gxpref = 
                                case_when( (keybl_gxpcat.f=="PTB- Positive") | 
                                           (keybl_gxpcat.f=="EPTB- Positive") ~ 1,
                                           
                                           (keybl_gxpcat.f=="PTB- Negative") | 
                                           (keybl_gxpcat.f=="EPTB- Negative") ~ 0,
                                           
                                        
                                        
                                        
                                ),


#Culture alone category
                        
                     cultref = 
                                case_when((keybl_cultcat.f=="PTB- Positive")|
                                          (keybl_cultcat.f=="EPTB- Positive") ~ 1,
                                          
                                          (keybl_cultcat.f=="PTB- Negative")|
                                          (keybl_cultcat.f=="EPTB- Negative") ~ 0,
                                          
                                          ((keybl_cultcat.f=="PTB- Positve, NTM")|(keybl_cultcat.f=="PTB- Positive, no type")) ~ as.numeric(NA)  #Paul requested to exclude when using culture only ref
                                                  
                                          
                                          
                                          )


                             ) 



##Checking variable creation
#MRS
table(kv$keybl_MTBcat.f, kv$MRSref, kv$keybl_gxpcat.f, useNA = "ifany") # Looks OK! Need to figure out what the pending/other issue is though
                                        

pending<-subset(kv, kv$keybl_MTBcat.f=="Pending/Other") ###Remember to review once data has been uploaded


#Xpert only reference 
table(kv$keybl_gxpcat.f, kv$gxpref, useNA = "ifany") # Looks OK! Participants with an inadequate specimen or not ordered set to  NA

gxpnotordered<-subset(kv, (kv$keybl_gxpcat.f=="Specimen inadequate") | (kv$keybl_gxpcat.f=="Not ordered"))
#Looks like all EPTB patients, makes sense


#Culture only
table(kv$keybl_cultcat.f, kv$cultref, useNA = "ifany")

#Deleting participants with FLOW-TB tested on fresh urine
kv<-kv[!kv$PTID %in% c(
        'NLH-0111-UR',
        'NLH-0112-UR',
        'NLH-0113-UR',
        'NLH-0114-UR',
        'NLH-0115-UR',
        'NLH-0116-UR',
        'NLH-0117-UR',
        'NLH-0118-UR',
        'NLH-0119-UR',
        'NLH-0120-UR',
        'NLH-0121-UR',
        'NLH-0122-UR',
        'NLH-0499-UR',
        'NLH-0500-UR',
        'NLH-0501-UR',
        'NLH-0502-UR',
        'NLH-0503-UR',
        'NLH-0504-UR',
        'NLH-0505-UR',
        'NLH-0506-UR',
        'NLH-0507-UR',
        'NLH-0508-UR',
        'NLH-0509-UR',
        'NLH-0510-UR',
        'NLH-0511-UR',
        'NLH-0512-UR',
        'NLH-0513-UR',
        'NLH-0514-UR',
        'NLH-0515-UR',
        'NLH-0516-UR',
        'NLH-0517-UR',
        'NLH-0518-UR',
        'NLH-0519-UR',
        'NLH-0520-UR',
        'NLH-0521-UR',
        'NLH-0522-UR',
        'NLH-0523-UR',
        'NLH-0524-UR',
        'NLH-0525-UR',
        'NLH-0526-UR',
        'NLH-0527-UR',
        'NLH-0528-UR',
        'NLH-0529-UR',
        'NLH-0530-UR',
        'NLH-0531-UR',
        'NLH-0532-UR',
        'NLH-0533-UR',
        'NLH-0534-UR',
        'NLH-0535-UR',
        'NLH-0536-UR',
        'NLH-0537-UR',
        'NLH-0538-UR',
        'NLH-0539-UR',
        'NLH-0540-UR',
        'NLH-0541-UR',
        'NLH-0542-UR',
        'NLH-0543-UR',
        'NLH-0544-UR',
        'NLH-0545-UR',
        'NLH-0546-UR',
        'NLH-0547-UR',
        'NLH-0548-UR',
        'NLH-0549-UR',
        'NLH-0550-UR',
        'NLH-0551-UR',
        'NLH-0651-UR',
        'NLH-0745-UR',
        'NLH-0746-UR',
        'NLH-0747-UR',
        'NLH-0748-UR',
        'NLH-0749-UR',
        'NLH-0750-UR'
        
), ]


#Deleting participants with urine collected beyond 48 hour window (n=11)

kv<-kv[!kv$PTID %in% c(
        'NLH-0210-UR',
        'NLH-0230-UR',
        'NLH-0261-UR',
        'NLH-0298-UR',
        'NLH-0369-UR',
        'NLH-0726-UR',
        'NLH-0728-UR',
        'NLH-0733-UR',
        'NLH-0734-UR',
        'NLH-0735-UR',
        'NLH-0741-UR'
),]



##Manually Changing MTB_Cat status 


######################
###NLH FLOW TB Results
#####
##Fixing PTID issue in order to merge results
nlh <- nlhraw %>%
        
        # Rename to make this field easier to reference
        rename(raw_ptid = ï..PTIDraw ) %>%
        
        # Create a standardized PTID
        mutate(PTID = case_when(
                
                # If the -UR suffix is already present at the end of the PTID,
                # just replace spaces with dashes
                str_detect(raw_ptid, "\\sUR$") ~ str_replace_all(raw_ptid, " ", "-"),
                
                # If the -UR suffix isn't already at the end of the PTID,
                # replace the space with a dash, then add the suffix
                str_detect(raw_ptid, "\\sUR$", negate = TRUE) ~ paste0(str_replace(raw_ptid, " ", "-"),
                                                                       "-UR"))) %>%

        select(-c(raw_ptid, PTIDnochar, PTIDfix))

#Converting character dates to date variable

nlh$datetest<-as.Date(nlh$datetest, "%d/%m/%Y")

nlh$datethaw<-as.Date(nlh$datethaw, "%d/%m/%Y")



skimr::skim(nlh)

head(nlh)
summary(nlh)

glimpse(nlh)


#Creating factor version of qualitative test result

table(nlh$testqual)



#Urine volume (checking PTID)
missvol<-nlh %>% group_by(PTID) %>%filter(is.na(urvolrem))

head(missvol)

#PTID NLH-0738-UR missing remaining urine volume, when crosschecked with CRF this should be 10ml

nlh$urvolrem[nlh$PTID=="NLH-0738-UR"]<-10

nlh[nlh$PTID=="NLH-0738-UR",] #Fixed


#Deleting participants with FLOW-TB tested in fresh urine

nlh<-nlh[!nlh$PTID %in% c(
        'NLH-0111-UR',
        'NLH-0112-UR',
        'NLH-0113-UR',
        'NLH-0114-UR',
        'NLH-0115-UR',
        'NLH-0116-UR',
        'NLH-0117-UR',
        'NLH-0118-UR',
        'NLH-0119-UR',
        'NLH-0120-UR',
        'NLH-0121-UR',
        'NLH-0122-UR',
        'NLH-0499-UR',
        'NLH-0500-UR',
        'NLH-0501-UR',
        'NLH-0502-UR',
        'NLH-0503-UR',
        'NLH-0504-UR',
        'NLH-0505-UR',
        'NLH-0506-UR',
        'NLH-0507-UR',
        'NLH-0508-UR',
        'NLH-0509-UR',
        'NLH-0510-UR',
        'NLH-0511-UR',
        'NLH-0512-UR',
        'NLH-0513-UR',
        'NLH-0514-UR',
        'NLH-0515-UR',
        'NLH-0516-UR',
        'NLH-0517-UR',
        'NLH-0518-UR',
        'NLH-0519-UR',
        'NLH-0520-UR',
        'NLH-0521-UR',
        'NLH-0522-UR',
        'NLH-0523-UR',
        'NLH-0524-UR',
        'NLH-0525-UR',
        'NLH-0526-UR',
        'NLH-0527-UR',
        'NLH-0528-UR',
        'NLH-0529-UR',
        'NLH-0530-UR',
        'NLH-0531-UR',
        'NLH-0532-UR',
        'NLH-0533-UR',
        'NLH-0534-UR',
        'NLH-0535-UR',
        'NLH-0536-UR',
        'NLH-0537-UR',
        'NLH-0538-UR',
        'NLH-0539-UR',
        'NLH-0540-UR',
        'NLH-0541-UR',
        'NLH-0542-UR',
        'NLH-0543-UR',
        'NLH-0544-UR',
        'NLH-0545-UR',
        'NLH-0546-UR',
        'NLH-0547-UR',
        'NLH-0548-UR',
        'NLH-0549-UR',
        'NLH-0550-UR',
        'NLH-0551-UR',
        'NLH-0651-UR',
        'NLH-0745-UR',
        'NLH-0746-UR',
        'NLH-0747-UR',
        'NLH-0748-UR',
        'NLH-0749-UR',
        'NLH-0750-UR'
        
), ]


#Deleting participants with urine collected beyond 48 hour window (n=11)

nlh<-nlh[!nlh$PTID %in% c(
        'NLH-0210-UR',
        'NLH-0230-UR',
        'NLH-0261-UR',
        'NLH-0298-UR',
        'NLH-0369-UR',
        'NLH-0726-UR',
        'NLH-0728-UR',
        'NLH-0733-UR',
        'NLH-0734-UR',
        'NLH-0735-UR',
        'NLH-0741-UR'
),]


#Variable creation

#Creating factor version of qualitative result

nlh$testqual_c[nlh$testqual=="neg"]<-0
nlh$testqual_c[nlh$testqual=="Neg"]<-0
nlh$testqual_c[nlh$testqual=="NEG"]<-0
nlh$testqual_c[nlh$testqual=="1+"]<-1
nlh$testqual_c[nlh$testqual=="2+"]<-2
nlh$testqual_c[nlh$testqual=="3+"]<-3
nlh$testqual_c[nlh$testqual=="4+"]<-4
nlh$testqual_c[nlh$testqual=="5+"]<-5
nlh$testqual_c[nlh$testqual=="6+"]<-6
nlh$testqual_c[nlh$testqual=="7+"]<-7
nlh$testqual_c[nlh$testqual=="8+"]<-8
nlh$testqual_c[nlh$testqual=="9+"]<-9


nlh$testqual.f<-factor(nlh$testqual_c, levels= c(0:9),
                       labels= c("Negative",
                                 "1+",
                                 "2+",
                                 "3+",
                                 "4+",
                                 "5+",
                                 "6+",
                                 "7+",
                                 "8+",
                                 "9+"))





####################
#PATH MSD Data - Updated 9.13.22

#Received from PATH 9.8.22
####################

msd<- msdraw %>% dplyr::select(-c(ï..PTID.specimen.ID, ptidnum))

#Deleting participants with FLOW-TB tested in fresh urine

msd<-msd[!msd$PTIDclean %in% c(
        'NLH-0111-UR',
        'NLH-0112-UR',
        'NLH-0113-UR',
        'NLH-0114-UR',
        'NLH-0115-UR',
        'NLH-0116-UR',
        'NLH-0117-UR',
        'NLH-0118-UR',
        'NLH-0119-UR',
        'NLH-0120-UR',
        'NLH-0121-UR',
        'NLH-0122-UR',
        'NLH-0499-UR',
        'NLH-0500-UR',
        'NLH-0501-UR',
        'NLH-0502-UR',
        'NLH-0503-UR',
        'NLH-0504-UR',
        'NLH-0505-UR',
        'NLH-0506-UR',
        'NLH-0507-UR',
        'NLH-0508-UR',
        'NLH-0509-UR',
        'NLH-0510-UR',
        'NLH-0511-UR',
        'NLH-0512-UR',
        'NLH-0513-UR',
        'NLH-0514-UR',
        'NLH-0515-UR',
        'NLH-0516-UR',
        'NLH-0517-UR',
        'NLH-0518-UR',
        'NLH-0519-UR',
        'NLH-0520-UR',
        'NLH-0521-UR',
        'NLH-0522-UR',
        'NLH-0523-UR',
        'NLH-0524-UR',
        'NLH-0525-UR',
        'NLH-0526-UR',
        'NLH-0527-UR',
        'NLH-0528-UR',
        'NLH-0529-UR',
        'NLH-0530-UR',
        'NLH-0531-UR',
        'NLH-0532-UR',
        'NLH-0533-UR',
        'NLH-0534-UR',
        'NLH-0535-UR',
        'NLH-0536-UR',
        'NLH-0537-UR',
        'NLH-0538-UR',
        'NLH-0539-UR',
        'NLH-0540-UR',
        'NLH-0541-UR',
        'NLH-0542-UR',
        'NLH-0543-UR',
        'NLH-0544-UR',
        'NLH-0545-UR',
        'NLH-0546-UR',
        'NLH-0547-UR',
        'NLH-0548-UR',
        'NLH-0549-UR',
        'NLH-0550-UR',
        'NLH-0551-UR',
        'NLH-0651-UR',
        'NLH-0745-UR',
        'NLH-0746-UR',
        'NLH-0747-UR',
        'NLH-0748-UR',
        'NLH-0749-UR',
        'NLH-0750-UR'
        
), ]


#Deleting participants with urine collected beyond 48 hour window (n=11)

msd<-msd[!msd$PTIDclean %in% c(
        'NLH-0210-UR',
        'NLH-0230-UR',
        'NLH-0261-UR',
        'NLH-0298-UR',
        'NLH-0369-UR',
        'NLH-0726-UR',
        'NLH-0728-UR',
        'NLH-0733-UR',
        'NLH-0734-UR',
        'NLH-0735-UR',
        'NLH-0741-UR'
),]



###Data cleaning

#Renaming variable names

msd<- msd %>% rename(
        
        FA.conc = FA.Concentration,
        FA.conc.cv = FA.concentration.CV,
        OA.conc = OA.Concentration,
        OA.conc.cv = OA.concentration.CV,
        X5.conc = X5e3.Concentration,
        X5.conc.cv = X5e3.concentration.CV,
        
        FA.detect = FA.Detection.Range,
        OA.detect = OA.Detection.Range,
        X5.detect = X5e3.Detection.Range,
        
        
        PTID = PTIDclean)


#Log transformed versions of the concentrations


msd<- msd %>% mutate(
        
        logFA.conc = log(FA.conc),
        logOA.conc = log(OA.conc),
        logX5.conc = log(X5.conc),
        logFA.sign = log(FA.Signal),
        logOA.sign = log(OA.Signal),
        logX5.sign = log(X5e3.Signal)
        )

#Fixing infinite values for log to 0    9.30.22

msd$logFA.conc[msd$FA.conc==0]<-0
msd$logOA.conc[msd$OA.conc==0]<-0
msd$logX5.conc[msd$X5.conc==0]<-0
msd$logFA.sign[msd$FA.Signal==0]<-0
msd$logOA.sign[msd$OA.Signal==0]<-0
msd$logX5.sign[msd$X5e3.Signal==0]<-0


#Creating format version of detection variable, combining "Below Fit Curve Range/Below Detection Range"
# and "Below Detection Range/Below Fit Curve Range" into single category

#FA

msd$FA.detect.f[msd$FA.detect=="In Detection Range"]<-1
msd$FA.detect.f[msd$FA.detect=="Below Detection Range"]<-2
msd$FA.detect.f[msd$FA.detect=="Below Fit Curve Range"]<-3
msd$FA.detect.f[msd$FA.detect=="Below Detection Range/Below Fit Curve Range"]<-4
msd$FA.detect.f[msd$FA.detect=="Below Fit Curve Range/Below Detection Range"]<-4


msd$FA.detect.f<-factor(msd$FA.detect.f, levels=c(1:4),
                                         labels=c("In Detection Range",
                                                  "Below Detection Range",
                                                  "Below Fit Curve Range",
                                                  "Below Detection Range/Below Fit Curve Range"))


#OA

msd$OA.detect.f[msd$OA.detect=="In Detection Range"]<-1
msd$OA.detect.f[msd$OA.detect=="Below Detection Range"]<-2
msd$OA.detect.f[msd$OA.detect=="Below Fit Curve Range"]<-3
msd$OA.detect.f[msd$OA.detect=="Below Detection Range/Below Fit Curve Range"]<-4
msd$OA.detect.f[msd$OA.detect=="Below Detection Range/Below Fit Curve"]<-4
msd$OA.detect.f[msd$OA.detect=="Below Fit Curve Range/Below Detection Range"]<-4


msd$OA.detect.f<-factor(msd$OA.detect.f, levels=c(1:4),
                        labels=c("In Detection Range",
                                 "Below Detection Range",
                                 "Below Fit Curve Range",
                                 "Below Detection Range/Below Fit Curve Range"))

#X5

msd$X5.detect.f[msd$X5.detect=="In Detection Range"]<-1
msd$X5.detect.f[msd$X5.detect=="Below Detection Range"]<-2
msd$X5.detect.f[msd$X5.detect=="Below Fit Curve Range"]<-3
msd$X5.detect.f[msd$X5.detect=="Below Fit Curve Range/Below Detection Range"]<-4

msd$X5.detect.f<-factor(msd$X5.detect.f, levels=c(1:4),
                        labels=c("In Detection Range",
                                 "Below Detection Range",
                                 "Below Fit Curve Range",
                                 "Below Detection Range/Below Fit Curve Range"))

#Creating variable for whether signal was below detection range or fit curve


msd<- msd %>% mutate(
        FA.belowfit = case_when(FA.detect== "Below Fit Curve Range" ~ 1,
                                FA.detect== "Below Detection Range/Below Fit Curve Range" ~ 1,
                                FA.detect== "Below Fit Curve Range/Below Detection Range" ~ 1,
                                TRUE ~ 0),
        
        FA.belowdetect = case_when(FA.detect== "Below Detection Range" ~ 1,
                                   FA.detect== "Below Detection Range/Below Fit Curve Range" ~ 1,
                                   FA.detect== "Below Fit Curve Range/Below Detection Range" ~ 1,
                                   TRUE ~ 0),
        
        
        
        OA.belowfit = case_when(OA.detect== "Below Fit Curve Range" ~ 1,
                                OA.detect== "Below Detection Range/Below Fit Curve Range" ~ 1,
                                OA.detect== "Below Detection Range/Below Fit Curve" ~ 1,
                                OA.detect== "Below Fit Curve Range/Below Detection Range" ~ 1,
                                
                                TRUE ~ 0),
        
        OA.belowdetect = case_when(OA.detect== "Below Detection Range" ~ 1,
                                OA.detect== "Below Detection Range/Below Fit Curve Range" ~ 1,
                                OA.detect== "Below Detection Range/Below Fit Curve" ~ 1,
                                OA.detect== "Below Fit Curve Range/Below Detection Range" ~ 1,
                                
                                TRUE ~ 0),
        
        X5.belowfit = case_when(X5.detect== "Below Fit Curve Range" ~ 1,
                                X5.detect== "Below Fit Curve Range/Below Detection Range" ~ 1,
                                is.na(X5.detect.f) ~ NA_real_,
                               
                                TRUE ~ 0),
        
        
        X5.belowdetect = case_when(X5.detect== "Below Detection Range" ~ 1,
                                X5.detect== "Below Fit Curve Range/Below Detection Range" ~ 1,
                                is.na(X5.detect.f) ~ NA_real_,
                                TRUE ~ 0),
)

#Checking to see if correctly coded
table(msd$FA.belowdetect, msd$FA.detect.f, useNA = "ifany")
table(msd$FA.belowfit, msd$FA.detect.f, useNA = "ifany")            
                
table(msd$OA.belowdetect, msd$OA.detect.f, useNA = "ifany")
table(msd$OA.belowfit, msd$OA.detect.f, useNA = "ifany")     

table(msd$X5.belowdetect, msd$X5.detect.f, useNA = "ifany")
table(msd$X5.belowfit, msd$X5.detect.f, useNA = "ifany")     


#Creating MSD Reference categories 


msd<- msd %>% mutate(
        
        #FA
        
        MSDref.FA = case_when(FA.detect.f == "In Detection Range" ~ 1,
                              FA.detect.f == "Below Detection Range" ~ 0,
                              FA.detect.f == "Below Fit Curve Range" ~ 0,
                              FA.detect.f == "Below Detection Range/Below Fit Curve Range" ~ 0)
        ,
        
        
        #OA
        
        MSDref.OA = case_when(OA.detect.f == "In Detection Range" ~ 1,
                              OA.detect.f == "Below Detection Range" ~ 0,
                              OA.detect.f == "Below Fit Curve Range" ~ 0,
                              OA.detect.f == "Below Detection Range/Below Fit Curve Range" ~ 0)
        ,
        
        
        #X5
        
        MSDref.X5 = case_when(X5.detect.f == "In Detection Range" ~ 1,
                              X5.detect.f == "Below Detection Range" ~ 0,
                              X5.detect.f == "Below Fit Curve Range" ~ 0,
                              X5.detect.f == "Below Detection Range/Below Fit Curve Range" ~ 0,
                              is.na(X5.detect.f) ~ NA_real_)
)


#Checking for correct creation
table(msd$MSDref.FA, msd$FA.detect.f, useNA = "ifany")
table(msd$MSDref.OA, msd$OA.detect.f, useNA = "ifany")
table(msd$MSDref.X5, msd$X5.detect.f, useNA = "ifany")






#Checking for missing values
sum(is.na(msd$PTID))

sum(is.na(msd$FA.Signal))
sum(is.na(msd$FA.Signal.CV))
sum(is.na(msd$FA.conc))
sum(is.na(msd$FA.conc.cv))

sum(is.na(msd$OA.Signal))
sum(is.na(msd$OA.Signal.CV))
sum(is.na(msd$OA.conc))
sum(is.na(msd$OA.conc.cv))

sum(is.na(msd$X5e3.Signal))   #617 missing
sum(is.na(msd$X5e3.Signal.CV)) #617 missing
sum(is.na(msd$X5.conc)) #617 missing
sum(is.na(msd$X5.conc.cv)) #617 missing


##Distribution of values

#FA
summary(msd$FA.Signal)
hist(msd$FA.Signal)


summary(msd$FA.Signal.CV)
hist(msd$FA.Signal.CV)

summary(msd$FA.conc)
hist(msd$FA.conc)


summary(msd$FA.conc.cv)
hist(msd$FA.conc.cv)

#OA

summary(msd$OA.Signal)
hist(msd$OA.Signal)


summary(msd$OA.Signal.CV)
hist(msd$OA.Signal.CV)

summary(msd$OA.conc)
hist(msd$OA.conc)


summary(msd$OA.conc.cv)
hist(msd$OA.conc.cv)

#X5


#Plotting variables against each other

pairs(~FA.Signal + logFA.sign + FA.conc + logFA.conc , data=msd)

pairs(~OA.Signal + logOA.sign + OA.conc + logFA.conc , data=msd)

pairs(~X5e3.Signal + logX5.sign + X5.conc + logX5.conc , data=msd)

pairs( ~ FA.conc  +  OA.conc + X5.conc  ,  data=msd)

pairs( ~ logFA.conc +  logOA.conc + logX5.conc,  data=msd)



#Removing outliers for comparison
msd_noout<-subset(msd, (OA.conc < 30000) & (FA.conc < 618901))

plot(msd_noout$OA.conc, msd_noout$FA.conc)

plot(msd_noout$logOA.conc, msd_noout$logFA.conc)
#Seems to match when log transforming both 







###########################
#Merging datasets together
#####
#Datasets to merge:
# rc  (Raw REDCap dataset), 697 x 123
# kv  (Calculated key variables), 697 x 30

## 9.17 update ****  msd (MSD data), 698 x 32 vars

#First checking IDs between different datasets for mismatch

#Creating dfs containing IDs

rc_id<-rc %>% dplyr::select(c(ptid))
kv_id<-kv %>% dplyr::select(c(PTID))
nlh_id<-nlh %>% dplyr::select(c(PTID))

        #added 9/23/22
msd_id<-msd %>% dplyr::select(c(PTID))

summary(comparedf(rc_id, kv_id))
summary(comparedf(rc_id, nlh_id))
summary(comparedf(kv_id, nlh_id))
#All PTIDs match between the three datasets

summary(comparedf(rc_id, msd_id))
# All PTIDS match between msd & rc dataset

#Creating a dataset with just RC & KV dataset combined, using full join
rc_kv<-rc %>% full_join(kv, by=c("ptid" = "PTID")) #To be used when all data entered
                        
#10.4 - No longer using NLH dataset post data cleaning

##9.23.22 - adding MSD data 

fulldat<-rc_kv%>% full_join(msd, by=c("ptid" = "PTID"))



#Successfully joined!



####10.4.22 - Moving variable creation sections back here to do a full comparison of flow results (even EPTB)

#Variable creation

#Create new composite variable that combines pocfrz_flowresult.factor & pocfrz_flowresultpos.factor

fulldat<- fulldat %>% mutate(
        rc.flowresultqual = case_when(
                pocfrz_flowresult.factor == "Negative" ~ 0,
                pocfrz_flowresultpos.factor=="1+" ~ 1,
                pocfrz_flowresultpos.factor=="2+" ~ 2,
                pocfrz_flowresultpos.factor=="3+" ~ 3,
                pocfrz_flowresultpos.factor=="4+" ~ 4,
                pocfrz_flowresultpos.factor=="5+" ~ 5,
                pocfrz_flowresultpos.factor=="6+" ~ 6,
                pocfrz_flowresultpos.factor=="7+" ~ 7,
                pocfrz_flowresultpos.factor=="8+" ~ 8,
                pocfrz_flowresultpos.factor=="9+" ~ 9,
        )
)





#Factor versions of created variables
fulldat$rc.flowresultqual.f<-factor(fulldat$rc.flowresultqual, levels=c(0:9),
                               labels=c("Negative",
                                        "1+",
                                        "2+",
                                        "3+",
                                        "4+",
                                        "5+",
                                        "6+",
                                        "7+",
                                        "8+",
                                        "9+")
)

#Checking variable

table(fulldat$pocfrz_flowresult.factor, fulldat$pocfrz_flowresultpos.factor, fulldat$rc.flowresultqual, useNA = 'ifany')




#Checking how many participants have a discordant quant vs qual result
#10.4.22 - Commenting out NLH variables
fulldat<-fulldat %>% mutate(
        
        #NLH
#        quant0.nlh = case_when(testquant ==0 ~ 1,
#                               testquant > 0 ~ 0),
#        
#        qual0.nlh = case_when(testqual.f == "Negative" ~ 1,
#                              testqual.f != "Negative" ~ 0),
#        
        #RC
        quant0.rc = case_when(pocfrz_flowcubetest ==0 ~ 1,
                              pocfrz_flowcubetest > 0 ~ 0),
        
        qual0.rc = case_when(rc.flowresultqual.f == "Negative" ~ 1,
                             rc.flowresultqual.f != "Negative" ~ 0) 
        
)





table( fulldat$rc.flowresultqual.f, fulldat$ quant0.rc, useNA = 'ifany')



#Creating 
fulldat<- fulldat %>% mutate(
        
        
        ##NLH
        #highvol_c.nlh = case_when( urvolrem >5 ~ 1,
        #                           urvolrem <= 5 ~ 0),
        
        
        
        #invquant_c.nlh = case_when( controlquant <= 0 ~ 1, #Invalid control value 
        #                            
        #                            
        #                            highvol_c.nlh==1 & quant0.nlh==1 ~ 1, #High flow volume remaining + Quantative negative result 
        #                            
        #                            highvol_c.nlh==0 & quant0.nlh==0 ~ 0,#Cube reader value >0 & remaining volume <=5 
        #                           highvol_c.nlh==1 & quant0.nlh==0 ~ 0,#Cube reader value >0 (positive) & high remaining volume
        #                            highvol_c.nlh==0 & quant0.nlh==1 ~ 0), #Quantative negative result & remaining volume <=5 
        
        #invqual_c.nlh = case_when( controlquant <= 0 ~ 1, #Invalid control value 
                                   
                                   
        #                           highvol_c.nlh==1 & qual0.nlh==1 ~ 1, #High flow volume remaining + Qualitative negative result 
        #                           
        #                          highvol_c.nlh==0 & qual0.nlh==0 ~ 0,#Positive qualitative result & remaining volume <=5 
        #                           highvol_c.nlh==1 & qual0.nlh==0 ~ 0,#Positive qualitative result & high remaining volume
        #                           highvol_c.nlh==0 & qual0.nlh==1 ~ 0), #Qualitative negative result & remaining volume <=5
        
        
        ##REDCap
        
        highvol_c.rc = case_when( pocfrz_flowvolremain >5 ~ 1,
                                  pocfrz_flowvolremain <= 5 ~ 0),
        
        
        
        invquant_c.rc = case_when( pocfrz_flowcubecontrol <= 0 ~ 1, #Invalid control value 
                                   
                                   
                                   highvol_c.rc==1 & quant0.rc==1 ~ 1, #High flow volume remaining + Quantative negative result 
                                   
                                   highvol_c.rc==0 & quant0.rc==0 ~ 0,#Cube reader value >0 & remaining volume <=5 
                                   highvol_c.rc==1 & quant0.rc==0 ~ 0,#Cube reader value >0 (positive) & high remaining volume
                                   highvol_c.rc==0 & quant0.rc==1 ~ 0), #Quantative negative result & remaining volume <=5 
        
        invqual_c.rc = case_when( pocfrz_flowcubecontrol <= 0 ~ 1, #Invalid control value 
                                  
                                  
                                  highvol_c.rc==1 & qual0.rc==1 ~ 1, #High flow volume remaining + Qualitative negative result 
                                  
                                  highvol_c.rc==0 & qual0.rc==0 ~ 0,#Positive qualitative result & remaining volume <=5 
                                  highvol_c.rc==1 & qual0.rc==0 ~ 0,#Positive qualitative result & high remaining volume
                                  highvol_c.rc==0 & qual0.rc==1 ~ 0), #Qualitative negative result & remaining volume <=5   
        
)

#Checking variable creation
#table(fulldat$highvol_c.nlh, fulldat$urvolrem, useNA = "ifany")     

#table(fulldat$highvol_c.nlh, fulldat$quant0.nlh, fulldat$invquant_c.nlh, useNA = "ifany", deparse.level = 2) #0 participants with invalid results

#table(fulldat$highvol_c.nlh, fulldat$qual0.nlh, fulldat$invqual_c.nlh, useNA = "ifany", deparse.level = 2) #18 participants with invalid results

#table(fulldat$highvol_c.rc, fulldat$pocfrz_flowvolremain, useNA = "ifany")   

#table(fulldat$highvol_c.rc, fulldat$quant0.rc, fulldat$invquant_c.rc, useNA = "ifany", deparse.level = 2) #12 missing, consider invalid

#table(fulldat$highvol_c.rc, fulldat$qual0.rc, fulldat$invqual_c.rc, useNA = "ifany", deparse.level = 2) #17 invalid, 12 missing





#Comparing discordant between qualitative & quantitative criteria for an invalid result

table(fulldat$invquant_c.rc, fulldat$invqual_c.rc, useNA='ifany', deparse.level = 2)



#############################
#NLH VS RC RESULT COMPARISON
#############################

flow<- fulldat %>% dplyr::select(c(
        ptid,
        
        #Volume remaining
        urvolrem,
        pocfrz_flowvolremain,
        
        #Qualitative
        testqual.f,
        rc.flowresultqual.f,
        
        #Quantitative
        testquant,
        pocfrz_flowcubetest,
        
        #Control
        controlquant,
        pocfrz_flowcubecontrol
)) 


summary(flow)
glimpse(flow) 



flow<- flow %>% mutate(
        urine = case_when(urvolrem == pocfrz_flowvolremain ~ 0,
                          urvolrem != pocfrz_flowvolremain ~ 1),
        
        qual = case_when(testqual.f == rc.flowresultqual.f ~ 0,
                         testqual.f != rc.flowresultqual.f ~ 1),
        
        control = case_when(controlquant == pocfrz_flowcubecontrol ~ 0,
                            controlquant != pocfrz_flowcubecontrol ~ 1),
        
        quant = case_when(testquant == pocfrz_flowcubetest ~ 0,
                          testquant != pocfrz_flowcubetest ~ 1),
        
)

col_list<-c("urine", "qual", "control", "quant")

flow$sumdisc = rowSums(flow[ , col_list])

table(flow$sumdisc, useNA = 'ifany')

write.csv(flow, "flowdiscrep.csv")



table(flow$testqual.f, flow$rc.flowresultqual.f, useNA = "ifany", deparse.level = 2) # ~ 2 participants with discordant results








#Exporting an R object 
saveRDS(fulldat, "~/Mark's Work/UW/Prof Drain/PROVE-TB/Analysis/VN/WD/fulldat.rds")

#Reading fulldat back in

#fulldat<-readRDS("fulldat.rds")


###################################
#Creating analysis specific data sets

###########
#Analysis for GF Presentation
#September 9th, 2022
#####
#Restricting to PTB participants only 

#Reading fulldat back in

#fulldat<-readRDS("fulldat.rds")

table(fulldat$ptb01, useNA = "ifany")

#Should restrict to 630 participants

gf<-subset(fulldat, fulldat$ptb01==1)


#Removing HIV+ participants

table(gf$keybl_hiv.f, useNA = "ifany")

#Should remove 3 participants, resulting dataset should be n=627

gf<-subset(gf, gf$keybl_hiv.f=="Negative")

#Excluding participant NLH-0027-UR since PTB and only culture specimen available using pleural fluid
gf<-gf[!gf$ptid %in% c('NLH-0027-UR'),]



#####################################
#Additional pre-analytical cleaning
#####
#Checking all variables used in analysis 

skimr::skim(gf)

###Patient demographics

#Gender
table(gf$char_gender.factor, useNA = "ifany")

#Age
summary(gf$char_age, useNA = "ifany")
histogram(gf$char_age)

#Hospitalization Status
table(gf$keybl_inpatient.f, useNA = "ifany")

#Diabetic 
table(gf$labcc_a1cstatus.factor, useNA = "ifany") #Only Done for 68 participants, majority not done
table(gf$labcc_a1cresult, useNA = "ifany") #558 missing
table(gf$labstr_a1cresult, useNA="ifany") #124 missing, use this

histogram(gf$labstr_a1cresult) 

##Symptom presentation


table(gf$labcc_a1cstatus.factor, useNA = "ifany")

#Cough
coughtab<-table(gf$cough_c.f, useNA = "ifany")
prop.table(coughtab)*100

#Fever
(fevertab<-table(gf$feverchill_c.f, useNA = "ifany"))
prop.table(fevertab)*100

#Night Sweats
(nitesweattab<-table(gf$nitesweat_c.f, useNA = "ifany"))
prop.table(nitesweattab)*100

#Weightloss
(wtlosstab<-table(gf$wtloss_c.f, useNA="ifany"))
prop.table(wtlosstab)*100


#Fatigue
(fattab<-table(gf$fatigue_c.f, useNA="ifany"))
prop.table(fattab)*100


##CXR
table(gf$chab_xraystatus.factor, useNA="ifany") #395 participants with CXR status taken
table(gf$chab_xrayresultstatus.factor, useNA="ifany")  #394 with available results

#Xray results
table(gf$chab_xrayresultstatus.factor, useNA="ifany")
table(gf$chab_xrayresult___1.factor, useNA="ifany") #NAD (6)
table(gf$chab_xrayresult___2.factor, useNA="ifany") #TB (91)
table(gf$chab_xrayresult___3.factor, useNA="ifany") #Calcified granuloma (66)
table(gf$chab_xrayresult___4.factor, useNA="ifany") #Bilateral clearing (0)
table(gf$chab_xrayresult___5.factor, useNA="ifany") #Cavitary lesions (41)


#History of TB exposure 
table(gf$int_lvnowtb.factor, useNA="ifany") #40 yes, living with TB now
table(gf$int_lv3motb.factor, useNA="ifany") #5 yes, living with someone with TB within previous 3 months, need to check this



#Smear positivity

(afbtab<-table(gf$keybl_afbcat.f, useNA = "ifany")) #Key variable, 127
prop.table(afbtab)*100

table(gf$labcc_afbstatus.factor, useNA="ifany") #624 tested per clinical care
table(gf$afbval_c.f, useNA="ifany") 


#Microbiologically confirmed

(mrstab<-table(gf$MRSref, useNA ="ifany"))
prop.table(mrstab)*100


#Xpert only
(gxptab<-table(gf$gxpref, useNA ="ifany"))
prop.table(gxptab)*100


#Culture only
#Xpert only
(culttab<-table(gf$cultref, useNA ="ifany"))
prop.table(culttab)*100


#Rif resistant
(riftab<-table(gf$rifdet_c.f, useNA ="ifany"))
prop.table(riftab)*100



#Checking flow results

##RC derived results

#FLow status
table(gf$pocfrz_flowstatus.factor, useNA = "ifany") 

#Flpw result
table(gf$pocfrz_flowresult.factor, useNA = "ifany")
table(gf$pocfrz_flowresultpos.factor, useNA = "ifany")

table(gf$pocfrz_flowresult.factor, gf$pocfrz_flowresultpos.factor, useNA = "ifany")


flowneg1plus<-subset(gf, gf$pocfrz_flowresult.factor=="Negative"  &  gf$pocfrz_flowresultpos.factor=="1+")

#Repeat test
table(gf$pocfrz_flowresultrepeat___1.factor, useNA = "ifany")

#Cubetest
table(gf$pocfrz_flowcubetest, useNA = "ifany")

#Control
table(gf$pocfrz_flowcubecontrol, useNA = "ifany") ##Need to crosscheck with NLH results

#pocfrz_flowvolremain
table(gf$pocfrz_flowvolremain, useNA = "ifany") #13 missing
 


#Comparing discordant between qualitative & quantitative criteria for an invalid result

table(gf$invquant_c.nlh, gf$invqual_c.nlh, useNA='ifany', deparse.level = 2)


                 

#Age
summary(gf$char_age, useNA = "ifany")
histogram(gf$char_age)

#Hospitalization Status
table(gf$keybl_inpatient.f, useNA = "ifany")

#Diabetic 
table(gf$labcc_a1cstatus.factor, useNA = "ifany") #Only Done for 68 participants, majority not done
table(gf$labcc_a1cresult, useNA = "ifany") #558 missing
table(gf$labstr_a1cresult, useNA="ifany") #124 missing, use this

histogram(gf$labstr_a1cresult) #Concerning above 20.8, might need to check if correct (10.5.22, corrected)

##Symptom presentation


table(gf$labcc_a1cstatus.factor, useNA = "ifany")

#Cough
coughtab<-table(gf$cough_c.f, useNA = "ifany")
prop.table(coughtab)*100

#Fever
(fevertab<-table(gf$feverchill_c.f, useNA = "ifany"))
prop.table(fevertab)*100

#Night Sweats
(nitesweattab<-table(gf$nitesweat_c.f, useNA = "ifany"))
prop.table(nitesweattab)*100

#Weightloss
(wtlosstab<-table(gf$wtloss_c.f, useNA="ifany"))
prop.table(wtlosstab)*100


#Fatigue
(fattab<-table(gf$fatigue_c.f, useNA="ifany"))
prop.table(fattab)*100


##CXR
table(gf$chab_xraystatus.factor, useNA="ifany") #396 participants with CXR status taken
table(gf$chab_xrayresultstatus.factor, useNA="ifany")  #395 with available results

#Xray results
table(gf$chab_xrayresultstatus.factor, useNA="ifany")
table(gf$chab_xrayresult___1.factor, useNA="ifany") #NAD (6)
table(gf$chab_xrayresult___2.factor, useNA="ifany") #TB (91)
table(gf$chab_xrayresult___3.factor, useNA="ifany") #Calcified granuloma (66)
table(gf$chab_xrayresult___4.factor, useNA="ifany") #Bilateral clearing (0)
table(gf$chab_xrayresult___5.factor, useNA="ifany") #Cavitary lesions (41)


#History of TB exposure 
table(gf$int_lvnowtb.factor, useNA="ifany") #40 yes, living with TB now
table(gf$int_lv3motb.factor, useNA="ifany") #5 yes, living with someone with TB within previous 3 months, need to check this



#Smear positivity

(afbtab<-table(gf$keybl_afbcat.f, useNA = "ifany")) #Key variable, 127
prop.table(afbtab)*100

table(gf$labcc_afbstatus.factor, useNA="ifany") #624 tested per clinical care
table(gf$afbval_c.f, useNA="ifany") 


#Microbiologically confirmed

(mrstab<-table(gf$MRSref, useNA ="ifany"))
prop.table(mrstab)*100


#Xpert only
(gxptab<-table(gf$gxpref, useNA ="ifany"))
prop.table(gxptab)*100


#Culture only

(culttab<-table(gf$cultref, useNA ="ifany"))
prop.table(culttab)*100


#Rif resistant
(riftab<-table(gf$rifdet_c.f, useNA ="ifany"))
prop.table(riftab)*100



#########################################
#FUNCTIONS **** Courtesy of Meagan Bemer
#########################################

run_epi_tests <- function(df,
                          poc_var,
                          pcr_var){
        
        #' @description takes a data frame with one row per participant, 
        #'   creates a 2x2 table for use with the epi.tests package, and
        #'   passes the table into epiR::epi.test
        #' @param df a data frame containing one row per participant that
        #'   includes their POC test result (intervention test) as 
        #'   well as their PCR test result (reference test), where each test
        #'   result is coded as 0 = negative and 1 = positive
        #' @param poc_var the variable name in the data frame referenced above
        #'   that contains the point-of-care test results
        #' @param pcr_var the variable name in the data frame referenced above
        #'   that contains the PCR test results
        #' @returns a list of output from epiR::epi.test
        
        # Make two by two table
        # ---------------------
        # Both negative
        n_test_neg_gold_neg  <- nrow(df %>%
                                             filter({{poc_var}} == 0 &
                                                            {{pcr_var}} == 0))
        
        # Both positive
        n_test_pos_gold_pos  <- nrow(df %>%
                                             filter({{poc_var}} == 1 &
                                                            {{pcr_var}} == 1))
        
        # Test negative, gold-standard positive
        n_test_neg_gold_pos  <- nrow(df %>%
                                             filter({{poc_var}} == 0 &
                                                            {{pcr_var}} == 1))
        
        # Test positive, gold-standard negative
        n_test_pos_gold_neg  <- nrow(df %>%
                                             filter({{poc_var}} == 1 &
                                                            {{pcr_var}} == 0))
        
        # Make into a two-by-two table
        two_by_two <- as.table(matrix(c(n_test_pos_gold_pos,
                                        n_test_pos_gold_neg,
                                        n_test_neg_gold_pos,
                                        n_test_neg_gold_neg),
                                      nrow = 2,
                                      byrow = TRUE))
        
        colnames(two_by_two) <- c("GS+","GS-")
        rownames(two_by_two) <- c("Test+","Test-")
        
        epi_out <- epiR::epi.tests(two_by_two, method="clopper-pearson")
        
        return(epi_out)
        
}


make_ppa_npa_df <- function(epi_out_list){
        
        #' @description makes a data frame that's formatted to match the
        #'   shell tables for sensitivity/PPA and specificity/NPA calculations
        #'   plus their CIs
        #'  
        #' @param epi_out_list the output from passing a 2x2 table into epi.tests
        #' 
        #' @returns a tibble describing PPA and NPA along with CIs in the format
        #'   of the shell table proposed by Paul and Ina, formatted for output
        #'   to Excel
        #' 
        
        # Strip all spaces from the names in the epi.tests table
        names(epi_out_list$tab) <- str_replace_all(names(epi_out_list$tab), " ", "")
        
        ppa_npa_df <- tibble("N" = epi_out_list$tab$Total[3],
                             # Use a leading space so Excel doesn't try to 
                             # convert these into dates when the numbers are small
                             "TP/TP+FN" = paste0(" ",
                                                 epi_out_list$tab$"Outcome+"[1],
                                                 "/",
                                                 epi_out_list$tab$"Outcome+"[3]),
                             "PPA (95% CI)" = paste0(sprintf("%.1f", 
                                                             round(epi_out_list$detail$se$est * 100,
                                                                   digits = 1)),
                                                     " (",
                                                     sprintf("%.1f",
                                                             round(epi_out_list$detail$se$lower * 100,
                                                                   digits = 1)),
                                                     ", ",
                                                     sprintf("%.1f",
                                                             round(epi_out_list$detail$se$upper * 100,
                                                                   digits = 1)),
                                                     ")"),
                             # Use a leading space so Excel doesn't try to 
                             # convert these into dates when the numbers are small
                             "TN/TN+FP" = paste0(" ",
                                                 epi_out_list$tab$"Outcome-"[2],
                                                 "/",
                                                 epi_out_list$tab$"Outcome-"[3]),
                             "NPA (95% CI)" = paste0(sprintf("%.1f", 
                                                             round(epi_out_list$detail$sp$est * 100,
                                                                   digits = 1)),
                                                     " (",
                                                     sprintf("%.1f",
                                                             round(epi_out_list$detail$sp$lower * 100,
                                                                   digits = 1)),
                                                     ", ",
                                                     sprintf("%.1f",
                                                             round(epi_out_list$detail$sp$upper * 100,
                                                                   digits = 1)),
                                                     ")"))
        
        return(ppa_npa_df)
        
}








###################
#Analytical Tables
###################

#Creating specific analytical dataset keeping only key criteria used

#Dropping variable

write.csv(gf, "ad.csv")

ad<-gf


####8.19.21 - USING NLH DATA FOR GF PRESENTATION, issues with data cleaning and discrepancies 

####10.5.14 - Changing source from NLH dataset to REDCap now that cleaning has occurred
####In order to preserve code, now creating new variables with variable name previously used 
####but sourced to REDCap version for:
#Quantitative Result: testqual_c = rc.flowresultqual
#Cube reader result: testquant = pocfrz_flowcubetest
#Control value:    controlquant = pocfrz_flowcubecontrol
#Urine volume:     urvolrem = pocfrz_flowvolremain

ad<- ad %>% mutate(
        testqual_c = rc.flowresultqual,
        testqual.f = rc.flowresultqual.f,
        testquant = pocfrz_flowcubetest,
        controlquant = pocfrz_flowcubecontrol,
        urvolrem = pocfrz_flowvolremain
)



#Checking variable creation
table(ad$testqual_c, ad$rc.flowresultqual, useNA = "ifany")
table(ad$testqual.f, ad$rc.flowresultqual.f, useNA = "ifany")
table(ad$testquant, ad$pocfrz_flowcubetest, useNA = "ifany")
plot(ad$testquant, ad$pocfrz_flowcubetest)
plot(ad$controlquant, ad$pocfrz_flowcubecontrol)
plot(ad$urvolrem, ad$pocfrz_flowvolremain)



#Qualitative Threshold Creation - qualitative result

ad <- ad %>% mutate(
        
        qual1pos.rc = case_when(testqual_c == 0 ~ 0,
                                 testqual_c > 0 ~ 1),
        
        qual2pos.rc = case_when(testqual_c < 2 ~ 0,
                                 testqual_c >=  2 ~ 1),
        
        
        qual3pos.rc = case_when(testqual_c < 3 ~ 0,
                                 testqual_c >= 3 ~ 1),
        
        qual4pos.rc = case_when(testqual_c < 4 ~ 0,
                                 testqual_c >= 4 ~ 1),
        
        qual5pos.rc = case_when(testqual_c < 5 ~ 0,
                                 testqual_c >= 5 ~ 1),
        
        qual6pos.rc = case_when(testqual_c < 6 ~ 0,
                                 testqual_c >= 6 ~ 1),
        
        qual7pos.rc = case_when(testqual_c < 7 ~ 0,
                                 testqual_c >= 7 ~ 1),
        
        qual8pos.rc = case_when(testqual_c <  8 ~ 0,
                                 testqual_c >= 8 ~ 1),
        
        qual9pos.rc = case_when(testqual_c < 9 ~ 0,
                                 testqual_c >= 9 ~ 1),
        
        lowcont275.rc = case_when(controlquant<275 ~ 1,
                                   controlquant>=275 ~ 0),
        
        cont230275.rc = case_when(controlquant<=275 & controlquant>=230  ~ 1,
                                    TRUE~0)
)

table(ad$testqual.f ,ad$qual1pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual2pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual3pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual4pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual5pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual6pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual7pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual8pos.rc, useNA = "ifany" )
table(ad$testqual.f ,ad$qual9pos.rc, useNA = "ifany" )


#Low control

table(ad$controlquant, ad$lowcont275.rc, useNA = "ifany")
table(ad$controlquant, ad$cont230275.rc, useNA = "ifany")

######Threshold testing with decile approach
quantile(ad$testquant, probs=seq(0.1, 0.9, by=0.1))
#10%  20%  30%  40%  50%  60%  70%  80%  90% 
#8.5  10.0 11.0 12.0 13.5 15.0 17.0 19.0 25.0 


ad <- ad %>% mutate(
        
        quant0pos.rc = case_when(testquant == 0 ~ 0,
                                  testquant > 0 ~ 1),
        
        quant10dpos.rc = case_when(testquant < 8.5 ~ 0,
                                  testquant >= 8.5 ~ 1),
        
        quant20dpos.rc = case_when(testquant < 10.0 ~ 0,
                                   testquant >= 10.0 ~ 1),

        quant30dpos.rc = case_when(testquant < 11.0 ~ 0,
                                   testquant >= 11.0 ~ 1),
       
        quant40dpos.rc = case_when(testquant < 12.0 ~ 0,
                                   testquant >= 12.0 ~ 1),
        
        quant50dpos.rc = case_when(testquant < 14.0 ~ 0,
                                   testquant >= 14.0 ~ 1),
        
        quant60dpos.rc = case_when(testquant < 15 ~ 0,
                                   testquant >= 15 ~ 1),
        
        quant70dpos.rc = case_when(testquant < 17 ~ 0,
                                    testquant >= 17 ~ 1),
        
        quant80dpos.rc = case_when(testquant < 19 ~ 0,
                                    testquant >= 19 ~ 1),
        
        quant90dpos.rc = case_when(testquant < 25 ~ 0,
                                    testquant >= 25 ~ 1))

# Checking variable creation

table( ad$testquant, ad$quant0pos.rc, useNA = "ifany")
table(ad$testquant, ad$quant10dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant20dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant30dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant40dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant50dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant60dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant70dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant80dpos.rc, useNA = "ifany")
table( ad$testquant, ad$quant90dpos.rc, useNA = "ifany")



#Creating separate object for different calculations

#Use for qualitative evaluation
adqual<-subset(ad, ad$invqual_c.rc == 0)

#Use for culture reference & qualitative
adqualcult<-subset(adqual, (keybl_cultcat.f=="PTB- Positive") | (keybl_cultcat.f=="PTB- Negative"))


###############################
# Table 1 Patient demographics
###############################

tab1<- table1(~char_gender.factor+
                      char_age+
                      keybl_inpatient.f+
                      a1ccat_c.f+
                      cough_c.f+
                      feverchill_c.f+
                      nitesweat_c.f+
                      wtloss_c.f+
                      fatigue_c.f+
                      chab_xrayresult___2.factor+
                      int_lvnowtb.factor+
                      keybl_afbcat.f+
                      afbval_c.f+
                      keybl_gxpcat.f+
                      rifdet_c.f +
                      keybl_cultcat.f + 
                      as.factor(MRSref) +
                      as.factor(gxpref) +
                      as.factor(cultref)+
                      as.factor(MSDref.FA)+
                      as.factor(MSDref.OA)+
                      as.factor(MSDref.X5)+
                      testqual.f+
                      as.factor(invqual_c.rc)+
                      as.factor(invquant_c.rc)
                      
             
                      
                                        ,   data=ad, overall="overall") 

tab1




#####Distribution of results
############################


hist(ad$testquant,
     main = "Distribution of Cube Reader Results",
     xlab="Cube Reader Value"
     
)

summary(ad$testquant)


hist(ad$controlquant,
     main = "Distribution of Cube Reader Control Values",
     xlab="Control Value",
     xlim=c(0,400)
)

summary(ad$controlquant)




hist(ad$testquant,
     main = "Overall",
     xlab="Cube Reader Value"
     
)


hist(ad$controlquant,
     main = "Distribution of Cube Reader Control Values",
     xlab="Control Value",
     xlim=c(0,400)
)

hist(ad$controlquant,
     main = "Overall",
     xlab="Control Value",
     xlim=c(0,400)
)


#Looking at distribution of results for 1+ (visual assessment) participants only

flow1plus<-subset(ad, ad$qual1pos.rc==1)


hist(flow1plus$testquant,
     main = "1+ Participants Only",
     xlab="Cube Reader Value"
     
)

summary(flow1plus$testquant)

######################################################
#Correlation plot between visual & cube reader results
######################################################
plot(adqual$testqual_c, adqual$testquant, xlab = "Visual Assessment Score", ylab="Cube reader result")
abline(lm(adqual$testquant~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$testquant, method = "spearman"), 2)), x = 2, y = 230)


plot(adqual$testqual_c, log(adqual$testquant), xlab = "Visual Assessment Score", ylab="log(Cube reader result)")
abline(lm(log(adqual$testquant)~adqual$testqual_c), col="red", lwd=3)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, log(adqual$testquant)), 2)), x = 2, y = 5)


##############
#Table 2- Comparison to various reference 
#standards (Using qualitative result) 
##############

###MRS 
(tab.1qualposMRS<- run_epi_tests(adqual, qual1pos.rc, MRSref))
(make_ppa_npa_df(tab.1qualposMRS))


#gxp
(tab.1qualposgxp<- run_epi_tests(adqual, qual1pos.rc, gxpref))
(make_ppa_npa_df(tab.1qualposgxp))

#cult
#gxp
(tab.1qualposcult<- run_epi_tests(adqualcult, qual1pos.rc, cultref))
(make_ppa_npa_df(tab.1qualposcult))

#10.5.22 - Adding MSD standards

#MSD-FIND28/A194
(tab.1qualposMSDFA<-run_epi_tests(adqual, qual1pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.1qualposMSDFA))

#MSD-S420/A194
(tab.1qualposMSDOA<-run_epi_tests(adqual, qual1pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.1qualposMSDOA))

#MSD-BJ78/SE3
(tab.1qualposMSDX5<-run_epi_tests(adqual, qual1pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.1qualposMSDX5))

######################################################
#Table 3- Varying qualitative threshold, MRS standard
######################################################

###MRS 
#2+
 
(tab.2qualposMRS<- run_epi_tests(adqual, qual2pos.rc, MRSref))
(make_ppa_npa_df(tab.2qualposMRS))


#3+
(tab.3qualposMRS<- run_epi_tests(adqual, qual3pos.rc, MRSref))
(make_ppa_npa_df(tab.3qualposMRS))


#4+
(tab.4qualposMRS<- run_epi_tests(adqual, qual4pos.rc, MRSref))
(make_ppa_npa_df(tab.4qualposMRS))

#5+
(tab.5qualposMRS<- run_epi_tests(adqual, qual5pos.rc, MRSref))
(make_ppa_npa_df(tab.5qualposMRS))

#6+
(tab.6qualposMRS<- run_epi_tests(adqual, qual6pos.rc, MRSref))
(make_ppa_npa_df(tab.6qualposMRS))

#7+
(tab.7qualposMRS<- run_epi_tests(adqual, qual7pos.rc, MRSref))
(make_ppa_npa_df(tab.7qualposMRS))

#8+
(tab.8qualposMRS<- run_epi_tests(adqual, qual8pos.rc, MRSref))
(make_ppa_npa_df(tab.8qualposMRS))

#9+
(tab.9qualposMRS<- run_epi_tests(adqual, qual9pos.rc, MRSref))
(make_ppa_npa_df(tab.9qualposMRS))



#############################################################################
#Table 4- Varying quantitative threshold, MRS standard, using decile cutoffs
#############################################################################

######Threshold testing with decile approach
quantile(ad$testquant, probs=seq(0.1, 0.9, by=0.1))
#10%  20%  30%  40%  50%  60%  70%  80%  90% 
#8.5  10.0 11.0 12.0 14.0 15.0 17.0 19.0 25.0 

###MRS 

#0

(tab.0quanposMRS<- run_epi_tests(ad, quant0pos.rc, MRSref))
(make_ppa_npa_df(tab.0quanposMRS))

#>=8.5 (10th perc.)
(tab.quan10dposMRS<- run_epi_tests(ad, quant10dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan10dposMRS))


#>=10 (20th perc.)
(tab.quan20dposMRS<- run_epi_tests(ad, quant20dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan20dposMRS))

#>=11 (30th perc.)
(tab.quan30dposMRS<- run_epi_tests(ad, quant30dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan30dposMRS))

#>=12 (40th perc.)
(tab.quan40dposMRS<- run_epi_tests(ad, quant40dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan40dposMRS))

#>=14.0 (50th perc.)
(tab.quan50dposMRS<- run_epi_tests(ad, quant50dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan50dposMRS))

#>=15.0 (60th perc.)
(tab.quan60dposMRS<- run_epi_tests(ad, quant60dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan60dposMRS))

#>=17.0 (70th perc.)
(tab.quan70dposMRS<- run_epi_tests(ad, quant70dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan70dposMRS))

#>=19.0 (80th perc.)
(tab.quan80dposMRS<- run_epi_tests(ad, quant80dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan80dposMRS))

#>=25.0 (90th perc.)
(tab.quan90dposMRS<- run_epi_tests(ad, quant90dpos.rc, MRSref))
(make_ppa_npa_df(tab.quan90dposMRS))



###############################################################
#Table 5 -Comparison of smear positivity with visual assessment
###############################################################

table(ad$keybl_afbcat.f, useNA = "ifany")

#subsetting to participants with valid AFB grade
afb<-subset(ad, keybl_afbcat.f != "Pending")
table(afb$keybl_afbcat.f, useNA = "ifany")




#Checking grade

table(afb$afbval_c.f)

#Making AFB positive object
afbpos<-subset(afb, afbval_c > 0)

#Making AFB negative object
afbneg<-subset(afb, afbval_c == 0)




#####Qualitative assessment - NLH Data again
#Remove invalid qualitative tests

afbposqual<-subset(afbpos, afbpos$invqual_c.rc == 0) #Removes 5 participants
afbnegqual<-subset(afbneg, afbneg$invqual_c.rc == 0) #Removes 12 participants



table(afbposqual$qual1pos.rc, afbposqual$MRSref, useNA = "ifany")

#Diagnostic parameters - AFB positive

(tab.afbposqualMRS<- run_epi_tests(afbposqual, qual1pos.rc, MRSref))
(make_ppa_npa_df(tab.afbposqualMRS))

#Diagnostic parameters - AFB negative

(tab.afbnegqualMRS<- run_epi_tests(afbnegqual, qual1pos.rc, MRSref))
(make_ppa_npa_df(tab.afbnegqualMRS))

afbposquant_roc<-afbpos %>% dplyr::select(ptid, testquant, controlquant, urvolrem, MRSref, testqual_c,lowcont275.rc, cont230275.rc, highvol_c.rc)
afbnegquant_roc<-afbneg %>% dplyr::select(ptid, testquant, controlquant, urvolrem, MRSref, testqual_c,lowcont275.rc, cont230275.rc, highvol_c.rc)



afb_roc<-roc(afbposquant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4) 
afb_roc<-roc(afbnegquant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#4daf4a", lwd=4, add=TRUE, print.auc.y=0.5)
title(main="Cube Reader - AFB Smear Positivity", line = +1)
legend(0.5,0.2,legend=c("Smear Positive", "Smear Negative"), col=c("#377eb8", "#4daf4a"), lty=1, cex=0.8, bty="n")


######################################################
#Quantitative analysis using ROC & Threshold testing
######################################################

#Subsetting to smaller dataset 

#Quantitative
quant_roc<- ad %>% dplyr::select(ptid, testquant, controlquant, urvolrem, MRSref, testqual_c, lowcont275.rc, cont230275.rc, highvol_c.rc)

quant_roc<-quant_roc[order(quant_roc$testquant),]

#Qualitative
qual_roc<-adqual %>% dplyr::select(ptid, testquant, controlquant, urvolrem, MRSref, testqual_c,lowcont275.rc, cont230275.rc, highvol_c.rc)

#Overall
plot(x=quant_roc$testquant, y=quant_roc$MRSref)

par(pty="s")

rocquantov<-roc(quant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, auc.polygon=TRUE) 
title(main="Cube Reader Result", line = -0.005)

coords(rocquantov, x="best", best.method="youden", ret="all")

rocquantov

rocqualov<-roc(qual_roc, MRSref, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, auc.polygon=TRUE)
title(main="Visual Assessment", line = -0.005)

coords(rocqualov, x="best", best.method="youden", ret="all")


#Low control group
lowquant_roc<-subset(quant_roc, lowcont275.rc==1)

rocquant_locont<-roc(lowquant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, auc.polygon=TRUE) 
title(main="Cube Reader Result ROC - Control <275", line = -0.005)


highquant_roc<-subset(quant_roc, lowcont275.rc==0)

rocquant_hicont<-roc(highquant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, auc.polygon=TRUE) 
title(main="Cube Reader Result ROC - Control >=275", line = -0.005)

#Overlaying ROCs
control_roc<-roc(lowquant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4) 
control_roc<-roc(highquant_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#4daf4a", lwd=4, add=TRUE, print.auc.y=0.5)
title(main="Cube Reader - Control Values", line = +1)
legend(0.3,0.2,legend=c(">=275", "<275"), col=c("#4daf4a", "#377eb8"), lty=1, cex=0.8, bty="n")

#High Volume remaining group

highvol_roc<-subset(quant_roc, highvol_c.rc==1)

rocquant_hivol<-roc(highvol_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, auc.polygon=TRUE) 

title(main="Cube Reader Result ROC - Urine volume remaining >5mL", line = +1)


lowvol_roc<-subset(quant_roc, highvol_c.rc==0)

rocquant_lovol<-roc(lowvol_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, auc.polygon=TRUE) 
title(main="Cube Reader Result ROC - Urine volume remaining <=5mL", line = +1)

#Overlaying ROCs
roc_vol<-roc(highvol_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)
roc_vol<-roc(lowvol_roc, MRSref, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#4daf4a", lwd=4, add=TRUE, print.auc.y=0.5)
title(main="Cube Reader - Urine volume remaining", line = +1)
legend(0.3,0.2,legend=c("<=5mL", ">5mL"), col=c("#4daf4a", "#377eb8"), lty=1, cex=0.8, bty="n")


###############################
#Checking Kathy vs my mismatch
#
cultmismatch<-ad[ad$ptid %in% c(
        'NLH-0107-UR',
        'NLH-0152-UR',
        'NLH-0177-UR',
        'NLH-0180-UR',
        'NLH-0183-UR',
        'NLH-0193-UR',
        'NLH-0201-UR',
        'NLH-0205-UR',
        'NLH-0207-UR',
        'NLH-0211-UR',
        'NLH-0216-UR',
        'NLH-0217-UR',
        'NLH-0242-UR',
        'NLH-0244-UR',
        'NLH-0246-UR',
        'NLH-0407-UR'
        
        
        
),]


######################
#MSD Specific analyses
######################

#Creating an object for MSD positive participants only
OApos<-subset(ad, MSDref.OA==1)
FApos<-subset(ad, MSDref.FA==1)
X5pos<-subset(ad, MSDref.X5==1)
#Distribution of results


#FA

hist(ad$FA.conc,
     main = "Distribution of LAM concentrations (FIND28/A194)",
     xlab="Estimated LAM (pg/mL)"
     
)


hist(ad$logFA.conc,
     main = "Distribution of LAM concentrations (FIND 28:A194-01) - Overall",
     xlab="Log(Estimated LAM) (pg/mL)"
     
)

summary(ad$FA.conc)

#FAPOS only
hist(FApos$logFA.conc,
     main = "Distribution of LAM concentrations (FIND 28:A194-01) - MSD Positive",
     xlab="Log(Estimated LAM) (pg/mL)"
     
)

summary(FApos$FA.conc)



#OA

hist(ad$OA.conc,
     main = "Distribution of LAM concentrations (S4-20/A194-01) - Overall",
     xlab="Estimated LAM (pg/mL)"
     
)


hist(ad$logOA.conc,
     main = "Distribution of LAM concentrations (S4-20:A194-01) - Overall",
     xlab="Log(Estimated LAM) (pg/mL)"
     
)

summary(ad$OA.conc)

#Pos only
hist(OApos$logOA.conc,
     main = "Distribution of LAM concentrations (S4-20:A194-01) - MSD Positive",
     xlab="Log(Estimated LAM) (pg/mL)"
     
)

summary(OApos$OA.conc)




#X5

hist(ad$X5.conc,
     main = "Distribution of LAM concentrations (5E3:BJ-76) - Overall",
     xlab="Estimated LAM (pg/mL)"
     
)


hist(ad$logX5.conc,
     main = "Distribution of LAM concentrations (5E3:BJ-76) - Overall",
     xlab="Log(Estimated LAM) (pg/mL)"
     
)

summary(ad$X5.conc)

#Pos only
hist(X5pos$X5.conc,
     main = "Distribution of LAM concentrations (5E3:BJ-76) - MSD Positive",
     xlab="Estimated LAM) (pg/mL)"
     
)

summary(X5pos$X5.conc)


#Comparison of distribution of results

#Plotting variables against each other

pairs(~FA.conc + OA.conc + X5.conc, data=ad)

pairs(~logOA.conc + logFA.conc +logX5.conc, data=ad)



#############################################
#Correlation between MSD & FLOW-TB results

#S420/A194 - OA


#Qualitative
plot(adqual$testqual_c, adqual$OA.conc, xlab = "Visual Assessment Score", ylab="MSD LAM Concentration (pg/mL)", main = "S420:A194 - Visual Assessment")
abline(lm(adqual$OA.conc~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$OA.conc, method = "spearman"), 2)), x = 6, y = 20000)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, adqual$OA.conc, method = "pearson"), 2)), x = 6, y = 15000)


#Qualitative - log transformed
#Qualitative
plot(adqual$testqual_c, adqual$logOA.conc, xlab = "Visual Assessment Score", ylab="log(MSD LAM Concentration), (pg/mL)", main = "S4-20:A194-01 vs Visual Assessment")
abline(lm(adqual$logOA.conc~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$logOA.conc, method = "spearman"), 2)), x = 6, y = 2)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, adqual$logOA.conc, method = "pearson"), 2)), x = 6, y = 1)


#Quantitative 
plot(ad$testquant, ad$OA.conc, xlab = "Cube Reader Score", ylab="MSD LAM Concentration (pg/mL)", main = "S420:A194 - Cube Reader")
abline(lm(ad$OA.conc~ad$testquant), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(ad$testquant, ad$OA.conc, method = "spearman"), 2)), x = 200, y = 20000)
text(paste("Pearson Correlation:", round(cor(ad$testquant, ad$OA.conc, method = "pearson"), 2)), x = 200, y = 10000)

#Quantitative - log transformed MSD
plot(ad$testquant, ad$logOA.conc, xlab = "Cube Reader Score", ylab="log(MSD LAM Concentration) (pg/mL)", main = "S420:A194 -  Cube Reader")
abline(lm(ad$logOA.conc~ad$testquant), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(ad$testquant, ad$logOA.conc, method = "spearman"), 2)), x = 200, y = 2)
text(paste("Pearson Correlation:", round(cor(ad$testquant, ad$logOA.conc, method = "pearson"), 2)), x = 200, y = 1)

#Quantitative - log transformed both
plot(log(ad$testquant), ad$logOA.conc, xlab = "log(Cube Reader Score)", ylab="log(MSD LAM Concentration) (pg/mL)", main = "S4-20:A194-01 vs Cube Reader Result")
abline(lm(ad$logOA.conc~log(ad$testquant)), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(log(ad$testquant), ad$logOA.conc, method = "spearman"), 2)), x = 3, y = 10)
text(paste("Pearson Correlation:", round(cor(log(ad$testquant), ad$logOA.conc, method = "pearson"), 2)), x = 3, y = 9)








##FA
#FIND28/A194 - FA


#Qualitative
plot(adqual$testqual_c, adqual$FA.conc, xlab = "Visual Assessment Score", ylab="MSD LAM Concentration (pg/mL)", main = "FIND28:A194-01 - Visual Assessment")
abline(lm(adqual$FA.conc~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$FA.conc, method = "spearman"), 2)), x = 6, y = 400000)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, adqual$FA.conc, method = "pearson"), 2)), x = 6, y = 300000)


#Qualitative - log transformed
#Qualitative
plot(adqual$testqual_c, adqual$logFA.conc, xlab = "Visual Assessment Score", ylab="log(MSD LAM Concentration), (pg/mL)", main = "FIND28:A194-01 vs Visual Assessment")
abline(lm(adqual$logFA.conc~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$logFA.conc, method = "spearman"), 2)), x = 6, y = 2)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, adqual$logFA.conc, method = "pearson"), 2)), x = 6, y = 1)


#Quantitative 
plot(ad$testquant, ad$FA.conc, xlab = "Cube Reader Score", ylab="MSD LAM Concentration (pg/mL)", main = "FIND28:A194-01 - Cube Reader")
abline(lm(ad$FA.conc~ad$testquant), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(ad$testquant, ad$FA.conc, method = "spearman"), 2)), x = 200, y = 200000)
text(paste("Pearson Correlation:", round(cor(ad$testquant, ad$FA.conc, method = "pearson"), 2)), x = 200, y = 100000)

#Quantitative - log transformed MSD
plot(ad$testquant, ad$logFA.conc, xlab = "Cube Reader Score", ylab="log(MSD LAM Concentration) (pg/mL)", main = "FIND28:A194-01 - Cube Reader")
abline(lm(ad$logFA.conc~ad$testquant), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(ad$testquant, ad$logFA.conc, method = "spearman"), 2)), x = 200, y = 2)
text(paste("Pearson Correlation:", round(cor(ad$testquant, ad$logFA.conc, method = "pearson"), 2)), x = 200, y = 1)

#Quantitative - log transformed both
plot(log(ad$testquant), ad$logFA.conc, xlab = "log(Cube Reader Score)", ylab="log(MSD LAM Concentration) (pg/mL)", main = "FIND28:A194-01 vs Cube Reader Result")
abline(lm(ad$logFA.conc~log(ad$testquant)), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(log(ad$testquant), ad$logFA.conc, method = "spearman"), 2)), x = 3, y = 10)
text(paste("Pearson Correlation:", round(cor(log(ad$testquant), ad$logFA.conc, method = "pearson"), 2)), x = 3, y = 9)




##X5
#SE3:BJ76


#Qualitative
plot(adqual$testqual_c, adqual$X5.conc, xlab = "Visual Assessment Score", ylab="MSD LAM Concentration (pg/mL)", main = "SE3:BJ76 - Visual Assessment")
abline(lm(adqual$X5.conc~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$X5.conc, method = "spearman", use="complete.obs"), 2)), x = 6, y = 20)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, adqual$X5.conc, method = "pearson", use="complete.obs"), 2)), x = 6, y = 10)


#Qualitative - log transformed
#Qualitative
plot(adqual$testqual_c, adqual$logX5.conc, xlab = "Visual Assessment Score", ylab="log(MSD LAM Concentration), (pg/mL)", main = "5E3:BJ-76 vs Visual Assessment")
abline(lm(adqual$logX5.conc~adqual$testqual_c), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(adqual$testqual_c, adqual$logX5.conc, method = "spearman", use="complete.obs"), 2)), x = 6, y = 1.3)
text(paste("Pearson Correlation:", round(cor(adqual$testqual_c, adqual$logX5.conc, method = "pearson", use="complete.obs"), 2)), x = 6, y = 1)


#Quantitative 
plot(ad$testquant, ad$X5.conc, xlab = "Cube Reader Score", ylab="MSD LAM Concentration (pg/mL)", main = "5E3:BJ76 - Cube Reader")
abline(lm(ad$X5.conc~ad$testquant), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(ad$testquant, ad$X5.conc, method = "spearman", use="complete.obs"), 2)), x = 200, y = 40)
text(paste("Pearson Correlation:", round(cor(ad$testquant, ad$X5.conc, method = "pearson", use="complete.obs"), 2)), x = 200, y = 30)

#Quantitative - log transformed MSD
plot(ad$testquant, ad$logX5.conc, xlab = "Cube Reader Score", ylab="log(MSD LAM Concentration) (pg/mL)", main = "5E3:BJ76 - Cube Reader")
abline(lm(ad$logX5.conc~ad$testquant), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(ad$testquant, ad$logX5.conc, method = "spearman", use="complete.obs"), 2)), x = 200, y = 2)
text(paste("Pearson Correlation:", round(cor(ad$testquant, ad$logX5.conc, method = "pearson", use="complete.obs"), 2)), x = 200, y = 1)

#Quantitative - log transformed both
plot(log(ad$testquant), ad$logX5.conc, xlab = "log(Cube Reader Score)", ylab="log(MSD LAM Concentration) (pg/mL)", main = "5E3:BJ-76 vs Cube Reader Result")
abline(lm(ad$logX5.conc~log(ad$testquant)), col="red", lwd=3)
text(paste("Spearman Correlation:", round(cor(log(ad$testquant), ad$logX5.conc, method = "spearman", use="complete.obs"), 2)), x = 3, y = 3.5)
text(paste("Pearson Correlation:", round(cor(log(ad$testquant), ad$logX5.conc, method = "pearson", use="complete.obs"), 2)), x = 3, y = 3)



##########################################
#Tables - Varying qualitative threshold, MSD standards
##########################################


######################################################
#Table 7 - Varying qualitative threshold, S420 MSD standards
######################################################

###OA-MSD
#2+

(tab.2qualposOA<- run_epi_tests(adqual, qual2pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.2qualposOA))


#3+
(tab.3qualposOA<- run_epi_tests(adqual, qual3pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.3qualposOA))


#4+
(tab.4qualposOA<- run_epi_tests(adqual, qual4pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.4qualposOA))

#5+
(tab.5qualposOA<- run_epi_tests(adqual, qual5pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.5qualposOA))

#6+
(tab.6qualposOA<- run_epi_tests(adqual, qual6pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.6qualposOA))

#7+
(tab.7qualposOA<- run_epi_tests(adqual, qual7pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.7qualposOA))

#8+
(tab.8qualposOA<- run_epi_tests(adqual, qual8pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.8qualposOA))

#9+
(tab.9qualposMRS<- run_epi_tests(adqual, qual9pos.rc, MSDref.OA))
(make_ppa_npa_df(tab.9qualposOA))



######################################################
#Table 8 - Varying qualitative threshold, FINID28 MSD standards
######################################################

###FA-MSD
#2+

(tab.2qualposFA<- run_epi_tests(adqual, qual2pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.2qualposFA))


#3+
(tab.3qualposFA<- run_epi_tests(adqual, qual3pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.3qualposFA))


#4+
(tab.4qualposFA<- run_epi_tests(adqual, qual4pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.4qualposFA))

#5+
(tab.5qualposFA<- run_epi_tests(adqual, qual5pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.5qualposFA))

#6+
(tab.6qualposFA<- run_epi_tests(adqual, qual6pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.6qualposFA))

#7+
(tab.7qualposFA<- run_epi_tests(adqual, qual7pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.7qualposFA))

#8+
(tab.8qualposFA<- run_epi_tests(adqual, qual8pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.8qualposFA))

#9+
(tab.9qualposFA<- run_epi_tests(adqual, qual9pos.rc, MSDref.FA))
(make_ppa_npa_df(tab.9qualposFA))



######################################################
#Table 9 - Varying qualitative threshold, SE3:BJ78 MSD standards
######################################################

###FA-MSD
#2+

(tab.2qualposX5<- run_epi_tests(adqual, qual2pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.2qualposX5))


#3+
(tab.3qualposX5<- run_epi_tests(adqual, qual3pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.3qualposX5))


#4+
(tab.4qualposX5<- run_epi_tests(adqual, qual4pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.4qualposX5))

#5+
(tab.5qualposX5<- run_epi_tests(adqual, qual5pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.5qualposX5))

#6+
(tab.6qualposX5<- run_epi_tests(adqual, qual6pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.6qualposX5))

#7+
(tab.7qualposX5<- run_epi_tests(adqual, qual7pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.7qualposX5))

#8+
(tab.8qualposX5<- run_epi_tests(adqual, qual8pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.8qualposX5))

#9+
(tab.9qualposX5<- run_epi_tests(adqual, qual9pos.rc, MSDref.X5))
(make_ppa_npa_df(tab.9qualposX5))






###########################
#ROC comparison - MSD Data
###########################

## Qualitative

#Qualitative - OA
msdOA_qual_roc<-roc(adqual, MSDref.OA, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)

#Qualitative - FA
msdFA_qual_roc<-roc(adqual, MSDref.FA, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)

#Qualitative - X5
msdX5_qual_roc<-roc(adqual, MSDref.X5, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)



#Combined
msdOA_qual_roc<-roc(adqual, MSDref.OA, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=2, print.auc.x=0.7, print.auc.y=0.8)
msdFA_qual_roc<-roc(adqual, MSDref.FA, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, add=TRUE, col="#4daf4a", lwd=2, print.auc.x=0.7, print.auc.y=0.35)
msdX5_qual_roc<-roc(adqual, MSDref.X5, testqual_c, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, add=TRUE, col="#FF3633", lwd=2, print.auc.x=0.7, print.auc.y=0.95)
title(main="Visual Assessment", line = +3)
legend(0.4,0.4,legend=c("S4-20:A194-01", "FIND 28:A194-01", "5E3:BJ-76"), col=c("#377eb8", "#4daf4a", "#FF3633"), lwd=2, lty=1, cex=0.8, bty="n", title="MSD Ab Pair")

#Calculating youden's index for each
coords(msdOA_qual_roc, x="best", best.method="youden", ret="all")
coords(msdFA_qual_roc, x="best", best.method="youden", ret="all")
coords(msdX5_qual_roc, x="best", best.method="youden", ret="all")


##Quantitative

#quantitative - OA
msdOA_quant_roc<-roc(ad, MSDref.OA, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)

#Quantitative - FA
msdFA_quant_roc<-roc(ad, MSDref.FA, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)

#Quantitative - X5
msdX5_quant_roc<-roc(ad, MSDref.X5, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=4, print.auc.y=0.4)

#Combined
msdOA_quant_roc<-roc(ad, MSDref.OA, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=2, print.auc.x=0.7, print.auc.y=0.8)
msdFA_quant_roc<-roc(ad, MSDref.FA, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, add=TRUE, col="#4daf4a", lwd=2, print.auc.x=0.7, print.auc.y=0.35)
msdX5_quant_roc<-roc(ad, MSDref.X5, testquant, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, add=TRUE, col="#FF3633", lwd=2, print.auc.x=0.7, print.auc.y=0.95)
title(main="Cube Reader", line = +3)
legend(0.4,0.4,legend=c("S4-20:A194-01", "FIND28:A194-01", "5E3:BJ-76"), col=c("#377eb8", "#4daf4a", "#FF3633"), lwd=2, lty=1, cex=0.8, bty="n", title="MSD Ab Pair")

coords(msdOA_quant_roc, x="best", best.method="youden", ret="all")
coords(msdFA_quant_roc, x="best", best.method="youden", ret="all")
coords(msdX5_quant_roc, x="best", best.method="youden", ret="all")











####################################
#MSD estimated concentrations vs MRS

#OA vs MRS
msdOA_MRS_roc<-roc(ad, MRSref, OA.conc, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=2, print.auc.x=0.7, print.auc.y=0.8)

#FA vs MRS
msdFA_MRS_roc<-roc(ad, MRSref, FA.conc, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=2, print.auc.x=0.7, print.auc.y=0.8)

#X5 vs MRS
msdX5_MRS_roc<-roc(ad, MRSref, X5.conc, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=2, print.auc.x=0.7, print.auc.y=0.8)

#Combined
msdOA_MRS_roc<-roc(ad, MRSref, OA.conc, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, col="#377eb8", lwd=2, print.auc.x=0.7, print.auc.y=1.0)
msdFA_MRS_roc<-roc(ad, MRSref, FA.conc, plot=TRUE, legacy.axes=TRUE, add=TRUE, print.auc=TRUE, col="#4daf4a", lwd=2, print.auc.x=0.7, print.auc.y=0.95)
msdX5_MRS_roc<-roc(ad, MRSref, X5.conc, plot=TRUE, legacy.axes=TRUE, add=TRUE, print.auc=TRUE, col="#FF3633", lwd=2, print.auc.x=0.7, print.auc.y=0.9)
legend(0.4,0.2,legend=c("S420:A194", "FIND28:A194", "SE3:BJ76"), col=c("#377eb8", "#4daf4a", "#FF3633"), lwd=2, lty=1, cex=0.8, bty="n", title="MSD Ab Pair")

coords(msdOA_MRS_roc, x="best", best.method="youden", ret="all")
coords(msdFA_MRS_roc, x="best", best.method="youden", ret="all")
coords(msdX5_MRS_roc, x="best", best.method="youden", ret="all")




roc.test(msdOA_MRS_roc, msdFA_MRS_roc, are.paired=TRUE)



####Table 10 - Comparison of MSD to MRS


#OA
(tab.MSD.OA_MRS<- run_epi_tests(ad, MSDref.OA, MRSref))
(make_ppa_npa_df(tab.MSD.OA_MRS))


#FA
(tab.MSD.FA_MRS<- run_epi_tests(ad, MSDref.FA, MRSref))
(make_ppa_npa_df(tab.MSD.FA_MRS))


#X5

(tab.MSD.X5_MRS<- run_epi_tests(ad, MSDref.X5, MRSref))
(make_ppa_npa_df(tab.MSD.X5_MRS))



#Table for OA LLODs

table(ad$OA.LLOD, useNA = "ifany")

#grouping by LLOD & LAM Detected

tab11<- ad %>% group_by(OA.LLOD, MSDref.OA) %>%
        summarise(mean=mean(OA.conc), 
                  min=min(OA.conc),
                  max=max(OA.conc),
                  n= n())


tab11










par(pty="m")





