library(dplyr)
library(magrittr)
library(ggplot2)
library(ROCR)
library(forcats)
library(stringr)
library(reshape2)
library(Rmisc)
library(ggrepel)

#Load 2006 data
load("~/Downloads/ICPSR_21240/DS0001/21240-0001-Data.rda")
#Load 2012 data
load("~/Downloads/ICPSR_34933/DS0001/34933-0001-Data.rda")

#rename datasets and filter for adults only
df.06 = da21240.0001
names(df.06)
adults.df.06 = df.06 %>%
  dplyr::filter(as.numeric(AGE2) >=7)
nrow(adults.df.06)

df.12 = da34933.0001
adults.df.12 = df.12 %>%
  dplyr::filter(as.numeric(AGE2) >=7)
nrow(adults.df.12)

#rename 2006 Significant Psychological Distress in Past Year as adjusted 2008 variable name
SPDYRidx = which(names(adults.df.06) == "SPDYRADJ")
names(adults.df.06)[SPDYRidx] = "SPDYR"

createVar <- function(df){
  df %>%
  mutate(SPDind = ifelse(str_detect(SPDYR, "(0)") == TRUE,
                         0,
                         ifelse(str_detect(SPDYR,"(1)") == TRUE,
                                1,
                                NA))) %>%
  mutate(CIGever =ifelse(as.character(CIGEVER) =="(2) No",
                         0,
                         ifelse(as.character(CIGEVER) == "(1) Yes",
                               1,
                              NA) )) %>%
  mutate(Age1CIG= ifelse(CIGTRY<14,
                        "<14 Years",
                        ifelse(CIGTRY>= 14 & CIGTRY < 18,
                               "14-17 Years",
                               ifelse(CIGTRY>= 18 & CIGTRY < 21,
                                      "18-20 Years",
                                      ifelse(CIGTRY >=21 & CIGTRY<74,
                                             "21+ Years ",
                                             ifelse(CIGTRY == 991 | is.na(as.numeric(CIGTRY)),
                                                    "Never Used",
                                                    NA)))))) %>%
  mutate(Age1CIG = fct_explicit_na(Age1CIG, na_level = "Never Used")) %>%
  mutate(Age1CIG = factor(Age1CIG)) %>%
  mutate(CIGperDayAv = ifelse(CIGAVGD == 0.5,
                        "<1",
                        ifelse(CIGAVGD == 1.0,
                               "1 cigarette",
                               ifelse(CIGAVGD == 3.5,
                                      "2-5",
                                      ifelse(CIGAVGD == 10.5,
                                             "6-15",
                                             ifelse(CIGAVGD == 20.5,
                                                    "16-25",
                                                    ifelse(CIGAVGD == 30.5,
                                                           "26-35",
                                                           ifelse(CIGAVGD == 50.5,
                                                                  ">35",
                                                                  "Not Used/Unknown")))))))) %>% 
  mutate(CIGperDayAv = factor(CIGperDayAv, levels = c("<1","1","2-5","6-15","16-25","26-35",">35","Not Used",NA))) %>%
  mutate(CIGperDayAv = fct_explicit_na(CIGperDayAv, "Never Used/Unknown")) %>%
    mutate(CIGusePM = ifelse(as.character(CG05) == "(2) No",
                             0,
                             ifelse(as.character(CG05) == "(1) Yes",
                                    1,
                                    NA))) %>%
    mutate(ALever = ifelse(as.character(ALCEVER) =="(2) No",
                         0,
                         ifelse(as.character(ALCEVER) == "(1) Yes",
                                1,
                                NA) )) %>%
    mutate(MJusePM = ifelse(as.character(MRJMON) == "(0) Did not use in the past month (IRMJRC = 2-3,9)",
                            0,
                            ifelse(as.character(MRJMON) == "(1) Used within the past month (IRMJRC = 1)",
                                   1,
                                   NA))) %>%
    mutate(ALusePM= ifelse(as.character(ALCREC) == "(01) Within the past 30 days",
                           1,
                           0)) %>%
  mutate(Age1AL= ifelse(ALCTRY<14,
                        "<14 Years",
                        ifelse(ALCTRY>= 14 & ALCTRY < 18,
                               "14-17 Years",
                               ifelse(ALCTRY>= 18 & ALCTRY < 21,
                                      "18-20 Years",
                                      ifelse(ALCTRY >=21 & ALCTRY<74,
                                             "21+ Years ",
                                                    NA))))) %>%
  mutate(Age1AL = fct_explicit_na(Age1AL, na_level = "Never Used")) %>%
  mutate(Age1AL = factor(Age1AL)) %>%
  mutate(bingePM = ifelse(str_detect(SPDYR, "(0)") == T,
                          0,
                          1)) %>%
  mutate(MJever = ifelse(as.character(MJEVER) =="(2) No",
                         0,
                         ifelse(as.character(MJEVER) == "(1) Yes",
                                1,
                                NA) )) %>%
  mutate(Age1MJ = ifelse(MJAGE < 14, 
                         "<14 Years",
                         ifelse(MJAGE >=14 & MJAGE <18,
                                "14-17 Years",
                                ifelse(MJAGE >= 18 & MJAGE <21,
                                       "18-20 Years",
                                       ifelse(MJAGE >=21 & MJAGE <70,
                                              "21+ Years ",
                                              ifelse(MJAGE ==991,
                                                     "Never Used",
                                                     NA)))))) %>%
    mutate(Age1MJ = fct_explicit_na(Age1MJ, na_level = "Never Used")) %>%
  mutate(Age1MJ = factor(Age1MJ, )) %>%
  mutate(COCever = ifelse(as.character(COCEVER) =="(2) No",
                         0,
                         ifelse(as.character(COCEVER) == "(1) Yes",
                                1,
                                NA) )) %>%
 mutate(COCusePM = ifelse(as.character(COCMON) == "(0) Did not use in the past month (IRCOCRC = 2-3,9)",
                         0,
                         ifelse(as.character(COCMON) == "(1) Used within the past month (IRCOCRC = 1)",
                                 1,
                                 NA))) %>%
  mutate(Age1COC = ifelse(COCAGE <14,
                          "<14 Years",
                          ifelse(COCAGE >=14 & COCAGE <18,
                                 "14-17 Years",
                                 ifelse(COCAGE >= 18 & COCAGE <21,
                                        "18-20 Years",
                                        ifelse(COCAGE >=21 & COCAGE <62,
                                               "21+ Years ",
                                               ifelse(COCAGE ==991,
                                                      "Never Used",
                                                      NA))))))%>%
    mutate(Age1COC = fct_explicit_na(Age1COC, na_level = "Never Used")) %>%
  mutate(Age1COC = factor(Age1COC, )) %>%
  mutate(HERever = ifelse(as.character(HEREVER) =="(2) No",
                         0,
                         ifelse(as.character(HEREVER) == "(1) Yes",
                                1,
                                NA) )) %>%
    mutate(HERusePM = ifelse(as.character(HERMON) == "(0) Did not use in the past month (IRHERRC = 2-3,9)",
                            0,
                            ifelse(as.character(HERMON) == "(1) Used within the past month (IRHERRC = 1)",
                                   1,
                                   NA))) %>%
    mutate(Age1HER =ifelse(HERAGE <14,
                         "<14 Years",
                         ifelse(HERAGE >=14 & HERAGE <18,
                                "14-17 Years",
                                ifelse(HERAGE >= 18 & HERAGE <21,
                                       "18-20 Years",
                                       ifelse(HERAGE >=21 & HERAGE <46,
                                              "21+ Years ",
                                              ifelse(HERAGE ==991,
                                                     "Never Used",
                                                     NA))))) ) %>%
    mutate(Age1HER = fct_explicit_na(Age1HER, na_level = "Never Used")) %>%
  mutate(Age1HER = factor(Age1HER, )) %>%
  #This variable is only press 1 if Yes or otherwise continue without doing anything so I am coding
  #NA as 0 = Never used pain killers
  mutate(PKever = ifelse(as.character(ANLEVER) =="(1) Yes", 
                          1,
                          0)) %>%
  mutate(PKever = factor(PKever)) %>%
  mutate(PKever = fct_explicit_na(PKever, na_level = "0")) %>%
  mutate(PKever = as.numeric(PKever)) %>%
  mutate(PKever =ifelse(PKever == 2,
                         0,
                         1))%>%
  mutate(PKusePM = ifelse(as.character(ANLMON) == "(0) Did not use in the past month (IRANLRC = 2-3,9)",
                             0,
                             ifelse(as.character(ANLMON) == "(1) Used within the past month (IRANLRC = 1)",
                                    1,
                                    NA))) %>%
  mutate(Age1PK = ifelse(ANALAGE <14,
                         "<14 Years",
                         ifelse(ANALAGE >=14 & ANALAGE <18,
                                "14-17 Years",
                                ifelse(ANALAGE >= 18 & ANALAGE <21,
                                       "18-20 Years",
                                       ifelse(ANALAGE >=21 & ANALAGE <46,
                                              "21+ Years ",
                                              ifelse(ANALAGE ==991,
                                                     "Never Used",
                                                     NA))))) ) %>%
    mutate(Age1PK = fct_explicit_na(Age1PK, na_level = "Never Used")) %>%
  mutate(Age1PK = factor(Age1PK, )) %>%
  mutate(AnxDLife = ifelse(as.character(ANXDLIF) == "(0) No (LIFANXD=6,99)",
                           0,
                           ifelse(as.character(ANXDLIF) == "(1) Yes (LIFANXD=1)",
                                  1,
                                  NA))) %>%
  mutate(AnxPY = ifelse(as.character(ANXDYR)  =="(0) No (YRANXD=6,89,99)",
                          0,
                          ifelse(as.character(ANXDYR)== "(1) Yes (YRANXD=1)",
                                 1,
                                 NA))) %>%
  mutate(AsthmaLife = ifelse(as.character(ASMALIF) == "(0) No (LIFASMA=6,99)",
                             0,
                             ifelse(as.character(ASMALIF ) == "(1) Yes (LIFASMA=1)",
                                    1,
                                    NA))) %>%
  mutate(CirrLife = ifelse(as.character(CIRRLIF ) == "(0) No (LIFCIRR=6,99)",
                           0,
                           ifelse(as.character(CIRRLIF  ) == "(1) Yes (LIFCIRR=1)",
                                  1,
                                  NA))) %>%
  mutate(DeprsLife = ifelse(as.character(DEPRSLIF) == "(0) No (LIFDEPRS=6,99)",
                             0,
                             ifelse(as.character(DEPRSLIF ) == "(1) Yes (LIFDEPRS=1)",
                                    1,
                                    NA))) %>%
  mutate(DeprsPY = ifelse(as.character(DEPRSYR) == "(0) No (YRDEPRS=6,89,99)",
                            0,
                            ifelse(as.character(DEPRSYR ) == "(1) Yes (YRDEPRS=1)",
                                   1,
                                   NA))) %>%
  mutate(HepatLife = ifelse(as.character(HEPATLIF) == "(0) No (LIFHEPAT=6,99)",
                            0,
                            ifelse(as.character(HEPATLIF ) == "(1) Yes (LIFHEPAT=1)",
                                   1,
                                   NA))) %>%
  mutate(HIVLife = ifelse(as.character(HIVLIF) == "(0) No (LIFHIV=6,99)",
                            0,
                            ifelse(as.character(HIVLIF ) == "(1) Yes (LIFHIV=1)",
                                   1,
                                   NA))) %>%
  mutate(N.IPMHHospPY = ifelse(AUNMPSY2 <5,
                               "<5",
                               ifelse(AUNMPSY2 >=5 & AUNMPSY2 <10,
                                      "5-9",
                                      ifelse(AUNMPSY2 >=10 & AUNMPSY2 <15,
                                             "10-14",
                                             ifelse(AUNMPSY2 >=15 & AUNMPSY2 <20,
                                                    "15-19",
                                                    ifelse(AUNMPSY2 >=20 & AUNMPSY2 <25,
                                                           "20-24",
                                                           ifelse(AUNMPSY2 >=25 & AUNMPSY2 <30,
                                                                  "25-29",
                                                                  ifelse(AUNMPSY2 >= 30 & AUNMPSY2 <= 100,
                                                                         "30+",
                                                                         "Not Used")))))))) %>%
  mutate(N.IPMHHospPY = factor(N.IPMHHospPY, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30+","Not Used"),)) %>%
  mutate(N.IPMHHospPY = fct_explicit_na(N.IPMHHospPY,"Not Used")) %>%
  mutate(N.PrivTherPY =ifelse(AUNMTHE2 <5,
                              "<5",
                              ifelse(AUNMTHE2 >=5 & AUNMTHE2 <10,
                                     "5-9",
                                     ifelse(AUNMTHE2 >=10 & AUNMTHE2 <15,
                                            "10-14",
                                            ifelse(AUNMTHE2 >=15 & AUNMTHE2 <20,
                                                   "15-19",
                                                   ifelse(AUNMTHE2 >=20 & AUNMTHE2 <25,
                                                          "20-24",
                                                          ifelse(AUNMTHE2 >=25 & AUNMTHE2 <30,
                                                                 "25-29",
                                                                 ifelse(AUNMTHE2 >= 30 & AUNMTHE2 <= 31,
                                                                        "30+",
                                                                        "Not Used"))))))) ) %>%

    mutate(N.PrivTherPY = factor(N.PrivTherPY, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30+","Not Used"),)) %>%
    mutate(N.PrivTherPY = fct_explicit_na(N.PrivTherPY,"Not Used")) %>%
    mutate(N.PWardGhospPY =ifelse(AUNMPGE2 <5,
                            "<5",
                            ifelse(AUNMPGE2 >=5 & AUNMPGE2 <10,
                                   "5-9",
                                   ifelse(AUNMPGE2 >=10 & AUNMPGE2 <15,
                                          "10-14",
                                          ifelse(AUNMPGE2 >=15 & AUNMPGE2 <20,
                                                 "15-19",
                                                 ifelse(AUNMPGE2 >=20 & AUNMPGE2 <25,
                                                        "20-24",
                                                        ifelse(AUNMPGE2 >=25 & AUNMPGE2 <30,
                                                               "25-29",
                                                               ifelse(AUNMPGE2 >= 30 & AUNMPGE2 <= 31,
                                                                      "30+",
                                                                      "Not Used"))))))) )  %>%
    mutate(N.PWardGhospPY = factor(N.PWardGhospPY, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30+","Not Used"),))  %>%
    mutate(N.PWardGhospPY = fct_explicit_na(N.PWardGhospPY,"Not Used")) %>%
    mutate(N.DocOfMHPY =ifelse(AUNMDOC2 <5,
                              "<5",
                              ifelse(AUNMDOC2 >=5 & AUNMDOC2 <10,
                                     "5-9",
                                     ifelse(AUNMDOC2 >=10 & AUNMDOC2 <15,
                                            "10-14",
                                            ifelse(AUNMDOC2 >=15 & AUNMDOC2 <20,
                                                   "15-19",
                                                   ifelse(AUNMDOC2 >=20 & AUNMDOC2 <25,
                                                          "20-24",
                                                          ifelse(AUNMDOC2 >=25 & AUNMDOC2 <30,
                                                                 "25-29",
                                                                 ifelse(AUNMDOC2 >= 30 & AUNMDOC2 <= 31,
                                                                        "30+",
                                                                        "Not Used"))))))) )  %>%
    mutate(N.DocOfMHPY = factor(N.DocOfMHPY, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30+","Not Used"),))  %>%
    mutate(N.DocOfMHPY = fct_explicit_na(N.DocOfMHPY,"Not Used")) %>%
    mutate(RxMHPY = ifelse(as.character(AURXYR) == "(1) Yes",
                         1,
                         ifelse(as.character(AURXYR) =="(2) No",
                                0,
                                NA))) %>%
  mutate(UMNeedMHPY = ifelse(as.character(AUUNMTYR) == "(1) Yes",
                         1,
                         ifelse(as.character(AUUNMTYR) =="(2) No",
                                0,
                                NA))) %>%
  mutate(MHtrtPY = ifelse(as.character(AMHSVTYP)== "(8) Did Not Receive MH Treatment (See comment above)",
                          "No MH Treatment",
                          ifelse(as.character(AMHSVTYP)=="(3) Presc Medication Only (See comment above)",
                                 "Rx Only",
                                  ifelse(as.character(AMHSVTYP)=="(2) Outpatient Only (See comment above)",
                                               "Outpatient Only",
                                               ifelse(as.character(AMHSVTYP)=="6) Outpatient and Presc Med Only (See comment above)",
                                                      "Outpatient and Rx",
                                                      ifelse(as.character(AMHSVTYP)=="(1) Inpatient Only (See comment above)",
                                                             "Inpatient",
                                                             ifelse(as.character(AMHSVTYP)=="(5) Inpatient and Presc Med Only (See comment above)",
                                                                    "Inpatient and Rx",
                                                                    ifelse(as.character(AMHSVTYP)=="(4) Inpatient and Outpatient Only (See comment above)",
                                                                           "Inpatient and Outpatient",
                                                                           ifelse(as.character(AMHSVTYP)=="(7) Inp & Outp & Presc Medication (See comment above)",
                                                                                  "Inpatient & Outpatient & Rx",
                                                                                  NA))))))))) %>%
  mutate(MHtrtPY = factor(MHtrtPY , levels = c("No MH Treatment","Rx Only","Outpatient Only", "Outpatient and Rx","Inpatient","Inpatient and Rx","Inpatient and Outpatient","Inpatient & Outpatient & Rx",NA)))%>%
    mutate(MHtrtPY = fct_explicit_na(MHtrtPY,"Not Used")) %>%
    mutate(gender = ifelse(as.character(IRSEX) == "(2) Female",
                          "Female",
                          "Male")) %>%
  mutate(gender = factor(gender, )) %>%
  mutate(ageCat = ifelse(as.character(CATAG6) == "(2) 18-25 Years Old",
                         "18-25 Years",
                         ifelse(as.character(CATAG6) =="(3) 26-34 Years Old",
                                "26-34 Years",
                                ifelse(as.character(CATAG6) =="(4) 35-49 Years Old",
                                       "35-49 Years",
                                       ifelse(as.character(CATAG6) =="(5) 50-64 Years Old",
                                              "50-64 Years",
                                              "65+ Years"))))) %>%
    mutate(ageCat = factor(ageCat , levels = c("18-25 Years","26-34 Years","35-49 Years", "50-64 Years","65+ Years"))) %>%
    mutate(education = ifelse(str_detect(EDUCCAT2, "\\(1\\)") == TRUE,
                            "Less than high school",
                             ifelse(str_detect(EDUCCAT2, "\\(3\\)") == TRUE,
                                          "Some college",
                                          ifelse(str_detect(EDUCCAT2,"\\(4\\)") == TRUE,
                                          "College graduate",
                                          "High school graduate")))) %>%
  mutate(education = factor(education , levels = c("Less than high school","High school graduate","Some college", "College graduate"),))
}

adults.df.06 <- createVar(adults.df.06) 
adults.df.12 <- createVar(adults.df.12)

#Proportion of each subgroup according to whether they had smoked cigarettes in the past year and gender

dfr_prop.CIGever.06 <- adults.df.06 %>%
  group_by(CIGever,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(CIGever == 1)

dfr_prop.CIGever.12 <- adults.df.12 %>%
  group_by(CIGever,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))%>%
  dplyr::filter(CIGever == 1)

df3 <- dfr_prop.CIGever.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.CIGever.12 %>%
              mutate(Year = '2012'))

gg_prop.CIGever <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that ever consumed tobacco")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)


plot(gg_prop.CIGever)

#Proportion of each subgroup according consumption of cigarette in the past month and gender

dfr_prop.CIGusePM.06 <- adults.df.06 %>%
  group_by(CIGusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(CIGusePM == 1) 

dfr_prop.CIGusePM.12 <- adults.df.12 %>%
  group_by(CIGusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(CIGusePM == 1) 

df3 <- dfr_prop.CIGusePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.CIGusePM.12 %>%
              mutate(Year = '2012'))

gg_prop.CIGusePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that consumed tobacco in the past month")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

plot(gg_prop.CIGusePM)
multiplot(gg_prop.CIGever, gg_prop.CIGusePM)

#Mean # of cigarettes per day
dfr_prop.CIGperdayAv.06 <- adults.df.06%>%
  group_by(CIGperDayAv,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))
dfr_prop.CIGperdayAv.12 <- adults.df.12%>%
  group_by(CIGperDayAv,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.CIGperdayAv.06 <- ggplot(dfr_prop.CIGperdayAv.06, aes(x=CIGperDayAv, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = CIGperDayAv,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Average Cigarettes per Day") +
  labs(y= "Proportion of Population")+
  ggtitle("Average cigarette consumption per day")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
gg_prop.CIGperdayAv.12 <- ggplot(dfr_prop.CIGperdayAv.12, aes(x=CIGperDayAv, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = CIGperDayAv,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Average Cigarettes per Day") +
  labs(y= "Proportion of Population")+
  ggtitle("Average cigarette consumption per day")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.CIGperdayAv.06,gg_prop.CIGperdayAv.12)

#Proportion of each subgroup according consumption of marijuana ever and gender

dfr_prop.MJever.06 <- adults.df.06 %>%
  group_by(MJever,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))%>%
  dplyr::filter(MJever == 1)

dfr_prop.MJever.12 <- adults.df.12 %>%
  group_by(MJever,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(MJever == 1)

df3 <- dfr_prop.MJever.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.MJever.12 %>%
              mutate(Year = '2012'))

gg_prop.MJever <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that consumed marijuana ever")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
#################################################3

plot(gg_prop.MJever)

#Proportion of each subgroup according consumption of marijuana in the past month and gender

dfr_prop.MJusePM.06 <- adults.df.06 %>%
  group_by(MJusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))%>%
  dplyr::filter(MJusePM == 1)

dfr_prop.MJusePM.12 <- adults.df.12 %>%
  group_by(MJusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(MJusePM == 1)

df3 <- dfr_prop.MJusePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.MJusePM.12 %>%
              mutate(Year = '2012'))

##### GEOM_TEXT DODGED LABELS######################
gg_prop.MJusePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that consumed marijuana in the past month")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

plot(gg_prop.MJusePM)

multiplot(gg_prop.MJever, gg_prop.MJusePM)

#Proportion of each subjects according to age at first consumption of alcohol
dfr_prop.Age1AL.06 <- adults.df.06 %>%
  group_by(Age1AL,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1AL.06 <- ggplot(dfr_prop.Age1AL.06, aes(x=Age1AL, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1AL,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first alcohol consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first alcohol consumption in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

dfr_prop.Age1AL.12 <- adults.df.12%>%
  group_by(Age1AL,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1AL.12 <- ggplot(dfr_prop.Age1AL.12, aes(x=Age1AL, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1AL,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first alcohol consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first alcohol consumption in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.Age1AL.06,gg_prop.Age1AL.12)

#Proportion that ever consumed alcohol

dfr_prop.ALever.06 <- adults.df.06 %>%
  group_by(ALever  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(ALever == 1) 
dfr_prop.ALever.12 <- adults.df.12 %>%
  group_by(ALever,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(ALever == 1) 

df3 <- dfr_prop.ALever.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.ALever.12 %>%
              mutate(Year = '2012'))


gg_prop.ALever <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that ever consumed alcohol")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

plot(gg_prop.ALever)


#Proportion that consumed alcohol in past month

dfr_prop.bingePM.06 <- adults.df.06 %>%
  group_by(bingePM  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(bingePM == 1) 
dfr_prop.bingePM.12 <- adults.df.12 %>%
  group_by(bingePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(bingePM == 1) 

df3 <- dfr_prop.bingePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.bingePM.12 %>%
              mutate(Year = '2012'))


gg_prop.bingePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion binge drinking in the past month")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

plot(gg_prop.bingePM)

multiplot(gg_prop.ALever,gg_prop.bingePM)

#Proportion in Significant Psychological Distress in the past year by presence of binge drinking in past month and gender
dfr_prop.SPD.bingePM.06 <- adults.df.06 %>%
  group_by(SPDind,bingePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(SPDind==1) %>%
  dplyr::filter(bingePM ==1) 

dfr_prop.SPD.bingePM.12 <- adults.df.12 %>%
  group_by(SPDind, bingePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(SPDind==1) %>%
  dplyr::filter(bingePM ==1)

df3 <- dfr_prop.SPD.bingePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.SPD.bingePM.12 %>%
              mutate(Year = '2012'))

gg_prop.SPD.bingePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Binge Drinkers in past month")+
  labs(x=NULL)+
  ggtitle("Proportion of binge drinkers with the past month with SPD")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
plot(gg_prop.SPD.bingePM)
multiplot(gg_prop.bingePM,gg_prop.SPD.bingePM)

#prop of by age at first drinking to binge alcohol
dfr_prop.Age1AL.bingePM.06 <- adults.df.06 %>%
  group_by(bingePM, Age1AL,gender) %>%
  dplyr::summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  dplyr::filter(bingePM ==1)

dfr_prop.Age1AL.bingePM.12 <- adults.df.12 %>%
  group_by(bingePM, Age1AL,gender) %>%
  dplyr::summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  dplyr::filter(bingePM ==1)
gg_prop.Age1AL.bingePM.06 <- ggplot(data = dfr_prop.SPD.CIGperdayAv.06, aes(x = CIGperDayAv, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = CIGperDayAv,  y = freq + 0.05, label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Average cigarette consumption per day") +
  labs(y="Proportion with SPD") +
  ggtitle("Proportion with Significant Psychological Distress in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

gg_prop.Age1AL.bingePM.12 <- ggplot(data = dfr_prop.SPD.CIGperdayAv.12, aes(x = CIGperDayAv, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = CIGperDayAv,  y = freq + 0.05, label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Average cigarette consumption per day") +
  labs(y="Proportion with SPD") +
  ggtitle("Proportion with Significant Psychological Distress in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

multiplot(gg_prop.Age1AL.bingePM.06,gg_prop.Age1AL.bingePM.12)



#Proportion of each subjects according to age at first consumption of cigarettes
dfr_prop.Age1CIG.06 <- adults.df.06 %>%
  group_by(Age1CIG,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1CIG.06 <- ggplot(dfr_prop.Age1CIG.06, aes(x=Age1CIG, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1CIG,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first cigarette consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first cigarette consumption in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

dfr_prop.Age1CIG.12 <- adults.df.12%>%
  group_by(Age1CIG,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1CIG.12 <- ggplot(dfr_prop.Age1CIG.12, aes(x=Age1CIG, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1CIG,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first cigarette consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first cigarette consumption in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.Age1CIG.06,gg_prop.Age1CIG.12)

#Proportion of each subgroup according consumption of cocaine in the past month and gender

dfr_prop.COCusePM.06 <- adults.df.06 %>%
  group_by(COCusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(COCusePM == 1)

dfr_prop.COCusePM.12 <- adults.df.12 %>%
  group_by(COCusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(COCusePM == 1)

df3 <- dfr_prop.COCusePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.COCusePM.12 %>%
              mutate(Year = '2012'))


gg_prop.COCusePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that consumed cocaine in the past month")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,.05)

plot(gg_prop.COCusePM)

#Proportion of each subjects according to age at first consumption of cocaine
dfr_prop.Age1COC.06 <- adults.df.06 %>%
  group_by(Age1COC,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1COC.06 <- ggplot(dfr_prop.Age1COC.06, aes(x=Age1COC, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1COC,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first cocaine consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first cocaine consumption in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

dfr_prop.Age1COC.12 <- adults.df.12%>%
  group_by(Age1COC,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1COC.12 <- ggplot(dfr_prop.Age1COC.12, aes(x=Age1COC, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1COC,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first cocaine consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first cocaine consumption in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.Age1COC.06,gg_prop.Age1COC.12)

#Proportion of each subjects according to age at first consumption of cigarettes
dfr_prop.Age1CIG.06 <- adults.df.06 %>%
  group_by(Age1CIG,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

dfr_prop.Age1CIG.12 <- adults.df.12 %>%
  group_by(Age1CIG,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1CIG.06 <- ggplot(dfr_prop.Age1CIG.06, aes(x=Age1CIG, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1CIG,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first cigarette consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first cigarette consumption in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

dfr_prop.Age1CIG.12 <- adults.df.12%>%
  group_by(Age1CIG,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1CIG.12 <- ggplot(dfr_prop.Age1CIG.12, aes(x=Age1CIG, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1CIG,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first cigarette consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first cigarette consumption in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.Age1CIG.06,gg_prop.Age1CIG.12)

#Proportion of each subgroup according consumption of heroin in the past month and gender

dfr_prop.HERusePM.06 <- adults.df.06 %>%
  group_by(HERusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(HERusePM == 1)
dfr_prop.HERusePM.12 <- adults.df.12 %>%
  group_by(HERusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(HERusePM == 1)

df3 <- dfr_prop.HERusePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.HERusePM.12 %>%
              mutate(Year = '2012'))


gg_prop.HERusePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that consumed heroin in the past month")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,.05)

plot(gg_prop.HERusePM)



#Proportion of each subjects according to age at first consumption of heroin
dfr_prop.Age1HER.06 <- adults.df.06 %>%
  group_by(Age1HER,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1HER.06 <- ggplot(dfr_prop.Age1HER.06, aes(x=Age1HER, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1HER,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first heroin consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first heroin consumption in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

dfr_prop.Age1HER.12 <- adults.df.12%>%
  group_by(Age1HER,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1HER.12 <- ggplot(dfr_prop.Age1HER.12, aes(x=Age1HER, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1HER,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first heroin consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first heroin consumption in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.Age1HER.06,gg_prop.Age1HER.12)

#Proportion that consumed illicit painkillers ever

dfr_prop.PKever.06 <- adults.df.06 %>%
  group_by(PKever  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(PKever == 1) 
dfr_prop.PKever.12 <- adults.df.12 %>%
  group_by(PKever,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(PKever == 1) 

df3 <- dfr_prop.PKever.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.PKever.12 %>%
              mutate(Year = '2012'))


gg_prop.PKever <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that used illicit painkillers ever")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
plot(gg_prop.PKever)


#Proportion of each subgroup according consumption of painkillers in the past month and gender

dfr_prop.PKusePM.06 <- adults.df.06 %>%
  group_by(PKusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(PKusePM == 1)
dfr_prop.PKusePM.12 <- adults.df.12 %>%
  group_by(PKusePM,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(PKusePM == 1)

df3 <- dfr_prop.PKusePM.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.PKusePM.12 %>%
              mutate(Year = '2012'))


gg_prop.PKusePM <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion that used illicit painkillers in the past month")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

plot(gg_prop.PKusePM)
multiplot(gg_prop.PKever, gg_prop.PKusePM)

#Proportion of each subjects according to age at first consumption of illicit painkillers
dfr_prop.Age1PK.06 <- adults.df.06 %>%
  group_by(Age1PK,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1PK.06 <- ggplot(dfr_prop.Age1PK.06, aes(x=Age1PK, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1PK,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first illicit painkiller consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first illicit painkiller consumption in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

dfr_prop.Age1PK.12 <- adults.df.12%>%
  group_by(Age1PK,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n))

gg_prop.Age1PK.12 <- ggplot(dfr_prop.Age1PK.12, aes(x=Age1PK, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = Age1PK,  y = freq +.05 , label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Age at first illicit painkiller consumption") +
  labs(y= "Proportion of Population")+
  ggtitle("Age at first illicit painkiller in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
multiplot(gg_prop.Age1PK.06,gg_prop.Age1PK.12)

#Proportion in Significant Psychological Distress in the past year for each category or average cigarette consumption per day and gender
dfr_prop.SPD.CIGperdayAv.06 <- adults.df.06 %>%
  dplyr::filter(SPDind==1) %>%
  group_by(SPDind==1, CIGperDayAv,gender) %>%
  dplyr::summarise (n = n()) %>%
  mutate(freq = n / sum(n))
dfr_prop.SPD.CIGperdayAv.12 <- adults.df.12 %>%
  dplyr::filter(SPDind==1) %>%
  group_by(SPDind==1, CIGperDayAv,gender) %>%
  dplyr::summarise (n = n()) %>%
  mutate(freq = n / sum(n))
gg_prop.SPD.CIGperdayAv.06 <- ggplot(data = dfr_prop.SPD.CIGperdayAv.06, aes(x = CIGperDayAv, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = CIGperDayAv,  y = freq + 0.05, label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Average cigarette consumption per day") +
  labs(y="Proportion with SPD") +
  ggtitle("Proportion with Significant Psychological Distress in 2006")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

gg_prop.SPD.CIGperdayAv.12 <- ggplot(data = dfr_prop.SPD.CIGperdayAv.12, aes(x = CIGperDayAv, y = freq, fill = gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_text(aes(x = CIGperDayAv,  y = freq + 0.05, label = round(freq, 2)), position = position_dodge(width = 1))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x= "Average cigarette consumption per day") +
  labs(y="Proportion with SPD") +
  ggtitle("Proportion with Significant Psychological Distress in 2012")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

multiplot(gg_prop.SPD.CIGperdayAv.06,gg_prop.SPD.CIGperdayAv.12)

#Proportion that has depression in the past year

dfr_prop.DeprsLife.06 <- adults.df.06 %>%
  group_by(DeprsLife  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(DeprsLife == 1) 
dfr_prop.DeprsLife.12 <- adults.df.12 %>%
  group_by(DeprsLife,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(DeprsLife == 1) 

df3 <- dfr_prop.DeprsLife.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.DeprsLife.12 %>%
              mutate(Year = '2012'))


gg_prop.DeprsLife <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion diagnosed with depression ever")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
plot(gg_prop.DeprsLife)

#Proportion that has depression in the past year

dfr_prop.DeprsPY.06 <- adults.df.06 %>%
  group_by(DeprsPY  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(DeprsPY == 1) 
dfr_prop.DeprsPY.12 <- adults.df.12 %>%
  group_by(DeprsPY,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(DeprsPY == 1) 

df3 <- dfr_prop.DeprsPY.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.DeprsPY.12 %>%
              mutate(Year = '2012'))


gg_prop.DeprsPY <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion diagnosed with depression in the past year")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
plot(gg_prop.DeprsPY)

multiplot(gg_prop.DeprsLife, gg_prop.DeprsPY)

#Proportion that has axiety ever

dfr_prop.AnxDLife.06 <- adults.df.06 %>%
  group_by(AnxDLife  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(AnxDLife == 1) 
dfr_prop.AnxDLife.12 <- adults.df.12 %>%
  group_by(AnxDLife,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(AnxDLife == 1) 

df3 <- dfr_prop.AnxDLife.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.AnxDLife.12 %>%
              mutate(Year = '2012'))


gg_prop.AnxDLife <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion diagnosed with anxiety ever")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
plot(gg_prop.AnxDLife)

#Proportion that has axiety in last year

dfr_prop.AnxPY.06 <- adults.df.06 %>%
  group_by(AnxPY  ,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(AnxPY == 1) 
dfr_prop.AnxPY.12 <- adults.df.12 %>%
  group_by(AnxPY,gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  dplyr::filter(AnxPY == 1) 

df3 <- dfr_prop.AnxPY.06 %>%
  mutate(Year = '2006') %>%
  bind_rows(dfr_prop.AnxPY.12 %>%
              mutate(Year = '2012'))


gg_prop.AnxPY <- ggplot() +
  geom_bar(data = df3, stat = 'identity', position = "dodge", aes(x=Year, y = freq, fill = gender)) +
  geom_text(data = df3,position = position_dodge(width = 1),aes(x = Year,y= freq +0.05,  group = gender,label = round(freq, 3)))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y= "Proportion of Population")+
  labs(x=NULL)+
  ggtitle("Proportion diagnosed with anxiety in the past year")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)
plot(gg_prop.AnxPY)
multiplot(gg_prop.AnxDLife, gg_prop.AnxPY)

set.seed(123)
adults.06.train.idx = sample(1:nrow(adults.df.06),floor(nrow(adults.df.06)*.8))
adults.06.train = adults.df.06[adults.06.train.idx,]
adults.06.test = adults.df.06[-adults.06.train.idx,]
nrow(adults.df.06) == nrow(adults.06.test)+nrow(adults.06.train)

set.seed(123)
adults.12.train.idx = sample(1:nrow(adults.df.12),floor(nrow(adults.df.12)*.8))
adults.12.train = adults.df.12[adults.12.train.idx,]
adults.12.test = adults.df.12[-adults.12.train.idx,]
nrow(adults.df.12) == nrow(adults.12.test)+nrow(adults.12.train)

l.bingePM.06.train <- glm(bingePM ~ Age1AL + ageCat + gender, data = adults.06.train, family = "binomial")
summary(l.bingePM.06.train)
fitted.results <- predict(l.bingePM.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != adults.06.test$bingePM)
print(paste('Accuracy',1-misClasificError))

l.bingePM.12.train <- glm(bingePM ~ Age1AL + ageCat + gender, control = list(maxit = 50), data = adults.12.train, family = "binomial")
summary(l.bingePM.12.train)
fitted.results <- predict(l.bingePM.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != adults.12.test$bingePM)
print(paste('Accuracy',1-misClasificError))

  l.DeprsLife.06.train <- glm(DeprsLife ~ Age1AL + ageCat + gender, data = adults.06.train, family = "binomial")
summary(l.DeprsLife.06.train)
fitted.results <- predict(l.DeprsLife.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.06.test$DeprsLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.DeprsLife.12.train <- glm(DeprsLife ~ Age1AL + ageCat + gender, control = list(maxit = 50), data = adults.12.train, family = "binomial")
summary(l.DeprsLife.12.train)
fitted.results <- predict(l.DeprsLife.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.12.test$DeprsLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.DeprsPY.06.train <- glm(DeprsPY ~ Age1AL + ageCat + gender, data = adults.06.train, family = "binomial")
summary(l.DeprsPY.06.train)
fitted.results <- predict(l.DeprsPY.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.06.test$DeprsPY),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.DeprsPY.12.train <- glm(DeprsPY ~ Age1AL + ageCat + gender, data = adults.12.train, family = "binomial")
summary(l.DeprsPY.12.train)
fitted.results <- predict(l.DeprsPY.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.12.test$DeprsPY),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.AnxLife.06.train <- glm(AnxDLife ~ Age1AL + ageCat + gender + DeprsLife, data = adults.06.train, family = "binomial")
summary(l.AnxLife.06.train)
fitted.results <- predict(l.AnxLife.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.06.test$AnxDLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.AnxLife.12.train <- glm(AnxDLife ~ Age1AL + ageCat + gender + DeprsLife, data = adults.12.train, family = "binomial")
summary(l.AnxLife.12.train)
fitted.results <- predict(l.AnxLife.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.12.test$AnxDLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))


l.AnxPY.06.train <- glm(AnxPY ~ Age1AL + ageCat + gender + DeprsPY, data = adults.06.train, family = "binomial")
summary(l.AnxPY.06.train)
fitted.results <- predict(l.AnxPY.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.06.test$AnxPY),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.AnxPY.12.train <- glm(AnxPY ~ Age1AL + ageCat + gender + DeprsPY, data = adults.12.train, family = "binomial")
summary(l.AnxPY.12.train)
fitted.results <- predict(l.AnxPY.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.12.test$AnxPY),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.asthmaLife.06.train <- glm(AsthmaLife ~ Age1CIG + ageCat + gender, data = adults.06.train, family = "binomial")
summary(l.asthmaLife.06.train)
fitted.results <- predict(l.asthmaLife.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.06.test$AsthmaLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.asthmaLife.12.train <- glm(AsthmaLife ~ Age1CIG + ageCat + gender, data = adults.12.train, family = "binomial")
summary(l.asthmaLife.12.train)
fitted.results <- predict(l.asthmaLife.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.12.test$AsthmaLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.cirrLife.06.train <- glm(CirrLife ~ Age1AL + ageCat + gender + bingePM, data = adults.06.train, family = "binomial")
summary(l.cirrLife.06.train)
fitted.results <- predict(l.cirrLife.06.train, newdata = adults.06.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.06.test$CirrLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))

l.cirrLife.12.train <- glm(CirrLife ~ Age1AL + ageCat + gender + bingePM, data = adults.12.train, family = "binomial")
summary(l.cirrLife.12.train)
fitted.results <- predict(l.cirrLife.12.train, newdata = adults.12.test)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean((fitted.results != adults.12.test$CirrLife),na.rm = T)
print(paste('Accuracy',1-misClasificError))







