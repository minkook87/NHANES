library(tidyverse)
library(haven)

####DATA loading---- (2013 to 2020년, 8개년)
# Demographic data
demo13 <- read_xpt("DEMO/DEMO_H.XPT") %>% 
  mutate(DMDMARTZ=DMDMARTL) %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, DMDMARTZ, RIDSTATR, RIDEXPRG)

demo15 <- read_xpt("DEMO/DEMO_I.XPT") %>% 
  mutate(DMDMARTZ=DMDMARTL) %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, DMDMARTZ, RIDSTATR, RIDEXPRG)

demo17 <- read_xpt("DEMO/P_DEMO.XPT") %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, DMDMARTZ, RIDSTATR, RIDEXPRG)

demo <- rbind(demo13, demo15, demo17)

# Supplement data
suppl13 <- read_xpt("DS1TOT/DS1TOT_H.XPT") %>% 
  filter(DR1DRSTZ == 1) %>% 
  select(SEQN,DS1DS,DS1DSCNT,DS1TKCAL,DS1TPROT,DS1TTFAT,DS1TSFAT,DS1TMFAT,
         DS1TPFAT,DS1TCHOL,DS1TCARB,DS1TFIBE,DS1TSUGR,DS1TCALC,DS1TPHOS,
         DS1TSODI,DS1TPOTA,DS1TMAGN,DS1TIRON,DS1TZINC,DS1TVD,DS1TVB1,DS1TVB2,
         DS1TNIAC,DS1TFDFE,DS1TVC)

suppl15 <- read_xpt("DS1TOT/DS1TOT_I.XPT") %>% 
  filter(DR1DRSTZ == 1) %>% 
  select(SEQN,DS1DS,DS1DSCNT,DS1TKCAL,DS1TPROT,DS1TTFAT,DS1TSFAT,DS1TMFAT,
         DS1TPFAT,DS1TCHOL,DS1TCARB,DS1TFIBE,DS1TSUGR,DS1TCALC,DS1TPHOS,
         DS1TSODI,DS1TPOTA,DS1TMAGN,DS1TIRON,DS1TZINC,DS1TVD,DS1TVB1,DS1TVB2,
         DS1TNIAC,DS1TFDFE,DS1TVC)

suppl17 <- read_xpt("DS1TOT/P_DS1TOT.XPT") %>% 
  filter(DR1DRSTZ == 1) %>% 
  select(SEQN,DS1DS,DS1DSCNT,DS1TKCAL,DS1TPROT,DS1TTFAT,DS1TSFAT,DS1TMFAT,
         DS1TPFAT,DS1TCHOL,DS1TCARB,DS1TFIBE,DS1TSUGR,DS1TCALC,DS1TPHOS,
         DS1TSODI,DS1TPOTA,DS1TMAGN,DS1TIRON,DS1TZINC,DS1TVD,DS1TVB1,DS1TVB2,
         DS1TNIAC,DS1TFDFE,DS1TVC)

suppl <- rbind(suppl13, suppl15, suppl17)

# Depression data
depression13 <- read_xpt("DPQ/DPQ_H.XPT")
depression15 <- read_xpt("DPQ/DPQ_I.XPT")
depression17 <- read_xpt("DPQ/P_DPQ.XPT")
depression <- rbind(depression13, depression15, depression17)

# Nutrition
nurt13 <- read_xpt("DR1_TOT/DR1TOT_H.XPT") %>% 
  filter(DR1DRSTZ==1) %>% 
  select(SEQN, DR1TVARA, DR1TATOC, DR1TATOA, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR,
         DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TVB1, DR1TVB2,
         DR1TNIAC, DR1TFA, DR1TVC, DR1TVD, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON,
         DR1TZINC, DR1TSODI, DR1TPOTA, DR1TP183, DR1TP184, DR1TP205, DR1TP225, DR1TP226, DR1TP182, DR1TP204) %>% 
  mutate(DR1TN3 = DR1TP183 + DR1TP184 + DR1TP205 + DR1TP225 + DR1TP226, DR1TN6 = DR1TP182 + DR1TP204) %>% 
  select(-DR1TP183, -DR1TP184, -DR1TP205, -DR1TP225, -DR1TP226, -DR1TP182, -DR1TP204)

nurt15 <- read_xpt("DR1_TOT/DR1TOT_I.XPT") %>% 
  filter(DR1DRSTZ==1) %>% 
  select(SEQN, DR1TVARA, DR1TATOC, DR1TATOA, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR,
         DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TVB1, DR1TVB2,
         DR1TNIAC, DR1TFA, DR1TVC, DR1TVD, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON,
         DR1TZINC, DR1TSODI, DR1TPOTA, DR1TP183, DR1TP184, DR1TP205, DR1TP225, DR1TP226, DR1TP182, DR1TP204) %>% 
  mutate(DR1TN3 = DR1TP183 + DR1TP184 + DR1TP205 + DR1TP225 + DR1TP226, DR1TN6 = DR1TP182 + DR1TP204) %>% 
  select(-DR1TP183, -DR1TP184, -DR1TP205, -DR1TP225, -DR1TP226, -DR1TP182, -DR1TP204)

nurt17 <- read_xpt("DR1_TOT/P_DR1TOT.XPT") %>% 
  filter(DR1DRSTZ==1) %>% 
  select(SEQN, DR1TVARA, DR1TATOC, DR1TATOA, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR,
         DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TVB1, DR1TVB2,
         DR1TNIAC, DR1TFA, DR1TVC, DR1TVD, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON,
         DR1TZINC, DR1TSODI, DR1TPOTA, DR1TP183, DR1TP184, DR1TP205, DR1TP225, DR1TP226, DR1TP182, DR1TP204) %>% 
  mutate(DR1TN3 = DR1TP183 + DR1TP184 + DR1TP205 + DR1TP225 + DR1TP226, DR1TN6 = DR1TP182 + DR1TP204) %>% 
  select(-DR1TP183, -DR1TP184, -DR1TP205, -DR1TP225, -DR1TP226, -DR1TP182, -DR1TP204)

nurt <- rbind(nurt13, nurt15, nurt17)

pres13 <- read_xpt("RXQ_RX/RXQ_RX_H.XPT")
pres15 <- read_xpt("RXQ_RX/RXQ_RX_I.XPT")
pres17 <- read_xpt("RXQ_RX/P_RXQ_RX.XPT")
pres <- rbind(pres13, pres15, pres17)

rm(demo13)
rm(demo15)
rm(demo17)
rm(suppl13)
rm(suppl15)
rm(suppl17)
rm(depression13)
rm(depression15)
rm(depression17)
rm(nurt13)
rm(nurt15)
rm(nurt17)
rm(pres13)
rm(pres15)
rm(pres17)

#### Data join ----
# SEQN - Respondent sequence number - Key_variable
data2 <- inner_join(demo,depression,by='SEQN')

# Inner join
# 우울증에 대한 질문지 - 점수화
# 7 9 NA -> 제외
# 우울증 점수화 시켜보자.
# 0~3 * 9 = 0 ~ 27점

# TURE = 1, FALSE = 0
colSums(is.na(data2))

# filter 행을 가져오는 함수
data3 <- data2 %>%
  select(-DPQ100) %>% 
  filter(!is.na(DPQ010) & !is.na(DPQ020) & !is.na(DPQ030) &
           !is.na(DPQ040) & !is.na(DPQ050) & !is.na(DPQ060) &
           !is.na(DPQ070) & !is.na(DPQ080) & !is.na(DPQ090)) %>% 
  filter(if_all(starts_with("DPQ"), ~ between(., 0, 3)))

# 우울증 점수화 PHQ-9
data3 <- data3 %>% 
  mutate(dep_score = DPQ010 + DPQ020 + DPQ030 + DPQ040 +
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>% 
  mutate(dep_group = factor(case_when(
    between(dep_score,0,4) ~ "Normal",
    between(dep_score,5,9) ~ "Mild",
    between(dep_score,10,14) ~ "Moderate",
    between(dep_score,15,19) ~ "Moderately severe",
    between(dep_score,20,27) ~ "Severe",
  ), levels = c("Normal","Mild","Moderate","Moderately severe","Severe")),
  dep_group2 = factor(ifelse(dep_score>=10,"Yes","No"), levels = c("No","Yes")))

# 데이터 분포를 확인
library(skimr)

skim(data3$dep_score)
skim(data3$dep_group)
table(data3$dep_group)
table(data3$dep_group2)

# 변수 정제
data4 <- data3 %>%
  filter(RIDSTATR == 2,
         RIDEXPRG == 2 | is.na(RIDEXPRG)) %>% 
  select(-RIDSTATR, -RIDEXPRG) %>% 
  filter(DMDEDUC2 %in% 1:5,
         DMDMARTZ %in% 1:3) %>% 
  mutate(RIAGENDR = factor(RIAGENDR, labels = c("M","F")),
         RIDRETH1 = factor(RIDRETH1, labels = c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black",
                                                "Other Race")),
         DMDEDUC2 = factor(DMDEDUC2, labels = c("Less than 9th grade","9-11th grade",
                                                "High school graduate/GED or equivalent","Some college or AA degree",
                                                "College graduate or above")),
         DMDMARTZ = factor(DMDMARTZ, labels = c("Married/Living with Partner","Widowed/Divorced/Separated",
                                                "Never married")))

colSums(is.na(data4))

skim(data4)

#### Depression & Drug data ----
drug <- read_xpt("RXQ_DRUG.XPT") # only drug information

# anti-depressant (249)
dep_med <- drug %>% 
  filter(RXDDCI1B == 249 | RXDDCI2B == 249 | RXDDCI3B == 249 | RXDDCI4B == 249)

# RXDDRGID
pres2 <- inner_join(pres,dep_med,by='RXDDRGID')

# 약물중복처방 제거한 SEQN
pres3 <- unique(pres2$SEQN)

# 데이터프레임화
pres3 <- data.frame("SEQN" = pres3, "DEP_MED" = 1)

# LEFT_JOIN
pres4 <- left_join(data4, pres3, by="SEQN")

# F32와 F33을 ICD-10 code
dep <- pres %>% 
  filter(if_any(starts_with("RXDRSC"), ~ grepl("^F33|^F32", .)))

#F32와 F33을 ICD-10 Code로
pres3_1 <- unique(dep$SEQN)

pres4_1 <- data.frame("SEQN" = pres3_1, "DEP_DIA" = 1)

pres5 <- left_join(pres4, pres4_1, by="SEQN")

colSums(is.na(pres5))

# DEP_MED, DEP_DIA 결측치를 모두 0으로 코딩
pres5[is.na(pres5)] <- 0

pres5 <- pres5 %>% 
  mutate(dep = ifelse(DEP_MED==1|DEP_DIA==1, 1, 0))

skim(pres5$dep)

#### Nutrition & Supplement----
data5 <- inner_join(pres5,nurt,by='SEQN')
data5 <- inner_join(data5,suppl,by='SEQN')

colSums(is.na(data5))

# 결측치를 모두 0으로 코딩
data5[is.na(data5)] <- 0

#### CCI 계산----
library(comorbidity)

diag <- pres %>% 
  pivot_longer(
    cols = c(RXDRSC1,RXDRSC2,RXDRSC3),
    values_to = "RXDRSC",
  ) %>% 
  filter(RXDRSC!="")

skim(pres$RXDRSC1)
unique(pres$RXDRSC1)

cci <- comorbidity(x = diag, id = "SEQN", code = "RXDRSC", map = "charlson_icd10_quan", assign0 = TRUE)

cci <- cci %>%
  mutate(CCI = score(x=cci, weights="charlson", assign0=TRUE))

# left join
data5 <- left_join(data5, cci, by='SEQN')

skim(data5$CCI)

# CCI와 동반질환이 없는 경우는 전부 0으로
data5 <- data5 %>%
  mutate(CCI=ifelse(CCI>=3,3,CCI))

data5[is.na(data5)] <- 0

skim(data5$CCI)

#### Data analysis ----
# factor
# dep = 우울증약을 먹거나 우울증 진단 코드 (과거력)
data6 <- data5 %>% 
  mutate(DEP_MED = factor(DEP_MED),
         DEP_DIA = factor(DEP_DIA),
         CCI = factor(CCI),
         dep = factor(dep, labels = c("False", "True")),
         DS1DS = factor(DS1DS)) %>% 
  mutate(Group = factor(case_when(dep_group2=="No"&dep=="False" ~ "정상",
                                  dep_group2=="No"&dep=="True" ~ "약먹고 정상",
                                  dep_group2=="Yes"&dep=="False" ~ "약안먹고 우울증",
                                  dep_group2=="Yes"&dep=="True" ~ "약먹어도 우울증"),
                        levels=c("정상","약먹고 정상","약안먹고 우울증","약먹어도 우울증")))

skim(data6)

table(data6$dep_group2,data6$dep)
table(data6$Group)

#Baseline characteristics
library(gtsummary)

MyTbl <- data6 %>% 
  tbl_summary(by=Group, # 그룹으로 나눌 변수
              statistic=all_continuous()~ '{mean} \u00b1 {sd}', # 연속형 변수를 평균, 표준편차로 표시
              missing='no') %>% # 결측치 제거
  add_p(test = c() ~ "t.test") %>% # 연속형 변수는 t-test로 p-value 제시
  add_overall() %>%  # 전체 환자 자료도 포함
  modify_spanning_header( c('stat_1','stat_2')~'**Depression**') %>% 
  modify_caption('**Baseline Characteristics**')

MyTbl

#### Association analysis ####
# Univariable Logistic regression analysis
library(moonBook)

fit.uni <- glm(dep_group2~RIAGENDR,
                 family=binomial,
                 data=data6)

summary(fit.uni)
extractOR(fit.uni)

# Multivariable Logistic regression analysis
fit.multi <- glm(nafld_HSI~oscore2+sex+age+incm+HE_HP+HE_DM+DL+smk+alc+pa_aerobic,
                 family=binomial,
                 data=data4)

summary(fit.multi)
extractOR(fit.multi)
