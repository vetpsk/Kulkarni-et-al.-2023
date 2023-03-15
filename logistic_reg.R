library(tidyverse)

setwd("/Users/pranavkulkarni/OneDrive - Universiteit Utrecht/Paper 2/risk-of-replacement/data/inter/merged variable selection")
merged_18 <- read.csv("./merged_18.csv", header = T)
merged_18$UBN <- as.character(merged_18$UBN)

merged_18 <- merged_18 %>% mutate(PPC = primi_culled_CRV_year/n_primi,
                                  OC = n_culled/n_live,
                                  PC = primi_culled_CRV_year/n_live,
                                  POC = primi_culled_CRV_year/n_culled)

merged_18 <- merged_18 %>% filter(., between(OC, 0.136, 0.478)) %>%
  filter(., between(PPC, 0, 1))
merged_18$group[merged_18$group == "151-788"] <- "151-500"
merged_18$group <- factor(merged_18$group, levels = c("31-50",
                                                      "51-70",
                                                      "71-90",
                                                      "91-110",
                                                      "111-150",
                                                      "151-500"))

merged_18$primi_culled_CRV_year <- NULL
merged_18$primi_culled_year <- NULL

merged_18$UBN <- as.factor(merged_18$UBN)
merged_18 <- merged_18 %>% relocate(n_primi, n_live) %>% mutate(across(insem_calf_nulli:avg_high_scc, scale))

glmPPC <- glm(formula = 
                PPC~insem_calf_nulli + alva + iai + calvint + insem_calf +
                age_live + age_culled + lifeprodn +  
                avg_FPCM + avg_high_scc, 
              family = binomial, 
              weights = n_primi,
              data = merged_18, 
              na.action = na.omit, 
              # control = list(maxit = 1000)
              )
summary(glmPPC)

glmOC <- glm(formula = 
                OC~insem_calf_nulli + alva + iai + calvint + insem_calf +
                age_live + age_culled + lifeprodn +  
                avg_FPCM + avg_high_scc, 
              family = binomial, weights = n_live,
              data = merged_18, 
              na.action = na.omit
)
summary(glmOC)

a <- broom::tidy(glmPPC,exponentiate = T, conf.int = T)
a <- cbind(a$term,round(a[,2:7], digits = 3))

b <- broom::tidy(glmOC,exponentiate = T, conf.int = T)
b <- cbind(b$term,round(b[,2:7], digits = 3))

View(b[,c(1,2,6,7,5)])
View(a[,c(1,2,6,7,5)])
