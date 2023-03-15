library(tidyverse)

setwd("/.")
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

plot(merged_18$alva~merged_18$age_primi)
table(merged_18$alva > 1500)

merged_18 <- merged_18[merged_18$alva <= 1500,]

library(ggpattern)
graphdt <- merged_18 %>% select(group, PPC, PC, OC, POC) %>% 
  filter(., POC < 1) %>% 
  tidyr::gather(., proportion, value, OC, PC, PPC, POC) %>%
  mutate(proportion = factor(proportion, levels = c("OC", "PC", "PPC", "POC")))
graphdt %>% ggplot(., aes(x = group, y = value, fill = proportion)) + 
  geom_boxplot_pattern(aes(pattern = proportion), 
                       pattern_color = "black",
                       pattern_fill = "white",
                       varwidth = T, outlier.size = 0.5,
                       outlier.alpha = 0.3) +
  theme_classic() + theme(legend.position = "bottom") +
  labs(color = "Proportion Variables") + xlab("Herd size groups") + ylab("Percent/100") + 
  facet_wrap(vars(proportion), nrow = 2, ncol = 2, scales = "free_y")

library(ggcorrplot)

cor1 <- merged_18 %>% select(Age_tot = age_live, Age_culled = age_culled,
                             Life_prodn = lifeprodn,
                             avg_FPCM, services_per_conception_nulli = insem_calf_nulli,
                             services_per_conception = insem_calf,
                             AFC = alva, avg_DIM_first_service = iai, Calv_int = calvint,
                             avg_high_scc) %>% 
  cor(.,use = "complete.obs", method = "spearman")

cor1 %>% ggcorrplot(., hc.order = F, type = "lower", lab = T, 
                    legend.title = "Spearman's rank \n correlation coefficient")

cor2 <- merged_18 %>% select(Age_tot = age_live, Age_culled = age_culled,
                             Life_prodn = lifeprodn,
                             avg_FPCM, services_per_conception_nulli = insem_calf_nulli,
                             services_per_conception = insem_calf,
                             AFC = alva, avg_DIM_first_service = iai, Calv_int = calvint,
                             avg_high_scc, OC, PC, PPC, POC) %>%
  cor(.,use = "complete.obs", method = "spearman")

cor2 <- cor2[11:14,]
cor2 <- cor2[,14:1]

cor2 %>% ggcorrplot(., hc.order = F,  lab = T, 
                    legend.title = "Spearman's rank \n correlation coefficient")



#############scatterplots#####################







merged_18 %>% select(services_per_conception_nulli = insem_calf_nulli,
                     services_per_conception = insem_calf,
                     AFC = alva, avg_DIM_first_service = iai,
                     calv_int = calvint,
                     Age_tot = age_live,
                     Age_culled = age_culled,
                     Life_prodn = lifeprodn,
                     avg_FPCM, Avg_high_SCC = avg_high_scc,
                     OC,PC, POC,
                     PPC) -> scatter_df


par(mfrow = c(4,4))
for (i in 1:length(scatter_df)) {
  plot(scatter_df[,i]~scatter_df$OC, ylab = names(scatter_df[i]), xlab = "OC")
}

par(mfrow = c(4,4))
for (i in 1:length(scatter_df)) {
  plot(scatter_df[,i]~scatter_df$PC, ylab = names(scatter_df[i]), xlab = "PC")
}

par(mfrow = c(4,4))
for (i in 1:length(scatter_df)) {
  plot(scatter_df[,i]~scatter_df$PPC, ylab = names(scatter_df[i]), xlab = "PPC")
}

par(mfrow = c(4,4))
for (i in 1:length(scatter_df)) {
  plot(scatter_df[,i]~scatter_df$POC, ylab = names(scatter_df[i]), xlab = "POC")
}
