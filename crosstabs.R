library(data.table)
library(dplyr)

merged_18 <- fread("./data/inter/merged variable selection/merged_18.csv", header = T)
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

merged_18_na <- merged_18[complete.cases(merged_18),]
bucket_18 <- merged_18_na %>%
  select(insem_calf_nulli:insem_calf, age_live:avg_high_scc, OC, PPC) %>%
  mutate_if(., is.numeric, ~as.numeric(Hmisc::cut2(., g =5))) %>%
  relocate(OC, PPC)


tablist_18PPC <- invisible(lapply(bucket_18[,-2], function(x) prop.table(table(x, bucket_18$PPC))))

invisible(list2env(tablist_18PPC,envir = .GlobalEnv))

addtot <- function(x) {
  x <- as.data.frame.matrix(x)
  x["total",] <- colSums(x)
  x[,"total"] <- rowSums(x)
  rownames(x) <- c(1:5, "total")
  colnames(x) <- c(1:5, "total")
  x <- x*100
  return(x)
}

# longevity
age_live <- addtot(age_live)
age_culled <- addtot(age_culled)
age_primi <- addtot(age_primi)
lifeprodn <- addtot(lifeprodn)

#reproduction
iai <- addtot(iai)
alva <- addtot(alva)
calvint <- addtot(calvint)
insem_calf <- addtot(insem_calf)
insem_calf_nulli <- addtot(insem_calf_nulli)

# production
avg_FPCM <- addtot(avg_FPCM)

#SCC
avg_high_scc <- addtot(avg_high_scc)

tablist_18PPC <- lapply(tablist_18PPC, addtot)

lapply(1:length(tablist_18PPC), function(i) write.csv(tablist_18PPC[[i]], 
                                                file = paste0("./results/tables/crosstabsPPC/",names(tablist_18PPC[i]), ".csv")))

tablist_18OC <- invisible(lapply(bucket_18[,-1], function(x) prop.table(table(x, bucket_18$OC))))

invisible(list2env(tablist_18OC,envir = .GlobalEnv))
tablist_18OC <- lapply(tablist_18OC, addtot)
lapply(1:length(tablist_18OC), function(i) write.csv(tablist_18OC[[i]], 
                                                      file = paste0("./results/tables/crosstabsOC/",names(tablist_18OC[i]), ".csv")))

OCage_live <- as.data.frame(tablist_18OC$age_live)
PCage_live <- as.data.frame(tablist_18PPC$age_live)
OCcalvint <- as.data.frame(tablist_18OC$calvint)
PCcalvint <- as.data.frame(tablist_18PPC$calvint)

