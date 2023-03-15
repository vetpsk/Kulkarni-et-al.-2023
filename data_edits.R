library(data.table)
library(dplyr)
library(skimr)

herd_info_1819 <- fread("./data/inter/herd_info_1819.csv", header = T)
prod_1819 <- fread("./data/inter/prod_1819.csv", header = T)
fert_1819 <- fread("./data/inter/fert_1819.csv", header = T)
scc_1819 <- fread("./data/inter/scc_1819.csv", header = T)

prod_1819$test_date <- as.Date(prod_1819$test_date, format = "%Y-%m-%d")
scc_1819$test_date <- as.Date(scc_1819$test_date, format = "%Y-%m-%d")

## filter 1: Avg. Number of tests >= 4; Number of years  active == 2

prod_herd_testing <- prod_1819 %>% group_by(UBN, CRV_year) %>%
  summarise(., test_count = n())

scc_herd_testing <- scc_1819 %>% group_by(UBN, CRV_year_scc) %>%
  summarise(., test_count = n())

intersect(prod_herd_testing, scc_herd_testing)
identical(prod_herd_testing, scc_herd_testing)
all.equal(prod_herd_testing, scc_herd_testing)

diff_testing <- anti_join(prod_herd_testing, scc_herd_testing)

### SCC based testing criteria smaller than production data based testing criteria 
rm(scc_herd_testing, diff_testing)

prod_herd_testing02 <- prod_herd_testing %>% group_by(UBN) %>%
  summarise(., years = n_distinct(CRV_year), avg_count = mean(test_count))

UBN_select <- prod_herd_testing02$UBN[prod_herd_testing02$avg_count >= 4 & prod_herd_testing02$years > 1]

### filtering data by UBN_select

edit1_herd_info_1819 <- setDT(herd_info_1819[herd_info_1819$UBN %in% UBN_select,])
edit1_fert_1819 <- setDT(fert_1819[fert_1819$UBN %in% UBN_select,])
edit1_prod_1819 <- setDT(prod_1819[prod_1819$UBN %in% UBN_select,])
edit1_scc_1819 <- setDT(scc_1819[scc_1819$UBN %in% UBN_select,])


## fwrite edit 1 data
dflist <- mget(ls(pattern = "edit1_"))
lapply(1:length(dflist), 
       function (x) fwrite(dflist[[x]], 
                              file = paste('./data/inter/', names(dflist[x]),'.csv',sep=""), 
                                              row.names = F))

rm(list = ls(all = T))


my_files <- list.files("./data/inter",pattern = "edit1_",full.names = T)
all_csv <- lapply(my_files,fread)
names(all_csv) <- gsub(".csv","",
                       list.files("./data/inter",pattern = "edit1_", full.names = FALSE),
                       fixed = TRUE)
invisible(lapply(names(all_csv), function(x) assign(x,all_csv[[x]],envir=.GlobalEnv)))
rm(all_csv, my_files)


## Select UBNs with >30 avg_live animals (>=25 in any year)



edit2_UBN_select <- edit1_herd_info_1819 %>% select(c(UBN, CRV_year = year, numberlive)) %>% 
  group_by(UBN) %>%
  mutate(avg_numberlive = mean(numberlive), years_active = n_distinct(CRV_year)) %>% 
  ungroup()

edit2_UBN_select <- edit2_UBN_select[edit2_UBN_select$avg_numberlive >= 30 & edit2_UBN_select$numberlive >= 25 & edit2_UBN_select$years_active > 1,]
skim(edit2_UBN_select)

edit2_UBN <- as.character(unique(edit2_UBN_select$UBN))

edit2_UBN_select <- edit2_UBN_select[edit2_UBN_select$UBN %in% edit2_UBN,]
skim(ungroup(edit2_UBN_select))
fwrite(edit2_UBN_select, "./data/inter/edit2_UBN_select.csv", row.names = F)

## filter data with edit2_UBN_select
edit02 <- function(x) {
   x <- x[x$UBN %in% edit2_UBN,]
}


rm(edit2_UBN_select, edit2_UBN)
## edit2 list
edit2_herd_info_1819 <- edit02(edit1_herd_info_1819)
edit2_fert_1819 <- edit02(edit1_fert_1819)
edit2_prod_1819 <- edit02(edit1_prod_1819)
edit2_scc_1819 <- edit02(edit1_scc_1819)
skim(edit2_herd_info_1819)
skim(edit2_fert_1819)
skim(edit2_prod_1819)
skim(edit2_scc_1819)
edit2_list <- mget(ls(pattern = "edit2_"))

lapply(edit2_list, summary)
## write edit2_list dataframes
lapply(1:length(edit2_list), 
       function (x) fwrite(edit2_list[[x]], 
                           file = paste0('./data/inter/', names(edit2_list[x]),'.csv',sep="")))

## differences in edit1 and edit2
edit1_list <- mget(ls(pattern = "edit1_"))
diff_list <- mapply(setdiff, edit1_list,edit2_list)

names(diff_list) <- gsub("edit1_", "diff_", names(diff_list))
diff_list

## intermezzo 
edit2_fert_1819 <- fread("./data/inter/edit2_fert_1819.csv", header = T, sep = ",")
edit2_fert_1819[,c(4,6,8,10,12,14,16)][edit2_fert_1819[,c(4,6,8,10,12,14,16)] == 0] <- NA
edit2_fert_1819[,c(5,7,9,11,13,15,17)][edit2_fert_1819[,c(5,7,9,11,13,15,17)] == -9] <- NA
edit2_fert_1819$nr56[edit2_fert_1819$nr56 < 0] <- NA
edit2_fert_1819$num_nr56[edit2_fert_1819$num_nr56 < 0] <- NA

fwrite(edit2_fert_1819, "./data/inter/edit2_fert_1819.csv", row.names = F)


my_files <- list.files("./data/inter",pattern = "edit2_",full.names = T)
all_csv <- lapply(my_files,fread)
names(all_csv) <- gsub(".csv","",
                       list.files("./data/inter",pattern = "edit2_", full.names = FALSE),
                       fixed = TRUE)
invisible(lapply(names(all_csv), function(x) assign(x,all_csv[[x]],envir=.GlobalEnv)))
rm(my_files, edit2_UBN_select)

lapply(all_csv, summary)

rm(all_csv)

edit3_herd_info_1819 <- edit2_herd_info_1819 %>%
  filter(., numberlive != 0, agedayslive !=0, 
         numberproductiondaysculled != 0, numberproductiondayslive != 0, numbercowscalvingint != 0,
         calvint != 0)
summary(edit3_herd_info_1819)



edit3_herd_info_1819 <- edit3_herd_info_1819 %>%
  filter(., agedaysheifers != 0, fat < 13, protein < 10, fatculled < 13, proteinculled <10)
summary(edit3_herd_info_1819)

edit3_herd_info_1819 <- edit3_herd_info_1819 %>%
  select(-c(ageyearlive,ageyearculled,ageyearheifers))
summary(edit3_herd_info_1819)

fwrite(edit3_herd_info_1819, file = "./data/inter/edit3_herd_info_1819.csv", row.names = F)

summary(edit2_fert_1819)

edit3_prod_1819 <- edit2_prod_1819[edit2_prod_1819$UBN %in% edit3_herd_info_1819$UBN,]
edit3_fert_1819 <- edit2_fert_1819[edit2_fert_1819$UBN %in% edit3_herd_info_1819$UBN,]
edit3_scc_1819 <- edit2_scc_1819[edit2_scc_1819$UBN %in% edit3_herd_info_1819$UBN,]

wfunc <- function(y) {
  lapply(1:length(y), 
         function (x) fwrite(y[[x]], 
                             file = paste0('./data/inter/', names(y[x]),'.csv',sep="")))
}

wfunc(mget(ls(pattern = "edit3_")))


my.files <- list.files("./data/inter", pattern = "edit3_", full.names = T)
all_csv <- lapply(my.files, fread)
names(all_csv) <- gsub(".csv", "", list.files("./data/inter", pattern = "edit3_",
                                              full.names = F), fixed = T)
invisible(lapply(names(all_csv), function(x) assign(x, all_csv[[x]], 
                                                    envir = .GlobalEnv)))
rm(all_csv)
edit3_prod_1819$test_date <- as.Date(edit3_prod_1819$test_date, pattern = "%Y-%m-%d")
edit3_scc_1819$test_date <- as.Date(edit3_scc_1819$test_date, pattern = "%Y-%m-%d")

edit3_prod_1819$year <- year(edit3_prod_1819$test_date)
edit3_scc_1819$year_scc <- year(edit3_scc_1819$test_date)

edit31_prod <- edit3_prod_1819 %>%
  filter(., year!= 2020 )
  
edit31_scc <- edit3_scc_1819 %>%
  filter(., year_scc != 2020)

edit31_list <- mget(ls(pattern = "edit31_"))

edit_crv_prod <- edit3_prod_1819 %>%
  filter(., CRV_year!= 2020 )

edit_crv_scc <- edit3_scc_1819 %>%
  filter(., CRV_year_scc != 2020)

edit_crv_list <- mget(ls(pattern = "edit_crv_"))


lapply(1:length(edit31_list), 
       function(x) fwrite(edit31_list[[x]], 
                          file = paste0("./data/inter/yearly_merged/", names(edit31_list[x]), ".csv", 
                                        sep = "")))

lapply(1:length(edit_crv_list), 
       function(x) fwrite(edit_crv_list[[x]], 
                          file = paste0("./data/inter/yearly_merged/", names(edit_crv_list[x]), ".csv", 
                                        sep = "")))

rm(edit31_list, edit_crv_list)

yearly_test_prod <- edit31_prod %>% group_by(UBN, year) %>%
  summarise(., tests = n_distinct(test_date), avg_animals_control = mean(animals_control),
            sd_animals_control = sd(animals_control), avg_BSK = mean(BSK), sd_BSK = sd(BSK),
            avg_test_milk = mean(testday_milk), tot_test_milk = sum(testday_milk),
            avg_test_fat = mean(testday_fat), avg_test_prot = mean(testday_protein)) %>%
  ungroup()

yearly_test_prod$UBN <- as.factor(yearly_test_prod$UBN)  
yearly_test_prod$year <- as.factor(yearly_test_prod$year)

skim(yearly_test_prod)

yearly_305_prod <- edit31_prod %>% group_by(UBN, year) %>%
  summarise(., tests = n_distinct(test_date), avg_netto = mean(netto), sd_netto = sd(netto),
            avg_milk305 = mean(milk305), avg_fat305 = mean(fat305), 
            avg_prot305 = mean(protein305)) %>% ungroup()
yearly_305_prod$UBN <- as.factor(yearly_305_prod$UBN)  
yearly_305_prod$year <- as.factor(yearly_305_prod$year)

skim(yearly_305_prod)

yearly_scc <- edit31_scc %>% group_by(UBN, year_scc) %>%
  summarise(., tests_scc = n_distinct(test_date), 
            avg_animals_scc_control = mean(animals_SCC_control),
            avg_high_scc = mean(high_SCC), sd_high_scc = sd(high_SCC),
            max_high_scc = max(high_SCC), 
            avg_new_inf = mean(new_inf), sd_new_inf = sd(new_inf),
            max_new_inf = max(new_inf),
            avg_scc = mean(SCC), sd_scc = sd(SCC), mac_scc = max(SCC)) %>%
  ungroup()

yearly_scc$UBN <- as.factor(yearly_scc$UBN)  
yearly_scc$year_scc <- as.factor(yearly_scc$year_scc)
skim(yearly_scc)

yearly_list <- mget(ls(pattern = "yearly_"))

lapply(1:length(yearly_list), function(x) fwrite(yearly_list[[x]],
                                                file = paste0("./data/inter/yearly_merged/", names(yearly_list[x]), ".csv", sep = "")))





CRV_yearly_test_prod <- edit31_prod %>% group_by(UBN, CRV_year) %>%
  summarise(., tests = n_distinct(test_date), avg_animals_control = mean(animals_control),
            sd_animals_control = sd(animals_control), avg_BSK = mean(BSK), sd_BSK = sd(BSK),
            avg_test_milk = mean(testday_milk), tot_test_milk = sum(testday_milk),
            avg_test_fat = mean(testday_fat), avg_test_prot = mean(testday_protein)) %>%
  ungroup()

CRV_yearly_test_prod$UBN <- as.factor(CRV_yearly_test_prod$UBN)  
CRV_yearly_test_prod$CRV_year <- as.factor(CRV_yearly_test_prod$CRV_year)

skim(CRV_yearly_test_prod)

CRV_yearly_305_prod <- edit31_prod %>% group_by(UBN, CRV_year) %>%
  summarise(., tests = n_distinct(test_date), avg_netto = mean(netto), sd_netto = sd(netto),
            avg_milk305 = mean(milk305), avg_fat305 = mean(fat305), 
            avg_prot305 = mean(protein305)) %>% ungroup()
CRV_yearly_305_prod$UBN <- as.factor(CRV_yearly_305_prod$UBN)  
CRV_yearly_305_prod$CRV_year <- as.factor(CRV_yearly_305_prod$CRV_year)

skim(CRV_yearly_305_prod)

CRV_yearly_scc <- edit31_scc %>% group_by(UBN, CRV_year_scc) %>%
  summarise(., tests_scc = n_distinct(test_date), 
            avg_animals_scc_control = mean(animals_SCC_control),
            avg_high_scc = mean(high_SCC), sd_high_scc = sd(high_SCC),
            max_high_scc = max(high_SCC), 
            avg_new_inf = mean(new_inf), sd_new_inf = sd(new_inf),
            max_new_inf = max(new_inf),
            avg_scc = mean(SCC), sd_scc = sd(SCC), mac_scc = max(SCC)) %>%
  ungroup()

CRV_yearly_scc$UBN <- as.factor(CRV_yearly_scc$UBN)  
CRV_yearly_scc$CRV_year_scc <- as.factor(CRV_yearly_scc$CRV_year_scc)
skim(CRV_yearly_scc)

CRV_yearly_list <- mget(ls(pattern = "CRV_yearly_"))

lapply(1:length(CRV_yearly_list), function(x) fwrite(CRV_yearly_list[[x]],
                                                 file = paste0("./data/inter/yearly_merged/", names(CRV_yearly_list[x]), ".csv", sep = "")))


my_files <- list.files("./data/inter",pattern = "edit3_",full.names = T)
all_csv <- lapply(my_files,fread)
names(all_csv) <- gsub(".csv","",
                       list.files("./data/inter",pattern = "edit3_", full.names = FALSE),
                       fixed = TRUE)
invisible(lapply(names(all_csv), function(x) assign(x,all_csv[[x]],envir=.GlobalEnv)))
rm(all_csv, my_files)

df_list <- mget(ls(pattern = "edit3_"))

rm(df_list)

my_files <- list.files("./data/inter/yearly_merged",pattern = "yearly_",full.names = T)
all_csv <- lapply(my_files,fread)
names(all_csv) <- gsub(".csv","",
                       list.files("./data/inter/yearly_merged",pattern = "yearly_", full.names = FALSE),
                       fixed = TRUE)
invisible(lapply(names(all_csv), function(x) assign(x,all_csv[[x]],envir=.GlobalEnv)))
rm(all_csv, my_files)

df_list <- mget(ls())

rm(df_list)
rm(list = ls(pattern = "CRV_yearly_"))

rm(edit3_prod_1819, edit3_scc_1819, summary_read_csv)
colnames(yearly_scc)[2] <- "year"

edit4_nulli_fert <- edit3_fert_1819 %>% filter(., herd == 1) %>%
  select(UBN, year, num_nr56_nulli = num_nr56, nr56_nulli = nr56, 
         num_igi = num_iai_igi, igi = iai_igi, num_first_insem_calf_nulli = num_first_insem_calf,
         first_insem_calf_nulli = first_insem_calf, num_insem_calf_nulli = num_insem_calf,
         insem_calf_nulli = insem_calf, num_insem_nulli = num_insem, insem_nulli = insem,
         num_alva = num_lft_alva, alva = lft_alva) %>% ungroup()

edit4_multi_fert <- edit3_fert_1819 %>% filter(., herd == 2) %>%
  select(UBN, year, num_nr56, nr56, num_iai = num_iai_igi, iai = iai_igi, 
         num_tkt, tkt, num_first_insem_calf,
         first_insem_calf, num_insem_calf, insem_calf, num_insem, insem, num_lft = num_lft_alva,
         lft = lft_alva) %>% ungroup()

df_list <- list(edit4_nulli_fert, edit4_multi_fert, yearly_test_prod, yearly_scc, yearly_305_prod)
lapply(1:length(df_list), function(x) print(skim(df_list[[x]])))


merge_fert_prod_scc <- data.table(Reduce(function(x, y) inner_join(x,y, by = c("UBN", "year")), df_list))

merge_fert_prod_scc$tests.y <- NULL
colnames(merge_fert_prod_scc)[29] <- "tests_prod"
str(merge_fert_prod_scc)

fwrite(merge_fert_prod_scc, "./data/inter/yearly_merged/merged_fert_prod_scc.csv", row.names = F)
df_list <- mget(ls(pattern = "edit4_"))
lapply(1:length(df_list), 
       function(x) fwrite(df_list[[x]], 
                          file = paste0("./data/inter/", 
                                        names(df_list[x]), ".csv", 
                                                           sep = "")))
                                                           
merge_fert_prod_scc$UBN <- as.factor(merge_fert_prod_scc$UBN)
merge_fert_prod_scc$year <- as.factor(merge_fert_prod_scc$year)
merge_fert_prod_scc %>% skim() %>% partition()

merged_df <- fread("./data/inter/yearly_merged/merged_fert_prod_scc.csv", header = T)

edit3_herd_info <- fread("./data/inter/edit3_herd_info_1819.csv", header = T)

colnames(edit3_herd_info)[7:8] <- c("fatlive", "proteinlive")

edit4_merged <- edit3_herd_info %>% inner_join(., merged_df, by = c("UBN", "year"))
str(edit4_merged)
edit4_merged$UBN <- as.factor(edit4_merged$UBN)
edit4_merged$year <- as.factor(edit4_merged$year)
edit4_merged$tests_prod <- as.factor(edit4_merged$tests_prod)
edit4_merged$tests_scc <- as.factor(edit4_merged$tests_scc)
edit4_merged %>% skim() %>% partition()

fwrite(edit4_merged, "./data/inter/yearly_merged/edit4_merged.csv", row.names = F)

