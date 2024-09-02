library(tidyverse)
library(INLA)
source("code/sprint_fun.R")


dengue <- read_csv("~/Dropbox/Research/Mosqlimate/Sprint/data/dengue.csv.gz")
dengue24 <- read_csv("~/Dropbox/Research/Mosqlimate/Sprint/data/dengue_19ago2024.csv.gz")

# dengue %>% 
#   filter(year == 2024) %>% tail()
# 
# dengue24 %>% tail()
dengue <- dengue %>% 
  filter(year < 2024) %>% 
  bind_rows(dengue24[,-1])

# selufs <- c('AM', 'CE', 'GO', 'GO', 'MG', 'PR')

# dengue <- dengue %>% 
#   filter(uf %in% selufs)

dengue.tbl <- dengue %>% 
  group_by(uf, macroregional_geocode, macroregional) %>% tally() %>% 
  group_by(uf) %>% 
  mutate( n = n())

# For reproducibility purposes
set.seed(123456)

# Macroregioes ------------------------------------------------------------



# Criando as listas

macros <- unique(dengue$macroregional_geocode)

list.train.3 = vector(mode = "list", length = length(macros))
names(list.train.3) = macros


k = 1
for(k in 1:length(macros)){
  # Training 3
  data.train3.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k]
      # (train_2 == T | target_2 == T) 
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf) %>% 
    summarise(
      cases = sum(casos),
      train = T, target = F, .groups = "drop"
      # train = train_2[1], target = target_2[1]
    ) 
  
  aux.train3 <- tibble(Date = seq(from = ymd("2024-10-06"), by = 7, length.out = 52), 
                       train = F, target = T,
                       macroregional_geocode = data.train3.macro.k$macroregional_geocode[1],
                       uf = data.train3.macro.k$uf[1]
  )
  
  
  aux = forecasting.inla(dados = data.train3.macro.k %>% 
                           filter(Date >= "2015-10-11") %>% 
                         bind_rows(aux.train3), 
                         MC =T)
  
  aux$pred$uf = data.train3.macro.k$uf[1]
  aux$pred$macrocode = data.train3.macro.k$macroregional_geocode[1]
  
  aux$MC$uf = data.train3.macro.k$uf[1]
  aux$MC$macrocode = data.train3.macro.k$macroregional_geocode[1]
  
  list.train.3[[k]]$out <- aux
  
  cat(k, data.train3.macro.k$uf[1] , data.train3.macro.k$macroregional_geocode[1], "\n")
  
}


df.train.3 <- list.train.3 %>% map(function(x) x$out$MC) %>% bind_rows()


dt_train3 = tibble(date = seq(from = ymd("2024-10-06"), by = 7, length.out = 52)) %>% 
  mutate(week = week.season(date))


df.train.3 %>% left_join(dt_train3) %>% head()
# save.image(file = "~/Desktop/sprint.RData")
# saveRDS(df.train.1, file = "~/Desktop/train1.rds")
# saveRDS(df.train.2, file = "~/Desktop/train2.rds")
saveRDS(df.train.3 %>% left_join(dt_train3), file = "~/Desktop/samples_macro_week_2024_2025.rds")

# Training 3

tbl.total.uf.train3 <- df.train.3 %>%
  group_by(uf, samples) %>%
  summarise(
    values = sum(values)
  ) %>% group_by(uf) %>%
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05),
    ls = quantile(values, probs = 0.95),
  ) 


tbl.total.uf.week.train3 <- df.train.3 %>% 
  group_by(uf, week, samples) %>% 
  summarise(
    values = sum(values)
  ) %>% group_by(uf, week) %>% 
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05) %>% round(),
    ls = quantile(values, probs = 0.95)  %>% round(),
  ) %>% 
  left_join(dt_train3)




# write_csv(tbl.total.uf.train1, file = "~/Desktop/tbl.total.uf.train1.csv")
# write_csv(tbl.total.uf.train2, file = "~/Desktop/tbl.total.uf.train2.csv")
# write_csv(tbl.total.uf.week.train1, file = "~/Desktop/tbl.total.uf.week.train1.csv")
# write_csv(tbl.total.uf.week.train2, file = "~/Desktop/tbl.total.uf.week.train2.csv")
# write_csv(tbl.total.uf.week.train3 %>% select(uf, week, date, est, li, ls), file = "~/Desktop/tbl.total.uf.week.train3.csv")
