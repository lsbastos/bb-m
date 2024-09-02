library(tidyverse)
library(INLA)
source("code/sprint_fun.R")


dengue <- read_csv("~/Dropbox/Research/Mosqlimate/Sprint/data/dengue.csv.gz")

# selufs <- c('AM', 'CE', 'GO', 'GO', 'MG', 'PR')
selufs <- sort(unique(dengue$uf))

dengue <- dengue %>% 
  filter(uf %in% selufs)

dengue.tbl <- dengue %>% 
  group_by(uf, macroregional_geocode, macroregional) %>% tally() %>% 
  group_by(uf) %>% 
  mutate( n = n())

# For reproducibility purposes
set.seed(123456)

# Macroregioes ------------------------------------------------------------



# Criando as listas

macros <- unique(dengue$macroregional_geocode)

list.train.1 = vector(mode = "list", length = length(macros))
names(list.train.1) = macros

list.train.2 = list.train.1

#k = 1
for(k in 1:length(macros)){
  # Training 1
  data.train1.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_1 == T | target_1 == T)
      # (train_2 == T | target_2 == T) 
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf) %>% 
    summarise(
      cases = sum(casos),
      train = train_1[1], target = target_1[1], .groups = "drop"
      # train = train_2[1], target = target_2[1]
    ) 
  
  data.train2.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_2 == T | target_2 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf) %>% 
    summarise(
      cases = sum(casos),
      train = train_2[1], target = target_2[1], .groups = "drop"
    ) 
  
  
  
  aux = forecasting.inla(dados = data.train1.macro.k %>% 
                           filter(Date >= "2015-10-11"), 
                         MC =T)
  
  aux$pred$uf = data.train1.macro.k$uf[1]
  aux$pred$macrocode = data.train1.macro.k$macroregional_geocode[1]
  
  aux$MC$uf = data.train1.macro.k$uf[1]
  aux$MC$macrocode = data.train1.macro.k$macroregional_geocode[1]
  
  list.train.1[[k]]$out <- aux
  
  aux2 <- forecasting.inla(dados = data.train2.macro.k %>% 
                             filter(Date >= "2015-10-11"), 
                           MC =T)
  aux2$pred$uf = data.train2.macro.k$uf[1]
  aux2$pred$macrocode = data.train2.macro.k$macroregional_geocode[1]
  
  aux2$MC$uf = data.train2.macro.k$uf[1]
  aux2$MC$macrocode = data.train2.macro.k$macroregional_geocode[1]
  
  list.train.2[[k]]$out <- aux2
  
  cat(k, data.train1.macro.k$uf[1] , data.train1.macro.k$macroregional_geocode[1], "\n")
  
}


df.train.1 <- list.train.1 %>% map(function(x) x$out$MC) %>% bind_rows()
df.train.2 <- list.train.2 %>% map(function(x) x$out$MC) %>% bind_rows()


dt_train1 = tibble(date = unique(dengue$date[dengue$target_1==T])) %>% 
  mutate(week = week.season(date))

dt_train2 = tibble( date = unique(dengue$date[dengue$target_2==T])) %>% 
  mutate(week = week.season(date))

# save.image(file = "~/Desktop/sprint.RData")
# saveRDS(df.train.1, file = "~/Desktop/train1.rds")
# saveRDS(df.train.2, file = "~/Desktop/train2.rds")

# Training 1

tbl.total.uf.train1 <- df.train.1 %>%
  group_by(uf, samples) %>%
  summarise(
    values = sum(values)
  ) %>% group_by(uf) %>%
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05),
    lu = quantile(values, probs = 0.95),
  ) %>% left_join(
    dengue %>%
      filter(target_1 == T) %>%
      group_by(uf) %>%
      summarise(cases = sum(casos)), by = "uf"
  ) #%>%
#   bind_rows(
#     tibble(uf = "BR") %>% bind_cols(df.train.1 %>% 
#                                       group_by(samples) %>% 
#                                       summarise(
#                                         values = sum(values)
#                                       ) %>% #group_by(uf) %>% 
#                                       summarise(
#                                         est = median(values),
#                                         li = quantile(values, probs = 0.05),
#                                         lu = quantile(values, probs = 0.95),
#                                       ),
#                                     dengue %>% 
#                                       filter(target_1 == T) %>% 
#                                       # group_by(uf) %>% 
#                                       summarise(cases = sum(casos))
#     )
#   )


tbl.total.uf.week.train1 <- df.train.1 %>% 
  group_by(uf, week, samples) %>% 
  summarise(
    values = sum(values)
  ) %>% group_by(uf, week) %>% 
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05),
    lu = quantile(values, probs = 0.95),
  ) %>% left_join(
    dengue %>% 
      filter(target_1 == T) %>% 
      mutate(
        week = week.season(date)
      ) %>% 
      group_by(uf, week) %>% 
      summarise(cases = sum(casos)), by = c("uf", "week")
  ) %>% 
  left_join(dt_train1)
  # bind_rows(
  #   tibble(uf = "BR") %>% 
  #     bind_cols(df.train.1 %>% 
  #                 group_by(week, samples) %>% 
  #                 summarise(
  #                   values = sum(values)
  #                 ) %>% group_by(week) %>% 
  #                 summarise(
  #                   est = median(values),
  #                   li = quantile(values, probs = 0.05),
  #                   lu = quantile(values, probs = 0.95),
  #                 ) %>% left_join(
  #                   dengue %>% 
  #                     filter(target_1 == T) %>% 
  #                     mutate(
  #                       week = week.season(date)
  #                     ) %>% 
  #                     group_by(week) %>%
  #                     summarise(cases = sum(casos)), by = "week"
  #                 )
  #     )
  # )




tbl.total.uf.train2 <- df.train.2 %>%
  group_by(uf, samples) %>%
  summarise(
    values = sum(values)
  ) %>% group_by(uf) %>%
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05),
    lu = quantile(values, probs = 0.95),
  ) %>% left_join(
    dengue %>%
      filter(target_2 == T) %>%
      group_by(uf) %>%
      summarise(cases = sum(casos)), by = "uf"
  ) #%>%
#   bind_rows(
#     tibble(uf = "BR") %>% bind_cols(df.train.2 %>% 
#                                       group_by(samples) %>% 
#                                       summarise(
#                                         values = sum(values)
#                                       ) %>% #group_by(uf) %>% 
#                                       summarise(
#                                         est = median(values),
#                                         li = quantile(values, probs = 0.05),
#                                         lu = quantile(values, probs = 0.95),
#                                       ),
#                                     dengue %>% 
#                                       filter(target_2 == T) %>% 
#                                       # group_by(uf) %>% 
#                                       summarise(cases = sum(casos))
#     )
#   )


tbl.total.uf.week.train2 <- df.train.2 %>% 
  group_by(uf, week, samples) %>% 
  summarise(
    values = sum(values)
  ) %>% group_by(uf, week) %>% 
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05),
    lu = quantile(values, probs = 0.95),
  ) %>% left_join(
    dengue %>% 
      filter(target_2 == T) %>% 
      mutate(
        week = week.season(date)
      ) %>% 
      group_by(uf, week) %>% 
      summarise(cases = sum(casos)), by = c("uf", "week")
  ) %>%  
  left_join(dt_train2)
# bind_rows(
  #   tibble(uf = "BR") %>% 
  #     bind_cols(df.train.2 %>% 
  #                 group_by(week, samples) %>% 
  #                 summarise(
  #                   values = sum(values)
  #                 ) %>% group_by(week) %>% 
  #                 summarise(
  #                   est = median(values),
  #                   li = quantile(values, probs = 0.05),
  #                   lu = quantile(values, probs = 0.95),
  #                 ) %>% left_join(
  #                   dengue %>% 
  #                     filter(target_2 == T) %>% 
  #                     mutate(
  #                       week = week.season(date)
  #                     ) %>% 
  #                     group_by(week) %>%
  #                     summarise(cases = sum(casos)), by = "week"
  #                 )
  #     )
  # )


tbl.total.uf.week.train2 <- df.train.2 %>% 
  group_by(uf, week, samples) %>% 
  summarise(
    values = sum(values)
  ) %>% group_by(uf, week) %>% 
  summarise(
    est = median(values),
    li = quantile(values, probs = 0.05),
    lu = quantile(values, probs = 0.95),
  ) %>% left_join(
    dengue %>% 
      filter(target_2 == T) %>% 
      mutate(
        week = week.season(date)
      ) %>% 
      group_by(uf, week) %>% 
      summarise(cases = sum(casos)), by = c("uf", "week")
  ) %>%  
  left_join(dt_train2)


# write_csv(tbl.total.uf.train1, file = "~/Desktop/tbl.total.uf.train1.csv")
# write_csv(tbl.total.uf.train2, file = "~/Desktop/tbl.total.uf.train2.csv")
write_csv(tbl.total.uf.week.train1, file = "~/Desktop/tbl.total.uf.week.train1.csv")
write_csv(tbl.total.uf.week.train2, file = "~/Desktop/tbl.total.uf.week.train2.csv")


tbl.total.uf.train1 %>% 
  ggplot(aes(y = uf)) + 
  geom_pointrange(aes(x = est, xmin = li, xmax = lu, color = "Estimates")) +
  geom_point(aes(x = cases, color = "Observed")) + 
  scale_x_log10() + 
  theme_bw()

tbl.total.uf.train2 %>% 
  ggplot(aes(y = uf)) + 
  geom_pointrange(aes(x = est, xmin = li, xmax = lu, color = "Estimates")) +
  geom_point(aes(x = cases, color = "Observed")) + 
  scale_x_log10() + 
  theme_bw()

