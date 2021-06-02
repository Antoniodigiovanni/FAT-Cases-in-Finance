test <- factors
test <- test %>% filter(BM_m != "Inf" & BM_m != "-Inf" & GPA != "Inf" & GPA != "-Inf" & pf.size == "Big") %>% 
  select(Id, country, Date, MV.USD, RET.USD, RET, ym, BM_m, GPA, pf.size)
#test <- test %>% mutate(fac_score = 0.5*BM_m + 0.5*GPA) %>% group_by(ym) %>%
  #summarise(n_obs = n()) %>% arrange(fac_score) %>% drop_na()

test <- test %>% mutate(fac_score = 0.5*BM_m + 0.5*GPA) %>% group_by(ym) %>% arrange(desc(fac_score)) %>% 
  filter(ym > "Dec 1999" & !is.nan(fac_score))

counts <- test %>% summarise(n_obs = n())

test <- merge(test, counts, by="ym")

quint <- test %>% group_by(ym) %>% summarize(top20 = quantile(fac_score, probs = c(0.2), na.rm = T),
                            top40 = quantile(fac_score, probs = c(0.4), na.rm = T),
                            top60 = quantile(fac_score, probs = c(0.6), na.rm = T),
                            top80 = quantile(fac_score, probs = c(0.8), na.rm = T)) %>% 
  select(ym, top20, top40, top60, top80)

test <- merge(test, quint, by = "ym")

test <- data.table(test)

test <- test[ , bucket := ifelse(fac_score>top80, 1,
                        ifelse(fac_score<=top80 & fac_score>top60,2,
                               ifelse(fac_score<=top60 & fac_score>top40,3,
                                      ifelse(fac_score<=top40 & fac_score>top20,4,
                                             ifelse(fac_score<=top20,5,NA)))))]


inv_universe <- test %>% filter(bucket == 1) %>% group_by(ym) %>% arrange(ym, desc(fac_score)) %>% select(-n_obs)
univ_count <- inv_universe %>% summarise(n_obs = n())
inv_universe <- merge(inv_universe, univ_count)


check <- inv_universe %>% group_by(ym) %>%  mutate(weights = 1 / n_obs)
# First equal weights
check <- check %>% mutate(weighted_return = weights*RET)
cum_return <- check %>% group_by(ym) %>% summarise(yearly_ret = sum(weighted_return))

cum_return <- cum_return %>% mutate(Ret = (yearly_ret/100+1)) %>% summarise(Ret = prod(Ret) - 1)

# This wasnt so good now try MV Weighted
total_mv_yearly <- inv_universe %>% group_by(ym) %>% 
  summarise(mv_total = sum(MV.USD))
check <- merge(inv_universe, total_mv_yearly, by = "ym")
# Weights are calculated by MV
check <- check %>% group_by(ym)  %>%  
  mutate(weights = MV.USD / mv_total,
         weights = ifelse(weights < 0.0001, 0, weights),
         weights = weights / sum(weights),
          cum_weights = cumsum(weights),
         weighted_return = weights * RET.USD)

cum_return_vw <- check %>% group_by(ym) %>% summarise(yearly_ret = sum(weighted_return)) %>% drop_na()
cum_return_vw <- cum_return_vw %>% mutate(Ret = (yearly_ret/100+1)) %>% summarise(Ret = prod(Ret) - 1)


# Lets try fac_score weighted
total_fac_score <- inv_universe %>% group_by(ym) %>% summarise(fac_total = sum(fac_score))
check <- merge(inv_universe, total_fac_score, by = "ym")
check <- check %>% group_by(ym)  %>%  
  mutate(weights = fac_score / fac_total,
         weights = ifelse(weights < 0.0001, 0, weights),
         weights = weights / sum(weights),
         cum_weights = cumsum(weights),
         weighted_return = weights * RET.USD)

cum_return_fs <- check %>% group_by(ym) %>% summarise(yearly_ret = sum(weighted_return)) %>% drop_na()
cum_return_fs <- cum_return_fs %>% mutate(Ret = (yearly_ret/100+1)) %>% summarise(Ret = prod(Ret) - 1)

