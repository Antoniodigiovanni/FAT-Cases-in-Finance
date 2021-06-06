View(VW_Factor_Portfolio)

value_port <- vw_port %>% filter(weights > 0) %>% select(ym, Id, country, LMV.USD, RET.USD, RET,
                                                         pf.size, weights)


load(file.path("Data","FAT_monthly.RData"))
load(file.path("Data","FAT_static.RData"))

# Check country distribution of value weighted portfolio
n_country <- value_port %>% group_by(country) %>% summarise(sum_weights = sum(weights))
value_port

# Compare this to overall sample
n_country_full_sample <- FAT.monthly %>% group_by(country) %>% summarise(n_obs = n())

ggplot(data = n_country,aes(x = country, y = n_obs)) +
  geom_bar(stat="identity")

# SGP is a little bit underrepresented in our value weighted portfolio
# Everything else is quite okay

# Sector distribution
value_port <- merge(value_port, FAT.static, by = c("Id", "country"))
value_port <- value_port %>% select(ym, Id, country, LMV.USD, RET.USD, RET,
                                    pf.size, weights, NAME, PNAME, INDM)

n_sectors <- value_port %>% group_by(INDM) %>% summarise(sum_weights = sum(weights))
ggplot(data = n_sectors, aes(x = INDM, y = n_obs)) + 
  geom_bar(stat="identity")

n_sectors_weighted <- FAT.monthly
n_sectors_weighted <- merge(all_data, FAT.static, by = "Id")
#n_sectors_weighted <- merge(all_data, FAT.static, by = c("Id", "country"))
n_sectors_weighted <- n_sectors_weighted %>%  select(Id, INDM, MV.USD) %>% 
  mutate(MV.Total = sum(MV.USD, na.rm = T))
n_sectors_weighted <- n_sectors_weighted %>% mutate(weights = MV.USD/MV.Total) %>% drop_na()
n_sectors_weighted <- n_sectors_weighted %>% group_by(INDM) %>% summarise(weighted_obs = sum(weights))
sum(n_sectors_weighted$weighted_obs)
