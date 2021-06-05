View(VW_Factor_Portfolio)

value_port <- vw_port %>% filter(weights > 0) %>% select(ym, Id, country, LMV.USD, RET.USD, RET,
                                                         pf.size, weights)


load(file.path("Data","FAT_static.RData"))

# Check country distribution of value weighted portfolio
n_country <- value_port %>% group_by(country) %>% summarise(n_obs = n())

# Compare this to overall sample
n_country_full_sample <- all_data %>% group_by(country) %>% summarise(n_obs = n())

ggplot(data = n_country,aes(x = country, y = n_obs)) +
  geom_bar(stat="identity")

# SGP is a little bit underrepresented in our value weighted portfolio
# Everything else is quite okay

# Sector distribution
value_port <- merge(value_port, FAT.static, by = c("Id", "country"))
value_port <- value_port %>% select(ym, Id, country, LMV.USD, RET.USD, RET,
                                    pf.size, weights, NAME, PNAME, INDM)

n_sectors <- value_port %>% group_by(INDM) %>% summarise(n_obs = n())
ggplot(data = n_sectors, aes(x = INDM, y = n_obs)) + 
  geom_bar(stat="identity")


