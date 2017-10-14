library(tidyr)

fra <- read.csv("data/fao-fra.csv")
str(fra)
# summarize by continent
fra_region <- fra %>% 
  group_by(country) %>% 
  filter(sum(!is.na(forest)) == 5, sum(!is.na(nat_for)) == 5) %>% 
  group_by(region, year) %>% 
  group_by(region) %>% 
  # annual forest loss
  mutate(change = 100 * (forest / lag(forest) - 1) / (year - lag(year)),
         nat_change = 100 * (nat_for / lag(nat_for) - 1) / (year - lag(year))) %>% 
  ungroup %>% 
  mutate(region = reorder(factor(region), -change, FUN = max, na.rm = T))
if (!dir.exists("output/")) dir.create("output/")
write.csv(fra_region, "output/fao-fra-region.csv")

# plot natural forest loss
fra_region %>% 
  filter(year != 1990) %>% 
  ggplot(aes(x = year, y = nat_change, color = region)) +
  geom_point() +
  geom_line() + 
  labs(x = "Year", y = "% change in natural forest cover / year") +
  scale_color_brewer(name = "Continent", palette = "Set1")
ggsave('output/forest-change.png')
