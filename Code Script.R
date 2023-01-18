library(tidyverse)

library(readr)
data <- read_csv("C:/Users/reshi/Downloads/HS115_members.csv")

ggplot() + 
  geom_point(mapping = aes(x = nokken_poole_dim1, y = nokken_poole_dim2), data = data)

data %>% 
  select(nokken_poole_dim1, nokken_poole_dim2) %>% 
  group_by(data$party_code) %>%
  ggplot(aes(x = nokken_poole_dim1, y = nokken_poole_dim2,
             color = as.factor(data$party_code))) +
  labs(x = 'N-P Dimension 1', y = 'N-P Dimension 2', color = 'Party Affiliation' ) +
  geom_point() +
  scale_color_manual(values=c("Blue", "Red", "Gray"), 
                     labels = c('Democrat', 'Republican', 'Independent'))

  