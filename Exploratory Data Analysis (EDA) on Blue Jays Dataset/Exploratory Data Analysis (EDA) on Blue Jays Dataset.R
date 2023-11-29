# Shah Wali Ullah Baig

install.packages("readxl")
install.packages("readr")
library(readr)
library(ggplot2)

blue_jays <- read_csv('blue_jays.csv',show_col_types = FALSE)
head(blue_jays, 5)

# <--------------------------------- Question-01 --------------------------------->

ggplot(blue_jays, aes(body_mass_g, head_length_mm)) +
  geom_point(alpha = 1/2, size = 1.66) +
  geom_density_2d(
    binwidth = 0.004
    ,color = "black"
    ,size = 0.39) +xlim(57, 82) +
  labs(
    x = "Body Mass (g)", y = "Head length (mm)") + theme_minimal()

# <--------------------------------- Question-02 --------------------------------->

ggplot(blue_jays, aes(body_mass_g, head_length_mm )) +
  geom_point(alpha = 1/2, size = 1.66) +
  geom_density_2d_filled(
    binwidth = 0.004
    ,size = 0.39 , color = "Red") +
  xlim(57, 82) +
  labs(x = "Body Mass (g)" ,y = "Head length (mm)") + theme_minimal()
 
# <--------------------------------- Question-03 --------------------------------->

# 1st: 

ggplot(blue_jays, 
       aes(body_mass_g ,head_length_mm, 
           fill = `Positive` ,color = sex)) +
  geom_point(alpha = 1/2, size = 1.66) +
  geom_density_2d(
    binwidth = 0.004
    , size = 0.39) +
  xlim(57, 82) +
  labs(
    x = "Body Mass (g)"
    ,y = "Head length (mm)") 

# 2nd:

ggplot(blue_jays, 
       aes(body_mass_g, head_length_mm)) +
  geom_point(alpha = 1/2, size = 1.66) +
  geom_density_2d(
    binwidth = 0.004
    , color = "black"
    , size = 0.39) +
  xlim(57, 82) +
  labs(
    x = "Body Mass (g)"
    , y = "Head length (mm)") +
  theme_minimal()+
  facet_wrap(~ sex)

# <--------------------------------- Question-04 --------------------------------->

# 1st:

ggplot(blue_jays, aes(body_mass_g, head_length_mm)) + 
  geom_smooth(method = "lm", se = TRUE, col = "Red") +
  geom_point(size = 2, col = "orange") + 
  labs(x = "Body Mass (g)", y = "Head Length (mm)") + 
  theme_classic()

# 2nd:

ggplot(blue_jays, 
       aes(
         body_mass_g, head_length_mm
         , color = sex
       )) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(
    method = "lm"
    , formula = y~x
    , se = FALSE
    , fullrange = TRUE 
  ) + geom_smooth(method = lm)


