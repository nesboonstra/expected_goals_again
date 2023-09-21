library(tidyverse)

aggregate_dom <- aggregate %>% 
  filter(lig!=c(8,19))

#### Plot(s) ####

xg_gf_per90_plot <- ggplot(aggregate,aes(x=xg_per90,y=gf_per90,color=pts_per90)) +
  geom_point() +
  geom_smooth(method = lm, se= F, color = "black") +
  scale_color_gradient(low="blue", high="green") +
  coord_fixed() +
  theme_linedraw() +
  labs(
    title = "Relationship between Expected Goals, Goals Scored, & Points",
    x = "Expected Goals (xG) per 90",
    y = "Goals Scored per 90",
    color = "Points per 90",
    caption = "Data collected from fbref.com for teams in Europe's Big 5, the UCL & the UEL across 2017-18 to 2022-23 seasons"
  )

xg_gf_per90_dom_plot <- ggplot(aggregate_dom,aes(x=xg_per90,y=gf_per90,color=pts_per90)) +
  geom_point() +
  geom_smooth(method = lm, se= F, color = "black") +
  scale_color_gradient(low="blue", high="green") +
  coord_fixed() +
  theme_linedraw() +
  labs(
    title = "Relationship between Expected Goals, Goals Scored, & Points",
    x = "Expected Goals (xG) per 90",
    y = "Goals Scored per 90",
    color = "Points per 90",
    caption = "Data collected from fbref.com for teams in Europe's Big 5 domestic leagues across 2017-18 to 2022-23 seasons"
  )

#### Regression(s) ####

xg_gf_per90_lm <- lm(gf_per90 ~ xg_per90, data = aggregate)
xg_gf_per90_smm <- summary(xg_gf_per90_lm)

xg_gf_per90_dom_lm <- lm(gf_per90 ~ xg_per90, data = aggregate_dom)
xg_gf_per90_dom_smm <- summary(xg_gf_per90_dom_lm)

