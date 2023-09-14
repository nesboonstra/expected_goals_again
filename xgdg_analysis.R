library(tidyverse)


#### Plots ####

xg_pts_plot <- ggplot(aggregate,aes(xg,pts)) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_linedraw() +
  ylim(0,110) +
  labs(
    title = "Expected Goals (xG) and Points since 2017-18",
    x = "Expected Goals (xG)",
    y = "Points"
  )

dg_pts_plot <- ggplot(aggregate,aes(dg,pts)) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_linedraw() +
  ylim(0,110) +
  labs(
    title = "G-xG and Points since 2017-18",
    x = "G-xG",
    y = "Points"
  )

xg_rank_plot <- ggplot(aggregate,aes(rank,xg)) +
  geom_point()

dg_rank_plot <- ggplot(aggregate,aes(rank,dg)) +
  geom_point()

xg_dg_plot <- ggplot(aggregate,aes(xg,dg)) +
  geom_point()


#### Regressions ####

goals_pts_lm <- lm(pts ~ gf + ga, data = aggregate)
goals_pts_smm <- summary(goals_pts_lm)

xg_pts_lm <- lm(pts ~ xg + xga, data = aggregate)
xg_pts_smm <- summary(xg_pts_lm)

dg_pts_lm <- lm(pts ~ dg + da, data = aggregate)
dg_pts_smm <- summary(dg_pts_lm)

xplus_pts_lm <- lm(pts ~ gf + ga + xg + xga, data = aggregate)
xplus_pts_smm <- summary(xplus_pts_lm)

dplus_pts_lm <- lm(pts ~ gf + ga + dg + da, data = aggregate)
dplus_pts_smm <- summary(dplus_pts_lm)

bagel_lm <- lm(pts ~ gf + ga + xg + xga + dg + da, data = aggregate)
bagel_smm <- summary(bagel_lm) # autocorrelation... right?

