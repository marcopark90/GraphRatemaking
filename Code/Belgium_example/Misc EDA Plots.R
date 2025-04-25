library(ggplot2)
library(gridExtra)
str(modeling_data)

p1 <- modeling_data %>%
  filter(amount > 0) %>%
  ggplot(aes(x = log(loss_cost))) +
  geom_density()
ggsave(
  "./Graphs/claim_density.png",
  p1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

p1 <- modeling_data %>%
  group_by(agec) %>%
  summarise(losscost = mean(log(loss_cost + 1))) %>%
  ggplot(aes(agec, losscost)) +
  geom_line(linewidth = 1)
p2 <- modeling_data %>%
  group_by(ageph) %>%
  summarise(losscost = mean(log(loss_cost + 1))) %>%
  ggplot(aes(ageph, losscost)) +
  geom_line(linewidth = 1)
p3 <- modeling_data %>%
  group_by(bm) %>%
  summarise(losscost = mean(log(loss_cost + 1))) %>%
  ggplot(aes(bm, losscost)) +
  geom_line(linewidth = 1)
p4 <- modeling_data %>%
  group_by(power) %>%
  summarise(losscost = mean(log(loss_cost + 1))) %>%
  ggplot(aes(power, losscost)) +
  geom_line(linewidth = 1)

p5 <- grid.arrange(p1, p2, p3, p4, nrow = 2)
ggsave(
  "./Graphs/continuous_vars.png",
  p5,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

#modeling_data %>% group_by(coverage) %>% filter(amount>0) %>% ggplot(aes(coverage,log(loss_cost))) + geom_boxplot()
p1 <- modeling_data %>%
  group_by(coverage) %>%
  filter(amount > 0) %>%
  ggplot(aes(coverage, log(loss_cost))) +
  geom_boxplot()
p2 <- modeling_data %>%
  group_by(fuel) %>%
  filter(amount > 0) %>%
  ggplot(aes(fuel, log(loss_cost))) +
  geom_boxplot()
p3 <- modeling_data %>%
  group_by(use) %>%
  filter(amount > 0) %>%
  ggplot(aes(use, log(loss_cost))) +
  geom_boxplot()
p4 <- modeling_data %>%
  group_by(fleet) %>%
  filter(amount > 0) %>%
  ggplot(aes(fleet, log(loss_cost))) +
  geom_boxplot()
p5 <- modeling_data %>%
  group_by(sex) %>%
  filter(amount > 0) %>%
  ggplot(aes(sex, log(loss_cost))) +
  geom_boxplot()

p6 <- grid.arrange(p1, p2, p3, p4, p5, nrow = 2)
ggsave(
  "./Graphs/binned_vars.png",
  p6,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)
