library(bayestestR)
library(tidyverse)
library(strengejacke)

set.seed(123)
posterior <- distribution_chisquared(100, 3)
dat <- as.data.frame(density(posterior))

dat %>% mutate(fill = ifelse(x < hdi(posterior)$CI_low, "low",
                     ifelse(x > hdi(posterior)$CI_high, "high", "middle"))) %>%
  ggplot(aes(x=x, y=y, fill=fill)) +
  geom_ribbon(aes(ymin=0, ymax=y)) +
  geom_vline(xintercept=0, linetype="dotted") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .25)) +
  scale_fill_manual(values=c("high"="#FFC107", "low"="#FFC107", "middle"="#E91E63"), guide=FALSE) +
  annotate("text", x=2.5, y=.05, label="The 89% HDI", color="white", size=8) +
  xlab("\nParameter Value") +
  ylab("Probability Density\n")

ggsave("paper/Figure2.png", width = 13, height = 8, units = "in", dpi = 300)
