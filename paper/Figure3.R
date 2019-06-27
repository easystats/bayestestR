library(bayestestR)
library(tidyverse)
library(strengejacke)

set.seed(123)
posterior <- distribution_chisquared(100, 3)
dat <- as.data.frame(density(posterior))

dat %>% mutate(fill = ifelse(x < -.5, "low",
                     ifelse(x > .5, "high", "middle"))) %>%
  ggplot(aes(x=x, y=y, fill=fill)) +
  geom_ribbon(aes(ymin=0, ymax=y)) +
  geom_vline(xintercept=0, linetype="dotted") +
  theme_classic(base_size = 20) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .25)) +
  scale_fill_manual(values=c("high"="#FFC107", "low"="#FFC107", "middle"="#E91E63"), guide=FALSE) +
  xlab(NULL) +
  ylab("Probability Density\n")

ggsave("paper/Figure3.png", width = 13, height = 6, units = "in", dpi = 300)
