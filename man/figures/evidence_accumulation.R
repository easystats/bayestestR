library(rstanarm)
library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)
library(see)

set.seed(333)

data <- bayestestR::simulate_correlation(n=100, r=0.2)

priors <- data.frame()
posteriors <- data.frame()
params <- data.frame()
for(i in 4:nrow(data)){
  print(i)
  posterior <- insight::get_parameters(rstanarm::stan_glm(V1 ~ V2,
                                                          prior = normal(0, 0.2),
                                                          data=data[1:i, ],
                                                          refresh = 0,
                                                          iter=10000,
                                                          chains=4,
                                                          warmup=4000))$V2

  param <- data.frame("Median" = median(posterior),
                      "Max" = bayestestR::density_at(posterior, median(posterior), method="KernSmooth"))
  param$Evidence <- i
  params <- rbind(params, param)

  posterior <- bayestestR::estimate_density(posterior, method="KernSmooth")
  posterior$Evidence <- i
  posteriors <- rbind(posteriors, posterior)

  prior <- bayestestR::estimate_density(bayestestR::distribution_normal(1000, 0, 0.2), method="KernSmooth")
  prior$Evidence <- i
  priors <- rbind(priors, prior)
}



p <- ggplot(posteriors, aes(x=x, y=y)) +
  geom_area(data=priors, fill="#2196F3", alpha=1) +
  geom_segment(data=params, aes(x = Median , y = 0, xend = Median, yend = Max, color=Evidence), size=0.5, linetype = "dashed") +
  geom_line(aes(color=Evidence), size=1.5) +
  geom_vline(xintercept=0.2, color="#E91E63", size=1) +
  scale_colour_gradient(low = "#FFC107", high = "#E91E63", guide = FALSE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  see::theme_modern() +
  xlab("Effect") +
  ylab("Probability") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  coord_cartesian(xlim=c(-0.6, 0.6)) +
  gganimate::transition_time(Evidence) +
  # view_follow(fixed_y = TRUE) +
  labs(title = "Evidence (sample size): {frame_time}")


anim <- animate(p, duration=nrow(data)/4, detail=100)
anim
gganimate::anim_save("evidence_accumulation.gif", anim)

# TODO: Add on the side the animation of the indices of significance

# https://github.com/thomasp85/gganimate/wiki/Animation-Composition
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# + transition_reveal(Day)
