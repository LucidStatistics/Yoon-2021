

library(readxl)
library(ggeffects)
library(brms)
library(ggplot2)
library(gridExtra)
library(tidyverse)

b01 <- read_xlsx("~/r02_long.xlsx")
b01$responder <- as.factor(b01$responder)
b01$tx <- as.factor(b01$tx)
b01$ctime <- b01$time - mean(b01$time)
b01$ctime <- as.numeric(b01$ctime)

b01r <- b01[b01$responder=="Y",]
b01n <- b01[b01$responder=="N",]

######
# Q0 #
######

brm.1 <- brm(bf(q0 ~ ctime + responder + ctime:responder + (1|sid),
               hu ~ ctime + responder + ctime:responder + (1|sid)), 
            data = b01, family = hurdle_lognormal(), 
            prior = c(set_prior("normal(0,10)", class = "b"),
                      set_prior("normal(0,10)", class = "Intercept")), 
            inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
            cores = getOption("mc.cores", 4L),
            control = list(adapt_delta = 0.95), silent = TRUE, seed = 13,save_all_pars = T)

plot(ggpredict(brm.1, terms = c("ctime","responder")))

brm.1r <- brm(bf(q0 ~ ctime + (1|sid), hu ~ ctime + (1|sid)), 
             data = b01r, family = hurdle_lognormal(), 
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept")), 
             inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
             cores = getOption("mc.cores", 4L),
             control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

brm.1n <- brm(bf(q0 ~ ctime + (1|sid),hu ~ ctime + (1|sid)), 
             data = b01n, family = hurdle_lognormal(), 
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept")), 
             inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
             cores = getOption("mc.cores", 4L),
             control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

########
# OMAX #
########

brm.3 <- brm(bf(omax ~ ctime + responder + ctime:responder + (1|sid),
                hu ~ ctime + responder + ctime:responder + (1|sid)), 
             data = b01, family = hurdle_lognormal(), 
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept")), 
             inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
             cores = getOption("mc.cores", 4L),
             control = list(adapt_delta = 0.95), silent = TRUE, seed = 13,save_all_pars = T)
plot(ggpredict(brm.3,terms = c("ctime","responder")))

brm.3r <- brm(bf(omax ~ ctime + (1|sid), hu ~ ctime + (1|sid)), 
              data = b01r, family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

brm.3n <- brm(bf(omax ~ ctime + (1|sid), hu ~ ctime + (1|sid)), 
              data = b01n, family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

########
# PMAX #
########

brm.4 <- brm(bf(pmax ~ ctime + responder + ctime:responder + (1|sid),
                hu ~ ctime + responder + ctime:responder + (1|sid)), 
             data = b01, family = hurdle_lognormal(), 
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept")), 
             inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
             cores = getOption("mc.cores", 4L),
             control = list(adapt_delta = 0.95), silent = TRUE, seed = 13,save_all_pars = T)
plot(ggpredict(brm.4,terms = c("ctime","responder")))

brm.4r <- brm(bf(pmax ~ ctime + (1|sid), hu ~ ctime + (1|sid)), 
              data = b01r, family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

brm.4n <- brm(bf(pmax ~ ctime + (1|sid), hu ~ ctime + (1|sid)), 
              data = b01n, family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

##############
# Breakpoint #
##############

brm.5 <- brm(bf(bp ~ ctime + responder + ctime:responder + (1|sid),
                hu ~ ctime + responder + ctime:responder + (1|sid)), 
             data = b01, family = hurdle_lognormal(), 
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept")), 
             inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
             cores = getOption("mc.cores", 4L),
             control = list(adapt_delta = 0.95), silent = TRUE, seed = 13,save_all_pars = T)
plot(ggpredict(brm.5,terms = c("ctime","responder")))

brm.5r <- brm(bf(bp ~ ctime + (1|sid),
                 hu ~ ctime + (1|sid)), 
              data = b01r, 
              family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", 
              chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

brm.5n <- brm(bf(bp ~ ctime + (1|sid),
                 hu ~ ctime + (1|sid)), 
              data = b01n, 
              family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", 
              chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

##################
# Expected Value #
##################

brm.7 <- brm(bf(ev_imp ~ ctime + responder + ctime:responder + (1|sid),
                hu ~ ctime + responder + ctime:responder + (1|sid)), 
             data = b01, family = hurdle_lognormal(), 
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept")), 
             inits = "random", chains = 4, iter = 4000, warmup = 2000, thin = 1, 
             cores = getOption("mc.cores", 4L),
             control = list(adapt_delta = 0.95), silent = TRUE, seed = 13,save_all_pars = T)
plot(ggpredict(brm.7,terms = c("ctime","responder")))

brm.7r <- brm(bf(ev_imp ~ ctime + (1|sid),
                 hu ~ ctime + (1|sid)), 
              data = b01r, 
              family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", 
              chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

brm.7n <- brm(bf(ev_imp ~ ctime + (1|sid),
                 hu ~ ctime + (1|sid)), 
              data = b01n, 
              family = hurdle_lognormal(), 
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")), 
              inits = "random", 
              chains = 4, iter = 4000, warmup = 2000, thin = 1, 
              cores = getOption("mc.cores", 4L),
              control = list(adapt_delta = 0.99), silent = TRUE, seed = 13,save_all_pars = T)

############
# BIG PLOT #
############

brm.1.me   <- conditional_effects(brm.1, ci = T, method = "fitted", robust = T)
brm.1.me.mu <- conditional_effects(brm.1, ci = T, method = "fitted", robust = T, dpar="mu",scale = "response")
brm.1.me.hu <- conditional_effects(brm.1, ci = T, method = "fitted", robust = T, dpar="hu")
p1.mu.dat <- brm.1.me.mu$`ctime:responder`
p1.hu.dat <- brm.1.me.hu$`ctime:responder`
p1.mu.dat$Group <- 'Non-Responder'; p1.mu.dat$Group <- ifelse(p1.mu.dat$responder=="Y","Responder",p1.mu.dat$Group)
p1.mu.dat$y2 <- exp(p1.mu.dat$estimate__)
p1.hu.dat$Group <- 'Non-Responder'; p1.hu.dat$Group <- ifelse(p1.hu.dat$responder=="Y","Responder",p1.hu.dat$Group)

brm.3.me   <- conditional_effects(brm.3, ci = T, method = "fitted", robust = T)
brm.3.me.mu <- conditional_effects(brm.3, ci = T, method = "fitted", robust = T, dpar="mu",scale = "response")
brm.3.me.hu <- conditional_effects(brm.3, ci = T, method = "fitted", robust = T, dpar="hu")
p3.mu.dat <- brm.3.me.mu$`ctime:responder`
p3.hu.dat <- brm.3.me.hu$`ctime:responder`
p3.mu.dat$Group <- 'Non-Responder'; p3.mu.dat$Group <- ifelse(p3.mu.dat$responder=="Y","Responder",p3.mu.dat$Group)
p3.mu.dat$y2 <- exp(p3.mu.dat$estimate__)
p3.hu.dat$Group <- 'Non-Responder'; p3.hu.dat$Group <- ifelse(p3.hu.dat$responder=="Y","Responder",p3.hu.dat$Group)

brm.4.me   <- conditional_effects(brm.4, ci = T, method = "fitted", robust = T)
brm.4.me.mu <- conditional_effects(brm.4, ci = T, method = "fitted", robust = T, dpar="mu",scale = "response")
brm.4.me.hu <- conditional_effects(brm.4, ci = T, method = "fitted", robust = T, dpar="hu")
p4.mu.dat <- brm.4.me.mu$`ctime:responder`
p4.hu.dat <- brm.4.me.hu$`ctime:responder`
p4.mu.dat$Group <- 'Non-Responder'; p4.mu.dat$Group <- ifelse(p4.mu.dat$responder=="Y","Responder",p4.mu.dat$Group)
p4.mu.dat$y2 <- exp(p4.mu.dat$estimate__)
p4.hu.dat$Group <- 'Non-Responder'; p4.hu.dat$Group <- ifelse(p4.hu.dat$responder=="Y","Responder",p4.hu.dat$Group)

brm.5.me   <- conditional_effects(brm.5, ci = T, method = "fitted", robust = T)
brm.5.me.mu <- conditional_effects(brm.5, ci = T, method = "fitted", robust = T, dpar="mu",scale = "response")
brm.5.me.hu <- conditional_effects(brm.5, ci = T, method = "fitted", robust = T, dpar="hu")
p5.mu.dat <- brm.5.me.mu$`ctime:responder`
p5.hu.dat <- brm.5.me.hu$`ctime:responder`
p5.mu.dat$Group <- 'Non-Responder'; p5.mu.dat$Group <- ifelse(p5.mu.dat$responder=="Y","Responder",p5.mu.dat$Group)
p5.mu.dat$y2 <- exp(p5.mu.dat$estimate__)
p5.hu.dat$Group <- 'Non-Responder'; p5.hu.dat$Group <- ifelse(p5.hu.dat$responder=="Y","Responder",p5.hu.dat$Group)

brm.7.me   <- conditional_effects(brm.7, ci = T, method = "fitted", robust = T)
brm.7.me.mu <- conditional_effects(brm.7, ci = T, method = "fitted", robust = T, dpar="mu",scale = "response")
brm.7.me.hu <- conditional_effects(brm.7, ci = T, method = "fitted", robust = T, dpar="hu")
p7.mu.dat <- brm.7.me.mu$`ctime:responder`
p7.hu.dat <- brm.7.me.hu$`ctime:responder`
p7.mu.dat$Group <- 'Non-Responder'; p7.mu.dat$Group <- ifelse(p7.mu.dat$responder=="Y","Responder",p7.mu.dat$Group)
p7.mu.dat$y2 <- exp(p7.mu.dat$estimate__)
p7.hu.dat$Group <- 'Non-Responder'; p7.hu.dat$Group <- ifelse(p7.hu.dat$responder=="Y","Responder",p7.hu.dat$Group)

p1a <- ggplot(data = p1.mu.dat, aes(x = ctime, y = y2, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("Q"[0]))) + 
  ggtitle((expression(paste("")))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667),
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p1a <- p1a + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "top" ,legend.justification = "center") + ylim(0,16) + 
  theme(plot.title = element_text(hjust = 0.5, face="bold")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p1b <- ggplot(data = p1.hu.dat, aes(x = ctime, y = estimate__, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("p(Q"[0]," = 0)"))) +  
  ggtitle((expression(paste("")))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p1b <- p1b + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "top" ,legend.justification = "center") + ylim(0,1) + 
  theme(plot.title = element_text(hjust = 0.5, face="bold")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p3a <- ggplot(data = p3.mu.dat, aes(x = ctime, y = y2, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("O"[max]))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)

p3a <- p3a + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + 
  theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,77) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p3b <- ggplot(data = p3.hu.dat, aes(x = ctime, y = estimate__, group = Group)) + 
  geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("p(O"[max]," = 0)"))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667),
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p3b <- p3b + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,1) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p4a <- ggplot(data = p4.mu.dat, aes(x = ctime, y = y2, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("P"[max]))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p4a <- p4a + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + 
  theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,19) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p4b <- ggplot(data = p4.hu.dat, aes(x = ctime, y = estimate__, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("p(P"[max]," = 0)"))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p4b <- p4b + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,1) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p5a <- ggplot(data = p5.mu.dat, aes(x = ctime, y = y2, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("BP"))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p5a <- p5a + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + 
  theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,78) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))
p5b <- ggplot(data = p5.hu.dat, aes(x = ctime, y = estimate__, group = Group)) + geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("p(BP"[]," = 0)"))) + 
  scale_x_continuous(name = "", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p5b <- p5b + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,1) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))

p7a <- ggplot(data = p7.mu.dat, aes(x = ctime, y = y2, group = Group)) + geom_line(aes(linetype = Group), size = 2) + 
  theme_bw() + 
  ylab(expression(paste("EV"))) + 
  scale_x_continuous(name = "Time (Weeks)", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20) 
p7a <- p7a + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + 
  theme(axis.title.y = element_text(size = 20,color = "black")) + 
  theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + 
  theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + 
  theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + 
  theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + 
  theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + 
  theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + 
  scale_y_continuous(limits = c(0,3), breaks = c(0,1,2,3), labels = c("00","01","02","03")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(#axis.title.x = element_blank(), axis.text.x = element_blank(), 
         axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))

p7b <- ggplot(data = p7.hu.dat, aes(x = ctime, y = estimate__, group = Group)) + 
  geom_line(aes(linetype = Group), size = 2) + theme_bw() + 
  ylab(expression(paste("p(EV"[]," = 0)"))) + 
  scale_x_continuous(name = "Time (Weeks)", limits = c(-2.4, 2.7), 
                     breaks = c(-2.33333333333333, -1.33333333333333, -0.333333333333333, 0.66666666666667,1.66666666666667,  2.66666666666667), 
                     labels = c(0,1,2,3,4,5)) + theme_ggeffects(base_size = 20)
p7b <- p7b + labs(linetype = "Group",labels = c("Non-Responders","Responders")) + 
  theme(axis.title.y = element_text(size = 20,color = "black")) + theme(axis.title.x = element_text(size = 20,color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 18)) + 
  theme(axis.text.y = element_text(color = "black", size = 18)) + 
  theme(axis.line.x = element_line(color = "black")) + 
  theme(axis.line.y = element_line(color = "black")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size = 2)) + 
  theme(legend.title.align = 0.5) + 
  theme(legend.title = element_text(color = "black", size = 20)) + 
  theme(legend.text = element_text(color = "black", size = 18)) + 
  theme(legend.background = element_rect(fill = NA)) + 
  theme(legend.key.width = unit(1.05, "cm")) + theme(legend.spacing.y = unit(0.15, "cm")) + 
  theme(legend.position = "bottom" ,legend.justification = "center") + ylim(0,1)  + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
         panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  theme(#axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 1)) + 
  theme(axis.ticks.length.x = unit(0.5, "cm"))

plot1b <- p1b + theme(legend.position = "none")
plot1a <- p1a + theme(legend.position = "none")
plot3b <- p3b + theme(legend.position = "none")
plot3a <- p3a + theme(legend.position = "none")
plot4b <- p4b + theme(legend.position = "none")
plot4a <- p4a + theme(legend.position = "none")
plot5b <- p5b + theme(legend.position = "none")
plot5a <- p5a + theme(legend.position = "none")
plot7b <- p7b + theme(legend.position = "none")
plot7a <- p7a + theme(legend.position = "none")

library(ggpubr)
library(cowplot)

legend <- get_legend(p7a + theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))

left  <- plot_grid(plot1b,plot3b,plot4b,plot5b,plot7b,nrow=5)
l_title <- ggdraw() + draw_label("Probability Value = 0  (vs. > 0)", 
                                 fontface = "bold", size = 28, hjust=0.45)
left2 <- plot_grid(l_title, left, ncol = 1,rel_heights=c(0.05, 1), align = "vh")

right <- plot_grid(plot1a,plot3a,plot4a,plot5a,plot7a,nrow=5)
r_title <- ggdraw() + draw_label("Predicted Value (If Value > 0)", 
                                 fontface = "bold", size = 28,hjust=0.45)
right2 <- plot_grid(r_title, right,  ncol = 1,rel_heights=c(0.05, 1), align = "vh")

plots <- plot_grid(left2,right2)
plots2 <- plot_grid(plots, legend, rel_heights = c(1,0.05),ncol=1)
plots2

ggsave(filename = "plot.tiff", plot = plots2, compression = "lzw",dpi = 600, width = 16, height = 24)
ggsave(filename = "plot.pdf", plot = plots2, dpi = 600, width = 16, height = 24)

