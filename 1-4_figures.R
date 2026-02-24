# Generate descriptive figures
# Author: Daniel Vader
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(patchwork)
library(ggnewscale)
#library(hrbrthemes)

source("1-5_functions.R")


cmvdat <- loaddat(sel="cmv")

# Density ridges histogram test ################################################
ggplot(cmvdat[as.numeric(cmvdat$id) < 30,], aes(y=id, x=time, fill=id)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=30) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  scale_fill_viridis(discrete=T)

# Density plots ################################################################
#ddir <- "Z:/18-015433_CMV in SOT_Downes/05 Data/dan_analytic_data/1_adat_2021-05-20.rds"
ddir <- "B:/restricted/00_BSC_Daniel-Vader/06_irregular_measures/1_adat_2021-06-02.rds"
d.demo <- readRDS(ddir) %>%
  mutate(blood_test_result = factor(blood_test_result, 
                                    levels=c(0,1), 
                                    labels=c("Negative", "Positive")),
         blood_cmv_test = factor(blood_cmv_test, 
                                 levels=c(0,1), 
                                 labels=c("Not tested", "Tested")),
         failure = factor(failure01, 
                          levels=c(0,1), 
                          labels=c("No failure", "Failure")),
         drany = ifelse(cmv_donor_recip_sens > 0, 1, 0),
         drstatus = ifelse(drany > 0, "At Risk", "Low Risk"),
         sot_type = ifelse(sot_type == 0, "Kidney",
                           ifelse(sot_type == 1, "Liver",
                                  ifelse(sot_type == 3 | sot_type == 4, "Lung", "Heart"))),
         ) %>%  
  filter(follow_day > 13)

# Proportion of positive tests
p1 <- ggplot(d.demo, 
             aes(x=follow_day, fill=blood_cmv_test, after_stat(count))) + 
  geom_density(alpha=0.6, 
               position="fill", 
               outline.type = "lower",
               bw="sj") + 
  scale_fill_grey(start=1, end=0.2) +
  ylab("Probability") +
  xlab("Follow-up day") +
  ylim(0,0.05) +
  #labs(fill="Organ", color="Organ") +
  ggtitle("Probability of being tested") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

p1s <- p1 + facet_grid(rows=vars(sot_type), cols=vars(drstatus)) +
  ylim(0,.1) +
  theme(plot.title = element_blank())

# Proportion of positive tests
d.demo.t <-  d.demo %>% filter(blood_cmv_test == "Tested")
p2 <- ggplot(d.demo.t, aes(x=follow_day, fill=blood_test_result, after_stat(count))) + 
  geom_density(alpha=0.6, 
               position="fill", 
               outline.type = "lower",
               bw="sj") + 
  scale_fill_grey(start=1, end=0.2) +
  ylim(0,0.3) +
  ylab("Probability") +
  xlab("Follow-up day") +
  labs(fill="Blood Test Result") +
  ggtitle("Probability of positive test when tested") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

p2s <- p2 + facet_grid(rows=vars(sot_type), cols=vars(drstatus)) + 
  ylim(0,.8) +
  theme(plot.title = element_blank())

tiff(filename = "H:/projects/cmv_sot/figures/testing-patterns.tiff", units="in", width=4, height=6, res=300)
ggpubr::ggarrange(p1,p2, ncol=1,nrow=2, labels = c("A", "B"))
dev.off()

tiff(filename = "H:/projects/cmv_sot/figures/tp-stratified1.tiff", units="in", width=4, height=6, res=300)
p1s
dev.off()

tiff(filename = "H:/projects/cmv_sot/figures/tp-stratified2.tiff", units="in", width=4, height=6, res=300)
p2s
dev.off()

# Heat maps ####################################################################
cmvdat2 <- loaddat(sel="none") 

# Arrange subjects by dropout time to make graph easier to read
cmvdat3 <- cmvdat2 %>%
  group_by(id) %>%
  summarize(endt = max(endt)) %>%
  arrange(endt) %>%
  mutate(id2 = row_number()) %>%
  right_join(cmvdat2, by="id")

# Check dist
scmv <- cmvdat3 %>% 
  group_by(id) %>% 
  summarize(drany = drany[1], sot_type = sot_type[1])

scmv.t <- table(scmv$sot_type, scmv$drany)
  
# Fill in LTFU time points with NA
cmvdat4 <- cmvdat3 %>%
  mutate(drstatus = ifelse(drany > 0, "HR", "LR"),
         sot_type = ifelse(sot_type == 0, "Kidney",
                           ifelse(sot_type == 1, "Liver",
                           ifelse(sot_type == 3 | sot_type == 4, "Lung", "Heart"))),
         drsot = ifelse(drstatus == "HR", 
                        ifelse(sot_type == "Kidney", 1,
                               ifelse(sot_type == "Liver", 3,
                                      ifelse(sot_type == "Lung", 5, 7))),
                        ifelse(sot_type == "Kidney", 2,
                               ifelse(sot_type == "Liver", 4,
                                      ifelse(sot_type == "Lung", 6, 8)))),
         drsotf = factor(drsot, levels=c(1,2,3,4,5,6,7,8),
                        labels=c(paste0("At-risk, kidney\n(N=", scmv.t[1,2], ")"), 
                                 paste0("Low-risk, kidney\n(N=", scmv.t[1,1], ")"),
                                 paste0("At-risk, liver\n(N=", scmv.t[2,2], ")"),
                                 paste0("Low-risk, liver\n(N=", scmv.t[2,1], ")"),
                                 paste0("At-risk, lung\n(N=", scmv.t[4,2] + scmv.t[5,2], ")"),
                                 paste0("Low-risk, lung\n(N=", scmv.t[4,1] + scmv.t[5,1], ")"),
                                 paste0("At-risk, heart\n(N=", scmv.t[3,2] + scmv.t[6,2], ")"),
                                 paste0("Low-risk, heart\n(N=", scmv.t[3,1] + scmv.t[6,1], ")"))
                        )
         ) %>%
  complete(id2, time) %>%
  group_by(id2) %>%
  mutate(drany = max(drany, na.rm=T),
         sot_type_collapse = sot_type_collapse[1],
         sot_type = sot_type[1],
         drstatus = drstatus[1],
         drsotf = drsotf[1]
         )
  
# Graph
heatmap <- ggplot(cmvdat4, aes(y=as.factor(id2), x=time, fill=test_2wk)) +
  geom_tile() +
  #coord_flip(expand=F) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.x = element_blank()
        ) +
  labs(fill = "Tests/\n2 weeks") +
  ylab("Patient") +
  xlab("Time (days)") +
  scale_colour_manual(values=NA) +              
  guides(color=guide_legend("Lost to\nfollow-up", override.aes=list(fill="grey40"))) +
  scale_fill_viridis_c(na.value = "grey40", option="magma")

tiff("H:/projects/cmv_sot/figures/heatmap_all.tiff", 
     res=300, units = "in", width=5, height=5)
heatmap
dev.off()


# Graph by risk group and organ type
png("figures/heatmap_organdr_revised.png", 
     res=300, units = "in", width=5, height=6)
heatmap + #coord_flip(expand=F) +
  facet_grid(space="free", scales="free", rows=vars(drsotf)) +
  theme(strip.text.y.right = element_text(angle = 0),
        legend.position = "right")
dev.off()




################################################################################

## Heatmap for just high risk [Kevin's paper] ##
heatmap <- ggplot(cmvdat3[cmvdat3$drany == 1,], aes(y=as.factor(id2), x=time, fill=test_2wk, color="")) +
  geom_tile() +
  #coord_flip(expand=F) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.x = element_blank()
  ) +
  labs(fill = "Tests/\n2 weeks") +
  ylab("Subject") +
  xlab("Time (days)") +
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("Lost to\nfollow-up", override.aes=list(colour="grey30"))) +
  scale_fill_viridis_c(na.value = "grey30", option="magma")


tiff("H:/projects/cmv_sot/figures/heatmap_organ-hr-only.tiff", 
     res=300, units = "in", width=5, height=5)
heatmap + #coord_flip(expand=F) +
  facet_grid(space="free", scales="free", rows=vars(sot_type)) +
  theme(strip.text.y.right = element_text(angle = 0),
        legend.position = "right")
dev.off()

# Table 1 ######################################################################
library(tidyverse)
library(furniture)

source("1-5_functions.R")

cmvdat.tab <- loaddat(sel="cmv") %>% 
  group_by(id) %>%
  summarize(
    sex = sex[1],
    age = age[1],
    reth = race_cat2[1],
    sot_type = sot_type[1],
    drany = drany[1],
    cmvtests = as.numeric(sum(blood_cmv_test)),
    cmv = sum(cmv),
    steroid = sum(steroid_2wk)
  ) %>%
  mutate(sex = ifelse(sex == 1, "Female", "Male"),
         reth = case_when(reth == 1 ~ "White",
                          reth == 3 ~ "Black",
                          reth %in% c(2,9) ~ "Other"),
         sot_type = ifelse(sot_type == 0, "Kidney",
                           ifelse(sot_type == 1, "Liver",
                                  ifelse(sot_type == 3 | sot_type == 4, 
                                         "Lung", "Heart"))),
         drstatus = ifelse(drany > 0, "High Risk", "Low Risk"),
         anycmv = ifelse(cmv > 0, "Yes", "No"))

cvars <- c("sex", "reth", "sot_type", "drstatus", "anycmv")
nvars <- c("age", "cmvtests", "steroid")
tabout <- tibble(var=character(), cat=character(),
                 N = numeric(), Percent = numeric(), upper=numeric())

for(i in 1:length(cvars)){
  t1 <- table(cmvdat.tab[, cvars[i]])
  tp1 <- prop.table(t1)
  for(j in 1:length(t1)){
    tabout <- add_row(tabout, 
                      var=cvars[i], cat=names(t1)[j],
                      N=t1[j], Percent=tp1[j])
  }
}

for(i in 1:length(nvars)){
  d <- cmvdat.tab %>% pull(nvars[i])
  m1 <- median(d)
  lq <- quantile(d, .25)
  uq <- quantile(d, .75)
  tabout <- add_row(tabout,
                    var=nvars[i], N=m1, Percent=lq, upper=uq)
  
}

write.csv(tabout, "H:/projects/cmv_sot/tables/t1.csv")

# Person-day histograms and bar plots #########################################

# Basic data setup
cmvdat.all <- loaddat(sel="none") %>% 
  filter(time <= endt) %>%
  mutate(week4 = ceiling(time / 28),
         cmv_f = factor(cmv, 
                      levels=0:1,
                      labels = c("CMV -", "CMV +")),
         sot_type = ifelse(sot_type == 0, "Kidney",
                           ifelse(sot_type == 1, "Liver",
                                  ifelse(sot_type == 3 | sot_type == 4, "Lung", "Heart"))),
         drany = factor(drany,
                        levels=1:0,
                        labels=c("At-risk", "Low-risk"))
         )

weeklabels <- c("3-6", "7-10", "11-14", "15-18",
                       "19-22", "23-26", "27-30",
                       "31-34", "35-38", "39-42",
                       "43-46", "47-50", "51-54")

# Tests and test positives by person-day
cmvdat.pt.a <- cmvdat.all %>%
  group_by(week4, cmv_f) %>%
  summarize(person_time = n(),
            cmv_tests = sum(blood_cmv_test)) %>%
  ungroup() %>%
  group_by(week4) %>%
  mutate(
    person_time = sum(person_time)
  ) %>%
  filter(!is.na(cmv_f)) %>%
  ungroup() %>%
  mutate(testrate100 = (cmv_tests/person_time) * 100)
  

a1 <- ggplot(data=cmvdat.pt.a, 
       aes(x=week4, y=testrate100, fill=cmv_f)) +
  geom_bar(stat="identity") +
  scale_y_continuous(breaks=seq(0, 8, by = 2), limits=c(0,9)) +
  scale_fill_viridis("Testing result", discrete=T, option="B", begin=.2, end=.7) +
  ylab("Rate per 100 person-days") +
  xlab("Week") +
  scale_x_continuous(breaks = 1:13, 
                     labels=weeklabels) +
  theme_classic()

# Testing and positivity by organ
# Testing
cmvdat.pt.b.1 <- cmvdat.all %>%
  group_by(week4, sot_type) %>%
  summarize(person_time = n(),
            cmv_tests = sum(blood_cmv_test)) %>%
  ungroup() %>%
  group_by(week4) %>%
  ungroup() %>%
  mutate(testrate100 = (cmv_tests/person_time) * 100)

b1 <- ggplot(data=cmvdat.pt.b.1, 
       aes(x=week4, y=testrate100, fill=sot_type)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_grey("Organ transplant type\n(Testing rate)", start=.4) +
  scale_y_continuous(breaks=seq(0, 8, by = 2), limits=c(0,9)) +
  theme_classic()

cmvdat.pt.b.2 <- cmvdat.all %>%
  group_by(week4, sot_type) %>%
  summarize(person_time = n(),
            cmv_positive = sum(cmv, na.rm=T)) %>%
  ungroup() %>%
  mutate(posrate100 = (cmv_positive/person_time) * 100,
         posrate100 = case_when(posrate100 == 0 ~ .05,
                                  T ~ posrate100))

b2 <- #ggplot(data=cmvdat.pt.b.2, 
       #aes(x=week4, y=posrate100, fill=sot_type)) +
  b1 +
  new_scale("fill") +
  geom_bar(aes(x=cmvdat.pt.b.2$week4, y=cmvdat.pt.b.2$posrate100,
               fill=cmvdat.pt.b.2$sot_type),
           stat="identity", position = position_dodge()) +
  scale_fill_viridis("Organ transplant type\n(CMV rate)", discrete = T, option = "C", end=.8) +
  ylab("Rate per 100 person-days") +
  xlab("Week") +
  scale_x_continuous(breaks = 1:13, 
                     labels=weeklabels) +
  theme_classic()

b2

# Testing and positivity by risk group
cmvdat.pt.c.1 <- cmvdat.all %>%
  group_by(week4, drany) %>%
  summarize(person_time = n(),
            cmv_tests = sum(blood_cmv_test)) %>%
  ungroup() %>%
  group_by(week4) %>%
  ungroup() %>%
  mutate(testrate100 = (cmv_tests/person_time) * 100)

c1 <- ggplot(data=cmvdat.pt.c.1, 
             aes(x=week4, y=testrate100, fill=drany)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_grey("CMV risk group\n(Testing rate)", start=.4) +
  scale_y_continuous(breaks=seq(0, 8, by = 2), limits=c(0,9)) +
  theme_classic()

cmvdat.pt.c.2 <- cmvdat.all %>%
  group_by(week4, drany) %>%
  summarize(person_time = n(),
            cmv_positive = sum(cmv, na.rm=T)) %>%
  ungroup() %>%
  mutate(posrate100 = (cmv_positive/person_time) * 100,
         posrate100 = case_when(posrate100 == 0 ~ .05,
                                T ~ posrate100))

c2 <- #ggplot(data=cmvdat.pt.b.2, 
  #aes(x=week4, y=posrate100, fill=sot_type)) +
  c1 +
  new_scale("fill") +
  geom_bar(aes(x=cmvdat.pt.c.2$week4, y=cmvdat.pt.c.2$posrate100,
               fill=cmvdat.pt.c.2$drany),
           stat="identity", position = position_dodge()) +
  scale_fill_viridis("CMV risk group\n(CMV rate)", discrete = T, option = "C", end=.8) +
  ylab("Rate per 100 person-days") +
  xlab("Week") +
  scale_x_continuous(breaks = 1:13, 
                     labels=weeklabels) +
  theme_classic()

c2

png("figures/f2.png", height=10, width=5, units="in", res=300)
a1 /  b2 / c2 +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") & 
  theme(legend.justification = "left", 
        plot.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        legend.key.size = unit(.5,"cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

