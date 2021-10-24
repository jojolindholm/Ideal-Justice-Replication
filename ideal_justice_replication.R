# IDEAL JUSTICE REPLICATION
# Johan Lindholm, Umeå University
# 24 October 2021
#
# This script can be used to replicate the sequence analysis 
# used for the paper 'The Ideal Justice'.


library(TraMineR)
library(WeightedCluster)
library(tidyverse)


# setup

par(mar = c(1,1,1,1))   # to ensure correct figure plotting
current_year <- as.integer(format(Sys.Date(), "%Y"))   # current year used as variable in plotting


# load data

load("judges_info.RData")   # load Justices' basic information
load("judges_seq.RData")   # load Justices' backgrounds as sequences


# compute pairwise distances

judges_om <- seqdist(judges_seq, method = "OM", sm = "TRATE") 


# generate cluster models

ward_cluster <- agnes(judges_om, diss = TRUE, method = "ward")   # creates cluster object


# compare range of clusters

summary(ward_range, max.rank=5)
plot(ward_range, 
     stat=c("ASWw", "HG", "PBC", "HC"), 
     norm = "zscore")
ward_range <- as.clustrange(ward_cluster, diss=judges_om, ncluster = 10)   # creates cluster quality information on different number cluster solutions, up to 8 clusters


# calculate transition probabilities and costs

trans_prob <- seqtrate(judges_seq) %>%
  as_tibble() %>%
  mutate(from = c("acad", "ct", "dep", "pol", "priv", "publ"), 
         .before = 1)

cost <- seqcost(judges_seq, method = "TRATE")


# plot clusters

seqplot(judges_seq,
        group = ward_range$clustering$cluster6,
        type = "I",
        sortv = "from.end",
        border = NA,
        xtlab = c(-100:-1))


# plot two-dimensional mds

judges_mds <- cmdscale(judges_om, 2)

mds <- tibble(x = judges_mds[,1],
              y = judges_mds[,2],
              community = ward_range$clustering$cluster6)

ggplot(mds, 
       aes(x = x, 
           y = y, 
           color = community)) +
  geom_count(alpha = 0.35) +
  labs(x = "",
       y = "",
       color = "Community")


# add cluster membership to Justices info

  judges_info$membership <- ward_range$clustering$cluster6
  mem_labels <- c("Ministry Judge", "Professor", "Public Servant Judge", "Practising Lawyer", "Career Judge", "Politician Judge")
  judges_info$mem_labels <- mem_labels[judges_info$membership]


# plot types in Supreme Court over time

  ggplot(filter(judges_info, court == "hd") %>%
           group_by(start_decade, mem_labels) %>%
           tally() %>%
           mutate(all_n = sum(n)) %>%
           mutate(per = n/all_n), 
         aes(y = per * 100, 
             x = start_decade)) +
    lapply(c(1800, 1850, 1900, 1950, 2000), function(xint) geom_vline(aes(xintercept = xint), lty = 2, col = "grey")) +
    scale_x_continuous(limits = c(1780, current_year)) +
    scale_y_continuous(limits = c(0, 100)) +
    geom_col(aes(fill = mem_labels)) +
    facet_wrap(~mem_labels, nrow = 3) +
    labs(y = "Percent of appointments",
         x = "Appontment decade") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90),
          panel.background = element_blank())


# plot types in Supreme Administrative Court over time

ggplot(filter(judges_info, court == "hfd") %>%
         group_by(start_decade, mem_labels) %>%
         tally() %>%
         mutate(all_n = sum(n)) %>%
         mutate(per = n/all_n), 
       aes(y = per * 100, 
           x = start_decade)) +
  lapply(c(1800, 1850, 1900, 1950, 2000), function(xint) geom_vline(aes(xintercept = xint), lty = 2, col = "grey")) +
  scale_x_continuous(limits = c(1780, current_year)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_col(aes(fill = mem_labels)) +
  facet_wrap(~mem_labels, nrow = 3) +
  labs(y = "Percent of appointments",
       x = "Appontment decade") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.background = element_blank())