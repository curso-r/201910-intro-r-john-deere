library(tidyverse)
library(ggQC)
library(plotly)

# Setup Data --------------------------------------------------------------
set.seed(5555)
Process1 <- data.frame(processID = as.factor(rep(1,100)),
                       metric_value = rnorm(100,0,1) + if_else(runif(100) < 0.03, -5, 0),
                       subgroup_sample = rep(1:20, each=5),
                       Process_run_id = 1:100)
set.seed(5556)
Process2 <- data.frame(processID = as.factor(rep(2,100)),
                       metric_value = rnorm(100,5, 1),
                       subgroup_sample = rep(1:10, each=10),
                       Process_run_id = 101:200)

Both_Processes <- rbind(Process1, Process2)

# usando o ggQC ---------------------------------------------------
Both_Processes %>%
  ggplot(aes(x=Process_run_id, y = metric_value)) +
  geom_point() + geom_line() + 
  stat_QC(method="XmR") +
  facet_wrap(~processID, scales = "free_x", ncol = 1)

# sem usar o ggQC -----------------------------------------------
nivel_de_confianca <- 1 - 0.01/2
qn <- qnorm(nivel_de_confianca)

Both_Processes_summaries <- Both_Processes %>%
  group_by(processID) %>%
  summarise(
    lim_sup = mean(metric_value) + qn*sd(metric_value),
    mean = mean(metric_value),
    lim_inf = mean(metric_value) - qn*sd(metric_value)
  ) %>%
  gather(summary_type, summary_value, -processID)

Both_Processes %>%
  dplyr::group_by(processID) %>%
  dplyr::mutate(outlier = abs(metric_value) > mean(metric_value) + qn*sd(metric_value)) %>%
  ggplot(aes(x=Process_run_id, y = metric_value)) +
  geom_line() + 
  geom_point(aes(colour = outlier)) + 
  geom_hline(aes(yintercept = summary_value), data = Both_Processes_summaries, colour = "purple", linetype = "dashed") +
  facet_wrap(~processID, scales = "free_x", ncol = 1)
