library(tidyverse)
library(ggQC)
library(plotly)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)

# dados --------------------------------------------------------------
torques <- read_excel("dados/Torques.xlsx") %>% clean_names()
glimpse(torques)
skim(torques)
View(torques)

# arrumando os dados --------------------------------------------------

# As colunas que começam com 'torque' e 'angulo' apresentam valores que 
# possuem tanto '.' quanto ',' como separadores de casas decimais.
# Precisamos arrumar para que tudo fique com '.' como separador.
arruma_os_decimais <- function(x) {
  x %>% str_replace_all(",", ".") %>% as.numeric()
}

torques <- torques %>%
  mutate_at(vars(starts_with("torque"), starts_with("angulo")), arruma_os_decimais)

skim(torques)

# grafico de um dia -------------------------------------------------------------
nivel_de_confianca <- 1 - 0.05/2
qn <- qnorm(nivel_de_confianca)

torques %>%
  filter(
    torque < 2000,
    ferramenta == "TG4065",
    p_set == 1
  ) %>%
  mutate(
    outlier = abs(torque) > mean(torque) + qn*sd(torque),
    ano_mes_dia = data_hora %>% floor_date(unit = "days") %>%  as.character(),
    hora_minuto_segundo = data_hora %>% format(format="%H:%M:%S") %>% strptime(format="%H:%M:%S") %>% as.POSIXct(),
    antes_do_meio_dia = if_else(hora_minuto_segundo < "2019-10-16 11:30:00:00", "Antes do meio dia", "Depois do meio dia")
  ) %>%
  # filter(ano_mes_dia == "2019-09-17") %>%
  ggplot(aes(x = data_hora, y = torque)) +
  geom_point(aes(colour = outlier)) + 
  geom_line(alpha = 0.1) +
  stat_QC(method="XmR") +
  geom_ribbon(aes(ymin = torque_min, ymax = torque_max), alpha = 0.1) +
  facet_wrap(~antes_do_meio_dia, ncol = 1, scales = "free_x") +
  theme(
    legend.position = "bottom"
  )







# grafico de 5 dias -------------------------------------------------------------
nivel_de_confianca <- 1 - 0.05/2
qn <- qnorm(nivel_de_confianca)

torques %>%
  filter(
    torque < 2000,
    ferramenta == "TG4065",
    p_set == 1
  ) %>%
  mutate(
    ano_mes_dia = data_hora %>% floor_date(unit = "days") %>%  as.character(),
    hora_minuto_segundo = data_hora %>% format(format="%H:%M:%S") %>% strptime(format="%H:%M:%S") %>% as.POSIXct(),
    antes_do_meio_dia = if_else(hora_minuto_segundo < "2019-10-16 11:30:00:00", "Antes do meio dia", "Depois do meio dia")
  ) %>%
  filter(ano_mes_dia > today() - days(10)) %>%
  group_by(ano_mes_dia) %>%
  mutate(
    outlier = abs(torque) > mean(torque) + qn*sd(torque)
  ) %>%
  ggplot(aes(x = hora_minuto_segundo, y = torque)) +
  geom_point(aes(colour = outlier, size = outlier), alpha = 0.6) + 
  geom_line(alpha = 0.2) +
  stat_QC(method="XmR") +
  geom_ribbon(aes(ymin = torque_min, ymax = torque_max), alpha = 0.1) +
  facet_grid(ano_mes_dia~., scales = "free_x") +
  theme(
    legend.position = "top"
  )




