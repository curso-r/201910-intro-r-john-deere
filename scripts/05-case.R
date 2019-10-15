library(tidyverse)
library(readxl)

base_jd <- read_excel(
  "dados/DadosTreinamento_Shared.xlsx",
  col_types = "text",
  sheet = 2
) %>% 
  janitor::clean_names() %>% 
  mutate(
    part_cost = as.numeric(part_cost),
    total_claim_cost = as.numeric(total_claim_cost),
    machine_usage_hours = as.numeric(machine_usage_hours)
  )

base_jd_agg <- read_excel("dados/DadosTreinamento_Shared.xlsx", sheet = 1)

# Frequência de falhas por equipamento

base_jd %>%
  count(pin_number, sort = TRUE)

# Frequência de falhas por peça

base_jd %>% 
  count(primary_failed_part_number, sort = TRUE)


# Frequência por equipamento e peça

base_jd %>% 
  count(pin_number, primary_failed_part_number, sort = TRUE)

# Distribuição do número de horas até a falha

base_jd %>% 
  ggplot(aes(x = machine_usage_hours)) +
  geom_histogram(color = "black", fill = "white") +
  labs(x = "Horas de uso", y = "Número de falhas") +
  theme_minimal()

# Distribuição do número de horas até a falha para os equipamentos que mais falharam

base_equip <- base_jd %>%
  group_by(pin_number) %>% 
  summarise(
    n = n(),
    total_cost = sum(total_claim_cost, na.rm = TRUE)
  ) %>% 
  arrange(desc(n), desc(total_cost)) %>% 
  slice(1:10)
  
base_jd %>% 
  filter(pin_number %in% base_equip$pin_number) %>% 
  mutate(pin_number = forcats::fct_reorder(pin_number, machine_usage_hours, .desc = TRUE)) %>% 
  ggplot(aes(x = pin_number, y = machine_usage_hours)) +
  geom_boxplot() +
  labs(x = "Equipamento", y = "Horas até a falha") +
  theme_minimal()

# Distribuiçãao do número de horas até a falha para as peças dos 3 equipamentos que mais falharam

base_equip <- base_jd %>%
  group_by(pin_number) %>% 
  summarise(
    n = n(),
    total_cost = sum(total_claim_cost, na.rm = TRUE)
  ) %>% 
  arrange(desc(n), desc(total_cost)) %>% 
  slice(1:3)

base_jd %>% 
  filter(pin_number %in% base_equip$pin_number) %>% 
  select(
    pin_number, 
    primary_failed_part_number, 
    machine_usage_hours, 
    total_claim_cost
  ) %>% 
  group_by(pin_number, primary_failed_part_number) %>% 
  summarise(
    n = n(),
    custo = sum(total_claim_cost)
  ) %>% 
  arrange(pin_number, desc(n), desc(custo)) %>% 
  rename(
    Equiplamento = pin_number,
    `Peça` = primary_failed_part_number,
    `Frequência` = n,
    `Custo total` = custo
  )

