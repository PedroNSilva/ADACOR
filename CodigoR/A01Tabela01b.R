# Capítulo 1 - distribuição dos pesos de domicílios da TICDOM 2019
library(survey)
library(tidyverse)

ticdom2019_dom <- readRDS(file="./data/ticdom2019_dom.rds")
domic_dat <- ticdom2019_dom$variables
colnames(domic_dat)
domic_dat <- domic_dat %>%
  mutate(regiao = factor(COD_REGIAO_2, 
                         labels = c("Sudeste", "Nordeste", "Sul", "Norte",
                                    "Centro-Oeste")))
domic_dat <- domic_dat %>%
  mutate(peso_domic = weights(ticdom2019_dom))

table(domic_dat$regiao)
tabela01b <- domic_dat %>%
  group_by(regiao) %>%
  summarise(Minimo = min(peso_domic),
            Quartil1 = quantile(peso_domic, probs=c(0.25)),
            Mediana = median(peso_domic),
            Quartil2 = quantile(peso_domic, probs=c(0.75)),
            Maximo = max(peso_domic))

tabela01br <- domic_dat %>%
  ungroup() %>%
  summarise(Minimo = min(peso_domic),
            Quartil1 = quantile(peso_domic, probs=c(0.25)),
            Mediana = median(peso_domic),
            Quartil2 = quantile(peso_domic, probs=c(0.75)),
            Maximo = max(peso_domic)) %>% 
  mutate(regiao = factor(c("6"), label=c("Brasil"))) %>%
  select(regiao, everything())

tabela01b <- bind_rows(tabela01b, tabela01br)
