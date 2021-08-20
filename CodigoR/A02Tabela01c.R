################################################################################
#####  Livro Análise de Dados Amostrais Com R             
#####  Capítulo 1 - estimativas de EPAs com domicílios da TICDOM 2019            
#####  Programador: Pedro Silva
#####  Última atualização: 17/08/2021
################################################################################
print("Executado em:") ; date() ; inicio <- proc.time()

# Carrega pacotes requeridos
library(srvyr)
library(survey)
library(tidyverse)

# Carrega objeto com dados de domicílios da pesquisa
ticdom2019_dom <- readRDS(file="./data/ticdom2019_dom.rds")
# Transforma objeto para permitir uso do tidyverse
ticdom2019_dom <- as_survey_design(ticdom2019_dom)
# Examina propriedades do objeto
summary(ticdom2019_dom)

# Ajusta objeto para inclusão de rótulos das categorias de variáveis e para
# codificar corretamente variáveis contadoras de número de computadores
ticdom2019_dom <- ticdom2019_dom %>%
  mutate(regiao = factor(COD_REGIAO_2, 
                         labels = c("Sudeste", "Nordeste", "Sul", "Norte",
                                    "Centro-Oeste")),
         A1_A = factor(A1_A, 
                       labels = c("Não tem", "Tem", "Não sabe", 
                                  "Não respondeu")),
         A1_B = factor(A1_B, 
                       labels = c("Não tem", "Tem", "Não sabe", 
                                  "Não respondeu")),
         A1_C = factor(A1_C, 
                       labels = c("Não tem", "Tem", "Não sabe", 
                                  "Não respondeu")),
         A2_QTD_DESK = ifelse(A1_A == "Não tem",
                              0, 
                              ifelse(A2_QTD_DESK > 100, NA, A2_QTD_DESK)),
         A2_QTD_NOTE = ifelse(A1_B == "Não tem",
                              0, 
                              ifelse(A2_QTD_NOTE > 100, NA, A2_QTD_NOTE)),
         A2_QTD_TAB = ifelse(A1_C == "Não tem",
                             0, 
                             ifelse(A2_QTD_TAB > 100, NA, A2_QTD_TAB)),
         A2_QTD_COMPUT = A2_QTD_DESK + A2_QTD_NOTE + A2_QTD_TAB)

# Cria arquivo somente com os dados dos domicílios
domic_dat <- ticdom2019_dom$variables

# Calcula estimativa do tamanho da população de domicílios
N_domic <- sum(weights(ticdom2019_dom))

# Acrescenta variável ao arquivo de domicílios
domic_dat <- cbind(domic_dat, N=N_domic)

# Cria objeto do plano amostral supondo AAS de domicílios
domic_aas <- svydesign(data=domic_dat,
                       ids=~1,
                       fpc=~N)

# Estimativas da proporção de domicílios com computador de mesa
tab1_aas <- svymean(~A1_A, domic_aas)
tab1_ver <- svymean(~A1_A, ticdom2019_dom, deff=TRUE)
tab1 <- data.frame(nomes = names(tab1_ver), 
                   est_aas = 100*coef(tab1_aas),
                   est_ver = 100*coef(tab1_ver),
                   dp_ver = 100*SE(tab1_ver),
                   epa    = deff(tab1_ver),
                   row.names=NULL)
tab1 <- tab1 %>%
  filter(nomes == "A1_ATem")

# Estimativas da proporção de domicílios com notebook
tab2_aas <- svymean(~A1_B, domic_aas)
tab2_ver <- svymean(~A1_B, ticdom2019_dom, deff=TRUE)
tab2 <- data.frame(nomes = names(tab2_ver), 
                   est_aas = 100*coef(tab2_aas),
                   est_ver = 100*coef(tab2_ver),
                   dp_ver = 100*SE(tab2_ver),
                   epa    = deff(tab2_ver),
                   row.names=NULL)
tab2 <- tab2 %>%
  filter(nomes == "A1_BTem")

# Estimativas da proporção de domicílios com tablete
tab3_aas <- svymean(~A1_C, domic_aas)
tab3_ver <- svymean(~A1_C, ticdom2019_dom, deff=TRUE)
tab3 <- data.frame(nomes = names(tab3_ver), 
                   est_aas = 100*coef(tab3_aas),
                   est_ver = 100*coef(tab3_ver),
                   dp_ver = 100*SE(tab3_ver),
                   epa    = deff(tab3_ver),
                   row.names=NULL)
tab3 <- tab3 %>%
  filter(nomes == "A1_CTem")

# Estimativas da proporção de domicílios com algum computador
tab4_aas <- svymean(~A1_AGREG, domic_aas)
tab4_ver <- svymean(~A1_AGREG, ticdom2019_dom, deff=TRUE)
tab4 <- data.frame(nomes = names(tab4_ver), 
                   est_aas = 100*coef(tab4_aas),
                   est_ver = 100*coef(tab4_ver),
                   dp_ver = 100*SE(tab4_ver),
                   epa    = deff(tab4_ver),
                   row.names=NULL) %>%
  rename(dp_ver = A1_AGREG)

# Estimativas do total de domicílios com algum computador
tab5_aas <- svytotal(~A1_AGREG, domic_aas)
tab5_ver <- svytotal(~A1_AGREG, ticdom2019_dom, deff=TRUE)
tab5 <- data.frame(nomes = names(tab5_ver), 
                   est_aas = coef(tab5_aas)/1000000,
                   est_ver = coef(tab5_ver)/1000000,
                   dp_ver = SE(tab5_ver)/1000000,
                   epa    = deff(tab5_ver),
                   row.names=NULL) %>%
  rename(dp_ver = A1_AGREG)

# Estimativas do número médio de computadores por domicílio
tab6_aas <- svymean(~A2_QTD_COMPUT, 
                    subset(domic_aas, A2_QTD_COMPUT>0), 
                    na.rm = TRUE)
tab6_ver <- svymean(~A2_QTD_COMPUT, 
                    subset(ticdom2019_dom, A2_QTD_COMPUT>0),
                    deff=TRUE, na.rm = TRUE)
tab6 <- data.frame(nomes = names(tab6_ver), 
                   est_aas = coef(tab6_aas),
                   est_ver = coef(tab6_ver),
                   dp_ver = SE(tab6_ver),
                   epa    = deff(tab6_ver),
                   row.names=NULL) %>%
  rename(dp_ver = A2_QTD_COMPUT)

# Junta estimativas e prepara para gravação
tabela01c <- bind_rows(tab1, tab2, tab3, tab4, tab5, tab6)
tabela01c$nomes <- c("Porcentagem de domicílios com computador de mesa",
                     "Porcentagem de domicílios com notebook",
                     "Porcentagem de domicílios com tablete",
                     "Porcentagem de domicílios com computador",
                     "Total de domicílios com computador (milhões)",
              "Número médio de computadores por domicílio que tem computador")
colnames(tabela01c) <- c("Parâmetro", "Est_por_AAS", "Est_Verd", 
                         "DP_Est_Verd", "EPA")
tabela01c$Est_por_AAS <- round(tabela01c$Est_por_AAS, digits=2)
tabela01c$Est_Verd <- round(tabela01c$Est_Verd, digits=2)
tabela01c$DP_Est_Verd <- round(tabela01c$DP_Est_Verd, digits=2)
tabela01c$EPA <- round(tabela01c$EPA, digits=2)

saveRDS(tabela01c, file="./data/tabela01c.rds")
