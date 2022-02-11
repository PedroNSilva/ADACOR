
## @knitr simula
# Carrega pacotes necessários
library(survey)
library(sampling)

# Fixa semente para permitir replicação dos resultados
set.seed(123)

# Carrega dados da população do exemplo
popul_dat <- readRDS(file = "data/popul.rds")

# Prepara dados da população para efetuar os cálculos
N <- nrow(popul_dat) # Tamanho da população
n1 <- 30; n2 <- 30 # Tamanhos de amostra nos dois estratos definidos
nh <- c(n1, n2) # Combina tamanhos de amostra num vetor
n <- sum(nh) # Tamanho total da amostra
Nh <- table(popul_dat$estrat) # Tamanhos da população nos estratos definidos
fh <- nh/Nh # Frações amostrais por estrato
Wh <- Nh/N # Pesos para uso no estimador de média
f <- n/N # Fração amostral global
# Expressa salários em milhares de reais
popul_dat$sal <- popul_dat$sal/1000
# Expressa receitas em milhões de reais
popul_dat$rec <- popul_dat$rec/1000000

# Define objetos para salvar resultados
est_aas <- c(0,0)
est_aes <- c(0,0)
cov_mat_aas_est <- matrix(0,2,2)
cov_mat_aes_est <- matrix(0,2,2)

# Gera amostras com dois estratos, selecionando 30 empresas por estrato
for(i in 1:500){
  # Aplica função do pacote 'sampling' para selecionar amostra estratificada
  s <- strata(popul_dat, "estrat", c(30,30), method= "srswor")
  # Recupera dados da amostra selecionada no arquivo da população
  dados <- getdata(popul_dat, s)
  # Calcula média amostral simples de salário e receita
  est_aas <- est_aas + c(mean(dados$sal), mean(dados$rec))
  # Calcula estimativa simples de variância usando estimador ingênuo v_0
  cov_mat_aas_est <- cov_mat_aas_est + (1 - f) * 
                     cov(cbind(dados$sal, dados$rec))/n
  # Prepara objeto do plano amostral para permitir estimação não enviesada
  popul_plan <- svydesign(~1, strata=~estrat, data=dados, fpc=~Prob)
  # Calcula estimativas não viciadas (HT) da média do salario e da receita
  sal_rec_aes_est <- svymean(~sal+rec, popul_plan)
  est_aes <- est_aes + coef(sal_rec_aes_est)
  # Calcula estimativa da matriz de variância do estimador não viciado
  cov_mat_aes_est <- cov_mat_aes_est + attr(sal_rec_aes_est, 'var')
}

# Calcula médias populacionais das variáveis
med_pop <- round(c(mean(popul_dat$sal), mean(popul_dat$rec)), 3)

# Calcula médias das estimativas na simulação

## @knitr estaas
mean_est_aas <- est_aas/500
mean_est_aas

## @knitr estaes
mean_est_aes <- est_aes/500
mean_est_aes

## @knitr estcovaas
mean_cov_mat_aas_est <- cov_mat_aas_est/500
mean_cov_mat_aas_est

## @knitr estcovaes
mean_cov_mat_aes_est <- cov_mat_aes_est/500
mean_cov_mat_aes_est

## @knitr matcovpop
mat_cov_pop <- by(popul_dat, popul_dat$estrat, 
                  function(t)var(cbind(t$sal,t$rec)))
mat_cov_pop

## @knitr matcovverd
mat_cov_aleat_verd <- (Wh[1]^2 * (1 - fh[1]) / nh[1]) * mat_cov_pop[[1]] + 
                      (Wh[2]^2*(1-fh[2])/nh[2]) * mat_cov_pop[[2]]
mat_cov_aleat_verd

## @knitr epamult
DELTA = solve(mean_cov_mat_aas_est) %*% mean_cov_mat_aes_est
eigen(DELTA)$values
