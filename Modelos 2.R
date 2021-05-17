#================================== Inicio ==================================#

# Limpando area de trabalho
rm(list = ls())

# Tirando notacao cientifica
options(scipen=999)

# Carregando bibliotecas
pacman::p_load(tidyverse, rio, janitor, ggplot2, stargazer, margins, 
               sjPlot, interplot, pscl, pROC, car, broom, Rmisc)

# Definindo local de trabalho
setwd("C:/Users/RX/OneDrive/PC-RX/Mestrado/Dissertação/Final/Dados")

# Importando a base
pisa <- import("PISA_2015_Tidy_3.sav")

#================================== Brasil ==================================#

# Criando uma base so do Brasil
pisa_BR <- pisa %>%
  filter(pais == "Brazil") 

# Normalizando por z-score
pisa_BR$desemp <- as.numeric(scale(pisa_BR$desemp))
pisa_BR$nse <- as.numeric(scale(pisa_BR$nse))
pisa_BR$aspira_isei <- as.numeric(scale(pisa_BR$aspira_isei))

# Retirando todos os NA's
pisa_BR <- pisa_BR %>%
  filter(repetiu != "")

pisa_BR <- pisa_BR %>% 
  drop_na()

pisa_BR <- pisa_BR %>% 
  mutate(sexo = case_when(sexo == "Feminino" ~ "Mulheres",
                          sexo == "Masculino" ~ "Homens"))

#============================================================================#
#=============================== Modelos Expect =============================#
#============================================================================#

#Modelo E1
modelo_E1 <- glm(expect ~ desemp + nse + desemp*nse,
                family = binomial(link = "logit"), 
                data = pisa_BR)

summary(modelo_E1)

#Modelo E2
modelo_E2 <- glm(expect ~ desemp + nse + sexo +
                          desemp*nse + desemp*sexo,
                family = binomial(link = "logit"), 
                data = pisa_BR)

summary(modelo_E2)

#Modelo E3
modelo_E3 <- glm(expect ~ desemp + nse + sexo + serie_ideal +
                          desemp*nse + desemp*sexo,
                 family = binomial(link = "logit"), 
                 data = pisa_BR)

summary(modelo_E3)

stargazer(modelo_E1, modelo_E2, modelo_E3, type = "text")

#=============================== Escohendo modelo =============================#

# Comparação dos modelos
# library(pscl) pseudo-R2
pR2_E1 <- pR2(modelo_E1)
pR2_E2 <- pR2(modelo_E2)
pR2_E3 <- pR2(modelo_E3)
diag <- data.frame(pR2_E1, pR2_E2, pR2_E3)

# Obs:
# llh : The log-likelihood from the fitted model
# llhNull : The log-likelihood from the intercept-only restricted model
# G2 : Minus two times the difference in the log-likelihoods
# McFadden : McFadden's pseudo r-squared
# r2ML 	: Maximum likelihood pseudo r-squared
# r2CU : Cragg and Uhler's pseudo r-squared

# Comparação dos modelos
# Verificando se a adiçãos das variáveis foi bom
BIC(modelo_E1, modelo_E2, modelo_E3)
AIC(modelo_E1, modelo_E2, modelo_E3)
a <- anova(modelo_E1, modelo_E2, modelo_E3)

# Comparação dos modelos
# Verificando capacidade preditiva dos modelos
# Curva de ROC
rocplot1 <- roc(expect ~ fitted(modelo_E1), data = pisa_BR)
plot.roc(rocplot1, legacy.axes = F)
auc(rocplot1)

rocplot2 <- roc(expect ~ fitted(modelo_E2), data = pisa_BR)
plot.roc(rocplot2, legacy.axes = F)
auc(rocplot2)

rocplot3 <- roc(expect ~ fitted(modelo_E3), data = pisa_BR)
plot.roc(rocplot3, legacy.axes = F)
auc(rocplot3)

#======================== Outros diagnósticos do modelo =======================#

# Diagnósticos e estatísticas de ajuste do modelo
# Verificando capacidade preditiva dos modelos
# Mapa de calor  Esarey
pred <- predict(modelo_E3, type = "response")
heatmapFit::heatmap.fit(pisa_BR$expect, pred3, reps = 1000)

# # Diagnósticos e estatísticas de ajuste do modelo
# Verificando multicolinearidade
vif(modelo_E3)

# Diagnóstico, comparação dos modelos, Estatísticas de ajuste do modelo
# Influential values e Outliers
dataE3 <- augment(modelo_E3) %>%
  mutate(index = 1:n())   

dataE3 %>% 
  top_n(3, .cooksd)

dataE3 %>% # Se nao tiver nada acima ou abaixo de +ou-3 estamos bem 
  ggplot(aes(index, .std.resid)) + 
  geom_point(aes(color = expect), alpha = .5) +
  theme_bw() #estamos bem!
  
dataE3 %>% # Se nao retornar nada, estamos bem
  filter(abs(.std.resid) > 3) #estamos bem!


# Diagnóstico, comparação dos modelos, Estatísticas de ajuste do modelo
# Verificando capacidade preditiva dos modelos
# Tabela de contigência
probabilities <- modelo_E3 %>% 
  predict(pisa_BR, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == pisa_BR$expect, na.rm = T)
# Proporção correta das predicoes = 0.71

#================= Efeitos marginais e probabilidade predita ================#

#criando uma variavel com as probabilidade preditas do modelo 
pisa_BR$probs_modelo <- predict(modelo_E3, type = "response")

# Probabilidade predita do desempenho
pisa_BR %>% 
  ggplot(aes(desemp, probs_modelo)) +
  geom_smooth(method ="glm", method.args=list(family="binomial"), color = "black") +
  labs(x="Desempenho", y="Pr(expectativa = 1)")

# Probabilidade predita do NSE
pisa_BR %>% 
  ggplot(aes(nse, probs_modelo)) +
  geom_smooth(method ="glm", method.args=list(family="binomial"), color = "black") +
  labs(x="NSE", y="Pr(expectativa = 1)")

# Probabilidade predita do sexo
# Precisa descobrir o erro padrão
# Criando uma base só com mulheres e suas probabilidades preditas
# E calculando margem de erro para plotar o intervalo de confiança
pisa_M <- pisa_BR %>% 
  dplyr::select(sexo, probs_modelo) %>% 
  filter(sexo == "Mulheres")

pisa_H <- pisa_BR %>% 
  dplyr::select(sexo, probs_modelo) %>% 
  filter(sexo == "Homens")

ME_M <- CI(pisa_M$probs_modelo)
ME_H <- CI(pisa_H$probs_modelo)  

# Criando as variaveis na base com os máximos e mínimos
pisa_BR$ME_sexo_max <- if_else(pisa_BR$sexo == "Mulheres", ME_M[1], ME_H[1])
pisa_BR$ME_sexo_min <- if_else(pisa_BR$sexo == "Mulheres", ME_M[3], ME_H[3])

# Plotando 
pisa_BR %>% 
  ggplot(aes(sexo, probs_modelo)) +
  stat_summary(geom="point", fun.y ="mean") +
  labs(x=" ", y="Pr(expectativa = 1)") +
  geom_errorbar(aes(ymin=ME_sexo_min, ymax=ME_sexo_max), width=0.1) +
  coord_cartesian(ylim = c(0.5, 0.7)) +
  scale_y_continuous(breaks = seq(0.5, 0.7, by = 0.04))
  
# Probabilidade predita do serie_ideal
# Precisa descobrir o erro padrão
# Criando uma base só com mulheres e suas probabilidades preditas
# E calculando margem de erro para plotar o intervalo de confiança
pisa_id <- pisa_BR %>% 
  dplyr::select(serie_ideal, probs_modelo) %>% 
  filter(serie_ideal == "Ideal")

pisa_at <- pisa_BR %>% 
  dplyr::select(serie_ideal, probs_modelo) %>% 
  filter(serie_ideal == "Atrasado")

pisa_ad <- pisa_BR %>% 
  dplyr::select(serie_ideal, probs_modelo) %>% 
  filter(serie_ideal == "Adiantado")

ME_id <- CI(pisa_id$probs_modelo)
ME_at <- CI(pisa_at$probs_modelo)  
ME_ad <- CI(pisa_ad$probs_modelo)  

# Criando as variaveis na base com os máximos e mínimos
pisa_BR$ME_si_max <- case_when(
  pisa_BR$serie_ideal == "Ideal" ~ ME_id[1],
  pisa_BR$serie_ideal == "Atrasado" ~ ME_at[1],
  pisa_BR$serie_ideal == "Adiantado" ~ ME_ad[1])

pisa_BR$ME_si_min <- case_when(
  pisa_BR$serie_ideal == "Ideal" ~ ME_id[3],
  pisa_BR$serie_ideal == "Atrasado" ~ ME_at[3],
  pisa_BR$serie_ideal == "Adiantado" ~ ME_ad[3])

# Plotando 
pisa_BR %>%
  mutate(serie_ideal=fct_relevel(serie_ideal, "Atrasado","Ideal", "Adiantado")) %>% 
  ggplot(aes(serie_ideal, probs_modelo)) +
  stat_summary(geom="point", fun.y ="mean") +
  labs(x=" ", y="Pr(expectativa = 1)") +
  geom_errorbar(aes(ymin=ME_si_min, ymax=ME_si_max), width=0.1) +
  coord_cartesian(ylim = c(0.35, 0.85)) +
  scale_y_continuous(breaks = seq(0.35, 0.85, by = 0.1)) 
 
# Colocando tudo em tabela

# Mulheres
round(ME_M, digits = 3)
round(std.error(ME_M), digits =3)

# Homens
round(ME_H, digits = 3)
round(std.error(ME_H), digits =3)

# Atrasado
round(ME_at, digits = 3)
round(std.error(ME_at), digits =3)

# Ideal
round(ME_id, digits = 3)
round(std.error(ME_id), digits =3)

# Adiantado
round(ME_ad, digits = 3)
round(std.error(ME_ad), digits =3)

# Interações NSE x Desemp
# Criando faixas de NSE
# desemp[-3, -2, 1, 0, 1, 2, 3]
# nse[-3, -2, 1, 0, 1, 2, 3]
pisa_BR$nse_group <- ntile(pisa_BR$nse, 4)

pisa_BR <- pisa_BR %>% 
  mutate(nse_group = case_when(nse_group == 4 ~ "1º Quartil",
                               nse_group == 3 ~ "2º Quartil",
                               nse_group == 2 ~ "3º Quartil",
                               nse_group == 1 ~ "4º Quartil"))


pisa_BR %>% 
  ggplot(aes(x=desemp, y=probs_modelo, color=nse_group)) +
  geom_smooth(method ="glm", method.args=list(family="binomial")) +
  labs(x="Desempenho", y="Pr(expectativa = 1)", color = "NSE")

# Interações NSE x Desemp (invertidas agora)
# Criando faixas de NSE
pisa_BR$desemp_percentil <- ntile(pisa_BR$desemp, 4)

pisa_BR <- pisa_BR %>% 
  mutate(desemp_percentil = case_when(desemp_percentil == 4 ~ "1º Quartil",
                                      desemp_percentil == 3 ~ "2º Quartil",
                                      desemp_percentil == 2 ~ "3º Quartil",
                                      desemp_percentil == 1 ~ "4º Quartil"))


pisa_BR %>% 
  ggplot(aes(x=nse, y=probs_modelo, color=desemp_percentil)) +
  geom_smooth(method ="glm", method.args=list(family="binomial")) +
  labs(x="NSE", y="Pr(expectativa = 1)", color = "Desempenho")


# Efeitoss marginais das duas variáveis
interplot(m = modelo_E3, var1 = "desemp", var2 = "nse") +
  labs(x="NSE", y="AME do desempenho (expectativa = 1)")

interplot(m = modelo_E3, var1 = "nse", var2 = "desemp")


# Criando os quartis em cada variável
pisa_BR$nse_group <- ntile(pisa_BR$nse, 3)
pisa_BR$desemp_group <- ntile(pisa_BR$desemp, 3)


# Nomeandos  os tercis
pisa_BR <- pisa_BR %>% 
  mutate(desemp_group = case_when(desemp_group == 3 ~ "Desemp. Alto",
                                  desemp_group == 2 ~ "Desemp. Médio",
                                  desemp_group == 1 ~ "Desemp. Baixo"))

pisa_BR <- pisa_BR %>% 
  mutate(nse_group = case_when(nse_group == 3 ~ "NSE. Alto",
                               nse_group == 2 ~ "NSE. Médio",
                               nse_group == 1 ~ "NSE. Baixo"))

#============================================================================#
#=============================== Modelos Aspira =============================#
#============================================================================#
#Modelo A1
modelo_A1 <- glm(aspira ~ desemp + nse + desemp*nse,
                 family = binomial(link = "logit"), 
                 data = pisa_BR)

summary(modelo_A1)

#Modelo A2
modelo_A2 <- glm(aspira ~ desemp + nse + sexo + desemp*sexo,
                 family = binomial(link = "logit"), 
                 data = pisa_BR)

summary(modelo_A2)

#Modelo A3
modelo_A3 <- glm(aspira ~ desemp + nse + sexo + expect + desemp*sexo,
                 family = binomial(link = "logit"), 
                 data = pisa_BR)

summary(modelo_A3)

#Modelo A4
modelo_A4 <- glm(aspira ~ expect + desemp + nse + sexo + serie_ideal,
                 family = binomial(link = "logit"), 
                 data = pisa_BR)

summary(modelo_A4)


stargazer(modelo_A1, modelo_A2, modelo_A3, modelo_A4, type = "text")

#=============================== Escohendo modelo =============================#

# Comparação dos modelos
# library(pscl) pseudo-R2
pR2_A1 <- pR2(modelo_A1)
pR2_A2 <- pR2(modelo_A2)
pR2_A3 <- pR2(modelo_A3)
pR2_A4 <- pR2(modelo_A4)
diag <- data.frame(pR2_A1, pR2_A2, pR2_A3, pR2_A4)
diag

# Obs:
# llh : The log-likelihood from the fitted model
# llhNull : The log-likelihood from the intercept-only restricted model
# G2 : Minus two times the difference in the log-likelihoods
# McFadden : McFadden's pseudo r-squared
# r2ML 	: Maximum likelihood pseudo r-squared
# r2CU : Cragg and Uhler's pseudo r-squared

# Comparação dos modelos
# Verificando se a adiçãos das variáveis foi bom
BIC(modelo_A1, modelo_A2, modelo_A3, modelo_A4)
AIC(modelo_A1, modelo_A2, modelo_A3, modelo_A4)

# Comparação dos modelos
# Verificando capacidade preditiva dos modelos
# Curva de ROC
rocplot1 <- roc(aspira ~ fitted(modelo_A1), data = pisa_BR)
plot.roc(rocplot1, legacy.axes = F)
auc(rocplot1)

rocplot2 <- roc(aspira ~ fitted(modelo_A2), data = pisa_BR)
plot.roc(rocplot2, legacy.axes = F)
auc(rocplot2)

rocplot3 <- roc(aspira ~ fitted(modelo_A3), data = pisa_BR)
plot.roc(rocplot3, legacy.axes = F)
auc(rocplot3)

rocplot4 <- roc(aspira ~ fitted(modelo_A4), data = pisa_BR)
plot.roc(rocplot4, legacy.axes = F)
auc(rocplot4)

#======================== Outros diagnósticos do modelo =======================#

# Diagnósticos e estatísticas de ajuste do modelo
# Verificando capacidade preditiva dos modelos
# Mapa de calor  Esarey
pred <- predict(modelo_A4, type = "response")
heatmapFit::heatmap.fit(pisa_BR$expect, pred, reps = 1000)

# # Diagnósticos e estatísticas de ajuste do modelo
# Verificando multicolinearidade
vif(modelo_A4)

# Diagnóstico, comparação dos modelos, Estatísticas de ajuste do modelo
# Influential values e Outliers
dataA4 <- augment(modelo_A4) %>%
  dplyr::mutate(index = 1:n())   

dataA4 %>% 
  top_n(3, .cooksd)

dataA4 %>% # Se nao tiver nada acima ou abaixo de +ou-3 estamos bem 
  ggplot(aes(index, .std.resid)) + 
  geom_point(aes(color = aspira), alpha = .5) +
  theme_bw() #estamos bem!

dataA4 %>% # Se nao retornar nada, estamos bem
  filter(abs(.std.resid) > 3) #estamos bem!


# Diagnóstico, comparação dos modelos, Estatísticas de ajuste do modelo
# Verificando capacidade preditiva dos modelos
# Tabela de contigência
probabilities <- modelo_A4 %>% 
  predict(pisa_BR, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == pisa_BR$expect, na.rm = T)
# Proporção correta das predicoes = 0.71

#================= Efeitos marginais e probabilidade predita ================#

#criando uma variavel com as probabilidade preditas do modelo 
pisa_BR$probs_modelo <- predict(modelo_A4, type = "response")

# Probabilidade predita do desempenho
pisa_BR %>% 
  ggplot(aes(desemp, probs_modelo)) +
  geom_smooth(method ="glm", method.args=list(family="binomial"), color = "black") +
  labs(x="Desempenho", y="Pr(expectativa = 1)")

# Probabilidade predita do NSE
pisa_BR %>% 
  ggplot(aes(nse, probs_modelo)) +
  geom_smooth(method ="glm", method.args=list(family="binomial"), color = "black") +
  labs(x="NSE", y="Pr(expectativa = 1)")

# Probabilidade predita do sexo
# Precisa descobrir o erro padrão
# Criando uma base só com mulheres e suas probabilidades preditas
# E calculando margem de erro para plotar o intervalo de confiança
pisa_M <- pisa_BR %>% 
  dplyr::select(sexo, probs_modelo) %>% 
  filter(sexo == "Mulheres")

pisa_H <- pisa_BR %>% 
  dplyr::select(sexo, probs_modelo) %>% 
  filter(sexo == "Homens")

ME_M <- CI(pisa_M$probs_modelo)
ME_H <- CI(pisa_H$probs_modelo)  

# Criando as variaveis na base com os máximos e mínimos
pisa_BR$ME_sexo_max <- if_else(pisa_BR$sexo == "Mulheres", ME_M[1], ME_H[1])
pisa_BR$ME_sexo_min <- if_else(pisa_BR$sexo == "Mulheres", ME_M[3], ME_H[3])

# Plotando 
pisa_BR %>% 
  ggplot(aes(sexo, probs_modelo)) +
  stat_summary(geom="point", fun.y ="mean") +
  labs(x=" ", y="Pr(expectativa = 1)") +
  geom_errorbar(aes(ymin=ME_sexo_min, ymax=ME_sexo_max), width=0.1) +
  coord_cartesian(ylim = c(0.5, 0.7)) +
  scale_y_continuous(breaks = seq(0.5, 0.7, by = 0.04))

# Probabilidade predita do serie_ideal
# Precisa descobrir o erro padrão
# Criando uma base só com mulheres e suas probabilidades preditas
# E calculando margem de erro para plotar o intervalo de confiança
pisa_id <- pisa_BR %>% 
  dplyr::select(serie_ideal, probs_modelo) %>% 
  filter(serie_ideal == "Ideal")

pisa_at <- pisa_BR %>% 
  dplyr::select(serie_ideal, probs_modelo) %>% 
  filter(serie_ideal == "Atrasado")

pisa_ad <- pisa_BR %>% 
  dplyr::select(serie_ideal, probs_modelo) %>% 
  filter(serie_ideal == "Adiantado")

ME_id <- CI(pisa_id$probs_modelo)
ME_at <- CI(pisa_at$probs_modelo)  
ME_ad <- CI(pisa_ad$probs_modelo)  

# Criando as variaveis na base com os máximos e mínimos
pisa_BR$ME_si_max <- case_when(
  pisa_BR$serie_ideal == "Ideal" ~ ME_id[1],
  pisa_BR$serie_ideal == "Atrasado" ~ ME_at[1],
  pisa_BR$serie_ideal == "Adiantado" ~ ME_ad[1])

pisa_BR$ME_si_min <- case_when(
  pisa_BR$serie_ideal == "Ideal" ~ ME_id[3],
  pisa_BR$serie_ideal == "Atrasado" ~ ME_at[3],
  pisa_BR$serie_ideal == "Adiantado" ~ ME_ad[3])

# Plotando 
pisa_BR %>%
  mutate(serie_ideal=fct_relevel(serie_ideal, "Atrasado","Ideal", "Adiantado")) %>% 
  ggplot(aes(serie_ideal, probs_modelo)) +
  stat_summary(geom="point", fun.y ="mean") +
  labs(x=" ", y="Pr(expectativa = 1)") +
  geom_errorbar(aes(ymin=ME_si_min, ymax=ME_si_max), width=0.1) +
  coord_cartesian(ylim = c(0.35, 0.85)) +
  scale_y_continuous(breaks = seq(0.35, 0.85, by = 0.1)) 

# Colocando tudo em tabela

# Mulheres
round(ME_M, digits = 3)
round(std.error(ME_M), digits =3)

# Homens
round(ME_H, digits = 3)
round(std.error(ME_H), digits =3)

# Atrasado
round(ME_at, digits = 3)
round(std.error(ME_at), digits =3)

# Ideal
round(ME_id, digits = 3)
round(std.error(ME_id), digits =3)

# Adiantado
round(ME_ad, digits = 3)
round(std.error(ME_ad), digits =3)

# Interações NSE x Desemp
# Criando faixas de NSE
# desemp[-3, -2, 1, 0, 1, 2, 3]
# nse[-3, -2, 1, 0, 1, 2, 3]
pisa_BR$nse_group <- ntile(pisa_BR$nse, 4)

pisa_BR <- pisa_BR %>% 
  mutate(nse_group = case_when(nse_group == 4 ~ "1º Quartil",
                               nse_group == 3 ~ "2º Quartil",
                               nse_group == 2 ~ "3º Quartil",
                               nse_group == 1 ~ "4º Quartil"))


pisa_BR %>% 
  ggplot(aes(x=desemp, y=probs_modelo, color=nse_group)) +
  geom_smooth(method ="glm", method.args=list(family="binomial")) +
  labs(x="Desempenho", y="Pr(expectativa = 1)", color = "NSE")

# Interações NSE x Desemp (invertidas agora)
# Criando faixas de NSE
pisa_BR$desemp_percentil <- ntile(pisa_BR$desemp, 4)

pisa_BR <- pisa_BR %>% 
  mutate(desemp_percentil = case_when(desemp_percentil == 4 ~ "1º Quartil",
                                      desemp_percentil == 3 ~ "2º Quartil",
                                      desemp_percentil == 2 ~ "3º Quartil",
                                      desemp_percentil == 1 ~ "4º Quartil"))


pisa_BR %>% 
  ggplot(aes(x=nse, y=probs_modelo, color=desemp_percentil)) +
  geom_smooth(method ="glm", method.args=list(family="binomial")) +
  labs(x="NSE", y="Pr(expectativa = 1)", color = "Desempenho")


# Efeitoss marginais das duas variáveis
interplot(m = modelo_A4, var1 = "desemp", var2 = "nse") +
  labs(x="NSE", y="AME do desempenho (expectativa = 1)")

interplot(m = modelo_A4, var1 = "nse", var2 = "desemp")
               