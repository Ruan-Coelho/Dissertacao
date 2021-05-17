#================================== Inicio ==================================#

# Limpando area de trabalho
rm(list = ls())

# Tirando notacao cientifica
options(scipen=999)

# Carregando bibliotecas
pacman::p_load(tidyverse, rio, janitor, ggplot2, stargazer, margins, sjplot, arm)

# Definindo local de trabalho
setwd("C:/Users/RX/OneDrive/PC-RX/Mestrado/Dissertação/Final/Dados")

# Importando a base
pisa <- import("PISA_2015_Tidy_3.sav")

# Visao geral da base e suas variaveis
glimpse(pisa)
summary(pisa)

#================================== Brasil ==================================#

# Criando uma base so do Brasil
pisa_BR <- pisa %>%
  filter(pais == "Brazil") 

# Normalizando por z-score
pisa_BR$desemp <- as.numeric(scale(pisa_BR$desemp))
pisa_BR$nse <- as.numeric(scale(pisa_BR$nse))
pisa_BR$aspira_isei <- as.numeric(scale(pisa_BR$aspira_isei))

#========================== Descritivas de Expect ==========================#

# Expectativa x nse (boxplot)
# Distribuição do índice nse por expectativa
pisa_BR %>%
  filter(!is.na(expect)) %>% 
  filter(!is.na(nse)) %>% 
  ggplot(aes(x=expect_label, y=nse)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Espera concluir o Ensino Superior?") +
  ylab("Índice nse")

# Expectativa x desemp (boxplot)
pisa_BR %>%
  filter(!is.na(expect)) %>% 
  filter(!is.na(nse)) %>% 
  ggplot(aes(x=expect_label, y=desemp)) +
  geom_boxplot() +
  ggtitle("Distribuição do desempenho por expectativa") +
  xlab("Espera concluir o Ensino Superior?") +
  ylab("Desempenho")

# expectativa x regiao (barplot)
# reordenando 
pisa_BR %>%
  filter(!is.na(expect)) %>%
  mutate(regiao = fct_relevel(regiao, "DF", "Centro-oeste", "Norte",
                                      "Sul", "Sudeste", "Nordeste")) %>%
  ggplot(aes(x=regiao, fill=expect_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Expectativa de completar o Ensino Superior por região") +
  labs(x="", y="", fill="") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgray","darkgrey"))

# expectativa x sexo (barplot)
pisa_BR %>%
  filter(!is.na(expect)) %>%
  ggplot(aes(x=sexo, fill=expect_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Expectativa de completar o Ensino Superior \nde acordo com o sexo") +
  labs(x="", y="", fill="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("lightgray","darkgrey"))

# expectativa x serie ideal (barplot)
pisa_BR %>%
  filter(!is.na(expect)) %>%
  mutate(serie_ideal = fct_relevel(serie_ideal, "Adiantado", "Ideal", "Atrasado")) %>%
  ggplot(aes(x=serie_ideal, fill=expect_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Expectativa de completar o Ensino Superior \nde acordo com a serie ideal") +
  labs(x="", y="", fill="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("lightgray","darkgrey"))


#========================== Descritivas de Aspira ==========================#

# Aspira x nse (scatterplot) #ocup_status usado para aspira
pisa_BR %>%
  filter(!is.na(aspira_isei)) %>%
  filter(!is.na(nse)) %>%
  ggplot(aes(x=nse, y=aspira_isei)) +
  geom_point(color="gray70", size=1) +
  geom_smooth(method = "lm", color="black", fill = "black", size=0.5) +
  ggtitle("Correlação entre o Índice nse e \nStatus da ocupação aspirada") +
  theme_light() +
  labs(x="Índice nse", y="Status da ocupação aspirada", fill="")
  

# Aspira x desemp (scatterplot) 
pisa_BR %>%
  filter(!is.na(aspira_isei)) %>%
  filter(!is.na(desemp)) %>%
  ggplot(aes(x=desemp, y=aspira_isei)) +
  geom_point(color="gray70", size=1) +
  geom_smooth(method = "lm", color="black", fill = "black", size=0.5) +
  ggtitle("Correlação entre o desempenho e \nStatus da ocupação aspirada") +
  theme_light() +
  labs(x="Desempenho", y="Status da ocupação aspirada", fill="")


# Aspira x regiao (boxplot) 
pisa_BR %>%
  filter(!is.na(aspira_isei)) %>% 
  filter(!is.na(regiao)) %>% 
  ggplot(aes(x=reorder(regiao, -aspira_isei, FUN = median), y=aspira_isei)) +
  geom_boxplot() +
  ggtitle("Distribuição do status das ocupações \naspiradas pelas regiões brasileiras") +
  xlab("") +
  ylab("Status da ocupação aspirada") +
  theme(plot.title = element_text(hjust = 0.5))

# Aspira x serie ideal (boxplot) 
pisa_BR %>%
  filter(!is.na(aspira_isei)) %>% 
  filter(!is.na(serie_ideal)) %>% 
  ggplot(aes(x=reorder(serie_ideal, -aspira_isei, FUN = median), y=aspira_isei)) +
  geom_boxplot() +
  ggtitle("Distribuição do status das ocupações aspiradas \nde acordo com série ideal") +
  xlab("") +
  ylab("Status da ocupação aspirada") +
  theme(plot.title = element_text(hjust = 0.5))

# Aspira x sexo (boxplot) 
pisa_BR %>%
  filter(!is.na(aspira_isei)) %>% 
  filter(!is.na(sexo)) %>% 
  ggplot(aes(x=sexo, y=aspira_isei)) +
  geom_boxplot() +
  ggtitle("Distribuição do status das ocupações aspiradas \nde acordo com o sexo") +
  xlab("") +
  ylab("Status da ocupação aspirada") +
  theme(plot.title = element_text(hjust = 0.5))


#========================== Descritivas de ExA ==========================#

# ExA x nse (boxplot) 
pisa_BR %>%
  filter(!is.na(nse)) %>% 
  filter(!is.na(ExA)) %>% 
  ggplot(aes(x=reorder(ExA, -nse, FUN = median), y=nse)) +
  geom_boxplot() +
  ggtitle("Distribuição do índice nse por \nExpectativa*Aspiração") +
  xlab("") +
  ylab("Índice nse") +
  theme(plot.title = element_text(hjust = 0.5))

# ExA x Desemp (boxplot) 
pisa_BR %>%
  filter(!is.na(desemp)) %>% 
  filter(!is.na(ExA)) %>% 
  ggplot(aes(x=reorder(ExA, -desemp, FUN = median), y=desemp)) +
  geom_boxplot() +
  ggtitle("Distribuição do desempenho por \nExpectativa*Aspiração") +
  xlab("") +
  ylab("Desempenho") +
  theme(plot.title = element_text(hjust = 0.5))

# ExA x Região (barplot)
plot <- pisa_BR %>%
  filter(!is.na(ExA)) %>% 
  tabyl(ExA, regiao) %>% 
  adorn_percentages("col")

pisa_BR$freq <- NA

pisa_BR <- pisa_BR %>% 
  mutate(freq = case_when(
    ExA == "(E1,A1)" & regiao == "Centro-oeste" ~ 0.53307052,
    ExA == "(E1,A1)" & regiao == "DF" ~ 0.5851319 ,
    ExA == "(E1,A1)" & regiao == "Nordeste" ~ 0.45826365,
    ExA == "(E1,A1)" & regiao == "Norte" ~ 0.52514547,
    ExA == "(E1,A1)" & regiao == "Sudeste" ~ 0.4553431,
    ExA == "(E1,A1)" & regiao == "Sul" ~ 0.4614279,
    ExA == "(E0,A1)" & regiao == "Centro-oeste" ~ 0.27420061,
    ExA == "(E0,A1)" & regiao == "DF" ~ 0.1474820 ,
    ExA == "(E0,A1)" & regiao == "Nordeste" ~ 0.30576251,
    ExA == "(E0,A1)" & regiao == "Norte" ~ 0.27701579,
    ExA == "(E0,A1)" & regiao == "Sudeste" ~ 0.2908886,
    ExA == "(E0,A1)" & regiao == "Sul" ~ 0.2582655,
    ExA == "(E1,A0)" & regiao == "Centro-oeste" ~ 0.07490145,
    ExA == "(E1,A0)" & regiao == "DF" ~ 0.1558753,
    ExA == "(E1,A0)" & regiao == "Nordeste" ~ 0.07039684,
    ExA == "(E1,A0)" & regiao == "Norte" ~ 0.07938487,
    ExA == "(E1,A0)" & regiao == "Sudeste" ~ 0.0935883,
    ExA == "(E1,A0)" & regiao == "Sul" ~ 0.1135601,
    ExA == "(E0,A0)" & regiao == "Centro-oeste" ~ 0.11782742,
    ExA == "(E0,A0)" & regiao == "DF" ~ 0.1115108,
    ExA == "(E0,A0)" & regiao == "Nordeste" ~ 0.16557701,
    ExA == "(E0,A0)" & regiao == "Norte" ~ 0.11845387,
    ExA == "(E0,A0)" & regiao == "Sudeste" ~ 0.1601800,
    ExA == "(E0,A0)" & regiao == "Sul" ~ 0.1667465))
  

pisa_BR %>%
  filter(!is.na(regiao)) %>% 
  filter(!is.na(ExA)) %>%
  mutate(regiao = fct_relevel(regiao, "DF", "Centro-oeste", "Norte", 
                              "Sul", "Nordeste", "Sudeste")) %>%
  mutate(ExA = fct_relevel(ExA, "(E1,A1)", "(E0,A1)", "(E0,A0)", "(E1,A0)")) %>% 
  ggplot(aes(x=regiao, y=freq, fill=ExA)) +
  geom_col(position = "dodge") +
  ggtitle("Expectativa*Aspiração por região do Brasil") +
  labs(x="", y="Porcentage dentro de cada região", fill="") +
  theme_light() +
  scale_fill_manual(values = c("gray75","gray60", "gray45", "gray30")) +
  scale_y_continuous(labels=scales::percent)

# ExA x Sexo (barplot)
pisa_BR %>%
  filter(!is.na(sexo)) %>% 
  filter(!is.na(ExA)) %>%
  mutate(ExA = fct_relevel(ExA, "(E0,A0)", "(E1,A0)", "(E0,A1)", "(E1,A1)")) %>% 
  ggplot(aes(x=ExA, fill=sexo)) +
  geom_bar(position = "fill") +
  ggtitle("Expectativa*Aspiração por sexo") +
  labs(x="", y="", fill="") +
  theme_light() +
  scale_fill_manual(values = c("lightgray","darkgray")) +
  scale_y_continuous(labels=scales::percent)

# ExA x Serie Ideal (barplot)
pisa_BR %>%
  filter(!is.na(serie_ideal)) %>% 
  filter(!is.na(ExA)) %>%
  mutate(regiao = fct_relevel(serie_ideal, "Adiantado", "Ideal", "Atrasado")) %>%
  mutate(ExA = fct_relevel(ExA, "(E1,A1)", "(E1,A0)", "(E0,A1)", "(E0,A0)")) %>% 
  ggplot(aes(x=ExA, fill=serie_ideal)) +
  geom_bar(position = "fill") +
  ggtitle("Expectativa*Aspiração por série ideal") +
  labs(x="", y="", fill="") +
  theme_light() +
  scale_fill_manual(values = c("gray80","gray60", "gray40")) +
  scale_y_continuous(labels=scales::percent)

pisa_BR <- pisa_BR %>% 
  filter(!is.na(aspira_isei)) %>% 
  filter(!is.na(nse)) %>%
  filter(!is.na(desemp))

cor(pisa_BR$aspira_isei, pisa_BR$nse, method = "pearson")
cor(pisa_BR$aspira_isei, pisa_BR$desemp, method = "pearson")  
  

#======================= Descritivas de nse E Desemp =======================#

# Cruzamento de nse com Desemp (scatter)
pisa_BR %>%
  filter(!is.na(desemp)) %>%
  filter(!is.na(nse)) %>%
  ggplot(aes(x=nse, y=desemp)) +
  geom_point(color="gray70", size=1) +
  geom_smooth(method = "lm", color="black", fill = "black", size=0.5) +
  ggtitle("Correlação entre o Índice nse e desempenho") +
  theme_light() +
  labs(x="Índice nse", y="Desempenho", fill="")

# Cruzamento de nse com sexo (boxplot)
pisa_BR %>%
  filter(!is.na(sexo)) %>% 
  filter(!is.na(nse)) %>% 
  ggplot(aes(x=sexo, y=nse)) +
  geom_boxplot() +
  ggtitle("Distribuição do Índice nse por sexo") +
  xlab("") +
  ylab("Índice nse") +
  theme(plot.title = element_text(hjust = 0.5))

# Cruzamento de desemp com sexo (boxplot)
pisa_BR %>%
  filter(!is.na(sexo)) %>% 
  filter(!is.na(desemp)) %>% 
  ggplot(aes(x=sexo, y=desemp)) +
  geom_boxplot() +
  ggtitle("Distribuição do desempenho por sexo") +
  xlab("") +
  ylab("Desempenho") +
  theme(plot.title = element_text(hjust = 0.5))

# Cruzamento de desemp com regiao (boxplot)
pisa_BR %>%
  filter(!is.na(regiao)) %>% 
  filter(!is.na(desemp)) %>% 
  ggplot(aes(x=reorder(regiao, -desemp, FUN = median), y=desemp)) +
  geom_boxplot() +
  ggtitle("Distribuição do desempenho por regiao") +
  xlab("") +
  ylab("Desempenho") +
  theme(plot.title = element_text(hjust = 0.5))

# Cruzamento de nse com regiao (boxplot)
pisa_BR %>%
  filter(!is.na(regiao)) %>% 
  filter(!is.na(nse)) %>% 
  ggplot(aes(x=reorder(regiao, -nse, FUN = median), y=nse)) +
  geom_boxplot() +
  ggtitle("Distribuição do Índice nse por região") +
  xlab("") +
  ylab("Índice nse") +
  theme(plot.title = element_text(hjust = 0.5))

# Cruzamento de nse com serie ideal (boxplot)
pisa_BR %>%
  filter(!is.na(serie_ideal)) %>% 
  filter(!is.na(nse)) %>% 
  ggplot(aes(x=reorder(serie_ideal, -nse, FUN = median), y=nse)) +
  geom_boxplot() +
  ggtitle("Distribuição do Índice nse por série ideal") +
  xlab("") +
  ylab("Índice nse") +
  theme(plot.title = element_text(hjust = 0.5))

# Cruzamento de desemp com serie ideal (boxplot)
pisa_BR %>%
  filter(!is.na(serie_ideal)) %>% 
  filter(!is.na(desemp)) %>% 
  ggplot(aes(x=reorder(serie_ideal, -desemp, FUN = median), y=desemp)) +
  geom_boxplot() +
  ggtitle("Distribuição do desempenho por série ideal") +
  xlab("") +
  ylab("Desempenho") +
  theme(plot.title = element_text(hjust = 0.5))

pisab_BR <- pisa_BR %>%
  filter(!is.na(nse)) %>% 
  filter(!is.na(desemp))
  
  
cor(pisab_BR$nse, pisab_BR$desemp)


#=============================================================================#
