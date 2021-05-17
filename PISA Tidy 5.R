# INICIO --------------------------------------------------------------------------------
rm(list = ls())
options(scipen=999)
pacman::p_load(tidyverse, rio, janitor, intsvy)

# abrir base
setwd("C:/Users/Homedesk/OneDrive/PC-RX/Mestrado/Dissertação/Final/PISA 15")
pisa_2015 <- import("CY6_MS_CMB_STU_QQQ.sav")

# importando apenas as variaveis que serao usadas
pisa <- pisa_2015 %>% 
  select(expect_level = ST111Q01TA,
         estado = STRATUM,
         pais = CNTRYID,
         ocupacao = OCOD3,
         serie = ST001D01T, 
         sexo = ST004D01T,
         nse = ESCS,
         aspira_isei = BSMJ,
         repetiu = REPEAT,
         PV1MATH:PV10SCIE,
         peso_aluno = W_FSTUWT,
         peso_replicate_ = W_FSTURWT1:W_FSTURWT80)

glimpse(pisa)

################################################################################
## AJUSTANDO AS NOTAS

# The use of PV has important implications for PISA data analysis:
#   
# - For each student, a set of plausible values is provided, that corresponds to distinct 
#  draws in the plausible distribution of abilities of these students. Since PISA 2015, 
#  ten plausible values are provided by student. Accurate analysis requires to average 
#  all statistics over this set of plausible values.
# 
# - Plausible values should not be averaged at the student level, i.e. by computing in 
#  the dataset the mean of the five or ten plausible values at the student level and then 
#  computing the statistic of interest once using that average PV value. In addition, even
#  if a set of plausible values is provided for each domain, the use of pupil fixed 
#  effects models is not advised, as the level of measurement error at the individual 
#  level may be large.
# 
# In practice, an accurate and efficient way of measuring proficiency estimates in PISA 
# requires five steps:
#   
# 1. Compute estimates for each Plausible Values (PV)
# 2. Compute final estimate by averaging all estimates obtained from (1)
# 3. Compute sampling variance (unbiased estimate are providing by using only one PV)
# 4. Compute imputation variance (measurement error variance, estimated for each PV and 
#    then average over the set of PVs)
# 5. Compute final standard error by combining (3) and (4)

# media dd PV1 das matérias
pisa$desemp <- rowMeans(pisa[c("PV1MATH","PV1READ", "PV1SCIE")])


###########################################################################
## CRIANDO VARIAVEL EXPECT

# criando a variavel expect binaria
pisa$expect <-  pisa$expect_level

pisa <- pisa %>% 
  mutate(expect = case_when(expect <= 4 ~ 0,  # ISCED 5-6 -> 1 (Sim)
                            expect >= 5 ~ 1)) # ISCED 1-4 -> 0 (Nao)

# criando uma variavel expect categorica para usar nos graficos
pisa <- pisa %>% 
  mutate(expect_label = if_else(expect == 1, "Sim", "N?o"))

##########################################################################
## CRIANDO A VARIAVEL SERIE IDEAL

# Criando tres categorias de serie: Atrasado, Ideal, Adiantado
pisa$serie_ideal <- pisa$serie

pisa <- pisa  %>% 
  mutate(serie_ideal = 
           case_when(serie_ideal == 12 ~ "Adiantado",
                     serie_ideal == 11 ~ "Ideal",
                     serie_ideal == 10 ~ "Ideal",
                     serie_ideal == 9 ~ "Atrasado",
                     serie_ideal == 8 ~ "Atrasado",
                     serie_ideal == 7 ~ "Atrasado"))

# Renomeando as categorias da s?rie
pisa <- pisa  %>% 
  mutate(serie = case_when(serie == 12 ~ "Terceiro EM", 
                           serie == 11 ~ "Segundo EM",
                           serie == 10 ~ "Primeiro EM",
                           serie == 9 ~ "Nono EF",
                           serie == 8 ~ "Oitava EF",
                           serie == 7 ~ "S?tima EF"))

#############################################################################
#RENOMENADO GENERO E REPROVACAO

# Renomeando as categorias de genero
pisa <- pisa %>% 
  mutate(sexo = case_when(sexo == 1 ~ "Mulheres",
                          sexo == 2 ~ "Homens"))

# Renomeando as categorias de reprovacao
pisa <- pisa %>% 
  mutate(repetiu = case_when(repetiu == 0 ~ "N?o",
                             repetiu == 1 ~ "Sim"))

#############################################################################
## NOMEANDO OS PAISES

pisa <- pisa  %>% 
  mutate(pais = 
           case_when(pais == 8 ~ "Albania",
                     pais == 12 ~ "Algeria",
                     pais == 36 ~ "Australia",
                     pais == 56 ~ "Belgium",
                     pais == 76 ~ "Brazil",
                     pais == 100 ~ "Bulgaria",
                     pais == 124 ~ "Canada",
                     pais == 152 ~ "Chile",
                     pais == 158 ~ "Chinese Taipei",
                     pais == 170 ~ "Colombia",
                     pais == 188 ~ "Costa Rica",
                     pais == 191 ~ "Croatia",
                     pais == 203 ~ "Czech Republic",
                     pais == 208 ~ "Denmark",
                     pais == 214 ~ "Dominican Republic",
                     pais == 233 ~ "Estonia",
                     pais == 246 ~ "Finland",
                     pais == 250 ~ "France",
                     pais == 268 ~ "Georgia",
                     pais == 276 ~ "Germany",
                     pais == 300 ~ "Greece",
                     pais == 344 ~ "Hong Kong",
                     pais == 348 ~ "Hungary",
                     pais == 352 ~ "Iceland",
                     pais == 360 ~ "Indonesia",
                     pais == 372 ~ "Ireland",
                     pais == 376 ~ "Israel",
                     pais == 380 ~ "Italy",
                     pais == 392 ~ "Japan",
                     pais == 400 ~ "Jordan",
                     pais == 410 ~ "Korea",
                     pais == 411 ~ "Kosovo",
                     pais == 422 ~ "Lebanon",
                     pais == 428 ~ "Latvia",
                     pais == 440 ~ "Lithuania",
                     pais == 442 ~ "Luxembourg",
                     pais == 446 ~ "Macao",
                     pais == 470 ~ "Malta",
                     pais == 484 ~ "Mexico",
                     pais == 498 ~ "Moldova",
                     pais == 499 ~ "Montenegro",
                     pais == 528 ~ "Netherlands",
                     pais == 554 ~ "New Zealand",
                     pais == 578 ~ "Norway",
                     pais == 604 ~ "Peru",
                     pais == 616 ~ "Poland",
                     pais == 620 ~ "Portugal",
                     pais == 630 ~ "Puerto Rico (USA)",
                     pais == 634 ~ "Qatar",
                     pais == 642 ~ "Romania",
                     pais == 643 ~ "Russian Federation",
                     pais == 702 ~ "Singapore",
                     pais == 703 ~ "Slovak Republic",
                     pais == 704 ~ "Vietnam",
                     pais == 705 ~ "Slovenia",
                     pais == 724 ~ "Spain",
                     pais == 752 ~ "Sweden",
                     pais == 756 ~ "Switzerland",
                     pais == 764 ~ "Thailand",
                     pais == 780 ~ "Trinidad and Tobago",
                     pais == 784 ~ "United Arab Emirates",
                     pais == 788 ~ "Tunisia",
                     pais == 792 ~ "Turkey",
                     pais == 807 ~ "FYROM",
                     pais == 826 ~ "United Kingdom",
                     pais == 840 ~ "United States",
                     pais == 858 ~ "Uruguay",
                     pais == 970 ~ "B-S-J-G (China)",
                     pais == 971 ~ "Spain (Regions)",
                     pais == 972 ~ "USA (Massachusetts)",
                     pais == 973 ~ "USA (North Carolina)",
                     pais == 974 ~ "Argentina (Ciudad Aut?noma de Buenos)"))

# Removendo os n?o pa?ses
pisa <- pisa %>%
  filter(pais != "USA (Massachusetts)")

pisa <- pisa %>%
  filter(pais != "USA (North Carolina)")

pisa <- pisa %>%
  filter(pais != "Spain (Regions)")

pisa <- pisa %>%
  filter(pais != "Argentina (Ciudad Aut?noma de Buenos)")

#############################################################################
## CRIANDO A VARIAVEL REGIAO na base BR

#Deixando s? o numero na categoria estado (casos nao BR = NA)
pisa$estado <- as.factor(gsub("BRA", "", pisa$estado))

pisa$estado <- as.numeric(substr(pisa$estado, start = 1, stop = 2))

#Nomenado os estados
pisa <-  pisa  %>% 
  mutate(estado = case_when(
    estado == 1 ~ "ACRE",
    estado == 2 ~ "ALAGOAS",
    estado == 3 ~ "AMAP?",
    estado == 4 ~ "AMAZONAS",
    estado == 5 ~ "BAHIA",
    estado == 6 ~ "CEAR?",
    estado == 7 ~ "DISTRITO FEDERAL",
    estado == 8 ~ "ESP?RITO SANTO",
    estado == 9 ~ "GOI?S",
    estado == 10 ~ "MARANH?O",
    estado == 11 ~ "MATO GROSSO",
    estado == 12 ~ "MATO GROSSO DO SUL",
    estado == 13 ~ "MINAS GERAIS",
    estado == 14 ~ "PAR?",
    estado == 15 ~ "PARA?BA",
    estado == 16 ~ "PARAN?",
    estado == 17 ~ "PERNAMBUCO",
    estado == 18 ~ "PIAU?",
    estado == 19 ~ "RIO DE JANEIRO",
    estado == 20 ~ "RIO GRANDE DO NORTE",
    estado == 21 ~ "RIO GRANDE DO SUL",
    estado == 22 ~ "ROND?NIA",
    estado == 23 ~ "RORAIMA",
    estado == 24 ~ "SANTA CATARINA",
    estado == 25 ~ "S?O PAULO",
    estado == 26 ~ "SERGIPE",
    estado == 27 ~ "TOCANTINS"
  ))

# Nomenado as regioes
pisa$regiao <- NA


pisa <-  pisa  %>% 
  mutate(regiao = case_when(
    estado == "ACRE" ~ "Norte",
    estado == "ALAGOAS" ~ "Nordeste",
    estado == "AMAP?" ~ "Norte",
    estado == "AMAZONAS" ~ "Norte",
    estado == "BAHIA" ~ "Nordeste",
    estado == "CEAR?" ~ "Nordeste",
    estado == "DISTRITO FEDERAL" ~ "DF",
    estado == "ESP?RITO SANTO" ~ "Sudeste",
    estado == "GOI?S" ~ "Centro-oeste",
    estado == "MARANH?O" ~ "Nordeste",
    estado == "MATO GROSSO" ~ "Centro-oeste",
    estado == "MATO GROSSO DO SUL" ~ "Centro-oeste",
    estado == "MINAS GERAIS" ~ "Sudeste",
    estado == "PAR?" ~ "Norte",
    estado == "PARA?BA" ~ "Nordeste",
    estado == "PARAN?" ~ "Sul",
    estado == "PERNAMBUCO" ~ "Nordeste",
    estado == "PIAU?" ~ "Nordeste",
    estado == "RIO DE JANEIRO" ~ "Sudeste",
    estado == "RIO GRANDE DO NORTE" ~ "Nordeste",
    estado == "RIO GRANDE DO SUL" ~ "Sul",
    estado == "ROND?NIA" ~ "Norte",
    estado == "RORAIMA" ~ "Norte",
    estado == "SANTA CATARINA" ~ "Sul",
    estado == "S?O PAULO" ~ "Sudeste",
    estado == "SERGIPE" ~ "Nordeste",
    estado == "TOCANTINS" ~ "Norte"
  ))


###############################################################################
## CRIANDO VARIAVEL ASPIRA BINARIA

#retirndo labels
pisa$aspira <- as.integer(pisa$ocupacao) 
pisa$aspira_label <- as.integer(pisa$ocupacao) 

# criando binaria 
pisa <- pisa  %>% 
  mutate(aspira = if_else(ocupacao < 4000 & ocupacao >= 1000, 1, 0))

# OUTRA POSSIBILIDADE
# pisa <- pisa  %>% 
#  mutate(aspira = if_else(ocupacao < 3000 & ocupacao >= 2000, 1, 0))


# criando categoria para label
pisa <- pisa %>% 
  mutate(aspira_label = if_else(aspira == 1, "Sim", "N?o"))


###############################################################################
## CRIANDO EXA
pisa$ExA <- NULL

# Usando 4 categorias
pisa <- pisa  %>% 
  mutate(ExA = 
           case_when(expect == 1 & aspira == 1 ~ "(E1,A1)",
                     expect == 1 & aspira == 0 ~ "(E1,A0)",
                     expect == 0 & aspira == 1 ~ "(E0,A1)",
                     expect == 0 & aspira == 0 ~ "(E0,A0)"))

###############################################################################
# SELECIONANDO AS VARIAVEIS QUE VOU USAR

pisa <- pisa %>% 
  select(estado:ocupacao, sexo:repetiu, desemp:ExA)

##############################################################################
## EXPORTANDO BASE

export(pisa, "Pisa_2015_Tidy_3.sav")


