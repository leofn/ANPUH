#setwd("I:\\Dropbox\\R_experimentos\\ANPUH")
library(dplyr)
library(tidyverse)
library(lubridate)
library(quanteda)
library(stringi)
library(stm)
library(quanteda)
library(rlist)
library(anytime)
require(tidytext)
# Lendo o arquivo incialmente
#df <- read.csv("I:\\Dropbox\\R_experimentos\\ANPUH\\csv\\anais-anpuh-resumos.csv", encoding = "UTF-8")
#saveRDS(df, "data\\df.rds")

### COMEÇAR A PARTIR DAQUI

# Restaure o objeto df com a base de dados

df <- readRDS(file = "data\\df.rds")

#Limpando o campo resumo
df$Resumo <- gsub("Resumo:", "", df$Resumo)
df$Resumo <- gsub("RESUMO:", "", df$Resumo)

# convertendo Ano em Date
df$data <- as.Date(as.character(df$Ano), format = "%Y")


escravidao <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "escravidão"))

africa <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "África|africanos|africanas"))



africa %>% 
  mutate(date = lubridate::round_date(data, "year")) %>% 
  mutate(date = as.Date(data)) %>% 
  group_by(date) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_point() +
  xlab("") +
  ylab("Número de mensagens")+
  ggtitle("Menções à África dos resumos da ANPUH (2013-2019)") +
  theme_gray(12)+ 
  ## Centralizando o título, tem que ser aqui
  theme(plot.title = element_text(hjust = 0.5))



