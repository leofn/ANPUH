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

# Restaure o objeto df com a base de dados

df <- readRDS(file = "data/df.rds") # para windows "data\\df.rds"

#Limpando o campo resumo
df$Resumo <- gsub("Resumo:", "", df$Resumo)
df$Resumo <- gsub("RESUMO:", "", df$Resumo)

# convertendo Ano em Date
df$data <- as.Date(as.character(df$Ano), format = "%Y")


escravidao <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "escravidão|escravos|escravas|escravizados|escravizadas|cativeiro|cativos|cativas"))

africa <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "África|africanos|africanas"))

digital <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "digital|digitais|tecnologia|informática"))

ditadura <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "ditadura"))

hist_publ <- df %>% 
  # Filtrando linhas que contém o termo
  filter(str_detect(Resumo, "história pública|História Pública"))

hist_publ %>% 
  mutate(date = lubridate::round_date(data, "year")) %>% 
  mutate(date = as.Date(data)) %>% 
  group_by(date) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_point() +
  xlab("") +
  ylab("Número de mensagens")+
  ggtitle("Menções à História Pública dos resumos da ANPUH (2013-2019)") +
  theme_gray(12)+ 
  ## Centralizando o título, tem que ser aqui
  theme(plot.title = element_text(hjust = 0.5))



