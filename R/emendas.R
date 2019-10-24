library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# LENDO E ORGANIZANDO CAMARA -------------------------------------------------------------------

# Proposições relacionadas à PEC00619
# pec <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes/2192459/relacionadas"
# json <- fromJSON(pec)
# prop_pec <- json$dados
prop_pec <- read_csv("../input//prop_psc.csv")

# Proposições que são emendas na comissão, emenda aglutinativa de plenário ou emenda de redação
emendas <- prop_pec %>% filter(siglaTipo == "EMC" | siglaTipo == "EMA" | siglaTipo == "ERD")

# Função que busca autores para proposições específicas
autor <- function(id_emenda) {
  url <- sprintf("https://dadosabertos.camara.leg.br/api/v2/proposicoes/%s/autores", id_emenda)
  autores <- fromJSON(url)
  autores <- data.frame(autores$dados, id_emenda)
  return(autores)
}

# Autores de todas as emendas (EMC, EMA, ERD) - considera todas as assinaturas
autores <- purrr::map(emendas$id, autor) %>% bind_rows()

# Apenas proponentes
autores_proponentes <- autores %>% filter(proponente == 1) %>%
  mutate(id_deputado = as.numeric(str_extract(uri, "[0-9]*$")))


# Informações sobre deputados da legislatura atual (cod. 56)
url <- "https://dadosabertos.camara.leg.br/api/v2/deputados?idLegislatura=56&ordem=ASC&ordenarPor=nome"
deputados <- fromJSON(url)
deputados <- deputados$dados

# Cruzando informações sobre autores e deputados
autores_proponentes <- autores_proponentes %>%
  left_join(deputados, by = c('id_deputado' = 'id'))


# PLOTANDO CAMARA ----------------------------------------------------------------

# Quantidade de emendas propostas por partido
autores_proponentes %>%
  group_by(siglaPartido) %>%
  summarise(n = n_distinct(id_emenda)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=reorder(siglaPartido, n), y=n)) +
  geom_col() +
  coord_flip()

# Quantidade de emendas propostas por partido (contando todos os parlamentares)
autores_proponentes %>%
  group_by(siglaPartido) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=reorder(siglaPartido, n), y=n)) +
  geom_col() +
  coord_flip()

# Lista e guarda  emendas em que o PT votou em bloco
autores_proponentes %>% group_by(id_emenda) %>%
  summarise(n = n_distinct(id_deputado)) %>% arrange(desc(n))
pt_emendas <- c("2205946","2205947", "2205948","2205949","2205950",
                "2205951","2205952","2205954","2205953","2200893")

# Quantidade de emendas propostas por parlamentar considerando todas do PT
autores_proponentes %>%
  group_by(id_deputado, nome.x, siglaPartido) %>%
  summarise(n = n_distinct(id_emenda)) %>%
  arrange(desc(n)) %>%
  head(60) %>%
  ggplot(aes(x=reorder(nome.x, n), y=n, fill=siglaPartido)) +
  geom_col() +
  coord_flip() +
  theme(axis.text=element_text(size=6))

# Quantidade de emendas propostas por parlamentar removendo as do PT
autores_proponentes %>%
  filter(!(id_emenda %in% pt_emendas)) %>%
  group_by(id_deputado, nome.x, siglaPartido) %>%
  summarise(n = n_distinct(id_emenda)) %>%
  arrange(desc(n)) %>%
  head(25) %>%
  ggplot(aes(x=reorder(nome.x, n), y=n, fill=siglaPartido)) +
  geom_col() +
  geom_text(aes(label=siglaPartido)) +
  coord_flip() +
  theme(axis.text=element_text(size=6))


# LENDO E ORGANIZANDO SENADO ----------------------------------------------

url <- "http://legis.senado.leg.br/dadosabertos/materia/emendas/137999"
json <- fromJSON(url)

emendas <- json$EmendaMateria$Materia$Emendas$Emenda
autoria <- emendas$AutoriaEmenda$Autor

dados <- data.frame()
for (i in seq(1,594)) {
  CodigoEmenda <- emendas$CodigoEmenda[[i]]
  autor <- data.frame(autoria[[i]][[6]], CodigoEmenda)
  dados <- bind_rows(dados, autor)
}

emendas_autor <- emendas %>% select(1:6) %>% left_join(dados) %>%
  janitor::clean_names()
emendas_autor %>% glimpse()

# Alguns senadores estÃ£o sem o partido, busca na API
url <- "http://legis.senado.leg.br/dadosabertos/senador/lista/atual"
json <- fromJSON(url)
senadores <- json$ListaParlamentarEmExercicio$Parlamentares$Parlamentar$IdentificacaoParlamentar
senadores <- senadores %>%
  select(CodigoParlamentar, NomeParlamentar, SiglaPartidoParlamentar) %>%
  janitor::clean_names()

emendas_autor <- emendas_autor %>%
  left_join(senadores, by = 'codigo_parlamentar')

emendas_autor <- emendas_autor %>%
  mutate(sigla_partido_parlamentar = ifelse(is.na(sigla_partido_parlamentar.x),
                                            sigla_partido_parlamentar.y,
                                            sigla_partido_parlamentar.x))


# PLOTANDO SENADO ---------------------------------------------------------

# Quantidade de emendas propostas por partido
emendas_autor %>%
  group_by(sigla_partido_parlamentar) %>%
  summarise(n = n_distinct(numero_emenda)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=reorder(sigla_partido_parlamentar, n), y=n)) +
  geom_col() +
  coord_flip()

# Quantidade de emendas propostas por partido (contando todos os parlamentares)
emendas_autor %>%
  group_by(sigla_partido_parlamentar) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=reorder(sigla_partido_parlamentar, n), y=n)) +
  geom_col() +
  coord_flip()

# Quantidade de emendas propostas por parlamentar
emendas_autor %>%
  group_by(codigo_parlamentar, nome_parlamentar.x, sigla_partido_parlamentar) %>%
  summarise(n = n_distinct(numero_emenda)) %>%
  arrange(desc(n)) %>%
  head(25) %>%
  ggplot(aes(x=reorder(nome_parlamentar.x, n), y=n, fill=sigla_partido_parlamentar)) +
  geom_col() +
  geom_text(aes(label=sigla_partido_parlamentar)) +
  coord_flip() +
  theme(axis.text=element_text(size=6))
