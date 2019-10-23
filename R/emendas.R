library(jsonlite)
library(dplyr)
library(stringr)

# Proposições relacionadas à PEC00619
pec <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes/2192459/relacionadas"
json <- fromJSON(pec)
prop_pec <- json$dados

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

# Quantidade de emendas propostas por partido
autores_proponentes %>%
  group_by(siglaPartido) %>%
  summarise(n = n_distinct(id_emenda)) %>%
  arrange(desc(n))

# Quantidade de emendas propostas por partido (contando todos os parlamentares)
autores_proponentes %>%
  group_by(siglaPartido) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

autores_proponentes %>% group_by(id_emenda) %>%
  summarise(n = n_distinct(id_deputado)) %>% arrange(desc(n))

autores_proponentes %>%
  group_by(id_deputado, nome.x, siglaPartido) %>%
  summarise(n = n_distinct(id_emenda)) %>%
  arrange(desc(n))

