library('bigrquery')
library('tidyverse')
library('beepr')

### Baixando os dados do Base dos Dados (por SQL)
projectid = "teste-base-dos-dados-341512"

# pegando dados de disputa para Senador
  # cargo = senador
  # ano %in% c(2002, 2006, 2010, 2014, 2018)
  # situacao = deferido ou deferido com recurso

#candidatos
sql <- "SELECT ano, sigla_uf, cpf, sequencial, numero, 
nome, nome_urna, numero_partido, sigla_partido, cargo, ocupacao, idade, 
genero, instrucao, estado_civil, sigla_uf_nascimento, raca
FROM `basedosdados.br_tse_eleicoes.candidatos` 
WHERE (ano = 2018 OR ano = 2014 OR ano = 2010 OR ano = 2006 OR ano = 2002)
AND cargo = 'senador' 
AND (situacao = 'deferido' OR situacao = 'deferido com recurso')"
tb <- bq_project_query(projectid, sql)
candidatos = bq_table_download(tb)


#despesas_candidato  ### Baixado manualmente pelo BigQuery
sql <- "SELECT ano, turno, sequencial_candidato, sigla_uf,
sigla_partido, data_despesa, valor_despesa
FROM `basedosdados.br_tse_eleicoes.despesas_candidato`
WHERE (ano = 2018 OR ano = 2014 OR ano = 2010 OR ano = 2006 OR ano = 2002)
AND cargo = 'senador'"
tb <- bq_project_query(projectid, sql)
despesas_candidato = bq_table_download(tb)

#receitas_candidato
sql <- "SELECT ano, sigla_uf, turno, cpf_candidato, numero_partido, 
sequencial_candidato, sigla_partido, sequencial_receita, data_receita, 
fonte_receita, origem_receita, especie_receita, valor_receita, 
cpf_cnpj_doador, cnae_2_doador, cargo_candidato_doador, 
sigla_partido_doador
FROM `basedosdados.br_tse_eleicoes.receitas_candidato` 
WHERE (ano = 2018 OR ano = 2014 OR ano = 2010 OR ano = 2006 OR ano = 2002)
AND cargo = 'senador'"
tb <- bq_project_query(projectid, sql)
receitas_candidato = bq_table_download(tb)

#resultados_candidato
sql <- "SELECT ano, sigla_uf, sequencial_candidato, resultado, votos
FROM `basedosdados.br_tse_eleicoes.resultados_candidato` 
WHERE (ano = 2018 OR ano = 2014 OR ano = 2010 OR ano = 2006 OR ano = 2002) 
AND cargo = 'senador'"
tb <- bq_project_query(projectid, sql)
resultados_candidato = bq_table_download(tb)

lista = list(candidatos, despesas_candidato, receitas_candidato, 
             resultados_candidato)
save(lista, file = 'dados-eleitorais.rda')

beep()






