# Projeto-Estatistica
 Projeto de Estatística/Ciência de dados do Verão do mestrado da PUC-Rio 2022

Este repositório contém os códigos, inputs e gráficos criados para fazer o pdf do artigo "Novas Regras do Jogo: Análise do Financiamento de Campanhas para Senador"

Lista de conteúdo:

* **Input**
  * **dados-eleitorais-senadores.R** - R Script que baixa os dados do BigQuery do Base dos Dados e os salva em um arquivo .rda
  * **dado-eleitorais.rda** - arquivo .rda criado pelo R script
  * **macroregioes.xlsx** - arquivo de referência para as macroregiões

* **analise_receitas_eleitorais_senadores.R** - R script que usa `dados-eleitorias.rda` como input e cria os gráficos usados no artigo

* **Output** - contém todos os gráficos criados por `analise_receitas_eleitorais_senadores.R`

* **Dockerfile** - arquivo dockerfile para rodar `analise_receitas_eleitorais_senadores.R` no Docker

* **Projeto_Estatistica_Verao.pdf** - artigo final
