library(tidyverse)
library(ggsci)
library(tidytext) # for tidytext:reorder_within( )
library(scales)

# Load
load('input/dados-eleitorais.rda')
candidatos = lista[[1]]
despesas_candidato = lista[[2]]
receitas_candidato = lista[[3]]
resultados_candidato = lista[[4]]
rm(lista)

# Name pattern

colnames(candidatos)[colnames(candidatos) == 
                                 "sequencial"] <- "sequencial_candidato"
colnames(candidatos)[colnames(candidatos) == 
                            "cpf"] <- "cpf_candidato"

#procurando um id
candidatos %>% select(ano, sequencial_candidato, sigla_uf) %>% duplicated() %>% sum()
candidatos %>% select(ano, sequencial_candidato, sigla_uf) %>% is.na() %>% sum()

# merge

create_id = function(x){
  mutate(x, id_candidato = paste0(ano, sequencial_candidato, sigla_uf))
} 

candidatos = create_id(candidatos)
despesas_candidato = create_id(despesas_candidato)
receitas_candidato = create_id(receitas_candidato)
resultados_candidato = create_id(resultados_candidato)

candidatos_full = left_join(candidatos, 
                            select(resultados_candidato,
                                   id_candidato, resultado, votos),
                            by = 'id_candidato' ) %>%
  filter(!(is.na(resultado) & sigla_uf != 'MT') )
rm(candidatos, resultados_candidato)

# agregando despesas por candidato
despesas_por_candidato = despesas_candidato %>%
  group_by(id_candidato, sigla_partido) %>%
  summarize(valor = sum(valor_despesa)) %>%
  arrange(by = desc(valor)) 
# despesas_por_candidato  %>% head(n = 10) 
# despesas_por_candidato  %>% tail(n = 10) 
# mean(despesas_por_candidato$valor)

# agregando receitas por candidato
receitas_por_candidato = receitas_candidato %>%
  group_by(id_candidato, sigla_partido) %>%
  summarize(valor = sum(valor_receita)) %>%
  arrange(by = desc(valor)) 

# Montando df completo (incluindo quem não reportou)
despesas_por_candidato = despesas_por_candidato %>% 
  select(id_candidato, valor) %>%
  right_join(candidatos_full, 
             by = 'id_candidato') 

receitas_por_candidato = receitas_por_candidato %>%
  select(id_candidato, valor) %>%
  right_join(candidatos_full, 
             by = 'id_candidato') 

# origem das receitas não identificadas
#receitas_candidato$origem_receita = 
 # replace_na(receitas_candidato$origem_receita, 'não identificado')
receitas_candidato = receitas_candidato %>%
  mutate(origem_receita = 
           replace_na(origem_receita, 'não identificado')) %>%
  mutate(origem_receita = ifelse(
    origem_receita == 'recursos de origens nao identificadas',
    'não identificado',
    origem_receita))

# despesa dos candidatos eleitos (por UF)
despesa_eleitos_por_uf = despesas_por_candidato %>%
  filter(resultado == 'eleito') %>%
  group_by(ano, sigla_uf) %>%
  summarise(despesa_media = mean(valor))


##################
###   PLOTS   ####
##################

# Configurações
theme_set(theme_linedraw())
theme_update(title = element_text(family = 'serif'),
             legend.text = element_text(family = 'serif'))
x_anos = list(scale_x_continuous(breaks = unique(candidatos_full$ano)),
                xlab('Ano'))
legend_bottom = list(theme(legend.position = "bottom" ),
                     guides(fill = guide_legend(ncol = 2, 
                                                title.position = 'top', 
                                                title.hjust = 0.5)))
milhoes = list(scale_y_continuous(labels = label_dollar(prefix = '',
                                                   scale = 1e-6)) )

# gastos e despesas totais reportados

despesas_anual = despesas_candidato %>%
  group_by(ano)%>%
  summarise(despesas = sum(valor_despesa)) 
receitas_anual = receitas_candidato %>%
  group_by(ano)%>%
  summarise(receitas = sum(valor_receita, na.rm = T))
full_join(despesas_anual, receitas_anual, by = 'ano') %>%
  gather(key = tipo, value = valor, -ano) %>%
  mutate(tipo = str_to_sentence(tipo)) %>%
  ggplot(aes(x = ano, y = valor, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_uchicago()+
  x_anos+
  legend_bottom+
  milhoes+
  labs(title = 'Receitas e Despesas totais por eleição',
  y = 'R$ milhões', fill = NULL)

ggsave('output/plot12.png')

# numero de candidatos e % de quem reportou gastos

plot_total_ano = function(x, title_text){ x %>%
  group_by(ano) %>%
  summarise(nao = sum(is.na(valor)), 
            sim = sum(!is.na(valor)) ) %>%
  gather(key = reportaram, value = freq, nao, sim)  %>%
  group_by(ano) %>%
  mutate(total_ano = sum(freq, na.rm = T),
         reportaram = ifelse(reportaram == 'nao', 
                             'não', reportaram) %>%
           str_to_title() ) %>% 
  ungroup() %>% mutate(pct = percent(freq/total_ano, 
                                     accuracy = 1)) %>%
  #plot
  ggplot(aes(x = ano, y = freq, fill = reportaram))+
  geom_bar(stat = 'identity', position = 'stack')+
  geom_text(aes(label = pct), color = 'white',
            position = position_stack(vjust = 0.5),
            size = 3)+
  geom_text(aes(y = total_ano, label = total_ano), vjust = -0.5,
            fontface = 'bold')+
  x_anos+
  scale_fill_uchicago()+
  ylim(c(0,350))+
  theme(panel.grid = element_line(color = 'white'))+
  labs(title = title_text, 
       y = 'Número de candidatos',
       fill = 'Reportaram?')
}

#plot_total_ano(receitas_por_candidato, 'report de receitas' )
plot_total_ano(despesas_por_candidato, 
               'Candidatos que reportaram despesas ao TSE' )

ggsave('output/plot1.png')

# plot origem da receita por partido 2018

receitas_por_partido_origem = receitas_candidato %>%
  select(ano, sigla_partido, origem_receita, valor_receita) %>%
  group_by(ano, sigla_partido, origem_receita) %>%
  summarise(valor = sum(valor_receita))
  # top partidos por receita
top_part_receita_2018 = receitas_candidato %>%
  filter(ano == 2018) %>%
  select(sigla_partido, valor_receita) %>%
  group_by(sigla_partido) %>%
  summarise(valor_total = sum(valor_receita)) %>%
  arrange(by = desc(valor_total)) %>%
  head(n = 10) 
receitas_por_partido_origem %>%
  filter(ano == 2018) %>%
  mutate(valor = valor/(10^6),
         origem_receita = str_to_sentence(origem_receita))%>%
  left_join(top_part_receita_2018, by = 'sigla_partido') %>%
  filter(sigla_partido %in% top_part_receita_2018$sigla_partido) %>%
  ggplot(aes(x= reorder(sigla_partido, valor_total),
             y = valor, fill = origem_receita))+
  geom_bar(stat = "identity", position = 'stack')+
  coord_flip()+
  scale_fill_uchicago()+
  labs(y = 'R$ milhões', x = 'Partidos', fill = 'Origem da receita', 
       title = 'Top 10 partidos que mais gastaram em 2018')

ggsave('output/plot2.png')

# plot origem receitas ao longo dos anos

receitas_por_origem = receitas_por_partido_origem %>% ungroup() %>%
  # para evitar dupla contagem:
  filter(origem_receita != 'recursos de outros candidatos',
         origem_receita != 'recursos de outros candidatos/comites') %>% 
  #agregando as categorias menos importantes
  mutate(origem_receita = 
           ifelse( origem_receita %in% c("recursos de pessoas juridicas",
                                           "recursos de pessoas fisicas",
                                           "recursos proprios",
                                           "recursos de partido politico" ,
                                           "não identificado"),
                   origem_receita, 
                   'outros') ) %>%
  group_by(ano, origem_receita) %>%
  summarise(valor_ano = sum(valor))
receitas_por_origem %>% 
  ggplot(aes(x = ano, y = valor_ano, fill = origem_receita))+
  geom_bar(stat = 'identity', position = 'stack')+
  legend_bottom+
  scale_fill_uchicago()+
  x_anos
ggsave('output/plot_extra1.png')

receitas_por_origem_percentual = receitas_por_origem %>% 
  group_by(ano) %>%
  mutate(total_ano = sum(valor_ano, na.rm = T)) %>% 
  ungroup() %>% mutate(pct = percent(valor_ano/total_ano, 
                                     accuracy = 1)) %>%
  mutate(pct = ifelse(pct<1, NA, pct))
  
receitas_por_origem_percentual %>% 
  filter(ano != 2002) %>%
  mutate(origem_receita = str_to_sentence(origem_receita)) %>%
  ggplot(aes(x = ano, y = valor_ano, fill = origem_receita))+
  geom_bar(stat = 'identity', position = 'fill')+
  geom_text(aes(label = pct), position = position_fill(vjust = 0.5))+
  legend_bottom+
  scale_fill_uchicago()+
  x_anos+ scale_y_continuous(labels = percent)+
  labs(caption = '*classificação não disponível para 2002',
       fill = 'Origem da receita' ,
       y = NULL,
       title = 'Composição das receitas totais por ano')

ggsave('output/plot3.png')

# plot candidatos que mais gastaram

despesas_por_candidato %>% 
  arrange(desc(valor)) %>%
  group_by(ano) %>%
  slice(1:5) %>% 
  mutate(nome_part = paste(nome_urna, sigla_partido, sep = ' - '),
         sigla_uf = ifelse(sigla_uf %in% c('MG', 'SP', 'RJ'),
                           sigla_uf, 'Outro')) %>%
  ggplot(aes(x = reorder_within(nome_part, by = valor,
                                within = ano), 
             y = valor, fill = sigla_uf))+
    geom_bar(stat = "identity")+
    coord_flip()+
    facet_grid(ano ~ ., scales = 'free')+
    scale_fill_uchicago()+
    scale_x_reordered()+
    milhoes+
    labs(y = 'Despesas (R$ milhões)', x = 'Candidato - Partido', fill = 'UF', 
         title = 'Candidatos que mais gastaram',
         subtitle = 'Top 5 por ano')

ggsave('output/plot4.png')

# plot UF com campanha mais cara

  # com base no gasto dos eleitos
despesa_eleitos_por_uf %>% ungroup() %>%
  arrange(desc(despesa_media)) %>%
  group_by(ano) %>%
  slice(1:5)%>%
  ggplot(aes(x = reorder_within(sigla_uf, by = despesa_media, 
                                within = ano), 
             y = despesa_media))+
  geom_bar(stat = "identity", fill = '#155F83FF')+
  theme(legend.position = 'none')+
  facet_grid(. ~ ano, scales = 'free')+
  milhoes+
  scale_x_reordered()+
  labs(caption = 'As despesas dos candidatos eleitos não foram reportadas para as seguintes UFs: 
       AP (2002) e DF (2010, 2014 e 2018)',
       y = 'R$ milhões', x = 'UF',
       title = 'Despesa média dos candidatos eleitos')+
  theme(plot.caption = element_text(color = 'gray40'))
#obs: alguns eleitos nao reportaram gasto
despesa_eleitos_por_uf[is.na(despesa_eleitos_por_uf$despesa_media), ]

ggsave('output/plot5.png')
  
# plot gastos dos eleitos ao longo do tempo por UF

macroregioes = readxl::read_excel('input/macroregioes.xlsx')
despesa_eleitos_por_uf %>%
  left_join(macroregioes, by = 'sigla_uf') %>%
  ggplot(aes(x = ano, y = despesa_media, color = sigla_uf))+
  geom_line()+
  geom_point()+
  facet_grid(regiao ~ ., scales = 'free')

ggsave('output/plot_extra2.png')

# plot partido que mais gastou em media por candidato

despesas_por_candidato %>%
  filter(!is.na(valor)) %>%
  group_by(ano, sigla_partido) %>%
  summarise(n_candidatos = n(), valor_total = sum(valor, na.rm = T)) %>%
  mutate(valor_por_candidato = valor_total/(n_candidatos*10^6),
         bar_label = paste0(n_candidatos, ' candidatos')) %>%
  arrange(by = desc(valor_por_candidato)) %>%
  group_by(ano) %>%
  slice(1:5) %>%
  ggplot(aes(x= reorder_within(sigla_partido, 
                               by = valor_por_candidato,
                               within = ano),
             y = valor_por_candidato, 
             fill = n_candidatos) )+
    geom_bar(stat = "identity")+
    #geom_text(aes(label = bar_label, hjust = 1), 
     #         size = 2, fontface = 'bold')+
    coord_flip()+
    labs(y = 'R$ milhões por candidato', x = 'Partidos', 
         caption = '*Considerando apenas o número de candidatos que reportou suas despesas',
         fill = '# de candidatos', 
         title = 'Gasto médio por candidato')+
    facet_grid(ano ~ ., scales = 'free')+
    scale_fill_distiller(type = 'seq', palette = 'YlOrRd', 
                         direction = 1)+
    scale_x_reordered()

ggsave('output/plot6.png')

# bar plot por resultado, genero, raca
  #raca só tem 2014 e 2018

candidatos_full %>% 
  filter(!is.na(resultado), ano == 2014 | ano == 2018) %>%
  ggplot(aes(x = genero, fill = raca)) +
  geom_bar(position = position_dodge2(preserve = "single"), 
           color = 'black') +
  facet_grid( resultado ~ ano , scales = 'free' ) +
  scale_fill_uchicago()+
  labs(fill = 'Raça', x = 'Gênero', y = NULL, 
       title = 'Distribuição dos candidatos por raça',
       caption = 'Informação de raça disponível apenas para 2014 e 2018' )
ggsave('output/plot7.png')

candidatos_full %>% 
  filter(!is.na(resultado), !is.na(genero)) %>%
  mutate(resultado = ifelse(resultado == 'nao eleito', 
                            'não eleito', resultado) %>%
           str_to_title() ) %>%
  ggplot(aes( x = ano, fill = genero)) +
  geom_bar(position = 'fill', 
           color = 'black') +
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  facet_grid(. ~ resultado)+
  scale_fill_uchicago()+
  x_anos+
  labs(fill = 'Gênero', y = NULL)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = 'Proporção de candidatos por gênero')

ggsave('output/plot8.png')

# scatter plot votos vs despesas

despesas_por_candidato %>% 
  filter(resultado == 'eleito') %>%     # eleitos
  ggplot(aes(x = valor, y = votos, color = genero)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(x = valor, y = votos, color = NULL), method = 'lm', se = F)+
  scale_color_lancet()+
  labs(caption = 'candidatos eleitos')+
  facet_wrap(ano ~.)

ggsave('output/plot_extra3.png')


### BOX PLOTS

# box plot por resultado

despesas_por_candidato %>% 
  filter(!is.na(resultado)) %>%
  ggplot(aes(x = resultado, y = valor, color = resultado)) +
  geom_boxplot(outlier.alpha = 0.25) +
  scale_color_lancet()+
  facet_grid(. ~ ano)+
  theme(legend.position = 'bottom')+
  milhoes+
  labs(title = 'Despesas comparadas: Eleitos e Não eleitos',
       x = NULL, y = 'Despesa (R$ milhões)')

ggsave('output/plot9.png')

# box plot por genero

despesas_por_candidato %>%
  filter(!is.na(resultado)) %>%
  ggplot(aes(x = resultado, y = valor, color = genero)) +
    geom_boxplot(outlier.alpha = 0.25) +
    scale_color_lancet()+
    facet_grid(. ~ ano)+
    theme(legend.position = 'bottom')+
    milhoes+
    labs(title = 'Despesas comparadas por gênero',
       x = NULL, y = 'Despesa (R$ milhões)',
       color = 'Gênero')

ggsave('output/plot10.png')

# box plot por raca

# despesas_por_candidato %>%
#   filter(!is.na(resultado)) %>%
#   ggplot(aes(x = resultado, y = valor, color = raca)) +
#   geom_boxplot(outlier.alpha = 0.25, outlier.shape = NA) +
#   scale_color_lancet()+
#   theme_linedraw()+
#   ylim(c(0, 5*10^6)) 

despesas_por_candidato %>%
  filter(!is.na(resultado), ano %in% c(2014, 2018)) %>%
  ggplot(aes(x = resultado, y = valor, color = raca)) +
  geom_boxplot(outlier.alpha = 0.25, outlier.shape = NA) +
  scale_color_lancet()+
  facet_grid(. ~ ano)+
  theme(legend.position = 'bottom')+
  scale_y_continuous(labels = label_dollar(prefix = '',
                                           scale = 1e-6),
                     limits = c(0, 5*10^6)) + 
  labs(title = 'Despesas comparadas por raça',
       x = NULL, y = 'Despesa (R$ milhões)',
       color = 'Raça')
  
ggsave('output/plot11.png')

