########## ANÁLISE EXPLORATÓRIA DE DADOS COM O R ############
#### Minicurso apresentado para a I Semana da Demografia ####
######## IFCH-Unicamp, 24 a 27 de outubro de 2022 ###########
###### Thais Madeira Filipi (t229887@dac.unicamp.br) ########
#############################################################

#### Início ####

# Carregando as bibliotecas (por enquanto, somente o tidyverse)
library(tidyverse)

# Carregando o banco de dados

# setwd("~/Dropbox/Git/minicurso_R_SD/Analise Exploratoria de dados com R")

getwd()
banco <-read.csv("/cloud/project/minicurso_r_sd.csv", sep = ";", dec = ",")


#### Primeiras impressoes - panorama do banco ####

summary(banco)
head(banco, n=10) # Mostra as 10 primeiras observaçoes da base de dados
str(banco) # Mostra a estrutura da base de dados
dim(banco) # As dimensoes do dataset: linhas e colunas, respectivamente
names(banco) # Para ver os nomes das variaveis


#[1] "regiao"            "sexo"              "idade_quinquenal" 
#[4] "raca_cor"          "educ_mae"          "ocup_mae"         
#[7] "educ_pai"          "ocup_pai"          "educ"             
#[10] "pessoas"           "classesocial"      "rendafa"          
#[13] "rendarange"        "ocup"              "freq_esc"         
#[16] "peso"              "peso_pond"         "se_trab"          
#[19] "empregados"        "estuda_atualmente"

# Lembrando como os nomes estavam originalmente no banco:
# banco <- select(dados,regiao=region, sexo=sex, idade_quinquenal=age_group, 
#  raca_cor=color_race, educ_mae=mother_edu_ens, ocup_mae=mother_occ,
#  educ_pai=father_edu_ens, ocup_pai=father_occ, educ=level_study,
#  pessoas=pv1, rendafa, ocup= code_occ_main, freq_esc= ever_attend,
#  peso=weight, peso_pond=aweight, se_trab= c11_new,
#  empregados=employed, estuda_atualmente=currently_attend)

# Alternativamente, podemos usar a função skim, do pacote skimr
library(skimr)
skim(banco)

# O que retirar das primeiras impressões?
# - As categorias das das variáveis estão codificadas (tipo numérico); portanto, elas precisam ser rotuladas com 
# apoio do dicionário de dados (documentação do banco);
# - banco$pessoas é numérica e temos um valor muito discrepante (997); isso também pode ser um código;
# - educ,rendafa, ocup e estuda_atualmente tem valores faltantes (NAs ou missing data).


## Checar valores duplicados ##
# Ter um banco com variáveis de identificação

sum(duplicated(banco))

# Pare remover valores duplicados
# banco[!duplicated(banco),]
# [linha, coluna]; o ponto de exclamação significa "tudo menos isto" ou "não isto"
# Então, banco é igual a banco sem os duplicados do banco

# Primeiro vamos rotular as variáveis categóricas, e em seguida tratar as demais.

#### Rotulando as variaveis ####

banco <- banco %>% 
  mutate(educ = case_when(
    (estuda_atualmente == 1 & educ == 1) ~ "Fundamental incompleto ou menos",
    (estuda_atualmente == 2 & educ == 1) ~ "Fundamental completo até médio incompleto",
    (estuda_atualmente == 3 & educ == 1) ~ "Fundamental incompleto ou menos",
    (estuda_atualmente == 1 & educ == 2) ~ "Fundamental completo até médio incompleto",
    (estuda_atualmente == 2 & educ == 2) ~ "Médio completo até superior incompleto", 
    (estuda_atualmente == 3 & educ == 2) ~ "Fundamental completo até médio incompleto", 
    (estuda_atualmente == 1 & educ == 3) ~ "Médio completo até superior incompleto",
    (estuda_atualmente == 2 & educ == 3) ~ "Superior completo ou mais",
    (estuda_atualmente == 3 & educ == 3) ~ "Médio completo até superior incompleto",
    (estuda_atualmente == 1 & educ == 4) ~ "Médio completo até superior incompleto",
    (estuda_atualmente == 2 & educ == 4) ~ "Superior completo ou mais",
    (estuda_atualmente == 3 & educ == 4) ~ "Médio completo até superior incompleto",
    (estuda_atualmente == 1 & educ == 5) ~ "Superior completo ou mais",
    (estuda_atualmente == 2 & educ == 5) ~  "Superior completo ou mais", 
    (estuda_atualmente == 3 & educ == 5) ~ "Superior completo ou mais"), 
    regiao = case_when(
      (regiao == 1) ~"Norte", 
      (regiao == 2) ~ "Centro-Oeste", 
      (regiao == 3) ~"Nordeste", 
      (regiao == 4) ~ "Sul",
      (regiao == 5) ~ "Sudeste"), 
    sexo = case_when(
      (sexo == 1) ~ "Masculino",
      (sexo == 2) ~ "Feminino"), 
    idade_quinquenal = case_when(
      (idade_quinquenal == 1) ~ "15 a 19 anos",
      (idade_quinquenal == 2) ~"20 a 24 anos", 
      (idade_quinquenal == 3) ~"25 a 29 anos"),
    raca_cor = case_when(
      (raca_cor == 1) ~ "Branca", 
      (raca_cor == 2) ~ "Negra", 
      (raca_cor == 3) ~ "Negra", 
      (raca_cor == 4) ~ "Outros", 
      (raca_cor == 5) ~ "Outros", 
      (raca_cor == 6) ~ "Outros", 
      (raca_cor == 7) ~ "Outros", 
      (raca_cor == 8) ~ "Negra", 
      (raca_cor == 9) ~ "Outros", 
      (raca_cor == 10) ~"Outros", 
      (raca_cor == 11) ~"Outros", 
      (raca_cor == 997) ~"Outros"), 
    educ_mae = case_when(
      (educ_mae == 1) ~ "Ensino Fundamental ou menos", 
      (educ_mae == 2) ~ "Ensino Médio ou técnico", 
      (educ_mae == 3) ~ "Ensino Superior ou mais", 
      (educ_mae == 4) ~ "Ensino Superior ou mais", 
      (educ_mae == 9) ~ "Ensino Fundamental ou menos",
      (educ_mae == 99) ~ "Não sabe"),
    educ_pai = case_when(
      (educ_pai == 1)~ "Ensino Fundamental ou menos", 
      (educ_pai == 2)~ "Ensino Médio ou técnico", 
      (educ_pai == 3)~ "Ensino Superior ou mais", 
      (educ_pai == 4)~ "Ensino Superior ou mais", 
      (educ_pai == 9)~ "Ensino Fundamental ou menos",
      (educ_pai == 99)~ "Não sabe"),
    ocup=case_when(
      between(ocup, 1111, 3522) ~ "Classe 1", 
      between(ocup, 4110, 5419) ~ "Classe 2",
      between(ocup, 6111, 6225) ~ "Classe 3",
      between(ocup, 7111, 8350) ~ "Classe 2",
      between(ocup, 9111, 9996) ~ "Classe 3", 
      between(ocup, 9997, 9999) ~ "Outros"),
    ocup_mae = case_when (between(ocup_mae, 1111, 3522) ~ "Classe 1", 
                          between(ocup_mae, 4110, 5419) ~ "Classe 2",
                          between(ocup_mae, 6111, 6225) ~ "Classe 3",
                          between(ocup_mae, 7111, 8350) ~ "Classe 2",
                          between(ocup_mae, 9111, 9996) ~ "Classe 3", 
                          between(ocup_mae, 9997, 9999) ~ "Outros"),
    ocup_pai = case_when (between(ocup_pai, 1111, 3522) ~ "Classe 1",
                          between(ocup_pai, 4110, 5419) ~ "Classe 2",
                          between(ocup_pai, 6111, 6225) ~ "Classe 3",
                          between(ocup_pai, 7111, 8350) ~ "Classe 2",
                          between(ocup_pai, 9111, 9996) ~ "Classe 3", 
                          between(ocup_pai, 9997, 9999) ~ "Outros"),
    empregados = case_when(
      empregados == 0 ~ "Não",
      empregados == 1 ~ "Sim"),
    classesocial = case_when(
      classesocial == 1 ~ "Muito pobre",
      classesocial == 2 ~ "Pobre",
      classesocial == 3 ~ "Média baixa",
      classesocial == 4 ~ "Média",
      classesocial == 5 ~ "Média alta",
      classesocial == 6 ~ "Alta",
      classesocial == 9 ~ "NS/NR",
      classesocial == 997 ~ "NS/NR"),
    pessoas = ifelse(pessoas == 997, NA, pessoas)
  )
# Continuem com bancp$estuda_atualmente e banco$setrab


#### Como lidar com NAs: estratégias ####

# Tínhamos quatro colunas com valores faltantes: ocup, rendafa, ocup_pai e rendafa
# Com o summary já não conseguimos ver todos os NAs após a aplicação dos rótulos.

# Outra forma de checar NAs (fazendo as somas de NAs para todas as variáveis)

banco %>%
  select(everything()) %>%  # substitua em select de acordo com as necessidades
  summarise_all(funs(sum(is.na(.))))

# Aqui também apareceu educ_pai e pessoas.
# Uma forma mais rápida de checar uma coluna individual é com o r base:

sum(is.na(banco$rendafa))

### Vamos começar a tratar algumas das colunas com valores faltantes

# Importante prestarmos atenção no montante de NAs; se forem poucos, podemos só excluir 
# esses casos.

# ocup. No dicionário de dados, é uma resposta válida apenas para quem está empregado. 
# Portanto, podemos substituir os NAs por "não se aplica", pois a resposta não faz sentido para quem
# declarou não estar ocupado no momento, então não necessariamente é um dado perdido.

banco <- banco %>% replace_na(list(ocup = "não se aplica"))
table(banco$ocup)

# Poderíamos fazer o mesmo com ocupação do pai? O que vocês acham? (agregar à categoria 'outros', por exemplo)

# Agora vamos para rendafa, quem tem 63 NAs de partida.
# A variavel renda tende a ser complicada e com muitos NAs. Vamos checar o dicionario de dados.

# Valores variam de R$ 100 a R$ 32.000;
# 13 - nenhuma renda;
# 14 - Não sabe;
# 15 - Recusa.
# Não podemos usar o case_when como nos casos acima sob o risco de transformar a variável contínua
# em categórica. Vamos usar a funcao ifelse. O argumento dela e como segue:
# se a variavel assume x valor, atribua isso (senao), atribua aquilo.
#    rendafa = ifelse(rendafa == 13, 0,
#             ifelse(rendafa == 14, NA, 
#             ifelse(rendada == 15, NA, rendafa)))

# Rendafa inicialmente tinha 63 NAS. Vamos checar de novo, dessa vez para todo o banco



# Depois da transformação ainda temos 1335 valores faltantes de renda. Vamos investigar essa variavel primeiro.
# Os valores 13, 14 e 15, como vimos, são códigos. Então apesar dessa ser uma variável quant., vamos fazer
# uma tabela de frequencia e ver se os valores faltantes estão concentrados nesses primeiros numeros.
# O padrao das tabelas e apresentacao crescente, dos valores mais baixos aos mais altos.
table(banco$rendafa)

# A categoria 13 (Sem renda) tem 27 casos; a categoria 14 (Nao sabe), 1139; a categoria 15 (NR), 133.
# Encontramos nosso problema. Vamos investigar esses casos mais de perto (corresponde a um terco do banco)

banco %>% filter_all(any_vars(rendafa == 14))

#### O banco tem duas variáveis que identificam renda e uma que identifica classe social (esta última de
# auto-declaração). A categoria de renda agrupada sofre de não respostas da mesma forma que a renda
# familiar em valores contínuos. Algumas soluções caberiam aqui: excluir ou substituir os valores faltantes.
# A estratégia de substituir também contém diversas alternativas: substituição pelo valor médio, pelo
# valor médio dos vizinhos, por modelagem, entre outras. Para efeitos do exercício, contudo, vamos aproveitar
# a variável de classe social do banco. Podemos converter "muito pobre", "pobre" etc. em valores de salário
# mínimo do ano de referência, 2013.

table(banco$classesocial)

# Salário mínimo em 2013 - R$ 678,00
# Vamos substituir os valores usando como base o salário mínimo do ano

# Muito pobre: até 2 SM - R$ 678 (média entre renda 0 e 1356)
# Pobre: entre 2 até 4 SM - R$ 2.034 (média entre 2 e 4 SM)
# Média baixa: entre 4 até 7 SM - R$ 3.729
# Média: entre 7 até 10 SM - R$ 5763
# Média alta: de 10 até 20 SM - R$ 10.170
# Alta: acima de 20 SM - R$ 13.561 ou R$ 22.780 (média com o valor máximo)


banco <- banco %>% 
  mutate(rendafa =
           ifelse(rendafa == 13, 0,
           ifelse(rendafa == 14 & classesocial == "Muito pobre", 678,
           ifelse(rendafa == 14 & classesocial == "Pobre", 2034,
           ifelse(rendafa == 14 & classesocial == "Média baixa", 3729,
           ifelse(rendafa == 14 & classesocial == "Média", 5763,
           ifelse(rendafa == 14 & classesocial == "Média alta", 10170,
           ifelse(rendafa == 14 & classesocial == "Alta", 13561, rendafa))))))))

banco <- banco %>% 
  mutate(rendafa = 
           ifelse(rendafa == 14, NA, 
           ifelse(rendafa == 15, NA, rendafa)))

# Uma variável interessante é a renda per capita, por ela ter valores menos extremos e mais
# comparáveis. Agora podemos calcular como ela seria:

banco <- banco %>% 
  mutate(rendapercap = (rendafa/pessoas))


# Mais uma checagem.
banco %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

# Podemos remover os NAs. Mas antes vamos remover uma coluna que não será tratada.

banco <- select(banco, -rendarange)
head(banco, n = 3)

# Agora remover os NAs
banco <- banco %>% drop_na()
skim(banco)


#### Análise exploratória ####

#### Tabelas de frequência para as variáveis categóricas ####

library(formattable)
regtab <- banco %>%
  group_by(regiao) %>%
  summarise(cnt = n()) %>%
  mutate(perc = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(perc))
print(regtab)

sexotab <- banco %>%
  group_by(sexo) %>%
  summarise(cnt = n()) %>%
  mutate(perc = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(perc))
print(sexotab)

# Continuem com mais algum exemplo


# Mas também podemos automatizar este processo com uma função:
# janitor é uma biblioteca de limepza e tratamento de dados
library(janitor)

# Essa é uma das funções para tabela do janitor. Retorna o n (frequencia, count) e percentual. Se há dados
# faltantes (NAs), ele calcula o percentual total e o percentual válido.
tabyl(banco$sexo)


tabyl_n <- function(df, x) {
  df %>% 
    tabyl({{x}}) %>%
    adorn_pct_formatting(digits = 1) # convert counts to proportions
}

map(c("regiao", "sexo", "idade_quinquenal", "raca_cor", "educ_mae",
      "ocup_mae", "educ_pai",  "ocup_pai", "classesocial", "ocup",
      "educ", "empregados"), ~tabyl_n(banco, .x))

#### Visualização gráfica de variáveis categóricas ####

# Gráficos de barra e de pizza
# Sexo - O mais simples
X11()
ggplot(banco, aes(x = sexo)) + geom_bar()

# Construímos o restante a partir disso
# Vamos mudar as cores da barra e deixar as categorias em ordem decrescente
# Ao mesmo tempo em que salvamos o arquivo em png

# Regiões
X11()
png("regioes.png")
ggplot(banco, aes(x = reorder(regiao, regiao,
                              function(x)-length(x)))) +
  geom_bar(stat = "count", width = 0.7, fill = "#697a55", position = "dodge") +
  theme_light() +
  ggtitle("Regiões") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("Frequência")
dev.off()

# Colocando a frequência nas barras

# Classe social
png("classesocial.png")
ggplot(banco, aes(x = reorder(classesocial, classesocial,
                              function(x)-length(x)))) +
  geom_bar(stat = "count", width = 1, color = "black", fill = "#dba3a3") +
  theme_light() +
  labs(title = "Classe \n social", x = "", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust = -1, color = "black",
            position = position_dodge(0.9), size=3.5) # adicionar detalhes
dev.off()

# Fazendo o gráfico a partir de percentuais

# Raça/Cor
X11()
png("racacor-perc.png")
ggplot(banco, aes(x = raca_cor)) + 
  theme_classic() +
  geom_bar(aes(y = after_stat(count / sum(count))), color="#697a55", fill="white") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Percentual")
dev.off()

# Solução mais certeira: calcular os percentuais de antemão usando os recursos do tidyverse;
# Como o ggplot2 foi construído com a mesma lógica, as duas bibliotecas converam bem

# Educação da mãe

educmae <- banco %>%  
  count(educ_mae) %>%  
  mutate(pct = n / sum(n)) %>%   
  ggplot(aes(x = educ_mae, fill = educ_mae)) + 
  geom_col(aes(y = pct)) +
  scale_y_continuous(labels = scales::percent)

# Bônus: |> essa é outra forma de escrever o pipe

# Perceberam que rodamos o código acima e nada aconteceu? 
# Criar um objeto mais simples como base do gráfico pode ajudar a visualização quando as especificações adicionais
# do gráfico ficarem mais complexas. É o que vai acontecer agora que vamos adicionais mais detalhes.
# Vamos mudar aspectos da legenda, mudar labels e a paleta de cores.

educmae + theme_light() +
  labs(x = "", y = "", fill = "Educação da mãe", caption = "Fonte: OIT, 2013.") +
  scale_x_discrete(breaks = c("Ensino Fundamental ou menos","Ensino Médio ou técnico","Ensino Superior ou mais", "Não sabe"),
                   labels = c("E. Fundamental \n ou menos", "E. Médio \n ou técnico", "E. Superior \n ou mais", "Não \n sabe")) +
  theme(legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(0.3, 'cm'), #change legend key width
        legend.title = element_text(size = 10), #change legend title font size
        legend.text = element_text(size = 8)) + #change legend text font size 
  scale_fill_brewer(palette = "Dark2")
  

# Vamos para os gráficos de pizza

# Sem percentual

banco %>% 
  ggplot(aes(x = "", fill = educ_pai)) + 
  geom_bar(position = "fill", width = 1) + 
  coord_polar(theta = "y") + 
  labs(title = "",
    x = "", 
    y = "",
    fill = "Educação do pai") +
  theme_void() +
  scale_fill_brewer(palette = "Dark2")


# Com percentual

# Transformação do dado (parecido com o que fizemos acima), mas alocando para uma tabela à parte

educpai <- banco %>% 
  group_by(educ_pai) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

  
  ggplot(educpai, aes(x = "", y = perc, fill = educ_pai)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
    labs(title = "",
         x = "", 
         y = "",
         fill = "Educação do pai") +
    theme_void() +
    scale_fill_brewer(palette = "Dark2") 


#### Visualização gráfica das variáveis numéricas (univariada) ####

# Histogramas e boxplots

# Os mais simples: histograma, boxplot e densidade
# Histograma
ggplot(banco, aes(x = rendapercap)) + geom_histogram()
# Boxplot
ggplot(banco, aes(y = rendapercap)) + geom_boxplot()
# Densidade
ggplot(banco, aes(x = rendapercap)) + geom_density()


# Histograma com destaque da média
ggplot(banco, aes(x = rendapercap)) + geom_histogram(binwidth = 1, color = "#a02a1e", fill = "#a02a1e") + 
  geom_vline(aes(xintercept = mean(rendapercap)),
             color = "black", linetype = "dashed", size=1) +
  theme_light() +
  labs(x = "Renda per capita", y = "Frequência", title = "Renda per capita (e média)") +
  annotate("text", x = 600, y = 125, label = "média", angle = 90) # anotação manual mesmo!
  
mean(banco$rendapercap) # pistas sobre onde anotar no eixo x

# Boxplot sem outliers : modificar os limites do eixo y
ggplot(banco, aes(y = rendapercap)) + geom_boxplot(fill = "#30407a") +
ylim(0, 6000) +
  theme_light() +
theme(axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5), text = element_text(family = "Times New Roman")) +
  labs(x = "", y = "Renda per capita", title = "Distribuição da renda per capita")

# Densidade com trasparência
ggplot(banco, aes(x = rendapercap)) + 
  geom_density(color = "black", fill = "#76949f", alpha = 0.4) +
  theme_light() +
  labs(x = "Renda per capita", y = "Densidade")

# Histograma com densidade

ggplot(banco, aes(x = rendapercap)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 binwidth = 30) +
  geom_density(lwd = 1.2,
               linetype = 2,
               colour = 2) +
theme_light() +
labs(x = "Renda per capita", y = "Função densidade", 
     title = "Distribuição da renda per capita")
  

#### Questão de pesquisa ####

# O que pode estar associado à condição de estar inserido no mercado de trabalho para os jovens?

#### Com as variáveis categóricas ####

# Aqui podemos fazer tabelas cruzados e mais alguns gráficos específicos para responder uma
# questão de pesquisa.

table(banco$empregados)

# Tabelas cruzadas (com outras variáveis categóricas)

# Sexo #
 # (com o janitor)
banco %>%
  tabyl(sexo, empregados)

# Mais complexa
library(flextable)

banco %>%
  tabyl(sexo, empregados) %>% 
  adorn_totals(where = "row") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Sexo",
    col_name = "Empregago",
    placement = "combined") %>% # para imprimir como imagem
  flextable::flextable() %>%    # converte para uma imagem bonita
  flextable::autofit()          # formata uma linha por vez

# Com o dplyr

sexo_emprego <- banco %>% # begin with linelist
  group_by(empregados) %>%   # group by outcome 
  count(sexo) %>%      # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator

print(sexo_emprego)

# O formato de dados do sexo_emprego é o "long", em vez de "wide"
# Pode ser mais fácil gerar gráficos dessa forma a depender do caso

ggplot(data = sexo_emprego, aes(x = empregados, y = percent, fill = sexo)) + 
  geom_bar(stat = "identity", position = "dodge")

# Troquem o percent pelo n e vejam a diferença
# Coloquem título no gráfico e labels nos eixos x, y e legenda (dica: para legenda, o comando pode ser "fill" ou "color"
# a depender do parâmetro usado no mapping do aes)
# Bônus: troquem o tema do gráfico

# Região #

reg <- ggplot(banco, aes(x = regiao, fill = empregados)) + geom_bar(position = "fill") +
  labs(x = "", y = "Proporção", fill = "Empregado") +
  theme_light() +
  scale_fill_manual(values = c("#f0e8d7", "#a02a1e"))


print(reg)

# Região e sexo #

reg + facet_grid(cols = vars(sexo)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Região e raça/cor #

reg + facet_grid(cols = vars(raca_cor)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Idade (com percentuais) #

idade <- banco %>% 
  group_by(idade_quinquenal) %>%   # group by outcome 
  count(empregados) %>%      # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator
print(idade)

idade_plot <- ggplot(idade, aes(x = idade_quinquenal, y = percent, fill = empregados)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Percentual", fill = "Empregado") +
  theme_light() +
  scale_fill_manual(values = c("#2d3f2f", "#ccd2e6"))
print(idade_plot)

# Idade com percentual e sexo #

idade2 <- banco %>% 
  group_by(idade_quinquenal, sexo) %>%   # group by outcome 
  count(empregados) %>%      # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n)))


ggplot(idade2, aes(x = idade_quinquenal, y = percent, fill = empregados)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Percentual", fill = "Empregado") +
  theme_light() +
  scale_fill_manual(values = c("#2d3f2f", "#ccd2e6")) + 
  
  facet_grid(cols = vars(sexo)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Idade com percentual e raça/cor #

idade3 <- banco %>% 
  group_by(idade_quinquenal, raca_cor) %>%   # group by outcome 
  count(empregados) %>%      # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n)))


ggplot(idade3, aes(x = idade_quinquenal, y = percent, fill = empregados)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Percentual", fill = "Empregado") +
  theme_light() +
  scale_fill_manual(values = c("#2d3f2f", "#ccd2e6")) + 

  facet_grid(cols = vars(raca_cor)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# Novo agrupamento de idade

idade4 <- banco %>% 
  group_by(empregados) %>%   # group by outcome 
  count(idade_quinquenal) %>%      # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n)))
print(idade4)

ggplot(idade4, aes(x = idade_quinquenal, y = percent)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#2d3f2f") +
  labs(x = "", y = "Percentual") +
  theme_light() +
  facet_grid(cols = vars(empregados)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))



# Educação e ocupação dos pais #
library(gridExtra)

# Educação
edmae <- ggplot(banco, aes(x = educ_mae, fill = empregados)) + geom_bar(position = "dodge") +
  ylim(0, 1050) +
  labs(x = "", y = "Frequência", fill = "Filho empregado?", title = "Educaçao da mãe") +
  scale_fill_manual(values = c("#d9b268", "#6e3b2a")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")
  
print(edmae)
  
edpai <- ggplot(banco, aes(x = educ_pai, fill = empregados)) + geom_bar(position = "dodge") +
  ylim(0, 1050) +
  labs(x = "", y = "", fill = "Filho empregado?", title = "Educação do pai") +
  scale_fill_manual(values = c("#d9b268", "#6e3b2a")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "bottom")
print(edpai) 

# Juntando os dois plots (educação dos pais e situação de emprego)
grid.arrange(edmae, edpai, nrow = 1)

# Ocupação
ocmae <- ggplot(banco, aes(x = ocup_mae, fill = empregados)) + geom_bar(position = "dodge") +
  ylim(0, 1100) +
  labs(x = "", y = "Frequência", fill = "Filho empregado?", title = "Ocupação da mãe") +
  scale_fill_manual(values = c("#ccd2e6", "#2d3f2f")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(ocmae)

ocpai <- ggplot(banco, aes(x = ocup_pai, fill = empregados)) + geom_bar(position = "dodge") +
  ylim(0, 1100) +
  labs(x = "", y = "", fill = "Filho empregado?", title = "Ocupação do pai") +
  scale_fill_manual(values = c("#ccd2e6", "#2d3f2f")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "bottom")
print(ocpai) 

# Juntando os dois plots (ocupação dos pais e situação de emprego)
grid.arrange(ocmae, ocpai, nrow = 1)


# Classe social

banco %>%
  mutate(classesocial = 
           ifelse(classesocial == "NS/NR", NA, classesocial)) %>% 
  drop_na(classesocial) %>% 
ggplot(aes(y = factor(classesocial, levels = c("Muito pobre", "Pobre", "Média baixa", "Média",
                                                "Média alta", "Alta", "NS/NR")),
                fill = empregados)) + geom_bar(position = "dodge") +
  labs(x = "Frequência", y = "", fill = "Empregados", title = "Condição de emprego por classe social") +
  scale_fill_manual(values = c("dark grey", "forest green")) +
  theme_light()

# Assim como no caso de educação e ocupação dos pais, algumas categorias têm maior destaque pelo seu volume maior no
# bando. Por isso é sempre interessante também verificar a distribuição percentual.

banco %>%
  tabyl(classesocial, empregados) %>% 
  adorn_totals(where = "row") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Classe social",
    col_name = "Empregago",
    placement = "combined") %>% 
  flextable::flextable() %>%    
  flextable::autofit()


#### Com as variáveis numéricas #### 

# O mais simples - Renda per capita e condição de emprego

ggplot(banco, aes(x = empregados, y = rendapercap)) + geom_boxplot()

# Não parece que temos muitas diferenças entre os grupos, certo? Vamos checar os números propriamente.

# Calcular a média por grupo
statsrenda <- banco %>% 
  group_by(empregados) %>% #o group_by apresenta os  resultados separados pelas categorias dessa variável
  summarise(mean = mean(rendapercap), sd = sd(rendapercap), 
            median = median(rendapercap), n = n(), se = (sd / sqrt(n)))

statsrenda

# Standard error of the mean (SEM) measures how far the sample mean (average) of the data is likely to 
# be from the true population mean. The SEM is always smaller than the SD.

# De fato, para os que estão empregado a renda per capita familiar é ligeiramente maior

# Error bars represent standard error of the mean
ggplot(statsrenda, aes(x = empregados, y = mean)) + 
  geom_bar(position = "dodge", stat="identity", fill = "#b2c6ba") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "Situação de emprego", y = "Renda familiar per capita média", title = "Rendimendo familiar per capita \n por situação de trabalho (e erro padrão)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

 
#### Qui-quadrado ####
install.packages("descr")
library(descr)

# x = independente; y = dependente
# A ordem nas tabelas é: x, y; linha, coluna

# Só o teste
# Sexo
chisq.test(table(banco$sexo, banco$empregados))

# raça/cor
chisq.test(table(banco$raca_cor, banco$empregados))

# Tabela com valores observados e esperados
# Sexo
crosstab(banco$sexo, banco$empregados,  expected = TRUE, chisq = TRUE, plot = FALSE)

# Raça/cor
crosstab(banco$raca_cor, banco$empregados,  expected = TRUE, chisq = TRUE, plot = FALSE)

# Atenção com o tamanho das caselas! Qui-quadrado com variável que têm muitas categorias pode gerar combinações
# em que o número de observações em determinadas caselas pode ser muito baixo; isso afeta os resultados e interpretação
# Evitar caselas de valor esperado com menos de 5 casos

#### Regressão linear simples (com gráfico) ####

shapiro.test(banco$rendapercap)

# Coeficiente de correlação

cor(banco$pessoas, banco$rendapercap)
# [1] -0.3275096 um coeficiente de correlação baixo e negativo. Ou seja, quando maior o número de pessoas
# no domicílio, menor a renda

# Agora vamos para a regressão linear simples

# regressao <- lm(formula = y(dependente) ~ x(independente),
          # data = seubanco)

reg <- lm(formula = rendapercap ~ pessoas,
        data = banco)                      

summary(reg)

# Gerar o valor do intercepto e da inclinação da linha
coeff <- coefficients(reg)          
intercept <- coeff[1]
slope <- coeff[2]

# Representação gráfica
reg_plot <- ggplot(banco, aes(pessoas, rendapercap)) +   
  geom_point() +
  ylim(-200, 11500) +
  labs(x = "Número de pessoas no domicílio", y = "Renda familiar per capita", 
       title = "Relação entre renda familiar per capita \n e número de pessoas no domicílio") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
  

print(reg_plot)

# Adicionar a linha de regressão a este gráfico
reg_plot + geom_abline(intercept = intercept, slope = slope, color = "#a02a1e", 
                linetype = "dashed", size=1.5)
