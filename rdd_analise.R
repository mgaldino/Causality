library(electionsBR)
library(tidyverse)

#importa dados votos
df_20 <- elections_tse(year = 2020, type = "vote_mun_zone")
df_16 <- elections_tse(year = 2016, type = "vote_mun_zone")

# filtra
df_candidates_1t_ord_20 <- df_20 %>%
  filter(NR_TURNO == 1 & NM_TIPO_ELEICAO == "Eleição Ordinária" & DS_CARGO == "Prefeito") 

df_candidates_1t_ord_16 <- df_16 %>%
  mutate(resultado_eleito = ifelse(DS_SIT_TOT_TURNO == "ELEITO", 1, 0)) %>%
  group_by(SQ_CANDIDATO, SG_UF, NM_MUNICIPIO) %>%
  mutate(resultado_eleito = max(resultado_eleito)) %>%
  ungroup() %>%
  filter(NR_TURNO == 1 & DS_CARGO == "Prefeito" & CD_TIPO_ELEICAO == 2)


# importa dados finanças pessoais
# df_financas_20 <- elections_tse(year = 2020, type = "personal_finances")
df_financas_16 <- elections_tse(year = 2016, type = "personal_finances")


### importa dados candidatos
# df_candidates_20 <- elections_tse(year = 2020, type = "candidate")
df_candidates_16 <- elections_tse(year = 2016, type = "candidate")

# filtra
df_candidates_16 <- df_candidates_16 %>%
  filter(DS_CARGO == "PREFEITO") %>%
  select( SQ_CANDIDATO, NR_CPF_CANDIDATO, SG_PARTIDO, NM_COLIGACAO, NR_IDADE_DATA_POSSE,
          DS_GENERO, DS_GRAU_INSTRUCAO, DS_COR_RACA, DS_ESTADO_CIVIL, NM_UE,SG_UF)

df_candidates_20 <- elections_tse(year = 2020, type = "candidate")

# filtra
df_candidates_20 <- df_candidates_20 %>%
  filter(DS_CARGO == "PREFEITO") %>%
  select( SQ_CANDIDATO, NR_CPF_CANDIDATO, SG_PARTIDO, NM_COLIGACAO, NR_IDADE_DATA_POSSE,
          DS_GENERO, DS_GRAU_INSTRUCAO, DS_COR_RACA, DS_ESTADO_CIVIL, NM_UE,SG_UF)

voto_mun_16 <- df_candidates_1t_ord_16 %>%
  filter(DS_SITUACAO_CANDIDATURA == "APTO" & DS_CARGO == "Prefeito") %>%
  group_by(NM_TIPO_ELEICAO,NR_TURNO, DT_ELEICAO, SG_UF, NM_MUNICIPIO, NR_CANDIDATO) %>%
  summarise(voto = sum(QT_VOTOS_NOMINAIS),
            SQ_CANDIDATO = max(SQ_CANDIDATO),
            NM_UE = max(NM_UE),
            resultado_eleito = max(resultado_eleito)) 

voto_mun_16 <- voto_mun_16 %>%
  mutate(voto_total = sum(voto),
         perc_voto = voto/voto_total,
         centered_perc_voto = perc_voto - .5)

voto_mun_20 <- df_candidates_1t_ord_20 %>%
  filter(DS_SITUACAO_CANDIDATURA == "APTO" & DS_CARGO == "Prefeito") %>%
  group_by(NM_TIPO_ELEICAO,NR_TURNO, DT_ELEICAO, SG_UF, NM_MUNICIPIO, NR_CANDIDATO) %>%
  summarise(voto = sum(QT_VOTOS_NOMINAIS),
            SQ_CANDIDATO = max(SQ_CANDIDATO),
            NM_UE = max(NM_UE)) 


voto_mun_20 <- voto_mun_20 %>%
  mutate(voto_total = sum(voto),
         perc_voto = voto/voto_total,
         centered_perc_voto = perc_voto - .5)

voto_mun_20 <- voto_mun_20 %>%
  inner_join(df_candidates_20, join_by(SQ_CANDIDATO)) %>%
  ungroup()  %>%
  distinct(NR_CPF_CANDIDATO, .keep_all=TRUE)

voto_mun_16 <- voto_mun_16 %>%
  inner_join(df_candidates_16, join_by(SQ_CANDIDATO)) %>%
  ungroup() %>%
  distinct(NR_CPF_CANDIDATO, .keep_all=TRUE) 

voto_mun_20 <- voto_mun_20 %>%
  left_join(select(voto_mun_16, perc_voto, centered_perc_voto, NR_CPF_CANDIDATO, resultado_eleito),
            join_by(NR_CPF_CANDIDATO)) 

voto_mun_20 <- voto_mun_20 %>%
  filter(!is.na(resultado_eleito)) %>%
  mutate(incumbente = resultado_eleito)

voto_mun_20 %>%
  ggplot(aes(x = perc_voto.y, y = perc_voto.x)) +
  geom_point() + geom_vline(xintercept = .5) +
  labs(x = "Vote Share 2020 (%)", y = "Vote Share 2016 (%)") +
  theme_minimal() 

library(rdrobust)
rdplot(voto_mun_20$perc_voto.x, voto_mun_20$perc_voto.y, c=.5)

voto_bens1t <- voto_mun %>%
  ungroup() %>%
  filter(NR_TURNO == 1 & NM_TIPO_ELEICAO == "Eleição Ordinária") %>%
  inner_join(df_candidates_1t_ord, join_by(SQ_CANDIDATO, SG_UF, NM_UE)) %>%
  mutate(bens = as.numeric(VR_BEM_CANDIDATO)) %>%
  inner_join(df_candidates_a, join_by(SQ_CANDIDATO, SG_UF, NM_UE))

voto_gender <- voto_bens1t %>%
  mutate(perc_voto_arred = round(perc_voto, 2)) %>%
  group_by(perc_voto_arred, DS_GENERO) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count, na.rm = TRUE), percentage = count / total * 100)

voto_raca <- voto_bens1t %>%
  mutate(perc_voto_arred = round(perc_voto, 2)) %>%
  group_by(perc_voto_arred, DS_COR_RACA) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count, na.rm = TRUE), percentage = count / total * 100)

voto_casado <- voto_bens1t %>%
  mutate(perc_voto_arred = round(perc_voto, 2)) %>%
  group_by(perc_voto_arred, DS_ESTADO_CIVIL) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count, na.rm = TRUE), percentage = count / total * 100)


# Create the plot
voto_casado %>%
  filter(DS_ESTADO_CIVIL == "CASADO(A)") %>%
  ggplot(aes(x = perc_voto_arred, y = percentage)) +
  geom_point() + geom_vline(xintercept = .5) +
  labs(x = "Vote Share (%)", y = "Percentual de Casados (%)") +
  geom_smooth() + 
  theme_minimal()  

voto_raca %>%
  filter(DS_COR_RACA == "BRANCA") %>%
  ggplot(aes(x = perc_voto_arred, y = percentage)) +
  geom_point() + geom_vline(xintercept = .5) +
  labs(x = "Vote Share (%)", y = "Percentual de Brancos (%)") +
  theme_minimal() 

library(rdrobust)
rdplot(voto_bens1t$bens, voto_bens1t$perc_voto, c=.5)
rdplot(voto_bens1t$NR_IDADE_DATA_POSSE, voto_bens1t$perc_voto, c=.5)
rdplot(voto_bens1t$DS_GENERO, voto_bens1t$perc_voto, c=.5)
rdplot(voto_bens1t$SG_PARTIDO, voto_bens1t$perc_voto, c=.5)