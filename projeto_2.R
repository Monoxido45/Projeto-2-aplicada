library(tidyverse)

sist_complexo <- "Trabalho_2/D25.xlsx" |>
  readxl::read_xlsx() |>
  arrange(Ano, Mês) |>
  mutate(data = as.Date(str_pad(paste0("01-", Mês, "-", Ano),
                                           7, pad = 0), 
                                   '%d-%m-%Y'),
         # transformando os meses em trimestres
         trimestre = case_when(Mês %in% c(1, 2, 3) ~ "1 Trimestre",
                                      Mês %in% c(4, 5, 6) ~ "2 Trimestre",
                                      Mês %in% c(7, 8, 9) ~ "3 Trimestre",
                                      Mês %in% c(10, 11, 12) ~ "4 Trimestre"))


# Descritivas -----------------------------------------------

# descritivas univariadas
# histograma para a contagem
sist_complexo |>
  ggplot(aes(x = NR))+
  geom_bar(aes(y = ..prop..), 
                 colour = "black", fill = "white",
                 alpha = 0.5)+
  geom_density(colour = "dodgerblue3", size = 1)+
  labs(y = "Proporção",
       x = "Número de reparos") +
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(min(sist_complexo$NR),
                                  max(sist_complexo$NR), by = 1))
ggsave("numeros_reparos_cont.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 8, 
       height = 6)


# descritivas de serie temporal e bivariadas
# transformando em objeto ts
sist_ts <- ts(sist_complexo$NR, start = sist_complexo$Ano[1], frequency = 12)

# grafico
sist_complexo |>
  ggplot(aes(x = data, y = NR))+
  geom_line(colour = "dodgerblue3") +
  labs(y = "Quantidade de reparos",
       x = "Mês/Ano")+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(min(sist_complexo$NR),
                                  max(sist_complexo$NR), by = 2))
ggsave("series_reparos.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 10, 
       height = 8)

# seasonplot para nossa serie
# checando tendencia e sazonalidade
forecast::ggseasonplot(sist_ts, xlab = "Mês",  ylab = "Número de reparos")+
  theme_bw() +
  labs(title= "",
       colour = "Ano")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_brewer(palette = "Set1")
ggsave("season_plot_reparos.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 10, 
       height = 8)

# ajustando medias moveis na serie
autoplot(sist_ts, series="Original") +
  autolayer(forecast::ma(sist_ts, 2), series = "2-MA")+
  autolayer(forecast::ma(sist_ts, 4), series = "4-MA")+
  xlab("Tempo") + ylab("Número de reparos") +
  ggtitle("Série temporal de valores simulados")+
  scale_colour_manual(values=c("Original"="grey50",
                               "2-MA" = "dodgerblue3",
                               "4-MA" = "red")) +
  ggtitle("Média móvel de 2 e 4 meses ajustadas")+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

# teste de cox stuart
randtests::cox.stuart.test(sist_ts)

# dotplots das contagens pelo ano
sist_complexo |>
  ggplot(aes(x = as.factor(Ano), y = NR,
             fill= as.factor(Ano))) +
  geom_dotplot(binaxis='y', stackdir='center') +
  labs(title= "",
       fill = "Ano",
       x = "Ano",
       y = "Número de reparos")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")
ggsave("dotplots_ano.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 8, 
       height = 6)

# dotplots das contagens pelo mes
cores <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(12)

sist_complexo |>
  ggplot(aes(x = as.factor(Mês), y = NR,
             fill= as.factor(Mês))) +
  geom_dotplot(binaxis='y', stackdir='center') +
  labs(title= "",
       fill = "Mês",
       x = "Mês",
       y = "Número de reparos")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = cores)
ggsave("dotplots_mes.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 8, 
       height = 6)

# dotplots por trimestre
sist_complexo |>
  ggplot(aes(x = trimestre, y = NR,
             fill= trimestre)) +
  geom_dotplot(binaxis='y', stackdir='center') +
  labs(title= "",
       fill = "Trimestre",
       x = "Trimestre",
       y = "Número de reparos")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = cores)
ggsave("dotplots_trimestre.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 8, 
       height = 6)

sist_complexo |>
  ggplot(aes(x = as.factor(Mês), y = NR,
             fill= as.factor(Mês))) +
  geom_dotplot(binaxis='y', stackdir='center') +
  labs(title= "",
       fill = "Mês",
       x = "Mês",
       y = "Número de reparos")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = cores)
ggsave("dotplots_mes.pdf", 
       path = "Trabalho_2/figuras",
       device = "pdf",
       width = 10, 
       height = 8)

# graficos de interacao
sist_complexo |> 
  group_by(trimestre, Ano) |>
  summarise(media_NR = mean(NR)) |>
  ggplot(aes(x = as.factor(trimestre), y = media_NR, color = as.factor(Ano),
             group = as.factor(Ano))) +
  geom_line() +
  geom_point()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Ano",
       x = "Trimestre",
       y = "Número de reparos médio") +
  scale_colour_manual(values = cores)


# acfs
p1 <- forecast::ggAcf(sist_ts, 48)+
  ylim(c(-0.6, 0.7)) +
  labs(title = "ACF para a série de contagens")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


p2 <- forecast::ggPacf(sist_ts, 48)+
  ylim(c(-0.4, 1))+
  labs(title = "PACF para a série de contagens")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

ggpubr::ggarrange(p1, p2, nrow = 2)
  ggsave(filename  = "autocorr_contagem.pdf",
         path = "Trabalho_2/figuras",
         width = 12, height = 8)

  
# tudo isso nos aponta que provavelmente a série é meio de ruidos brancos

# Modelagem por mlg -------------------------------------------------------
# modelo só com mes
mod_rep <- glm(NR ~ as.factor(Ano) + as.factor(Mês), data = sist_complexo,
               family = poisson)

mod_rep |> summary()
mod_rep |> broom::tidy()

# checando residuos
# checando envelope
set.seed(125)
envelope <- hnp::hnp(mod_rep, sim = 300, resid.type = 'deviance', how.many.out = T,
              plot.sim = FALSE, conf = 0.95, scale = T)

# funcao que plot envelope
plot_envelope <- function(envelope_obj){
  tibble(quantis = envelope_obj$x,
         int_inf = envelope_obj$lower,
         int_med = envelope_obj$median,
         int_up = envelope_obj$upper,
         res = envelope_obj$residuals) |>
    ggplot(aes(x = quantis,
               y = res)) +
    geom_point() +
    geom_line(aes(y = int_inf))+
    geom_line(aes(y = int_med), linetype = "dashed")+
    geom_line(aes(y = int_up))+
    labs(x = "Quantis teóricos",
         y = "Resíduos",
         title = "Gráfico de envelope")
}

envelope |>
  plot_envelope()+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))
ggsave(filename  = "envelope_poisson_comp.pdf",
       path = "Trabalho_2/figuras",
       width = 8, height = 6)
set.seed(125)

# analise de residuos
# pearson, deviance e quantilico
plot_res <- function(mod_rep, name_path){
dados_res <- data.frame(resids = resid(mod_rep, type = "deviance"),
                        qresids = statmod::qresiduals(mod_rep),
                        pearson = resid(mod_rep, type = "pearson"),
                        fitted = mod_rep$fitted.values,
                        predictors = mod_rep$linear.predictors)

p1 <- dados_res |>
  ggplot(aes(x = fitted, y = resids))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  theme_light(base_size = 9)+
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Resíduos deviance contra 
       valores ajustados",
       x = "Valores ajustados",
       y = "Resíduos")

p2 <- dados_res |>
  ggplot(aes(x = predictors, y = resids))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  theme_light(base_size = 9)+
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Resíduos deviance contra 
       preditor linear",
       x = "Preditor linear",
       y = "Resíduos")

p3 <- dados_res |>
  ggplot(aes(x = resids, y = pearson))+
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
  theme_light(base_size = 9)+
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Resíduos deviance contra resíduos de Pearson",
       x = "Componente do desvio",
       y = "Resíduo de Pearson")

p4 <- dados_res |>
  ggplot(aes(sample = qresids))+
  stat_qq(distribution = qnorm, color = "darkorchid2", alpha = 0.65, size = 1.25)+
  stat_qq_line(distribution = qnorm, color = "darkblue") +
  labs(title = "Qqplot dos residuos quantilicos",
       x = "quantis teóricos",
       y = "quantis amostrais") +
  theme_light(base_size = 9)+
  theme(text = element_text(size = 9, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
ggsave(filename  = glue::glue("residuos_poisson_{name_path}.pdf"),
       path = "Trabalho_2/figuras",
       width = 12, height = 8)
return(dados_res)
}

# teste de normalidade para residuos quantilicos
testa_qresids <- function(resids){
  funs <- list("Shapiro" = shapiro.test, 
               "Lilliefors" = nortest::lillie.test,
               "Anderson Darling" = nortest::ad.test, 
               "Shapiro Francia" = nortest::sf.test, 
               "Cramer Von Mises" = nortest::cvm.test)
  funs |> map_dfr(function(funs, dados){funs(dados)$p.value}, dados = resids)
}

# residuos do modelo completo
res_comp <- plot_res(mod_rep, "mod_comp")

# teste de normalidade nos res quantilicos
res_comp |>
  pull(qresids) |>
  testa_qresids()

# possivel melhora do modelo
mod_rep_tri <- glm(NR ~ as.factor(Ano) + as.factor(trimestre), data = sist_complexo,
               family = poisson)

# residuos
set.seed(350)
res_tri <- mod_rep_tri |> plot_res("mod_tri")

# teste de normalidade nos residuos quantilicos
res_tri |>
  pull(qresids) |>
  testa_qresids()

envelope <- hnp::hnp(mod_rep_tri, sim = 300, resid.type = 'deviance', how.many.out = T,
                     plot.sim = FALSE, conf = 0.95, scale = T)

envelope |> plot_envelope()+
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))
ggsave(filename  = "envelope_modelo_tri.pdf",
       path = "Trabalho_2/figuras",
       width = 8, height = 6)

# tabela ANODEV
car::Anova(mod_rep_tri)


# modelo do stepwise
mod_step <- mod_rep_tri |> step()
envelope <- hnp::hnp(mod_step, sim = 300, resid.type = 'deviance', how.many.out = T,
                     plot.sim = FALSE, conf = 0.95, scale = T)

envelope |> plot_envelope()


# residuos
res_step <- mod_step |> plot_res("mod_step")

# teste de normalidade nos residuos quantilicos
res_step |>
  pull(qresids) |>
  testa_qresids()

# stepwise no modelo original
mod_step_org <- mod_rep |> step()

library(car)
# modelo com interacao entre trimestre e ano
mod_rep_int <- glm(NR ~ as.factor(Ano) + as.factor(trimestre) + 
                     as.factor(Ano):as.factor(trimestre), data = sist_complexo,
                   family = poisson)
mod_rep_int |> summary()

envelope <- hnp::hnp(mod_rep_int, sim = 300, resid.type = 'deviance', how.many.out = T,
                     plot.sim = FALSE, conf = 0.95, scale = T)

envelope |> plot_envelope() +
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))
ggsave(filename  = "envelope_modelo_int.pdf",
       path = "Trabalho_2/figuras",
       width = 8, height = 6)


# tabela ANODEV
car::Anova(mod_rep_int, test.statistic = "LR")

# tipo III
car::Anova(mod_rep_int, type = "III")

# residuos
set.seed(453)
res_int <- mod_rep_int |> plot_res("mod_int")
res_int |>
  pull(qresids) |>
  testa_qresids()

# comparando AIC's
mod_idade <- glm(NR ~ as.factor(Ano), data = sist_complexo,
               family = poisson)

list(mod_rep_int, mod_rep_tri, mod_idade, mod_step) |> map_dbl(AIC)

mod_rep_tri |> summary()


# Analise de outliers do modelo final -------------------------------------

# mod_step eh esse modelo
limiar_cook <-  0.1
limiar_hat <- 0.1
limiar_deviance <- 2

# d de cook
cook <- cooks.distance(mod_step)

# matriz chapeu
hat <- hatvalues(mod_step)

# residuos deviance studentizados
deviance_stud = resid(mod_step)/(sqrt(1 - hat))

# dados para plotar
ggplot_data = data.frame("cook" = cook,
                         "chapeu" = hat,
                         "deviance" = abs(deviance_stud),
                         "observacao" =  1:length(cook))

ggplot_data <- ggplot_data |> pivot_longer(cook:deviance, names_to = "medida",
                            values_to = "valor") |>
  mutate(condition = case_when(
    medida == "cook" ~ valor > limiar_cook,
    medida == "chapeu" ~ valor > limiar_hat,
    medida == "deviance" ~ valor > limiar_deviance
  ),
  limiar = case_when(
    medida == "cook" ~ limiar_cook,
    medida == "chapeu" ~limiar_hat,
    medida == "deviance" ~ limiar_deviance))

ggplot_data |>
  ggplot(aes(x = observacao, y = valor,
             colour = condition, 
             group = condition))+
  geom_point() +
  geom_segment(aes(x = observacao, xend = observacao, y = 0, yend = valor)) +
  geom_hline(aes(yintercept = limiar)) +
  scale_colour_manual(values = c("blue", "red"), labels = c("Normal", "Extrema")) +
  labs(y = "Medidas para valores extremos",
       x = "Observações",
       colour = "Observação",
       title = "Analise de valores influentes e/ou outliers") +
  ggrepel::geom_text_repel(data = filter(ggplot_data, condition == T), 
                           color = "black", 
                  label = filter(ggplot_data, condition == T)$observacao) +
  facet_wrap(~medida, scales = "free_y") +
  theme_bw()+
  theme(text = element_text(size = 11, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))
ggsave(filename  = "deteccao_outlier.pdf",
       path = "Trabalho_2/figuras",
       width = 10, height = 6)

# destacar principalmente o 39
sist_complexo |>
  slice(c(27, 35, 39, 45))

# razão de taxas de cada semestre
# 3 com relacao a 1, 2 e 4
coefs <- mod_step |> coef()
rt_3 <- exp(coefs[3] - c(0, coefs[c(2, 4)]))

# 2 com relacao a 1 e 4
rt_2 <- exp(coefs[2] - c(0, coefs[4]))

# 4 com relacao a 1
rt_4 <- exp(coefs[4])

# combinacoes
grid <- expand.grid(c(0, coefs[2], coefs[3], coefs[4]), 
                    c(0, coefs[2], coefs[3], coefs[4]))

grid_idx <- expand.grid(c(1, 2, 3, 4),
                        c(1, 2, 3, 4))


taxas <- map2(.x = list(grid$Var1), 
         .y = list(grid$Var2),
         .f = function(.x, .y) exp(.x - .y)) |>
  unlist()

# montando data frame
data_grid <- data.frame(taxas,
                        comb1 = grid_idx$Var1,
                        comb2 = grid_idx$Var2)

data_grid |>
  ggplot(aes(x = comb2, comb1, fill = taxas)) +
  geom_tile()+
  geom_text(aes(label = round(taxas, 3))) +
  scale_fill_gradient(low="white", high="blue") +
  theme_minimal()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "",
       y = "",
       title = "Mapa de calor das razões de taxa")
ggsave(filename  = "razao_taxas_heatmap.pdf",
       path = "Trabalho_2/figuras",
       width = 10, height = 6)
  


