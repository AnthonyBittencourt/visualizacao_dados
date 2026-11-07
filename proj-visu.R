
# ============================================================
# VISUALIZAÇÃO DE DADOS: NBA temporada 2025-26
# Análise exploratória, testes estatísticos e gráficos
# ============================================================


# ============================================================
# 1.INSPEÇÃO INICIAL DOS DADOS
# ============================================================

dados <- read.csv2(
  file.choose(),
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

# Dimensões, nomes das colunas e primeiras observações
dim(dados)
names(dados)
head(dados, 10)

# Preservar a base original e criar uma cópia de trabalho
analise <- dados

# Examinar os tipos das variáveis e as últimas linhas
str(analise)
tail(analise, 10)

# Verificar valores ausentes na importação
colSums(is.na(analise))


# ============================================================
# 2. LIMPEZA E PREPARAÇÃO DOS DADOS
# ============================================================

# Remover linhas sem jogador, linhas de média da liga
# e cabeçalhos repetidos presentes no arquivo
analise <- dados[
  !is.na(dados$Player) &
    trimws(dados$Player) != "" &
    trimws(dados$Player) != "Player" &
    trimws(dados$Player) != "League Average",
]

# Remover espaços extras dos nomes
analise$Player <- trimws(analise$Player)

# Colunas que devem ser numéricas
colunas_numericas <- c(
  "Age", "MP", "FG.", "TRB",
  "AST", "STL", "PTS"
)

# Converter números com vírgula decimal para formato numérico
for (coluna in colunas_numericas) {
  analise[[coluna]] <- as.numeric(
    gsub(",", ".", analise[[coluna]], fixed = TRUE)
  )
}

# Conferir o resultado da preparação
str(analise)
dim(analise)

# Resumo das variáveis numéricas
summary(analise[, colunas_numericas])

# Verificar se existem valores ausentes após a conversão
colSums(is.na(analise[, colunas_numericas]))

# Identificar possíveis linhas com valores numéricos ausentes
problemas <- analise[
  !complete.cases(analise[, colunas_numericas]),
]

# Inspecionar os casos encontrados, se houver
problemas[, c(
  "Player", "Age", "MP", "FG.",
  "TRB", "AST", "STL", "PTS"
)]


# ============================================================
# 3. EXPLORAÇÃO INICIAL E CONFERÊNCIA DAS CATEGORIAS
# ============================================================

# Estatísticas descritivas
summary(analise[, colunas_numericas])

# Quantidade de titulares e não titulares
table(analise$Titular)

# Quantidade de jogadores por conferência
table(analise$CONF, useNA = "ifany")

# Investigar registros cuja conferência não foi identificada
analise[analise$CONF == "ERRO", c(
  "Player", "Team", "CONF"
)]

# Conferir a relação entre equipe e conferência
table(analise$Team, analise$CONF)

# Correlação inicial entre minutos e pontos
cor(analise$MP, analise$PTS)


# ============================================================
# 4. PREPARAÇÃO DAS VARIÁVEIS PARA AS ANÁLISES
# ============================================================

# Organizar as variáveis categóricas como fatores
analise$Titular <- factor(
  analise$Titular,
  levels = c("Não titular", "Titular")
)

analise$Pos <- factor(analise$Pos)

# Base auxiliar para gráficos
graficos <- analise

# Usar somente conferências identificadas quando necessário
conferencias <- graficos[
  graficos$CONF %in% c("EST", "WST"),
]

# Conferir as quantidades utilizadas
nrow(graficos)
table(graficos$Titular)
table(conferencias$CONF)


# ============================================================
# 5. ANÁLISE 1: CORRELAÇÃO ENTRE MINUTOS E PONTOS
# ============================================================

# Correlação de Pearson, teste de hipótese e IC de 95%
teste_cor <- cor.test(
  analise$MP,
  analise$PTS,
  method = "pearson",
  conf.level = 0.95
)

teste_cor


# ============================================================
# 6. ANÁLISE 2: TITULARES VERSUS NÃO TITULARES
# ============================================================

# Resumo dos pontos por grupo
aggregate(
  PTS ~ Titular,
  data = analise,
  FUN = function(x) c(
    n = length(x),
    media = mean(x),
    mediana = median(x),
    desvio = sd(x)
  )
)

# Teste t de Welch e intervalo de confiança de 95%
teste_t <- t.test(
  PTS ~ Titular,
  data = analise,
  conf.level = 0.95
)

teste_t

# Calcular o tamanho de efeito de Cohen (d)
nao_titular <- analise$PTS[
  analise$Titular == "Não titular"
]

titular <- analise$PTS[
  analise$Titular == "Titular"
]

n1 <- length(nao_titular)
n2 <- length(titular)

dp_combinado <- sqrt(
  ((n1 - 1) * var(nao_titular) +
     (n2 - 1) * var(titular)) /
    (n1 + n2 - 2)
)

d_cohen <- (
  mean(titular) - mean(nao_titular)
) / dp_combinado

d_cohen


# ============================================================
# 7. ANÁLISE 3: REGRESSÃO MÚLTIPLA E DIAGNÓSTICOS
# ============================================================

# Modelo com minutos, aproveitamento, assistências,
# titularidade e posição
modelo <- lm(
  PTS ~ MP + FG. + AST + Titular + Pos,
  data = analise
)

# Resultados e intervalos de confiança convencionais
summary(modelo)
confint(modelo, level = 0.95)

# Diagnósticos gráficos do modelo
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))

# Testes F para cada variável e para os fatores em conjunto
drop1(modelo, test = "F")

# Conferir categorias e contrastes da posição
levels(analise$Pos)
contrasts(analise$Pos)


# ============================================================
# 8. DIAGNÓSTICOS DE HETEROCEDASTICIDADE E INFLUÊNCIA
# ============================================================

# Instalar os pacotes necessários apenas se estiverem ausentes
if (!requireNamespace("lmtest", quietly = TRUE)) {
  install.packages("lmtest")
}

if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich")
}

# Teste de Breusch-Pagan
lmtest::bptest(modelo)

# Coeficientes com erros-padrão robustos HC3
lmtest::coeftest(
  modelo,
  vcov. = sandwich::vcovHC(modelo, type = "HC3")
)

# Distâncias de Cook para avaliar observações influentes
cook <- cooks.distance(modelo)
ordem <- order(cook, decreasing = TRUE)

# Cinco observações com maiores distâncias de Cook
analise[ordem[1:5], c(
  "Player", "Team", "MP", "PTS", "Titular", "Pos"
)]

cook[ordem[1:5]]

# Análise de sensibilidade sem a observação mais influente
indice <- ordem[1]

modelo_sensibilidade <- lm(
  PTS ~ MP + FG. + AST + Titular + Pos,
  data = analise[-indice, ]
)

# Comparar os coeficientes dos modelos
cbind(
  Completo = coef(modelo),
  Sem_mais_influente = coef(modelo_sensibilidade)
)

# Instalar o pacote car se necessário
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

# Teste conjunto robusto HC3 para a variável posição
car::linearHypothesis(
  modelo,
  c(
    "PosPF = 0",
    "PosPG = 0",
    "PosSF = 0",
    "PosSG = 0"
  ),
  vcov. = sandwich::vcovHC(modelo, type = "HC3"),
  test = "F"
)


# ============================================================
# 9. GRÁFICO 1: BOLHAS DE PONTOS, ASSISTÊNCIAS E REBOTES
# ============================================================

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

library(plotly)

# Usar somente observações completas nas variáveis do gráfico
grafico_dados <- analise[
  complete.cases(
    analise[, c("Player", "PTS", "AST", "TRB", "Pos")]
  ),
]

# Valores de referência para a legenda de rebotes
valores_trb <- c(1, 4, 7, 12)

grafico <- plot_ly(
  data = grafico_dados,
  x = ~PTS,
  y = ~AST,
  type = "scatter",
  mode = "markers",
  color = ~Pos,
  colors = "Set2",
  size = ~TRB,
  sizes = c(12, 60),
  marker = list(
    sizemode = "area",
    opacity = 0.80,
    line = list(width = 0.8, color = "white")
  ),
  text = ~paste0(
    "<b>", Player, "</b>",
    "<br>Posição: ", Pos,
    "<br>Pontos por jogo: ", PTS,
    "<br>Assistências por jogo: ", AST,
    "<br>Rebotes por jogo (TRB): ", TRB
  ),
  hoverinfo = "text",
  showlegend = TRUE
) %>%
  layout(
    title = list(
      text = "Pontuação, assistências e rebotes na NBA",
      x = 0.5
    ),
    xaxis = list(title = "Pontos por jogo (PTS)"),
    yaxis = list(title = "Assistências por jogo (AST)"),
    legend = list(title = list(text = "Posição")),
    hovermode = "closest",
    margin = list(r = 190)
  )

# Adicionar as bolhas de referência de TRB
for (v in valores_trb) {
  grafico <- grafico %>%
    add_trace(
      x = list(NA),
      y = list(NA),
      type = "scatter",
      mode = "markers",
      name = paste0("TRB = ", format(v, nsmall = 1)),
      marker = list(
        size = 12 +
          (v - min(grafico_dados$TRB, na.rm = TRUE)) /
          (max(grafico_dados$TRB, na.rm = TRUE) -
             min(grafico_dados$TRB, na.rm = TRUE)) * 78,
        sizemode = "area",
        color = "gray",
        opacity = 0.75
      ),
      showlegend = TRUE,
      inherit = FALSE
    )
}

# Exibir o gráfico interativo
grafico


# ============================================================
# 10. GRÁFICO 2: DISTRIBUIÇÃO DOS PONTOS
# ============================================================

library(ggplot2)

media_pts <- mean(analise$PTS, na.rm = TRUE)
mediana_pts <- median(analise$PTS, na.rm = TRUE)

grafico_distribuicao <- ggplot(
  analise,
  aes(x = PTS)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "#4C78A8",
    color = "white",
    alpha = 0.85
  ) +
  geom_density(
    color = "#F58518",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = media_pts,
    color = "#E45756",
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mediana_pts,
    color = "#54A24B",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = media_pts,
    y = Inf,
    label = paste("Média:", round(media_pts, 2)),
    color = "#E45756",
    angle = 90,
    vjust = 1.4,
    hjust = 1.1,
    size = 3.5
  ) +
  annotate(
    "text",
    x = mediana_pts,
    y = Inf,
    label = paste("Mediana:", round(mediana_pts, 2)),
    color = "#54A24B",
    angle = 90,
    vjust = 1.4,
    hjust = 1.1,
    size = 3.5
  ) +
  labs(
    title = "Como se distribui a pontuação na NBA?",
    subtitle = "Jogadores com pelo menos 15 minutos por jogo",
    x = "Pontos por jogo (PTS)",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafico_distribuicao)


# ============================================================
# 11. GRÁFICO 3A: BOXPLOT DE TITULARES E NÃO TITULARES
# ============================================================

grafico_boxplot <- ggplot(
  analise,
  aes(x = Titular, y = PTS, fill = Titular)
) +
  geom_boxplot(
    width = 0.48,
    alpha = 0.75,
    outlier.shape = NA,
    color = "gray25"
  ) +
  geom_jitter(
    aes(color = Titular),
    width = 0.13,
    alpha = 0.25,
    size = 1.3
  ) +
  scale_fill_manual(
    values = c(
      "Não titular" = "#4C78A8",
      "Titular" = "#F58518"
    )
  ) +
  scale_color_manual(
    values = c(
      "Não titular" = "#4C78A8",
      "Titular" = "#F58518"
    )
  ) +
  labs(
    title = "Titulares e não titulares",
    subtitle = "Distribuição dos pontos por jogo",
    x = NULL,
    y = "Pontos por jogo (PTS)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(grafico_boxplot)


# ============================================================
# 12. GRÁFICO 3B: DIFERENÇA AJUSTADA POR MINUTOS
# ============================================================

# Comparar a titularidade controlando os minutos por jogo
modelo_20 <- lm(
  PTS ~ MP + Titular,
  data = analise
)

coeficientes <- coef(summary(modelo_20))
intervalos <- confint(modelo_20, level = 0.95)

resultado_forest <- data.frame(
  grupo = "Titular vs. não titular",
  estimativa = coeficientes["TitularTitular", "Estimate"],
  inferior = intervalos["TitularTitular", 1],
  superior = intervalos["TitularTitular", 2],
  p_valor = coeficientes["TitularTitular", "Pr(>|t|)"]
)

resultado_forest$rotulo <- sprintf(
  "%.2f [%.2f; %.2f]",
  resultado_forest$estimativa,
  resultado_forest$inferior,
  resultado_forest$superior
)

grafico_forest <- ggplot(
  resultado_forest,
  aes(y = grupo, x = estimativa)
) +
  geom_vline(
    xintercept = 0,
    color = "gray50",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = inferior,
      xend = superior,
      yend = grupo
    ),
    color = "#4C78A8",
    linewidth = 1.3
  ) +
  geom_point(
    color = "#F58518",
    size = 3.5
  ) +
  geom_text(
    aes(label = rotulo),
    x = 0.93,
    y = 1.20,
    size = 3.5
  ) +
  scale_x_continuous(
    limits = c(-0.4, 2.2),
    breaks = seq(0, 2, 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "A diferença permanece após controlar os minutos?",
    subtitle = sprintf(
      "Diferença ajustada em PTS | p = %.4f",
      resultado_forest$p_valor
    ),
    x = "Diferença em pontos por jogo (titulares - não titulares)",
    y = NULL,
    caption = paste(
      "Ponto: estimativa | Linha: IC de 95%.",
      "Zero: ausência de diferença."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "gray20"),
    plot.caption = element_text(color = "gray40", hjust = 0)
  )

print(grafico_forest)


# ============================================================
# 13. GRÁFICO 4: COEFICIENTES DA REGRESSÃO MÚLTIPLA
# ============================================================

# Expressar o aproveitamento em incrementos de 10 pontos percentuais
analise$FG10 <- analise$FG. * 10

# Modelo final
modelo_final <- lm(
  PTS ~ MP + FG10 + AST + Titular + Pos,
  data = analise
)

# Calcular coeficientes e erros-padrão robustos HC3
coefs <- coef(modelo_final)

matriz_robusta <- sandwich::vcovHC(
  modelo_final,
  type = "HC3"
)

erros <- sqrt(diag(matriz_robusta))
critico <- qt(
  0.975,
  df = df.residual(modelo_final)
)

resultado_coef <- data.frame(
  variavel = names(coefs),
  estimativa = as.numeric(coefs),
  erro = as.numeric(erros)
)

resultado_coef$inferior <-
  resultado_coef$estimativa - critico * resultado_coef$erro

resultado_coef$superior <-
  resultado_coef$estimativa + critico * resultado_coef$erro

# Remover o intercepto
resultado_coef <- subset(
  resultado_coef,
  variavel != "(Intercept)"
)

# Rótulos e ordem das variáveis
resultado_coef$variavel <- factor(
  resultado_coef$variavel,
  levels = c(
    "MP",
    "FG10",
    "AST",
    "TitularTitular",
    "PosPF",
    "PosPG",
    "PosSF",
    "PosSG"
  ),
  labels = c(
    "Minutos por jogo",
    "Aproveitamento (+10 p.p.)",
    "Assistências por jogo",
    "Titular vs. não titular",
    "Ala-pivô vs. pivô",
    "Armador vs. pivô",
    "Ala vs. pivô",
    "Ala-armador vs. pivô"
  )
)

# Criar o gráfico de coeficientes
grafico_coef <- ggplot(
  resultado_coef,
  aes(x = estimativa, y = variavel)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_segment(
    aes(
      x = inferior,
      xend = superior,
      yend = variavel
    ),
    color = "#4C78A8",
    linewidth = 1
  ) +
  geom_point(
    aes(color = inferior > 0 | superior < 0),
    size = 2.8
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "#F58518",
      "FALSE" = "gray45"
    ),
    guide = "none"
  ) +
  labs(
    title = "O que permanece associado à pontuação?",
    subtitle = "Regressão múltipla com intervalos robustos de 95% (HC3)",
    x = "Coeficiente estimado em pontos por jogo",
    y = NULL,
    caption = paste(
      "Referências: pivô e não titular.",
      "Associações ajustadas, não efeitos causais."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(grafico_coef)


# ============================================================
# 14. EXPORTAR OS GRÁFICOS ESTÁTICOS
# ============================================================

# Os arquivos serão salvos na pasta indicada por getwd()
getwd()

ggsave(
  "grafico2_distribuicao.png",
  plot = grafico_distribuicao,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

ggsave(
  "grafico3_boxplot.png",
  plot = grafico_boxplot,
  width = 5,
  height = 3.5,
  units = "in",
  dpi = 300
)

ggsave(
  "grafico3_forest.png",
  plot = grafico_forest,
  width = 6,
  height = 2.5,
  units = "in",
  dpi = 300
)

ggsave(
  "grafico4_coeficientes.png",
  plot = grafico_coef,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300
)

# O gráfico de bolhas permanece interativo no RStudio.
# Para exportá-lo como imagem estática, é necessário
# configurar um exportador compatível com Plotly.
