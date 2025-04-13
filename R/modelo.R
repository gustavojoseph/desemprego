################################################################################
############################### MODELO DESEMPREGO ##############################
################################################################################

# PACOTES -----------------------------------------------------------------

library(sidrar)
library(rbcb)
library(gtrendsR)
library(readxl)
library(tidyverse)
library(urca)
library(vars)
library(forecast)
library(scales)
library(ggtext)
library(DT)
library(flexdashboard)
library(shiny)

# COLETA DE DADOS ---------------------------------------------------------

# Dados do Sidra/IBGE
dados_sidra <- sidrar::get_sidra(
  api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201"
)

# Dados da FGV (salvos na pasta data)
dados_fgv <- readxl::read_xls(
  path      = "./data/xgvxConsulta.xls",
  col_types = c("text", "numeric", "numeric"),
  skip      = 15,
  na        = "-",
  col_names = c("date", "iaemp", "iie")
)

# Dados do Google Trends
dados_google <- gtrendsR::gtrends(
  keyword      = c("empregos", "seguro desemprego"),
  geo          = "BR",
  time         = "all",
  onlyInterest = TRUE
)

# Dados do BCB
dados_bcb <- rbcb::get_series(
  code       = c("ibc" = 24364, "selic" = 4189),
  start_date = "2012-03-01",
  end_date   = Sys.Date()
)


# TRATAMENTO DE DADOS -----------------------------------------------------

# Dados em formato data.frame
dados <- purrr::reduce(
  .x = list(
    "sidra" = dados_sidra |> # Taxa de desocupação
      dplyr::select(
        "date" = "Trimestre M\u00f3vel (C\u00f3digo)",
        "desocupacao" = "Valor"
    ) |>
      dplyr::mutate(date = lubridate::ym(.data$date)),

    "bcb" = dados_bcb |> # Selic e IBC-Br
      purrr:: reduce(.f = left_join, by = "date"),

    "google" = dados_google$interest_over_time |> # Google Trends "empregos"
      dplyr::select("date", "gtrends" = "hits", "variable" = "keyword") |>
      dplyr::mutate(date = lubridate::as_date(.data$date)) |>
      dplyr::filter(.data$date >= lubridate::as_date("2012-03-01", format = "%Y-%m-%d")) |>
      tidyr::pivot_wider(
        id_cols     = .data$date,
        names_from  = .data$variable,
        values_from = .data$gtrends
    ),

    "fgv" = dados_fgv |> # IAEmp e IIE-Br
      dplyr::mutate(
        date = paste0("01/", .data$date) |> lubridate::dmy()
      ) |>
      dplyr::filter(.data$date >= lubridate::as_date("2012-03-01", format = "%Y-%m-%d"))
    ),

  .f = dplyr::full_join,
  by = "date"
  ) |>
  tidyr::drop_na()

# Dados em formato time series
dados_ts <- stats::ts(
  data = dados[-1],
  start = c(
    lubridate::year(min(dados$date)),
    lubridate::month(min(dados$date))
  ),
  frequency = 12
)

# MODELAGEM ---------------------------------------------------------------

#.1 Definiçoes do Modelo

# Seleção de defasagens VAR
var_lags <- vars::VARselect(
  y       = dados_ts,
  lag.max = 12,      # número de defasagens máximo
  type    = "both",  # incluir constante e tendência
  season  = 12       # incluir dummies sazonais
  )$selection |>
  table() |>
  sort(decreasing = TRUE) |>
  names() |>
  as.numeric() |>
  purrr::pluck(1)

# Teste de cointegração de Johansen/VECM
johansen_vecm <- urca::ca.jo(
  x      = dados[-1],
  type   = "trace",                            # teste do traço
  ecdet  = "const",                            # adicionar constante
  K      = ifelse(var_lags >= 2, 2, var_lags), # número de defasagens máximo
  spec   = "transitory",                       # especificação VECM
  season = 12                                  # incluir dummies sazonais
)

# Resultados
summary(johansen_vecm)
r <- 2

#.2 Previsão dentro da amostra

# Amostras
dados_treino <- dados |>
  dplyr::slice(1:(dplyr::n() - 12))

dados_teste <- dados |>
  dplyr::filter(date > dplyr::last(dados_treino$date))

# VECM
modelo_vecm <- urca::ca.jo(
  x      = dados_treino[-1],
  type   = "trace",
  ecdet  = "const",
  K      = ifelse(var_lags >= 2, 2, var_lags),
  spec   = "transitory",
  season = 12
)

# Transformar VECM para VAR em nível
modelo_var <- vars::vec2var(modelo_vecm, r = r)

# Previsões na amostra de teste
previsao_teste <- predict(
  object  = modelo_var,
  ci      = 0.95,
  n.ahead = nrow(dados_teste)
)

# Acurácia na amostra de teste
acuracia_vecm <- forecast::accuracy(
  previsao_teste[["fcst"]][["desocupacao"]][,"fcst"],
  dados_teste$desocupacao
)

# Visualização
grafico_teste <- dados_teste |>
  dplyr::select(1:2) |>
  dplyr::mutate(previsao = round(previsao_teste[["fcst"]][["desocupacao"]][,"fcst"], 2)) |>
  dplyr::rename("Desocupa\u00e7\u00e3o" = "desocupacao", "Previs\u00e3o" = "previsao") |>
  tidyr::pivot_longer(cols      = 2:3,
                      names_to  = "variavel",
                      values_to = "valor") |>

  ggplot2::ggplot() +

  ggplot2::aes(x = date, y = valor, color = variavel) +

  ggplot2::geom_line(size = 1.5) +

  ggplot2::theme_light() +
  theme(
    plot.title            = element_text(face = "bold", size = 20, colour = "#282f6b"),
    plot.subtitle         = element_text(face = "bold", size = 10),
    plot.title.position   = "plot",
    plot.caption          = element_text(hjust = 0, face = "bold"),
    plot.caption.position = "plot",
    legend.position       = "top",
    legend.text           = element_text(face = "bold")
    ) +

  ggplot2::scale_color_manual(values = c(
    "Desocupa\u00e7\u00e3o" = "#282f6b",
    "Previs\u00e3o"         = "#419391")
  ) +

  ggplot2::scale_y_continuous(
    labels = scales::number_format(suffix = "%"),
    breaks = seq(0, 100, by = 0.5)
  ) +

  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b/%Y"
  ) +

  ggplot2::labs(
    title    = "Performance do Modelo nos últimos 12 meses",
    subtitle = "Modelo Vetorial de Correção de Erros (VECM)",
    color    = NULL,
    y        = NULL,
    x        = NULL,
    caption  = "Dados: BCB, FGV, Google Trends e IBGE"
  )

#.3 Benchmark: RW e ARIMA

# Random walk
modelo_rw <- forecast::naive(
  y = dados_treino[["desocupacao"]],
  h = nrow(dados_teste)
)

# Auto ARIMA
modelo_arima <- forecast::auto.arima(
  y = dados_treino[["desocupacao"]]
) |>
  predict(n.ahead = nrow(dados_teste))

# Acurácia RW
acuracia_rw <- forecast::accuracy(modelo_rw, dados_teste$desocupacao)

# Acurácia auto ARIMA
acuracia_arima <- forecast::accuracy(modelo_arima$pred, dados_teste$desocupacao)

# Comparação
benchmark_modelos <- dplyr::tibble(
  "M\u00e9trica" = colnames(acuracia_vecm),
  "VECM"         = t(acuracia_vecm)[,1],
  "Random Walk"  = t(acuracia_rw["Test set", c("ME", "RMSE", "MAE", "MPE", "MAPE")])[1,],
  "ARIMA"        = t(acuracia_arima)[,1]
) |>
  DT::datatable(
    options = list(dom = "t", pageLength = 15, scrollX = FALSE, scrollY = FALSE),
    rownames = FALSE
  ) |>
  DT::formatRound(columns = 2:4, digits = 3, dec.mark = ",") |>
  DT::formatStyle(columns = 2, fontWeight = "bold")

#.4 Previsão fora da amostra

# Transformar VECM para VAR em nível
modelo_var2 <- vars::vec2var(johansen_vecm, r = r)

# Previsões fora da amostra
previsao <- predict(
  object  = modelo_var2,
  n.ahead = 6
  ) |>
  purrr::pluck("fcst") |>
  purrr::pluck("desocupacao") |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    "date"       = seq.Date(
      from       = dplyr::last(dados$date) %m+% months(1),
      by         = "month",
      length.out = 6
    ),
    "id" = "Previs\u00e3o"
  ) |>
  dplyr::rename("value" = "fcst")



# VISUALIZAÇÃO DOS RESULTADOS ----------------------------------------------

# Cores para gráficos
colors <- c(
  blue       = "#282f6b",
  red        = "#b22200",
  yellow     = "#eace3f",
  green      = "#224f20",
  purple     = "#5f487c",
  orange     = "#b35c1e",
  turquoise  = "#419391",
  green_two  = "#839c56",
  light_blue = "#3b89bc",
  gray       = "#666666"
)

# Gráfico
plt_previsao <- dados |>
  dplyr::filter(.data$date >= max(.data$date) %m-% lubridate::years(5)) |>
  dplyr::select(.data$date, "value" = .data$desocupacao) |>
  dplyr::mutate("id" = "Taxa de Desocupa\u00e7\u00e3o") |>
  dplyr::full_join(
    previsao,
    by = c("date", "value", "id")
  ) |>

  ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +

  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    fill  = colors[1],
    alpha = 0.4
  ) +

  ggplot2::geom_line(size = 1.5, colour = colors[1]) +

  ggplot2::geom_vline(
    xintercept = previsao |>
                  slice(1) |>
                  pull(date),
    linetype = "dashed",
    color = "black"
  ) +

  ggplot2::theme_light() +
  theme(
    plot.title            = element_text(face = "bold", size = 20, colour = colors[1]),
    plot.subtitle         = element_text(face = "bold", size = 10),
    plot.title.position   = "plot",
    plot.caption          = element_text(hjust = 0, face = "bold"),
    plot.caption.position = "plot",
    legend.position       = "top") +

  ggplot2::scale_y_continuous(
    labels = scales::number_format(suffix = "%"),
    breaks = seq(0, 100, by = 2.5)
  ) +

  ggplot2::scale_x_date(
    breaks = scales::breaks_width("4 months"),
    labels = function(x) {
      paste0(
        lubridate::month(x, label = TRUE, abbr = TRUE), "\n",
        lubridate::year(x)
      )}
  ) +

  ggplot2::labs(
    title    = "Previs\u00e3o da Taxa de Desocupa\u00e7\u00e3o",
    subtitle = "Modelo Vetorial de Correção de Erros (VECM)",
    y        = NULL,
    x        = NULL,
    caption  = "Dados: BCB, FGV, Google Trends e IBGE"
  )

# Tabela
tbl_previsao <- previsao |>
  dplyr::mutate(date = format(date, "%b/%Y")) |>
  dplyr::select(
    "M\u00eas"        = .data$date,
    "Limite inferior" = .data$lower,
    "Previs\u00e3o"   = .data$value,
    "Limite superior" = .data$upper
  ) |>
  DT::datatable(
    options = list(dom = "t", pageLength = 15, scrollX = FALSE, scrollY = FALSE),
    rownames = FALSE
  ) |>
  DT::formatRound(columns = 2:4, digits = 2, dec.mark = ",") |>
  DT::formatStyle(columns = 3, fontWeight = "bold")


# Gerar dashboard ---------------------------------------------------------

# Verificar se pasta "docs" existe no projeto
if(!dir.exists("docs")){dir.create("docs")}

# Renderizar dashboard
rmarkdown::run(file = "dashboard.Rmd")
