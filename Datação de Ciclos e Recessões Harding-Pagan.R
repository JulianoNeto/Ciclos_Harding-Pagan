
# Instalar/carregar pacotes

if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  "magrittr",
  "BCDating",
  "sidrar",
  "dplyr",
  "lubridate",
  "timetk",
  "zoo",
  "ggplot2",
  "ggthemes"
)

"Primeiro, verifica se o pacote pacman está instalado. Se não estiver, ele o instala.
Em seguida, usa a função p_load do pacote pacman para carregar (e instalar, se necessário) 
vários pacotes de uma só vez."

search()

# Coleta e tratamento de dados

pib <- sidrar::get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202") %>%
  dplyr::select("date" = `Trimestre (Código)`, "value" = `Valor`) %>%
  dplyr::mutate(value = value, date = lubridate::yq(date)) %>%
  dplyr::as_tibble()

"Usa a função get_sidra do pacote sidrar para obter dados do SIDRA (Sistema IBGE de Recuperação Automática).
Seleciona as colunas Trimestre (Código) e Valor, renomeando-as para date e value.
Converte a coluna date para um formato de data trimestral usando lubridate::yq.
Converte o resultado em um tibble (um tipo especial de dataframe)."

# Obter datação de ciclo de negócios

bc_dates <- pib %>%
  timetk::tk_ts(select = value, start = c(1996, 1), frequency = 4) %>%
  BCDating::BBQ(name = "Ciclo de Negócios do PIB do Brasil")

"Converte os dados do PIB em uma série temporal, começando em 1996 Q1, com frequência
trimestral. Aplica o algoritmo BBQ (Bry-Boschan Quarterly) do pacote BCDating para 
identificar picos e vales nos ciclos de negócios do PIB brasileiro"

# Exibir resultados

show(bc_dates)

"Mostra os resultados da análise de ciclos de negócios, incluindo as datas de picos e 
vales, bem como a duração de cada ciclo"

## Peaks Troughs Duration
## 1 2001Q1 2001Q4 3
## 2 2002Q4 2003Q2 2
## 3 2008Q3 2009Q1 2
## 4 2014Q1 2016Q4 11
## 5 2019Q4 2020Q2 2

"O output mostra cinco ciclos de negócios identificados entre 2001 e 2020, com as datas 
de pico e vale para cada ciclo, bem como sua duração em trimestres.Este código está
realizando uma análise dos ciclos de negócios do PIB brasileiro, identificando períodos 
de expansão e contração econômica ao longo do tempo."

class(bc_dates)
str(bc_dates)

# Extrair as datas de pico e vale

peaks <- time(bc_dates@y)[bc_dates@peaks]
troughs <- time(bc_dates@y)[bc_dates@troughs]

# Criar um data frame com as datas

bc_dates_df <- data.frame(
  Peaks = peaks,
  Troughs = troughs
)

print(bc_dates_df)
class(bc_dates_df$Peaks)
class(bc_dates_df$Troughs)

# Converter Troughs para Date

bc_dates_df$Troughs <- as.Date(paste0(floor(bc_dates_df$Troughs), "-01-01")) + 
  floor((bc_dates_df$Troughs %% 1) * 365.25)

# Converter Peaks para Date

bc_dates_df$Peaks <- as.Date(paste0(floor(bc_dates_df$Peaks), "-01-01")) + 
  floor((bc_dates_df$Peaks %% 1) * 365.25)

# Verificar se a conversão foi bem-sucedida

print(bc_dates_df)
class(bc_dates_df$Peaks)
class(bc_dates_df$Troughs)

# Verificar o formato das datas

print(head(pib))
print(bc_dates_df)

# Criar o gráfico base

p <- ggplot(pib, aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Ciclos de Negócios do PIB do Brasil",
    subtitle = "Períodos de recessão destacados em rosa",
    x = "Ano",
    y = "PIB (em bilhões)"
  ) +
  theme_minimal()

# Adicionar áreas sombreadas para recessões

p <- p + geom_rect(data = bc_dates_df,
                   aes(xmin = Peaks, xmax = Troughs, ymin = -Inf, ymax = Inf),
                   fill = "pink", alpha = 0.3, inherit.aes = FALSE)

# Adicionar pontos para picos e vales

peak_points <- data.frame(
  date = bc_dates_df$Peaks,
  value = sapply(bc_dates_df$Peaks, function(d) pib$value[which.min(abs(pib$date - d))])
)

trough_points <- data.frame(
  date = bc_dates_df$Troughs,
  value = sapply(bc_dates_df$Troughs, function(d) pib$value[which.min(abs(pib$date - d))])
)

p <- p +
  geom_point(data = peak_points, aes(x = date, y = value), color = "red", size = 3) +
  geom_point(data = trough_points, aes(x = date, y = value), color = "green", size = 3)

# Ajustar a escala do eixo x para mostrar anos

p <- p + scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# Exibir o gráfico

print(p)
