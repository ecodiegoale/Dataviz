# Preparacion ========================================================

rm(list = ls())

library(siebanxicor)
library(tidyverse)
library(scales)
library(ggtext)

library(extrafont)
font_import()
y
loadfonts(device = "win")

## Token para usar siebanxicor ======================================
token <- "TU TOKEN"

setToken(token)

# Datos de la Curva de Phillips =========================================================


idSeries <- c("SR17622","SP1")

metadata <- getSeriesMetadata(idSeries)

hoy <- Sys.Date() #mas reciente

pc <- getSeriesData(idSeries, '1993-01-01', hoy)

pib <- getSerieDataFrame(pc, "SR17622") %>%
  mutate(value = log(value))

p <- getSerieDataFrame(pc, "SP1")

## PIB y PIB potencial ========================================
library(mFilter)

lambda_hp <- 1600

pib.hp <- hpfilter(pib$value, type="lambda", freq=lambda_hp)

pib.hp <- pib.hp[["cycle"]]
pib.hp <- as_tibble(pib.hp)


pib <- bind_cols(pib, pib.hp)

names<-c("fecha", "pib", "brecha") 
names(pib) <- names

pib <- pib %>%
  filter(fecha >= as.Date("1994-01-01"))%>%
  mutate(
    year = year(fecha),
    trim = quarter(fecha),
    trim = case_when(
      trim == 1 ~ "I",
      trim == 2 ~ "II",
      trim == 3 ~ "III",
      trim == 4 ~ "IV"
    ),
    trim = paste(year, trim, sep = "-")
  )
## Inflacion ===============================================

p <- p %>%
  mutate(
    infl = value/lag(value, n=12L)-1,
    year = year(date),
    trim = quarter(date),
    trim = case_when(
      trim == 1 ~ "I",
      trim == 2 ~ "II",
      trim == 3 ~ "III",
      trim == 4 ~ "IV"
    ),
    trim = paste(year, trim, sep = "-")
  )

p <- p %>%
  filter(year > 1993 & year < 2024) %>%
  group_by(trim) %>% 
  summarize(
    infl = mean(infl) 
  )



pc <- pib %>% 
  left_join(p, by = c("trim"))

pc <- pc %>% 
  filter(year > 2002)

# plot ========================================================


library(patchwork)
library(grid)

gray_gradient <- linearGradient(scales::pal_grey()(10))
# Función para crear gráficos iterativos y exportarlos

ggplot(data = pc,aes(x = lag(brecha), y = infl)) +
      geom_point() +
      geom_smooth(method = "lm", 
                  fill = "#474F7A") +
      labs(
        title = "México: Curva de Phillips",
        subtitle = "2003-I a 2023-IV",
        y = "Inflación",
        x = expression(paste("Brecha de producto ",(t-1))),
        caption = paste("Nota: La brecha está en escala logarítmica y fue computada mediante el filtro HP (", 
                        expression(1600), 
                        ")\n API, Banco de México | @ecodiegoale")
      ) + 
      scale_y_continuous(labels = percent)+
  theme(panel.background = 
           element_rect(fil = gray_gradient),
         text = element_text(family="Dubai"),
         plot.title = element_text(
           hjust = 0.5,
           face = "bold", colour = "black"),
         plot.caption = element_text(
           hjust = 0.5,
           colour = "black"),
         plot.subtitle = element_text(
           hjust = 0.5,
           colour = "black")
  )
