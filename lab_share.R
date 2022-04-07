library(tidyverse)
library(maps)
library(MetBrewer)
library(patchwork)

#setwd("~/R_for_me/DATAVIZ")

df <- read.csv("labsh.csv")
map<-map_data("world")
map_data<-left_join(map, df, by=c("region"="Country"))

#Esta visualización está basada en el código de Tanya Shapiro (tashapiro)
#El enlace a su repositorio es el siguiente
#https://github.com/tashapiro/tanya-data-viz/tree/main/globe-bar-plot


#Americas------------------------------------------------------------------

#Mapa
plot.map.amer<-ggplot(map_data, aes(x=long, y= lat, group=group, fill=labsh2019))+
        geom_polygon()+
        scale_fill_gradientn(guide="none", colors = met.brewer("Greek"))+
        coord_map("ortho",orientation=c(10, -90, 0))+
        theme_void()
plot.map.amer

#Gráficos de barras

##Preparación de los datos

bar_amer<-df%>%
        filter(Continent %in% c('Americas'))%>%
        arrange(Continent, -labsh2019)
#dataframe de los países disponibles para el continente americano ordenado de mayor a menor

bar_amer$id <- seq(1, nrow(bar_amer))
#creamos una columna de identificadores para cada país

numero_bar_amer <- nrow(bar_amer)
#número de barras/países disponibles (27)

angulo_amer <- 90 - 360 * (bar_amer$id-0.5) /numero_bar_amer
#el ángulo donde se colocará cada barra


bar_amer$hjust <- ifelse(angulo_amer < -90, 1, 0)
bar_amer$angle <- ifelse(angulo_amer < -90, angulo_amer+180, angulo_amer)
#se crean las columnas hjust (posición horizontal) donde 0=izquierda y 1=derecha
#y angle que tiene al ángulo del circulo donde se pondrán las barras

plot.bar.amer<-ggplot(bar_amer, aes(x=as.factor(id), y=labsh2019, fill=labsh2019)) +       
        geom_bar(stat="identity") +
        geom_text(aes(x=as.factor(id), y=labsh2019+3, label=Country, hjust=hjust, angle=angle),
                  color="black", alpha=0.9, size=3, inherit.aes = FALSE )+
        scale_fill_gradientn(colors = met.brewer("Greek"),
                             limits= c(30,76), 
                             guide=guide_colorbar(title.position = "top",
                                                  barwidth = 10,
                                                  title.hjust = 0.5))+
        ylim(-100,100) +
        labs(title="Americas 2019",
             subtitle= "Share of labour compensation in GDP at current national prices", 
             caption="Data from Penn World Table 10.0 | @ecodiegoale",
             fill="Share of labour compensation")+
        theme_void() + 
        coord_polar()+
        theme(
                legend.position="bottom",
                plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
                plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
                plot.margin= margin(t=25,b=10)
        )
plot.bar.amer

americas<-plot.bar.amer + 
        inset_element(plot.map.amer, left = 0.3, bottom = 0.28, right = 0.7, top = 0.72)
americas
ggsave("americas_labsh.jpeg", height=9, width=6)



#Africa--------------------------------------------

plot.map.afr<-ggplot(map_data, aes(x=long, y= lat, group=group, fill=labsh2019))+
        geom_polygon()+
        scale_fill_gradientn(guide="none", colors = met.brewer("VanGogh3"))+
        coord_map("ortho",orientation=c(0, 20, 0))+
        theme_void()
plot.map.afr

#Gráficos de barras

##Preparación de los datos

bar_afr<-df%>%
        filter(Continent %in% c('Africa'))%>%
        arrange(Continent, -labsh2019)


bar_afr$id <- seq(1, nrow(bar_afr))

numero_bar_afr <- nrow(bar_afr)
#número de barras/países disponibles (34)

angulo_afr <- 90 - 360 * (bar_afr$id-0.5) /numero_bar_afr


bar_afr$hjust <- ifelse(angulo_afr < -90, 1, 0)
bar_afr$angle <- ifelse(angulo_afr < -90, angulo_afr+180, angulo_afr)
#se crean las columnas hjust (posición horizontal) donde 0=izquierda y 1=derecha
#y angle que tiene al ángulo del circulo donde se pondrán las barras

plot.bar.afr<-ggplot(bar_afr, aes(x=as.factor(id), y=labsh2019, fill=labsh2019)) +       
        geom_bar(stat="identity") +
        geom_text(aes(x=as.factor(id), y=labsh2019+3, label=Country, hjust=hjust, angle=angle),
                  color="black", alpha=0.9, size=3, inherit.aes = FALSE )+
        scale_fill_gradientn(colors = met.brewer("VanGogh3"),
                             limits= c(15,85), 
                             guide=guide_colorbar(title.position = "top",
                                                  barwidth = 10,
                                                  title.hjust = 0.5))+
        ylim(-100,100) +
        labs(title="Africa 2019",
             subtitle= "Share of labour compensation in GDP at current national prices", 
             caption="Data from Penn World Table 10.0 | @ecodiegoale",
             fill="Share of labour compensation")+
        theme_void() + 
        coord_polar()+
        theme(
                legend.position="bottom",
                plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
                plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
                plot.margin= margin(t=25,b=10)
        )
plot.bar.afr

africa<-plot.bar.afr + 
        inset_element(plot.map.afr, left = 0.3, bottom = 0.28, right = 0.7, top = 0.72)
africa
ggsave("africa_labsh.jpeg", height=9, width=6)

#Europa------------------------------------------------

plot.map.eur<-ggplot(map_data, aes(x=long, y= lat, group=group, fill=labsh2019))+
        geom_polygon()+
        scale_fill_gradientn(guide="none", colors = met.brewer("Ingres"))+
        coord_map("ortho", orientation=c(40, 20, 0))+
        theme_void()
plot.map.eur

#Gráficos de barras

##Preparación de los datos

bar_eur<-df%>%
        filter(Continent %in% c('Europe'))%>%
        arrange(Continent, -labsh2019)


bar_eur$id <- seq(1, nrow(bar_eur))

numero_bar_eur <- nrow(bar_eur)
#número de barras/países disponibles (34)

angulo_eur <- 90 - 360 * (bar_eur$id-0.5) /numero_bar_eur


bar_eur$hjust <- ifelse(angulo_eur < -90, 1, 0)
bar_eur$angle <- ifelse(angulo_eur < -90, angulo_eur+180, angulo_eur)
#se crean las columnas hjust (posición horizontal) donde 0=izquierda y 1=derecha
#y angle que tiene al ángulo del circulo donde se pondrán las barras

plot.bar.eur<-ggplot(bar_eur, aes(x=as.factor(id), y=labsh2019, fill=labsh2019)) +       
        geom_bar(stat="identity") +
        geom_text(aes(x=as.factor(id), y=labsh2019+3, label=Country, hjust=hjust, angle=angle),
                  color="black", alpha=0.9, size=3, inherit.aes = FALSE )+
        scale_fill_gradientn(colors = met.brewer("Ingres"),
                             limits= c(25,75), 
                             guide=guide_colorbar(title.position = "top",
                                                  barwidth = 10,
                                                  title.hjust = 0.5))+
        ylim(-100,100) +
        labs(title="Europe 2019",
             subtitle= "Share of labour compensation in GDP at current national prices", 
             caption="Data from Penn World Table 10.0 | @ecodiegoale",
             fill="Share of labour compensation")+
        theme_void() + 
        coord_polar()+
        theme(
                legend.position="bottom",
                plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
                plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
                plot.margin= margin(t=25,b=10)
        )
plot.bar.eur

europe<-plot.bar.eur + 
        inset_element(plot.map.eur, left = 0.3, bottom = 0.28, right = 0.7, top = 0.72)
europe
ggsave("europe_labsh.jpeg", height=9, width=6)


#Asia-------------------------------------------------------

plot.map.asia<-ggplot(map_data, aes(x=long, y= lat, group=group, fill=labsh2019))+
        geom_polygon()+
        scale_fill_gradientn(guide="none", colors = met.brewer("Paquin"))+
        coord_map("ortho", orientation=c(35, 80, 0))+
        theme_void()
plot.map.asia

#Gráficos de barras

##Preparación de los datos

bar_asia<-df%>%
        filter(Continent %in% c('Asia'))%>%
        arrange(Continent, -labsh2019)


bar_asia$id <- seq(1, nrow(bar_asia))

numero_bar_asia <- nrow(bar_asia)
#número de barras/países disponibles (30)

angulo_asia <- 90 - 360 * (bar_asia$id-0.5) /numero_bar_asia


bar_asia$hjust <- ifelse(angulo_asia < -90, 1, 0)
bar_asia$angle <- ifelse(angulo_asia < -90, angulo_asia+180, angulo_asia)
#se crean las columnas hjust (posición horizontal) donde 0=izquierda y 1=derecha
#y angle que tiene al ángulo del circulo donde se pondrán las barras

plot.bar.asia<-ggplot(bar_asia, aes(x=as.factor(id), y=labsh2019, fill=labsh2019)) +       
        geom_bar(stat="identity") +
        geom_text(aes(x=as.factor(id), y=labsh2019+3, label=Country, hjust=hjust, angle=angle),
                  color="black", alpha=0.9, size=3, inherit.aes = FALSE )+
        scale_fill_gradientn(colors = met.brewer("Paquin"),
                             limits= c(17,66), 
                             guide=guide_colorbar(title.position = "top",
                                                  barwidth = 10,
                                                  title.hjust = 0.5))+
        ylim(-100,100) +
        labs(title="Asia 2019",
             subtitle= "Share of labour compensation in GDP at current national prices", 
             caption="Data from Penn World Table 10.0 | @ecodiegoale",
             fill="Share of labour compensation")+
        theme_void() + 
        coord_polar()+
        theme(
                legend.position="bottom",
                plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
                plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
                plot.margin= margin(t=25,b=10)
        )
plot.bar.asia

asia<-plot.bar.asia + 
        inset_element(plot.map.asia, left = 0.3, bottom = 0.28, right = 0.7, top = 0.72)
asia
ggsave("asia_labsh.jpeg", height=9, width=7)



