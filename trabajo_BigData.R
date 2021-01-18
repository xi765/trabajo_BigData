#paquetes
library(tidyverse)
library(klippy)  #- remotes::install_github("rlesur/klippy")
library(knitr)
library(kableExtra)
library(rio)
library(here)
library(stringr)
library(forcats)
library(DT)
library(scales)
library(naniar)
library(ggtext)
#
#
#
#Exportaciones e importaciones por continentes en millones de euros (fuente datacomex)
datos <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/XMContinentes.csv"
dfxmc <- rio::import(datos) %>% filter(subfila != "Total seleccionado") %>% select(-c(4)) %>%
mutate(fila = case_when( fila == "AF - Africa" ~ "Africa", fila == "AM - AmÃ©rica" ~ "America", fila == "AS - Asia" ~"Asia", fila == "OC - OceanÃ­a" ~ "Oceania", fila == "UE - UniÃ³n Europea 28 paÃ­ses(d.2013-07 h.2020-01)" ~ "Union Europea (28)", fila == "Total Mundo" ~ "Total"), valor = str_replace(valor,"[,]","."), valor = as.numeric(valor), subfila = as.numeric(subfila))
#EXPORTACIONES POR CONTINENTE
aa <- inner_join(x = dfxmc %>% filter(fila != "Total"),y = dfxmc %>% filter(fila == "Total"), by = c("columna","subfila")) %>% select(-c(5)) %>% mutate(porc = (valor.x/valor.y)) %>% filter(subfila == 2019) %>% group_by(columna) %>% mutate(fila.x = factor(fila.x, levels = fila.x[order(porc, decreasing = TRUE)]))


#GRÁFICO

ggplot(aa, aes(x = fila.x, y = porc, fill = columna)) + geom_bar(stat = "identity", position = "dodge", color = "#000000") +
               scale_y_continuous(breaks = seq(0,0.65,0.1), labels = scales::percent) +
               scale_fill_discrete(labels = c("% del total de Exportaciones","% del total de Importaciones")) +
              labs(fill=NULL,
               x=NULL,
               y=NULL,
               title="Flujos comerciales por continente",
               subtitle = "Año 2019",
               caption="Fuente: DataComex") +
               theme(
              panel.background = element_rect( fill = "#ffffff", colour = "#ffffff"),
              panel.grid  = element_line( colour ="grey"),
              axis.ticks = element_blank(),
              plot.title = element_text(vjust = 2.5, size = 15),
              panel.border = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.margin = margin(t=25),
              legend.position = "top",
              legend.background = element_rect( fill = "#ffffff", colour = "#ffffff" ),
              plot.margin = margin(t = 5, r = 25, b = 5,15))
#
#
#
#Exportaciones e importaciones por país en millones de euros
datos <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/XMPais.csv"
dfxmp <- rio::import(datos) %>% mutate( valor = str_replace(valor,"[,]","."), valor = as.numeric(valor))
#EXPORTACIONES
dfxp <- dfxmp %>% filter(subcolumna == "EXPORT", subfila == "2019", !fila %in% c("Total seleccionado","UE - UniÃ³n Europea 28 paÃ­ses(d.2013-07 h.2020-01)")) %>% slice_max(order_by = valor,n = 10) %>% mutate(fila = factor(fila, levels = fila[order(valor, decreasing = TRUE)]))
#IMPORTACIONES
dfmp <- dfxmp %>% filter(subcolumna == "IMPORT", subfila == "2019", !fila %in% c("Total seleccionado","UE - UniÃ³n Europea 28 paÃ­ses(d.2013-07 h.2020-01)")) %>% slice_max(order_by = valor,n = 10) %>% mutate(fila = factor(fila, levels = fila[order(valor, decreasing = TRUE)]))


#GRÁFICO Exportaciones
ggplot(dfxp, aes(x = fila, y = valor, fill = fila)) + geom_bar(stat = "identity",color = "#000000") +
             scale_fill_brewer(palette = "Spectral", labels = c("Francia","Alemania","Italia","Portugal","Reino Unido","Estados Unidos","Países Bajos","Marruecos","Bélgica","China")) +
             scale_x_discrete(labels = c("Francia","Alemania","Italia","Portugal","Reino Unido","Estados Unidos","Países Bajos","Marruecos","Bélgica","China")) +
             scale_y_continuous(breaks = seq(0,45000,5000), labels = paste(scales::comma(seq(0,45000,5000)),"M€")) +
             labs(fill=NULL,
               x=NULL,
               y=NULL,
               title="Exportaciones por país",
               subtitle = "Año 2019",
               caption="Fuente: DataComex") +
               theme( plot.background = element_rect( fill = "#ffffff"),
              panel.background = element_rect( fill = "#ffffff", colour = "#ffffff", size = 0.1 ),
              panel.grid  = element_line( colour ="grey"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_text(angle = 75, face = "bold", vjust = 0.95, hjust = 1),
              plot.title = element_text(vjust = 2.5, size = 15),
              panel.border = element_blank(),
              legend.position = "none",
              plot.margin = margin(t = 5, r = 25, b = 5,15),
              strip.background = element_rect(fill = "#e8d7a3",colour = NULL))


#GRÁFICO Importaciones
ggplot(dfmp, aes(x = fila, y = valor, fill = fila)) + geom_bar(stat = "identity",color = "#000000") +
             scale_fill_brewer(palette = "Spectral", labels = c("Francia","Alemania","Italia","Portugal","Reino Unido","Estados Unidos","Países Bajos","Marruecos","Bélgica","China")) +
             scale_x_discrete(labels = c("Alemania","Francia","China","Italia","Estados Unidos","Países Bajos","Reino Unido","Portugal","Turquía","Bélgica")) +
             scale_y_continuous(breaks = seq(0,45000,5000), labels = paste(scales::comma(seq(0,45000,5000)),"M€")) +
             labs(fill=NULL,
               x=NULL,
               y=NULL,
               title="Importaciones por país",
              subtitle = "Año 2019",
               caption="Fuente: DataComex") +
               theme( plot.background = element_rect( fill = "#ffffff"),
              panel.background = element_rect( fill = "#ffffff", colour = "#ffffff", size = 0.1 ),
              panel.grid  = element_line( colour ="grey"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_text(angle = 75, face = "bold", vjust = 0.95, hjust = 1),
              plot.title = element_text(vjust = 2.5, size = 15),
              panel.border = element_blank(),
              legend.position = "none",
              plot.margin = margin(t = 5, r = 25, b = 5,15),
              strip.background = element_rect(fill = "#e8d7a3",colour = NULL))
#
#
#
url <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/DataComex_201130122036.csv"

data <- read.csv(url)
data <- data %>% rename_at(vars(colnames(data)), ~ c("Country","Date","Columna","Variable","Valor"))
data <- data %>% filter(Date != "Total Fechas")
data <- data %>% mutate(Valor = stringr::str_replace(Valor,"([,])","."),Valor = as.numeric(Valor), Date = as.numeric(Date))
espxm <- data %>% filter(Country == "Total Mundo") %>% select(-c(Columna,Country))
espxm <- espxm %>% mutate(ValorM = (Valor/1000000)) %>% group_by(Date) %>% mutate(min = min(ValorM), max = max(ValorM))
#EXPORTACIONES E IMPORTACIONES A LA UE
dfexpc <- dfxmc %>% filter(fila == "Union Europea (28)")
dfexpcb <- inner_join(dfexpc,espxm, by = c("subfila"="Date","columna"="Variable")) %>% select(-c(5)) %>% mutate(valorporc = (valor/ValorM))

#GRÁFICO
ggplot(dfexpcb, aes(x=subfila, y=valorporc, color = columna)) + geom_line(size=1) +
        scale_y_continuous(breaks = seq(0.5,0.75,0.05), labels = scales::percent)+
        scale_x_continuous(breaks = seq(1995,2020,1)) +
        labs(fill=NULL,
        x=NULL,
        y=NULL,
        title="Flujos comerciales con la Unión Europea (28)",
        subtitle = "<span style='color:#ff0505;'>Exportaciones</span> e <span style='color:#24ff05;'>Importaciones </span>en % del total",
        caption="Fuente: DataComex") +
        scale_color_manual( values = c("EXPORT"="#ff0000","IMPORT"="#24d800"), labels = c("Exp.","Imp.")) +
        theme( plot.background = element_rect( fill = "#ffffff"),
              panel.background = element_rect( fill = "#ffffff"),
              panel.grid  = element_line( colour ="#979e9e"),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.line = element_line(color = "#000000"),
              axis.ticks = element_line(colour = "#000000"),
              axis.text = element_text(colour = "#000000"),
              axis.text.x = element_text(angle = 90),
              axis.title.y = element_text(vjust = 4),
              axis.title.x = element_text(hjust = 0.5),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_markdown(hjust = 0.5),
              panel.border = element_rect(fill = NA,colour = "#ffffff"),
              legend.position = "none")
#
#
#
#Exportaciones e importaciones ESPAÑA
url <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/DataComex_201130122036.csv"
data <- read.csv(url)
data <- data %>% rename_at(vars(colnames(data)), ~ c("Country","Date","Columna","Variable","Valor"))
data <- data %>% filter(Date != "Total Fechas")
data <- data %>% mutate(Valor = stringr::str_replace(Valor,"([,])","."),Valor = as.numeric(Valor), Date = as.numeric(Date))
espxm <- data %>% filter(Country == "Total Mundo") %>% select(-c(Columna,Country))
espxm <- espxm %>% mutate(ValorM = (Valor/1000000)) %>% group_by(Date) %>% mutate(min = min(ValorM), max = max(ValorM))

#GRÁFICO
ggplot(espxm,aes(x = Date, y = ValorM, color = Variable)) +
        geom_line() +
        geom_point() +
        geom_ribbon(aes(x = Date, ymin = min, ymax = max), fill = "#f4ff05", alpha = 0.3, color = NA) +
        labs(fill=NULL,
        x=NULL,
        y=NULL,
        title="<span style='color:#ff0505;'>Exportaciones</span> e <span style='color:#24ff05;'>Importaciones </span>en España",
        caption="Fuente: DataComex") +
        scale_color_manual( values = c("EXPORT"="#ff0000","IMPORT"="#24d800")) +
        scale_fill_manual( values = c("EXPORT"="#ff0000","IMPORT"="#24d800")) +
        scale_y_continuous(breaks = seq(0,300000,50000),labels = (paste(scales::comma(seq(0,300000,50000)),"M€"))) +
        scale_x_continuous(breaks = seq(1995,2020,1)) +
        theme( plot.background = element_rect( fill = "#ffffff"),
              panel.background = element_rect( fill = "#ffffff"),
              panel.grid  = element_line( colour ="#979e9e"),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.ticks = element_line(colour = "#000000"),
              axis.text = element_text(colour = "#000000"),
              axis.text.x = element_text(angle = 90),
              axis.title.y = element_text(vjust = 4),
              axis.title.x = element_text(hjust = 0.5),
              axis.line = element_line(color = "#000000"),
              panel.border = element_rect(fill = NA,colour ="#ffffff"),               plot.title = element_markdown(hjust = 0.5),
              legend.position = "none",
              plot.margin = margin(t = 5, r = 10, b = 5,20) )
#
#
#
datosx <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/XMWorldC.csv"
datosg <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/GDPWorldC.csv"
#Cogemos datos de España y otros países relevantes en el comercio internacional
dfa <- rio::import(datosg) %>% filter(countryiso3code %in% c("ESP"))
dfb <- rio::import(datosx) %>% filter(countryiso3code %in% c("ESP"))
dft <- inner_join(dfa,dfb, by = c("countryiso3code","year")) %>% select(-c(1,8)) %>% relocate(5, .before = 1) %>% relocate(6, .before = 4) %>% relocate(7, .before = 6) %>% rename("indicador"=1,"country"=2,"gdp"=7,"xm"=8) %>% mutate(porcpib = (xm/gdp))
#Plot de exportaciones

dfplot <- dft %>% filter(year %in% c(1970:2020))

#GRÁFICO
ggplot(dft,aes(x=year,y=porcpib,color=indicador)) + geom_line(size=1) + labs(color=NULL,
        x=NULL,
        y=NULL,
        title="Peso de las <span style='color:#ff0505;'>Exportaciones</span> e <span style='color:#24ff05;'>Importaciones </span>en el PIB",
        caption="Fuente: DataComex") +
        scale_x_continuous(breaks = seq(1960,2020,5),limits = c(1960,2019)) +
        scale_y_continuous(breaks = seq(0,0.3,0.05),labels = scales::percent)+
        scale_color_manual( values = c("exportsy"="#ff0000","importsy"="#24d800"), labels = c("Exportaciones","Importaciones")) +
        theme( plot.background = element_rect( fill = "#ffffff"),
              panel.background = element_rect( fill = "#ffffff"),
              panel.grid  = element_line( colour ="#979e9e"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.ticks = element_line(colour = "#000000"),
              axis.text.x = element_text(angle = 90),
              axis.line = element_line(color = "#000000"),
              plot.title = element_markdown(hjust = 0.5),
              panel.border = element_rect(fill = NA,colour = "#ffffff"),
              legend.position = "none")
#
#
##GRÁFICO DE BARRAS PROVINCIAS MÁS EXPORTADORAS
url <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/exp_imp_prov_2019.csv"
exppr <- read.csv(url)
exppr <- exppr %>% select(-c(subfila,subcolumna))
exppr <- exppr %>% rename_at(vars(colnames(exppr)), ~ c("Provincia","Variable","Valor"))
exppr <- exppr %>% mutate(Valor = stringr::str_replace(Valor,"([,])",".")) %>% mutate(Valor = as.numeric(Valor), ValorEnMill = (Valor/1000000), ValorLog = log(Valor, base = exp(2)))
exppr <- exppr %>% mutate(Valorporc = (Valor/exppr[1,3]))
#
dataplot2 <- exppr %>% filter( Provincia != "Total seleccionado", Variable == "EXPORT") %>% slice_max(order_by = Valorporc, n = 10)

#GRÁFICO
theme_set(theme_classic())
ggplot(dataplot2,aes(y = ValorEnMill, x = forcats::fct_reorder(Provincia, ValorEnMill, .desc = TRUE))) + geom_point() + geom_segment(aes(x=forcats::fct_reorder(Provincia, ValorEnMill, .desc = TRUE),
                   xend=forcats::fct_reorder(Provincia, ValorEnMill, .desc = TRUE),
                   y=0,
                   yend=ValorEnMill))+
      scale_y_continuous(breaks = seq(0,60000,5000),labels = paste(scales::comma(seq(0,60000,5000)),"M€"), limits = c(0,62500)) +
      scale_x_discrete(labels = c("Barcelona","Madrid","Valencia","Zaragoza","A Coruña","Murcia","Navarra","Bizkaia","Pontevedra","Tarragona"))+ labs(title="Comunidades que más exportan",
       subtitle="Año: 2019",
       caption="Fuente: DataComex",
       x=NULL,
       y=NULL) +
  theme(panel.grid.major.y = element_line(colour = "#dadada"),axis.text.x = element_text(angle=65, vjust=0.95, hjust = 1))

#
#
#
datos <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/XComProd.csv"
dfccaa <- rio::import(datos) %>% select(c(1,3,5)) %>% filter(!columna %in% c("Total Nacional","No determinado")) %>% mutate( valor = str_replace(valor,"[,]","."), valor = as.numeric(valor), fila = str_remove(fila,"[[:digit:]]+"), fila = case_when(fila == "A GRASAS Y ACEITES" ~ "GRASAS Y ACEITES",fila == "B SEMILLAS Y FRUTOS OLEAGINOSOS" ~ "SEMILLAS Y FRUTOS OLEAGINOSOS",fila == "C PIENSOS ANIMALES" ~ "PIENSOS ANIMALES",fila == " PRODUCTOS CÃRNICOS" ~ "PRODUCTOS CÁRNICOS", fila == fila ~ fila), columna = case_when(columna == "AndalucÃ­a" ~ "Andalucía",columna == "AragÃ³n" ~ "Aragón",columna == "Castilla y LeÃ³n" ~ "Castilla y León",columna == "CataluÃ±a" ~ "Cataluña",columna == "Murcia, RegiÃ³n de" ~ "Murcia",columna == "PaÃ­s Vasco" ~ "País Vasco", columna == columna ~ columna), fila = paste(fila,"(Mill. de €)"))

aa <- dfccaa %>% pivot_wider(names_from = columna, values_from = valor) %>% column_to_rownames("fila")
#TABLA
DT::datatable(aa, extensions = c("FixedColumns","Buttons"),
  options = list(
    dom = "Btip",
    scrollX = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    buttons = c("excel"),
    pageLength = 5))
#
#
#
#XM POR PRODUCTO Y FECHA EN MILLONES DE EUROS
datos <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/XMProducto.csv"
dfprod <- rio::import(datos) %>% select(-c(2)) %>% filter(columna %in% c("1995","2019")) %>% mutate( valor = str_replace(valor,"[,]","."), valor = as.numeric(valor))

a <- inner_join(x = dfprod, y = dfprod %>% filter(fila == "Total seleccionado"), by = c("columna","subcolumna")) %>% mutate(porc = (valor.x/valor.y)) %>% select(-c(5,6)) %>% mutate(fila.x = case_when(fila.x == "Total seleccionado" ~ "Total", fila.x == "1 ALIMENTACIÃ“N, BEBIDAS Y TABACO" ~ "Alimentación, bebidas y tabaco", fila.x == "2 PRODUCTOS ENERGETICOS" ~ "Productos energéticos", fila.x == "3 MATERIAS PRIMAS" ~ "Materias primas", fila.x == "4 SEMIMANUFACTURAS" ~ "Semimanufacturas", fila.x == "5 BIENES DE EQUIPO" ~ "Bienes de equipo",fila.x == "6 SECTOR AUTOMOVIL" ~ "Sector automóvil",fila.x == "7 BIENES DE CONSUMO DURADERO" ~ "Bienes de consumo duradero",fila.x == "8 MANUFACTURAS DE CONSUMO" ~ "Manufacturas de consumo",fila.x == "9 OTRAS MERCANCIAS" ~ "Otras mercancías"), porc = percent(porc), valor.x = comma(valor.x, digits = 0))
aa <- a %>% pivot_wider(names_from = c(subcolumna,columna), values_from = c(valor.x,porc)) %>% rename(" " = 1)%>% select(1,2,6,3,7,4,8,5,9) %>% slice(1,7,5,6,2,10,8,3,4,9) %>% setNames(c("","Mill. de €","%","Mill. de €","%","Mill. de €","%","Mill. de €","%"))

#TABLA
kbl(aa, format = "html", booktabs = T, linesep = "") %>% add_header_above(c("","Exportaciones" = 2, "Importaciones" = 2, "Exportaciones" = 2, "Importaciones" = 2)) %>% add_header_above(c("", "1995" = 4, "2019" = 4), line = T) %>%  kable_styling(c("striped","hover")) %>% row_spec(1, bold = TRUE)
#
#
#
#Medios de transporte más utilizados 2019 en millones de euros
datos <- "https://raw.githubusercontent.com/xi765/MiProyectoIndividualArchivos/main/XMMedioTransporte.csv"
dftrans <- rio::import(datos)%>% select(-c(2)) %>% mutate( valor = str_replace(valor,"[,]","."), valor = as.numeric(valor), fila = str_remove(fila,"[[:digit:]]+"), fila = case_when( fila == "  MarÃ­timo" ~ "  Marítimo", fila == "  TrÃ¡fico postal" ~ "  Tráfico postal", fila == "  PropulsiÃ³n propia" ~ "  Propulsión propia", fila == fila ~ fila)) %>% filter(columna != "Total seleccionado")

a <- inner_join( x = dftrans%>%filter(subcolumna == "EXPORT"), y = dftrans%>%filter(subcolumna == "IMPORT"), by = c("fila","columna")) %>% mutate(totalxm = (valor.x+valor.y)) %>% select(-c(3:6))
aa <- inner_join( x = a%>%filter(fila!="Total modo transporte"), y = a %>% filter(fila=="Total modo transporte"), by = c("columna")) %>% mutate(porc = (totalxm.x/totalxm.y)) %>% select(-c(4,5)) %>% mutate(fila.x = case_when(fila.x == "  Desconocido" ~ "Otro",fila.x == "  Plataforma fija" ~ "Otro",fila.x == "  Propulsión propia" ~ "Otro",fila.x == "  Tráfico postal" ~ "Otro",fila.x == "  Transporte fluvial" ~ "Otro", fila.x == fila.x ~ fila.x)) %>% group_by(columna,fila.x) %>% summarise(totalxm.x = sum(totalxm.x), porc = sum(porc)) %>% group_by(columna) %>% mutate(fila.x = factor(fila.x, levels = fila.x[order(porc, decreasing = TRUE)]))

#GRÁFICO
ggplot(aa, aes(x = "", y=porc, fill = factor(fila.x))) +
  geom_bar(width = 1, stat = "identity",color = "#000000") +
  coord_polar(theta = "y", start=0) +
  labs(fill=NULL,
       x=NULL,
       y=NULL,
       title="Medios de transporte de mercancías más utilizados",
       caption="Fuente: DataComex") + facet_wrap(vars(columna),nrow = 2, ncol = 2) +
      scale_y_continuous(breaks = seq(0,0.8,0.2),labels = scales::percent) +
      scale_fill_brewer(palette = "Set1") +
      theme( strip.background = element_blank(),
              panel.background = element_rect( fill = "#ffffff", colour = "#ffffff" ),
              panel.grid  = element_line( colour ="#ffffff"),
              axis.ticks = element_line(colour = "#ffffff"),
              axis.text = element_text(size = 10, face = "bold", color = "#000000"),
              axis.line = element_blank(),
              plot.title = element_text(hjust = 0.5),
              plot.background = element_rect(fill = "#ffffff"),
              panel.border = element_blank(),
              legend.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
              strip.text = element_text(face = "bold",colour = "black"),
              legend.position = "bottom")
