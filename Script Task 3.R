#Task 3 Taller R
#Rafael Otero 201821640
#Maria Paula Alvarez 201820569
#Version de R: R version 4.1.0 (2021-05-18) ,Platform: x86_64-apple-darwin17.0 (64-bit), Running under: macOS Big Sur 11.5.2


rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages(pacman)
 require(pacman)
p_load(dplyr,data.table)
print

#Pregunta 1
p_load(tidyverse,viridis,sf,leaflet,raster,maps) # llamar y/o instalar librerias
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#Punto 1.1.1 
via=st_read("task_3 /data/input/VIAS.shp") #Importa objeto via
via%>%class()
puntos=st_read("task_3 /data/input/MGN_URB_TOPONIMIA.shp")#Importo objeto punto
puntos%>%class()

#Punto 1.1.2
c_medico= subset(puntos,CSIMBOL == "021001"|CSIMBOL == "021002"|CSIMBOL == "021003")#Escoge las observaciones en donde CSIMBOL=021001,021002,021003

#Punto 1.1.3
c_poblado=read_rds("task_3 /data/input/c poblado (2017).rds")# Importo objecto c_poblado
c_poblado=subset(c_poblado,cod_dane>=54001 & cod_dane<55000)#Dejo las observaciones con cod_dane>= 54001 cod_dane<55000
depto=read_rds("task_3 /data/input/dp deptos (2017).rds")
depto=subset(depto,name_dpto="SUCRE")


#Pregunta 2
rm(list = ls()) # limpia el entorno de R
p_load(tidyverse, # llamar y/o instalar las librerias de la clase
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer # export tables to latex 
)  

#Punto 2.1
df=readRDS("task_3 /data/output/f_mapmuse.rds")# Importo la base de datos f_mapmuse
ols=lm(formula= fallecido~ dist_vias+dist_cpoblado+dist_hospi+year+month+as.factor(tipo_accidente)+as.factor(genero)+as.factor(condicion)+as.factor(actividad)+as.factor(cod_mpio),data=df) #Corremos modelo de probabilidad lineal

#Punto 2.2
length(ols$coefficients) %>% table() # vemos cuantos coeficientes tiene la regresion con el fin de determinar cuantas graficas de coeficientes se deben hacer
ols$coefficients# Vemos los nombres de los coeficientes

# A partir de esta informacion nos damos cuenta que por la cantidad de coeficientes en el modelo, toca graficarlos en diferentes gráficas.

graph1=modelplot(ols,coef_omit= "as.factor") + coord_flip() + 
  labs(title = "Probabilidad de fallecer" , subtitle = "Coefplot variables continuas") #Grafica de coeficientes de variable continuas e intercepto
graph1 #Ver grafica 

graph2=modelplot(ols,coef_omit = "actividad|cod_mpio|year|month|dist_vias|dist_cpoblado|dist_hospi|Intercept",color="red") + coord_flip() + 
  labs(title = "Probabilidad de fallecer" , subtitle = "Coefplot tipo de accidente,genero y condicion") #Grafica de coeficientes de dummy accidente, genero y condicion
graph2#Ver grafica


graph3=modelplot(ols,coef_omit = "accidente|cod_mpio|year|month|dist_vias|dist_cpoblado|dist_hospi|Intercept|condcion|genero",color="blue") + coord_flip() + 
  labs(title = "Probabilidad de fallecer" , subtitle = "Coefplot actividad")+theme_dark() #Coefplot de dummy de actividad
graph3#Ver grafica

graph4=modelplot(ols,coef_omit = "accidente|actividad|year|month|dist_vias|dist_cpoblado|dist_hospi|Intercept|condcion|genero",color="orange") + coord_flip() + 
  labs(title = "Probabilidad de fallecer" , subtitle = "Coefplot actividad") +theme_gray()#Coefplot de dummy de codigo de municipio
graph4#Ver grafica

#Procedemos a guardar los graficos
ggsave(plot=graph1, file = "views/ejemplo1.jpeg")
ggsave(plot=graph2, file = "views/ejemplo2.jpeg")
ggsave(plot=graph3, file = "views/ejemplo3.jpeg")
ggsave(plot=graph4, file = "views/ejemplo4.jpeg")

#Punto 2.3
probit=glm(fallecido~ dist_vias+dist_cpoblado+dist_hospi+year+month+as.factor(tipo_accidente)+as.factor(genero)+as.factor(condicion)+as.factor(actividad)+as.factor(cod_mpio),data=df,family = binomial(link = "probit")) #Corremos modelo probit
logit=glm(fallecido~ dist_vias+dist_cpoblado+dist_hospi+year+month+as.factor(tipo_accidente)+as.factor(genero)+as.factor(condicion)+as.factor(actividad)+as.factor(cod_mpio),data=df,family = binomial(link = "logit")) #Corremos modelo logit

#Punto 2.4
msummary(list(ols, logit,probit))#Vista previa de lo que seria la Tabla ANOVA del model
stargazer(ols, probit,logit,
          type= 'text',
          title = "Tabla de resultados de los tres modelos",
          dep.var.labels = c('','Probabilidad de fallecer',''), 
          column.labels = c("Ols","Probit","Logit"),
          column.separate = c(1,1,1),
          style = "qje",
          df = FALSE,
          digits = 4, 
            out = paste0('views/resultadosmodelos.text'))
#Generamos la tabla de resultados en tipo texto

stargazer(ols, probit,logit,
          type= 'html',
          title = "Tablac de resultados de los tres modelos",
          dep.var.labels = c('','Probabilidad de fallecer',''), 
          column.labels = c("Ols","Probit","Logit"),
          column.separate = c(1,1,1),
          style = "qje",
          df = FALSE,
          digits = 4, 
          out = paste0('views/resultadosmodelos.html'))
#Generamos la tabla de resultados en tipo html con un fin pedagogico

#Punto 2.5
#Generamos los efectos marginales tanto para el modelo probit como para el modelo logit
logit_marg = margins(logit)
logit_marg %>% tidy(conf.int = TRUE)
probit_marg = margins(probit)
probit_marg %>% tidy(conf.int = TRUE)

logit_marg=logit_marg%>%subset(select=c(vector))
probit_marg=probit_marg%>%subset(select=c(vector))
#Lo anterior toca hacerlo porque la variable _weigths tiene puros N.A y modelplot no deja grafica

mods = list('Logit' = logit_marg , 'Probit' = probit_marg)#generamos lista de modelos probit y logit
graph5=modelplot(mods,coef_omit = "condicion|accidente|cod_mpio|year|month|dist_vias|dist_cpoblado|actividad|Intercept|condcion|genero") + coord_flip() + 
  labs(title = "Probabilidad de fallecer" , subtitle = "efectos marginales de variable dist_hospi para modelos Probit y Logit")+theme_light()#Coefplot efectos marginales Probit y logit para variable dist_hospi
graph5#Ver grafica

#Ahora graficamos con ggplot
graph6=modelplot(mods,coef_omit = "condicion|accidente|cod_mpio|year|month|dist_vias|dist_cpoblado|actividad|Intercept|condcion|genero") + coord_flip() + 
  labs(title = "Probabilidad de fallecer" , subtitle = "efectos marginales de variable dist_hospi para modelos Probit y Logit")+theme_dark()#Coefplot efectos marginales Probit y logit para variable dist_hospi
graph6#Ver grafica

#Guardamos
ggsave(plot=graph5, file = "views/ejemplo5.jpeg")
ggsave(plot=graph6, file = "views/ejemplo6.jpeg")

#Punto 3
rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(tidyverse,data.table,plyr,XML,rvest,xml2) # cargar y/o instalar paquetes a usar

#Punto 3.1
browseURL(url = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia",browser = getOption('browser'))# Ver pagina
url="https://es.wikipedia.org/wiki/Departamentos_de_Colombia"#Especificamos la url
info_html=read_html(url)
class(info_html)# Vemos el tipo de archivo que es el HTLM

#Punto 3.2
info_html %>% html_nodes(xpath = '//*[@id="firstHeading"]/div/p[1]')#armamos codigo
info_html %>% html_nodes(xpath = '//*[@id="firstHeading"]')%>% class()#vemos la clase del elemento
info_html %>% html_nodes(xpath = '//*[@id="firstHeading"]')%>%html_text()#armamos codigo para ver texto
Texto_titulo=info_html %>% html_nodes(xpath = '//*[@id="firstHeading"]')%>%html_text()#generamos elemento

#Punto 3.3
tablas= info_html %>% html_nodes('table') #extraemos todos los objetos de tipo table del elemento info_html
tablas# vemos el objeto y encontramos que tiene 5 elementos

tablas[1] %>% html_table(header = T,fill=T)
tablas[2] %>% html_table(header = T,fill=T) 
tablas[3] %>% html_table(header = T,fill=T) 
tablas[4] %>% html_table(header = T,fill=T)

cat("Lo que se hizo anteriormente fue ir insoeccionando en orden los elementos de tablas con el fin de determinar cual elemento corresponde a la tabla de los departamentos. Se encontro que es el elemtno 4")

tabla_departamentos= tablas[4] %>% html_table(header = T,fill=T)  %>% as.data.frame() # Genero el data.frame de la tabla de los departamentos de colombia

#Otra forma de hacerlo
parse = read_html(url) %>% htmlParse()
tablas_2 = parse %>% readHTMLTable(header = T)
tabla_departamentos_2=tablas_2[[4]]

# Se hace con el metodo de parse que nos deja archivos 'HTMLInternalDocument'


