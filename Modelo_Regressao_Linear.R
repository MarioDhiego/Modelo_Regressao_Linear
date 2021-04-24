
####################### CARREGAR PACOTES ####################################################
library(ggplot2)       # viasualização gráfica 
library(geobr)         # shafile do IBGE
library(raster)        # trabalhar com formato espacial
library(fields)        # paletas de cores em Mapas
library(ggspatial)     # adicionar escala anotaion
library(ggpubr)        # inserir equaÇÃO no grafico
library(gganimate)     # graficos animados
library(ggthemes)      # adicionar novos temas
library(plotly)        # grafico dinâmicos
library(modelr)        # B?sico de Modelos
library(lmtest)        # Teste de Breusch-Pagan
library(gridExtra)     #
library(dplyr)         # Manipulação de dados
library(rstatix)       #
library(car)           # Teste de durbin
library(psych)         # Painel de dados
library(QuantPsyc)     # coef. padronizados
library(scatterplot3d) # gráfico 3d
library(colorspace)    # paletas de cores mapas
library(RColorBrewer)  # paletas de cores graficos
################################################################################################

##################### DEFINIR DIRETÓRIO DE TRABALHO ###########################################
# Set Working Directory
setwd('C:/Users/mario Dhiego/Documents/MAPAS_TEMPERATURA/mapa_temperatura/dados')
getwd()
list.files()
###############################################################################################

##################### LEITURA DE BANCO DE DADOS #############################################
# Dados Temperatura de Minas
dados.temp <- read.csv('dados_temperatura.csv')

# Relevo de Minas
relevo.mg <- raster('relevo_minas_gerais.tif')
plot(relevo.mg)
##############################################################################################


################ Limites Territoriais/ IBGE #################################################
#geobr: shapefiles/IBGE e outros conjuntos de dados espaciais oficiais do Brasil

Minas <- read_state(code_state = 'MG', year = "2019")
plot(Minas$geom)
#############################################################################################


################### Faxineiro de Dados #######################################################
# Visualizar janela separa
View(dados.temp)

# Estrutura dos dados
str(dados.temp)
class(dados.temp)
names(dados.temp)

# Visualizar as linhas Iniciais
head(dados.temp)

# Visualizar as linhas Finais
tail(dados.temp)

# Visualiza um Resumo do Dados
glimpse(dados.temp)

# Resumo Descritivo
summary(dados.temp)
##########################################################################################

############### REGRESSÃO LINEAR MÚLTIPLA ################################################
# ConstruÇÃO do Modelo
Modelo1 <- lm(formula = temp~lon+lat+alt, data= dados.temp)
Modelo1

Modelo2 <- lm(formula = temp~alt+lon,data= dados.temp)
Modelo2

P1 <- predict(Modelo1, data.frame(lon=-49.50,lat= 50, alt= 49), se=T)
P1
#########################################################################################


###### AnÁlise de ResÍduos ##############################################################
par(mfrow=c(2,2))
plot(Modelo1)
########################################################################################


####### Normalidade dos resíduos #######################################################
shapiro.test(Modelo1$residuals)
########################################################################################


####### outliers dos resíduos #########################################################
rstandard(Modelo1)
summary(rstandard(Modelo1))
plot(rstandard(Modelo1))
#######################################################################################



####### Independência dos resíduos ########################################################
durbinWatsonTest(Modelo1)
#######################################################################################


####### Homocedasticidade dos resíduos (Breusch-Pagan) ####################################
bptest(Modelo1)
##########################################################################################

####### Diagrama de Dispersão com Equação da Reta ########################################

Plot2 <- ggplot(dados.temp, aes(y=temp, x=alt))+
  geom_point(size=2.5, pch=21, col='black',fill='coral')+
  geom_smooth(formula = y~x, method = 'lm', se=TRUE)+
  stat_regline_equation(aes(label= paste(..eq.label.., ..adj.rr.label..,
                        sep = "*plain(\",\")~~")),
                        label.x = -50, label.y = 28, size=6.0, color='black')+
  theme_bw()+
  labs(x="Altitude", y="Temperatura", 
       title="DIAGRAMA DE DISPERS?O", 
       subtitle = "Temperatura do Ar")
ggplotly(Plot2)

########################################################################
Plot3 <- ggplot(dados.temp, aes(y=temp, x=lon))+
  geom_point(size=2.0, pch=21, col='black',fill='red')+
  geom_smooth(formula = 'y~x', method = 'lm', se=TRUE)+
  stat_regline_equation(size= 6.0, show.legend = TRUE, inherit.aes = TRUE)+
  theme_bw()+
  labs(x="Longitude", y="Temperatura", title="Diagrama de Dispers?o")
ggplotly(Plot3)


Plot4 <- ggplot(dados.temp, aes(y=temp, x=lat))+
  geom_point(size=2.0, pch=21, col='black',fill='red')+
  geom_smooth(formula = 'y~x', method = 'lm', se=TRUE)+
  stat_regline_equation(size= 6.0, show.legend = TRUE, inherit.aes = TRUE)+
  theme_bw()+
  labs(x="Latitude", y="Temperatura", title="Diagrama de Dispers?o")
ggplotly(Plot4)
#######################################################################################


###### Gráficos Lado a Lado ###########################################################
grid.arrange(Plot2, Plot3, Plot4, ncol=2)
#######################################################################################

########################################################################################
###### Ausência de Multicolinearidade

###### Fator de Inflação da Variância (VIF)
pairs.panels(dados.temp)
vif(Modelo1)
#######################################################################################


###### Verificar Signific?ncia (p-valor) ##############################################
summary(Modelo1)
#######################################################################################


###### Coeicientes Padronizados #####################################################
lm.beta(Modelo1)
#######################################################################################


###### Intervalo de Confiança 95%
confint(Modelo1)
#######################################################################################


####### Comparação de Modelos #########################################################
AIC(Modelo1,Modelo2)
BIC(Modelo1,Modelo2)
#######################################################################################


###### Comparar Modelo Aninhados ###################################################
anova(Modelo1, Modelo2)
###### Melhor será o com menor RSS(residula sum of squares)

###### Gráfico Multiplo / pacote (scatterplot3d) ####################################
graph <- scatterplot3d(dados.temp$temp~dados.temp$alt+dados.temp$lon,
                       pch= 16, color ="darkblue", angle=55, grid=TRUE,box=TRUE,
                       highlight.3d=FALSE, col.grid = "lightblue",
                      xlab="Longitude", ylab="Altitude", zlab="Temperatura do Ar",
                      main = "Diagrama de Dispers?o 3d")

graph$plane3d(Modelo2, col="black", type = "h", 
              pch = 16, draw_polygon=FALSE,  
              lty = "dotted")
plotrgl(graph)
##############################################################################







































###### Giro Interativo no gráfico
library(plot3D)
library(plot3Drgl)
library(scatterplot3d)

###### Exemplo Usaremos o conjunto de dados da íris 
data(iris)
head(iris)

###### x, y and z coordinates
x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Sepal.Width



###### Gráico de Dispersão
scatter3D(x, y, z, ..., colvar = z, col = NULL, add = FALSE)
text3D(x, y, z, labels, colvar = NULL, add = FALSE)
points3D(x, y, z, ...)
lines3D(x, y, z, ...)
scatter2D(x, y, colvar = NULL, col = NULL, add = FALSE)
text2D(x, y, labels, colvar = NULL, col = NULL, add = FALSE)

scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))

scatter3D(x, y, z, colvar = NULL, col = "blue",
          pch = 19, cex = 0.5)

scatter3D(x, y, z, bty = "f", colkey = FALSE, main ="bty= 'f'")

scatter3D(x, y, z, bty = "b2", colkey = FALSE, main ="bty= 'b2'" )


scatter3D(x, y, z, bty = "g", colkey = FALSE, main ="bty= 'g'")

scatter3D(x, y, z, pch = 18, bty = "u", colkey = FALSE, 
          main ="bty= 'u'", col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue")



scatter3D(x, y, z, bty = "g", pch = 18, col = gg.col(100))

scatter3D(x, y, z, bty = "g", pch = 18,
          col = ramp.col(c("blue", "yellow", "red")) )




scatter3D(x, y, z, bty = "g", pch = 18, 
          col.var = as.integer(iris$Species), 
          col = c("#1B9E77", "#D95F02", "#7570B3"),
          pch = 18, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("setosa", "versicolor", "virginica")) )


scatter3D(x, y, z, phi = 0, bty ="g")

scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "Iris data", xlab = "Sepal.Length",
          ylab ="Petal.Length", zlab = "Sepal.Width")


scatter3D(x, y, z, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)


# Confidence interval
CI <- list(z = matrix(nrow = length(x),
                      data = rep(0.1, 2*length(x))))
head(CI$z)


# 3D Scatter plot com IC
scatter3D(x, y, z, phi = 0, bty = "g", col = gg.col(100), 
          pch = 18, CI = CI)


# Add small dots on basal plane and on the depth plane
scatter3D_fancy <- function(x, y, z,..., colvar = z)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}

#############################################################################
data(mtcars)
head(mtcars[, 1:6])

# x, y, z variables
x <- mtcars$wt
y <- mtcars$disp
z <- mtcars$mpg
# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "wt", ylab = "disp", zlab = "mpg",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "mtcars")
##################################################################################
data(USArrests)
with(USArrests, text3D(Murder, Assault, Rape, 
                       labels = rownames(USArrests), colvar = UrbanPop, 
                       col = gg.col(100), theta = 60, phi = 20,
                       xlab = "Murder", ylab = "Assault", zlab = "Rape", 
                       main = "USA arrests", cex = 0.6, 
                       bty = "g", ticktype = "detailed", d = 2,
                       clab = c("Urban","Pop"), adj = 0.5, font = 2))
###############################################################################
data(VADeaths)
#  hist3D and ribbon3D with greyish background, rotated, rescaled,...
hist3D(z = VADeaths, scale = FALSE, expand = 0.01, bty = "g", phi = 20,
       col = "#0072B2", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, ticktype = "detailed", d = 2)


hist3D (x = 1:5, y = 1:4, z = VADeaths,
        bty = "g", phi = 20,  theta = -60,
        xlab = "", ylab = "", zlab = "", main = "VADeaths",
        col = "#0072B2", border = "black", shade = 0.8,
        ticktype = "detailed", space = 0.15, d = 2, cex.axis = 1e-9)
# Use text3D to label x axis
text3D(x = 1:5, y = rep(0.5, 5), z = rep(3, 5),
       labels = rownames(VADeaths),
       add = TRUE, adj = 0)
# Use text3D to label y axis
text3D(x = rep(1, 4),   y = 1:4, z = rep(0, 4),
       labels  = colnames(VADeaths),
       add = TRUE, adj = 1)


hist3D_fancy<- function(x, y, break.func = c("Sturges", "scott", "FD"), breaks = NULL,
                        colvar = NULL, col="white", clab=NULL, phi = 5, theta = 25, ...){
  
  # Compute the number of classes for a histogram
  break.func <- break.func [1]
  if(is.null(breaks)){
    x.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(x),
                       scott = nclass.scott(x),
                       FD = nclass.FD(x))
    y.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(y),
                       scott = nclass.scott(y),
                       FD = nclass.FD(y))
  } else x.breaks <- y.breaks <- breaks
  
  # Cut x and y variables in bins for counting
  x.bin <- seq(min(x), max(x), length.out = x.breaks)
  y.bin <- seq(min(y), max(y), length.out = y.breaks)
  xy <- table(cut(x, x.bin), cut(y, y.bin))
  z <- xy
  
  xmid <- 0.5*(x.bin[-1] + x.bin[-length(x.bin)])
  ymid <- 0.5*(y.bin[-1] + y.bin[-length(y.bin)])
  
  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 2))
  hist3D(x = xmid, y = ymid, z = xy, ...,
         zlim = c(-max(z)/2, max(z)), zlab = "counts", bty= "g", 
         phi = phi, theta = theta,
         shade = 0.2, col = col, border = "black",
         d = 1, ticktype = "detailed")
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = gg.col(100),
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

hist3D_fancy(quakes$long, quakes$lat, colvar=quakes$depth,
             breaks =30)

hist3D_fancy(iris$Sepal.Length, iris$Petal.Width, 
             colvar=as.numeric(iris$Species))


# Create his3D using plot3D
hist3D_fancy(iris$Sepal.Length, iris$Petal.Width, colvar=as.numeric(iris$Species))
# Make the rgl version
library("plot3Drgl")
plotrgl()
###########################################################


# Veriicar as Paletas de Cores no pacote RColorBrewer
display.brewer.all()

#shapes = c(16, 17, 18) 
#shapes <- shapes[as.numeric(dados.temp$temp)]


########################### MODELO (Y1) ########################################
# TEMPERATURA = 23.49 - 0.25longitude + 0.48latitude - 0.005altitude
################################################################################

# Transformar dados do Relevo(raster) em Data.Frame(planilha)
relevo.mg.dataframe <- as.data.frame(relevo.mg, xy=TRUE)

# SEM NA
relevo.mg.dataframe.naomit <- na.omit(relevo.mg.dataframe)


names(relevo.mg.dataframe.naomit) <- c('lon', 'lat', 'alt')
relevo.mg.dataframe.naomit


# Estimar a Temperatura
# Modo 1

relevo.mg.dataframe.naomit$temp <- 23.49 -0.2558*relevo.mg.dataframe.naomit$lon + 0.48*relevo.mg.dataframe.naomit$lat - 0.0053*relevo.mg.dataframe.naomit$alt

# Modo 2
predict(Modelo1, relevo.mg.dataframe.naomit)
relevo.mg.dataframe.naomit$temp2 <- predict(Modelo1, relevo.mg.dataframe.naomit)


###################################################################################
# Mapa de Minas

class(Minas) 

P1_Minas <- ggplot(data= relevo.mg.dataframe.naomit)+
  geom_raster(aes(x=lon, y=lat, fill=temp))+
  geom_sf(data = Minas, fill='NA')+
  scale_fill_gradientn(colours= tim.colors(10))+
  annotation_scale()+
  annotation_north_arrow(location='tl', 
             style= north_arrow_fancy_orienteering())+
  labs(x="Latitude", y="Longitude", fill='[?C]', title="Temperatura do Ar")
  theme_minimal()
  ggplotly(P1_Minas)
  
P1_Minas

###### Salvar Gráfico
ggsave(plot = P1_Minas,filename="C:/Users/mario Dhiego/Documents/MAPAS_TEMPERATURA/mapa_temperatura/figuras/Temperatura1.jpeg",
       width= 5,height=5)
#########################################################################################


###### Automatizar a Produção de Gráficos/Mapas
# Utilizar a função FOR

for (i in 1:10) {
  print ('ola')
}

#######################################################
















####################### Banco de Dados Raiz do R #####################################

# Nome: Speed and Stopping Distances of Cars
# Decri??o: Os dados fornecem a velocidade dos carros e as dist?ncias percorridas para parar. Observe que os dados foram registrados na d?cada de 1920.
# Foemato: Um quadro de dados com 50 observa??es em 2 vari?veis.

# Vari?veis
# speed:	numeric	Speed (mph)
# dist:	numeric	Stopping distance (ft)
#######################################################################################


#################### Data_Frame ######################################################
cars

var(cars)
View(cars)
class(cars)
names(cars)
#####################################################################################


######################## Regress?o Linear Simples ###############################
Carros1 <- lm(formula = dist~speed,data= cars)
Carros1

# Resumo Descritivo
summary(Carros1)

#Teste de Correla??o
cor.test(cars$speed,cars$dist)

# Anova
anova(Carros1)

# Res?duos
boxplot(residuals(Carros1))

# IC 95%
confint(Carros1)

# Gr?fico do Ajuste
plot(fitted(Carros1))

# Teste de Normalidade Shapiro Wilk
shapiro.test(residuals(Carros1))


#Teste de Durbin Watson
dwtest(Carros1)

#Teste de Breusch Pagan
bptest(Carros1)

############# Gerar Gr?fico ######################################

Plot1 <- ggplot(cars, aes(y=dist, x=speed))+
  geom_point(size=2.8, pch=21, col='black',fill='red')+
  geom_smooth(formula = 'y~x', method = 'lm', se=TRUE)+
  stat_regline_equation(size= 9.0, show.legend = TRUE, inherit.aes = TRUE)+
  theme_bw()+
  labs(x="Velocidade(mph)", y="Dist?ncia Percorrida (ft)", title="Diagrama de Dispers?o")
Plot1
  
  
#theme_set(theme_economist()) 
#theme_fivethirtyeight()

#####################################################################

# Salvar Gr?fico
ggsave(plot = Plot1,filename="C:/Users/mario Dhiego/Documents/MAPAS_TEMPERATURA/mapa_temperatura/figuras/Plot1_Dados_Cars.jpeg",
       width= 5,height=5)









