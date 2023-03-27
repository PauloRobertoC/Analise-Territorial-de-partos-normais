#mapas usando ggplot2
#criando o primeiro banco de dados - as malhas digitais. (obtidas atraves do link: https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais)
#carregar pacotes necessários
library(ggplot2)
library(rgdal)

#obtendo a base digital
ma<- readOGR("C:/Users/est.paulo.sa/Desktop/Malha digital maranhao", "21MUE250GC_SIR")
head(ma@data) #vendo a base de dados

ma$CD_GEOCMU<- substr(ma$CD_GEOCMU, 1, 6) #reordenando a base de dados
head(ma@data) #vendo a base de dados reordenada

#######################################################################################################
#preparando a segunda base de dados 
#percentual de partos normais por municipios no Rio de Janeiro 
#obtidas atraves do link: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?pacto/2015/cnv/coapmunrj.def
partos<- read.csv("C:/Users/est.paulo.sa/Desktop/Malha digital maranhao/partos_normaisdados.csv", sep = ";", head = TRUE ,dec = ",")
View(partos) #visualizando a tabela partos 
head(partos) #mostrando as primeiras 6 linhas
partos       #visualizando a tabela por completo

partos<- na.omit(partos) #retirando valores NA e atualizando a tabela partos 
partos                   #visualizando tabela partos atualizada (sem valores NA)

partos<- partos[-(218:219), ] #retirando as duas ultimas linhas da tabela partos e atualizando a tabela 
partos                        #verificando a nova tabela 

names(partos)[2]<- c("Partosnormais") #mudando cabeçalho da segunda coluna X20._.Partos_normais = Partosnormais
head(partos)                          #verificando o novo cabeçalho da tabela

#### isolando o codigo do municipio ### (o codigo irá ligar as duas bases de dados - malhaibge e partos normais)
### padronização de variáveis entre os bancos de dados ###
partos$codigodomunicipio<- substr(partos$Município, 1, 6) #isolando o codigo e atualizando a tabela
head(partos) #verificando a nova variável (codigodomunicipio)

### alterando o nome da variavel 'codigodomunicipio' para 'CD_GEOCMU' (codigo dos municipios da primeira base de dados)
names(ma@data) #verificando os nomes das colunas -> 1° base de dados 
names(partos)  #verificando os nomes das colunas -> 2° base de dados

names(partos)[3]<-c("CD_GEOCMU") ### nome alterado ###
head(partos)                     #verificando a alteração

names(ma@data) #verificando  1° base de dados 
names(partos)  #verificando  2° base de dados

### CD_GEOCMU é a nossa variável de relacionamento, ou seja, o elemento que une as duas base de dados 
### ordenando os bancos de dados a partir da variável de relacionamento 

partos<- partos[order(partos$CD_GEOCMU), ] #ordenando e atualizando a tabela partos da 2° base de dados
malhas<- ma@data[order(ma$CD_GEOCMU), ]    #ordenando e atualizando a tabela partos da 1° base de dados
head(partos)  #visualizando a ordenação da tabela
head(malhas)  #visualizando a ordenação da tabela

### criando um novo banco de dados ###
#combinação dos dois bancos de dados trabalhados 

#para executar o merge, as bases devem ter as mesmas dimensões, numeros de linhas = numero de colunas 
ma_2<- merge(malhas, partos, by.y = "CD_GEOCMU") #unindo as duas bases e criando uma terceira > ma_2
head(ma_2) #verificando a 3° base de dados 

### plotagem gráfica dos elementos ###
#extraindo da base de dados ma_2 coordenadas de latitude e longitude 
#delimitando os limites dos municípios - geolocalização 
ma.f<- fortify(ma, region = "CD_GEOCMU") #capturando as delimitações dos territórios 
head(ma.f) #verificando...

#categorizando as variáveis #criando os percentuais de corte. ex: 5%, 10%...
ma_2$partosnormaisCat<- cut(ma_2$Partosnormais, breaks = c(0,10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("0 a 10%", "10 a 20%", "20 a 30%", "30 a 40%", "40 a 50%", "50 a 60%", "60 a 70%", "70 a 80%", "80 a 90%", "90 a 100%"), include.lowest = TRUE)
# percentuais criados
head(ma_2) #verificando novamente

### modificando o nome CD_GEOCMU para id da base de dados ma_2, para igualar ao nome da base ma.f e posteriormente uni-los ###
names(ma_2)[1]<- c("id")
head(ma_2) #verificando

### unindo ma_2 e ma.f por meio da nomenclatura 'id'
### criando e atualizando novo banco de dados ###
ma.f<- merge(ma.f, ma_2, by.y = "id") #atualizando base de dados ma.f com um novo elemnto
head(ma.f) #verificando

### gerando o mapa temático ###
install.packages("RColorBrewer", dependencies = TRUE) #isntalando pacote que permite criar escalas de cores 
library(RColorBrewer) #carregando o pacote
library(ggplot2)      #carregando pacote

### plotando o mapa ###
ggplot(ma.f, aes(ma.f$long, ma.f$lat, group = ma.f$group, fill = ma.f$partosnormaisCat)) + 
  geom_polygon(colour = "Black") + 
  coord_equal() + 
  labs(x = "longitude", y = "latitude", fill = "Percentual") + 
  scale_fill_manual(values = brewer.pal(9, "YlOrRd")[3:10]) +ggtitle("Partos Normais - Maranhão")









































































