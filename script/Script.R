require(rvest) # importa a biblioteca rvest
cidades<-read_html("http://cidades.ibge.gov.br/download/mapa_e_municipios.php?lang=&uf=rn") %>% 
  html_table(fill=TRUE)

require(stringr) #importa a biblioteca stringr

base<-data.frame(cidades[[1]][c(1,2,4)]) # Selecionando os dados de interesse
names(base)<-c("cidade","cep","pop2010") # Nomeando as colunas
row.names(base)<-1:168 # Enumerando as linhas
base$pop2010<-str_replace_all(base$pop2010,"\\.","") # Retira os pontos

base$pop2010[is.na(base$pop2010)] <- 0 # Substitui valores NAs por zeros
base <- head(base,-1) # Remove a ultima linha da tabela

base$pop2010<-as.numeric(base$pop2010) # Converte os caracteres em numéricos

head(base)

require(sp)

setwd("C:/Users/luanpereira00/Documents/Bash/TISaude/TIS-ExDataBase")

br <- readRDS("brasil_rds/BRA_adm2.rds") # Importa os polígonos do arquivo no diretório de trabalho

# Plotando o mapa do Brasil, organizado por municípios
plot(br)

rn = (br[br$NAME_1=="Rio Grande do Norte",]) # Filtrando apenas os municípios do RN

# Plotando o mapa do Rio Grande do Norte
plot(rn)

# Plotando o mapa do Rio Grande do Norte com a cidade de Natal destacada em vermelho
plot(rn)
plot(rn[rn$NAME_2=="Natal",], add=T, col="red")

rn$NAME_2[which(rn$NAME_2=="Governador Dix-Sept Rosad")]<-"Governador Dix-Sept Rosado"
rn$NAME_2[which(rn$NAME_2=="Lagoa de Anta")]<-"Lagoa d`Anta"
rn$NAME_2[which(rn$NAME_2=="Lagoas de Velhos")]<-"Lagoa de Velhos"
rn$NAME_2[which(rn$NAME_2=="Jardim-Piranhas")]<-"Jardim de Piranhas"
rn$NAME_2[which(rn$NAME_2=="Olho-d'Água do Borges")]<-"Olho-d`Água do Borges"
rn$NAME_2[which(rn$NAME_2=="Passabém")]<-"Passagem"
rn$NAME_2[which(rn$NAME_2=="Santana")]<-"Santana do Seridó"
rn$NAME_2[which(rn$NAME_2=="Junco")]<-"Messias Targino"
rn$NAME_2[which(rn$NAME_2=="São Miguel de Touros")]<-"São Miguel do Gostoso"
rn$NAME_2[which(rn$NAME_2=="Presidente Juscelino")]<-"Serra Caiada"
rn$NAME_2[which(rn$NAME_2=="Groaíras")]<-"Grossos"

rn <- merge(x=rn, y=base, by.x="NAME_2", by.y="cidade") # Faz um merge dos dataframes

# Criando os intervalos e classificando
col_no = as.factor(cut(rn$pop2010, breaks = c(0,3000,10000,100000,300000,500000,800000,1000000), labels=c("<3k", "3k-10k", "10k-
                                                                                       100k","100k-300k", "300k-500k", "500k-800k", ">800k"), right= FALSE))
# Nomeando os intervalos - irá aparecer na legenda do grafico
levels(col_no) = c("<3k", "3k-10k", "10k-100k","100k-300k", "300k-500k", "500k-800k", ">800k")

# Adicionando a informação da categoria no dataframe
rn$col_no = col_no

require(RColorBrewer)
myPalette = brewer.pal(7,"Greens")
spplot(rn, "col_no", col=grey(.9), col.regions=myPalette, main="Municípios do RN")

require(ggmap)

# Define o nome das cidade-alvos - a ser apresentada como label no mapa
nomes = c("Natal","Mossoró","Pau dos Ferros")

# Define or argumentos de busca para as cidade-alvos - a ser usada pelo ggmap
nam = c("Natal+Brazil+RN","Mossoro+Brazil+RN","Pau dos Ferros+Brazil+RN")

# Busca a geolocalização (Google) para cada cidade
pos = geocode(nam)

# Define a posição dos labels como sendo um pouco acima dos pontos
tlat = pos$lat+0.05 # -- the city name will be above the marker

# Cria um daframe com as informações (nome da cidade, longitude, latitude e posição do label)
cities = data.frame(nomes, pos$lon,pos$lat,tlat)

# Nomeia as colunas de longitude e latitude
names(cities)[2] = "lon"
names(cities)[3] = "lat"

# Criando os labels
text1 = list("panel.text", cities$lon, cities$tlat, cities$nomes,col="black", cex = 0.5)

# Criando os apontamentos
mark1 = list("panel.points", cities$lon, cities$lat, col="blue")

spplot(rn, "col_no", sp.layout=list(text1,mark1), main="Municípios RN", col=grey(.9), col.regions=myPalette)

myPalette = rainbow(7) # Alterando a palheta de cores 

spplot(rn, "col_no", sp.layout=list(text1,mark1), main="Municípios RN", col=grey(.9), col.regions=myPalette)

require(ggmap)

natalMap = get_map(location = "Natal+RN+Brasil", zoom = 11, source = "google", maptype="roadmap")

ggmap(natalMap)
