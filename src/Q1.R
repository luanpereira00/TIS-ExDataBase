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

require(sp)

setwd("C:/Users/luanpereira00/Documents/Bash/TISaude/TIS-ExDataBase")

br <- readRDS("brasil_rds/BRA_adm2.rds") # Importa os polígonos do arquivo no diretório de trabalho

rn = (br[br$NAME_1=="Rio Grande do Norte",]) # Filtrando apenas os municípios do RN

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

#Removendo Fernando de Noronha e Poço Dantas
#"Poço Dantas"
#"Fernando de Noronha"
#myData <- myData[-c(2, 4, 6), ] Função de remoção
rn<-rn[-c(which(rn$NAME_2=="Poço Dantas"))]
rn<-rn[-c(which(rn$NAME_2=="Fernando de Noronha"))]

rn <- merge(x=rn, y=base, by.x="NAME_2", by.y="cidade") # Faz um merge dos dataframes

# Criando os intervalos e classificando
col_no = as.factor(cut(rn$pop2010, breaks = c(0,3000,10000,100000,300000,500000,800000,1000000), labels=c("<3k", "3k-10k", "10k-
                                                                                                          100k","100k-300k", "300k-500k", "500k-800k", ">800k"), right= FALSE))
# Nomeando os intervalos - irá aparecer na legenda do grafico
levels(col_no) = c("<3k", "3k-10k", "10k-100k","100k-300k", "300k-500k", "500k-800k", ">800k")

# Adicionando a informação da categoria no dataframe
rn$col_no = col_no

require(RColorBrewer)

myPalette = rainbow(7) # Alterando a palheta de cores 
spplot(rn, "col_no", main="Municípios RN", col=grey(.9), col.regions=myPalette)

