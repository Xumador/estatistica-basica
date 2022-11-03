
#================================================================
#
#       ROTINA SUPER HIPER MEGA PLUS HYPER SONIC SHADOW
#                     BLASTER EXTENDED v2.0
#
#                     by Luqueta da Galera
#
#================================================================

#-----------------------------------------------------------------------------

#================================================================
#   Cálculos de AMOSTRAGEM
#================================================================

# Amostragem simples ao acaso

N=54  #tamanho da POPULAÇÃO
n=10  #tamanho da amostra
sample(N,n) ;  #sorteando "n" elementos entre  1 e "N")

# Amostragem Estratificada

TE=c(873,386,246,186,112) #tamanho dos estratos Ni

N=1803  #tamanho da POPULAÇÃO
n=100  #tamanho da amostra

ni=n/N*TE;ni
ni2=round(ni,0);ni2 # arredonda os valores de ni com "0" casas decimais

sample(TE[1],ni2[1]) ;  #sorteando "ni" elementos no estrato 1)
sample(TE[2],ni2[2]) ;  #sorteando "ni" elementos no estrato 2)

sample(TE[3],ni2[3]) ;  #sorteando "ni" elementos no estrato 3)

sample(TE[4],ni2[4]) ;  #sorteando "ni" elementos no estrato 4)

sample(TE[5],ni2[5]) ;  #sorteando "ni" elementos no estrato 5)


# Amostragem sistemática

N=1000
n=50

R=round(N/n,0) # passos de amostragem (razão) 
R
sort_elem1=sample(R,1) ;  #sorteando o 1º elemento entre 1 e k)
sort_elem1
uposicao=seq(sort_elem1,N,R);    #posição de todos elementos da amostra
posicao                       #sequencia do 1º elemento ate N com saltos R 


#-----------------------------------------------------------------------------

#================================================================
#   Criando gráficos de barra ou coluna para dados QUALITATIVOS
#================================================================

# Gráfico de barra

dados_quali<-matrix(0,1,2)

colnames(dados_quali)<-c("Elemento1","Elemento2")
dados_quali [1,1]<-25
dados_quali [1,2]<-75

barplot(dados_quali [1,],col=c("blue","red"),horiz=F,beside=F,space=0.3,
        xlab="Elementos", ylab="Quantidade") 

# Gráfico de setores (Pizza)

pie(dados_quali [1,],col=c("blue","red"))

#-----------------------------------------------------------------------------

#=================================================================
#   Criando gráficos de barra ou coluna para dados DISCRETOS
#=================================================================
dados=c(0,1,0,0,2,3,1,0,0,2,0,0,4,0,2,3,2,0,1,1,
        3,1,0,4,2,1,2,0,0,3,0,0,1,1,1,0,2,0,1,3)

d=table(dados) # faz a frequencias das respostas
d

# Gráfico de colunas
barplot(d, col="blue",horiz =F,beside=F,ylim=c(0,20),space=0.3,xlab="N�mero de pacotes", ylab="Frequ�ncia")

# Gráfico de barras
barplot(d, col="gray",horiz =T,xlim=c(0,20),space=0.3,xlab="Frequ�ncia", ylab="N�merode pacotes")

# Gráfico de setores (Pizza)
pie(d)

#-----------------------------------------------------------------------------

#=================================================================
#   Tabela de Frequência e Gráfico para dados CONTÍNUOS
#=================================================================

dados=c(86.6, 88.6 , 99.4,  90.4,  90.8, 100.3,  92.8,  82.4,  85.9,  87.3,  97.3,
        92.2,  92.4,  90.7,  86.7, 100.7,  93.0,  78.2,  94.2,  87.2,  83.6,  88.7,  83.8,
        85.6,  86.2,  79.9 , 95.0,  90.9,  83.2,  97.5, 92.6,  88.2,  95.4,  95.3,  94.9,
        94.1,  93.3,  89.6,  88.2,  87.7,  85.8 , 88.8,  82.4, 103.0,  97.2,  83.3,  87.6,
        87.2,  94.7,  89.5,  91.5,  89.8,  89.7,  98.2,  88.6, 99.1,  80.7,  93.5,  90.7,
        91.3,  92.3,  87.0 , 88.0,  83.9,  83.6,  91.8,  92.7,  90.3,  95.5, 102.3,  87.1,
        76.1,  96.0,  85.7,  85.9,  96.2,  88.3,  82.7,  91.1,  89.2,  90.0,  92.3,  87.8,
        93.9,  88.7,  92.0,  96.6,  92.6,  88.0 , 96.9,96.0,  93.3,  91.4,  86.2 , 98.2,
        86.4, 103.1 , 99.2,  88.6,  83.8)

dados
sort(dados)  # ordena os dados ROL

summary(dados)
sd(dados)
hist(dados)

# Confecção do histograma e da tabela de distribuição de frequência
# Determinar o número de classes
#----------Início da função-------------
numerodeclasses<-function(x)
{
  if(length(x)<=100){k<-sqrt(length(x))+0.5}
  else
  {k<-5*log10(length(x))}
  return(round(k,0))
}
#----------Fim da função----------------

k<-numerodeclasses(dados);k


# Amplitude total

A<-max(dados)-min(dados)        #calcula a amplitude total, A = >obs - <obs
A

# Amplitude de classe

C<-A/(k-1)
C<-round(C,2)                     #considera apenas duas casas decimais
C

# Limite inferior da primeira classe

LI1<-min(dados)-C/2             #limite inferior da 1� classe
LI1<-round(LI1,2)                #considera apenas duas casas decimais
LI1
limites<-LI1+C*(0:k)
limites

# Construção da Tabela de Valores no Terminal do R
TDF<-hist(dados,breaks=limites,plot=FALSE,right=FALSE)
TDF
n<-length(dados) 
tabela<-matrix(c(rep(7*k)),k,7)# s�o 7 colunas na Tabela
for(i in 1:k)
{tabela[i,1]<-round(TDF$breaks[i],2)
tabela[i,2]<-round(TDF$breaks[i+1],2)
tabela[i,3]<-round(TDF$mids[i],2)
tabela[i,4]<-(TDF$counts[i])
tabela[i,5]<-round(((TDF$counts[i])/n),4)
tabela[i,6]<-round((100*TDF$counts[i])/n,2)
tabela[i,7]<-sum(TDF$counts[1:i])
}
sum(TDF$counts[])
colnames(tabela)<-c("LI","LS","Xi","Fi","Fr","Fp","FAp")
tabela

# Gráfico e Histograma 

hist(dados,label=FALSE,col="gray",main="",right=FALSE,
     xlab="Taxa de Glicose (mg/dL)", ylab="Frequência absoluta", 
     xlim=c(min(TDF$mids)-C,max(TDF$mids)+2*C),
     ylim=c(0,(max(TDF$counts)+1)),
     breaks=limites,axes=FALSE)
axis(1,at=limites,pos=c(0,0))
axis(2,at=c(seq(0:(max(TDF$counts)+1))-1))

#C<-TDF$mids[2]-TDF$mids[1]

Xip<-c(TDF$mids[1]-C,TDF$mids,TDF$mids[length(TDF$mids)]+C)
frequencia<-c(0,TDF$counts,0)
Xip
lines(frequencia~Xip,type="l",col="blue",lwd=2)

#-----------------------------------------------------------------------------

#=========================================================================
#   Cálculo de medidas descritivas e Boxplot
#=========================================================================
Dados4.1.2=c (70 , 76 , 76 , 77 , 77 , 78 , 80 , 81 , 81 , 83 , 83 , 83 , 84 , 
              86 , 86 , 87 , 87 , 88 , 89 , 90 , 90 , 91 , 92 , 92 , 93 , 94 , 
              95 , 98 , 99)

dados<-Dados4.1.2
sort(dados)  # ordena os dados ROL

# Calculando a média
media=mean(dados)
media

# Calculando a mediana
mediana=median(dados)
mediana

# Calculando a moda (dados discretos)
moda=subset(table(dados),table(dados)==max(table(dados)))
moda

# Amplitude total:
A = max(dados)- min(dados)
A

# Variância
Va=var(dados)
Va

# Desvio padrão
DP=sd(dados)
DP

# Coeficiente de variação
CV=(DP/media)*100
CV

# Para quartis
Q1=quantile(dados,.25,type=4)
Q1
Q3=quantile(dados,.75,type=4)
Q3

# Para percentis
quantile(dados,0.90,type=4)

# Identificação de Outlier e gráfico Boxplot
DQI<- Q3 - Q1
DQI

LI_q<- Q1-(1.5*DQI); LI_q

LS_q<- Q3+(1.5*DQI); LS_q


boxplot(dados, ylab="Peso do comprimido (mg)")