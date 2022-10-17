Dados4.1.2=c (70,76,76,77,77,78,80,81,81,83,83,83,84,86,86,87,87,88,89,90,90,91,92,92,93,94,94,95,98,99)

dados<-Dados4.1.2
sort(dados) #ordena os dados ROL

#moda para dados discretos
moda=subset(table(dados),table(dados)==max(table(dados)))
moda

#calculando media "manualmente"
soma=sum(dados)
soma
n=length(dados)
n

media1=soma/n
media1

#calculando media e mediana com funcoes diretas do R
media=mean(dados) #media aritmetica
media
mediana=median(dados) #mediana
mediana
#===================================
#Medidas de variabilidade no R
#Amplitude total:

A=max(dados)-min(dados)
A
#===================================
#Verificando que a soma dos desvios em relação a media é zero
#===================================
di=dados-media
di

soma_di=sum(di);round(soma_di,10)
Val=(sum(di^2))/(n-1) #Variancia
Val

#Variancia
Va=var(dados) #Usando função pra calcular variancia
Va

#Desvio padrão
DP=sd(dados)
DP

#Coeficiente de variação
CV=(DP/media)*100
CV

#Para quartis

Q1=quantile(dados,.25,type=4)
Q1
Q3=quantile(dados,.75,tyope=4)
Q3

#Para porcentis


