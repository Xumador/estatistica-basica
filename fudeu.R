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

