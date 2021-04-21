library(tidyverse)
# library(fpp2)
# library(dplyr)
# library(zoo)
dados = tibble(read.csv(file = "/home/emisvaldo/Downloads/Planilha sem título - ssss.csv"));

#Transformando a coleta em uma serie temporal R
cpuTS = ts(dados, start = 1, frequency = 1);
mensagens = data.frame();
mensagensID = data.frame();

tamanhoJanela = 400;
janelasCriadasPorErro = 1;

#Separando uma janela com 300 elemento dos quais serao utilizados para cirar a 
#balisa inicial (balisa = devio padrao dessa janela + a média)
janela1 = window(cpuTS, start = 1, end = tamanhoJanela);
media = mean(janela1);
desvioP = sd(janela1);
balisa = media + desvioP;
mensagens =  append(mensagens, balisa);


#Janela dois a qual ira ser monitorada tendo como base a balisa geradada partindo
#da janela anterior
janela2 = window(cpuTS, start = length(janela1), end = length(janela1) + tamanhoJanela);


#Funcção para gerar uma nova janela partido do elemento que foi o quinto a passar 
#a balisa
iniciar = function(elemento){
  janela = window(cpuTS, start = elemento - tamanhoJanela, end = elemento);
  return(janela);
}

################Funcão funcionando ok###########################################
#over é a variavel que conta ocorrencias seguidas de valores acima do meu inter-
#valo de confiança
over = 0;
#Variavel de iteracao
interator = 1;
#variavel para checar se o elemento atual está na mesma seguencia que o anterior
controle1 = 2;
iterator2 = 1;

mensagensID =  append(mensagensID, iterator2);

while(interator <= length(janela2)){
  if (janela2[interator] >= balisa) {
    if(interator - 1 == controle1){
      over = over + 1;
      if(over >= 5){
        plot(janela2);
        lines(c(iterator2, length(janela2)), c(balisa, balisa), lwd = 1, col = "red");
        
        media = mean(janela2);
        desvioP = sd(janela2);
        balisa = media + desvioP;
        janela2 = iniciar(iterator2);
        
        mensagens =  append(mensagens, balisa);
        mensagensID =  append(mensagensID, iterator2);
        janelasCriadasPorErro = janelasCriadasPorErro + 1;
      }
    }else{
      over = 0;
    }
    controle1 = interator;
  }
  
  interator = interator + 1;
  iterator2 = iterator2 + 1;
  
  if(interator > tamanhoJanela & iterator2 < length(cpuTS)){
    # print(iterator2);
    # print(balisa);
    # print("");
    # 
    media = mean(janela2);
    desvioP = sd(janela2);
    balisa = media + desvioP;
    janela2 = window(cpuTS, start = iterator2, end = iterator2 + tamanhoJanela);
    interator = 1;
    
    mensagens =  append(mensagens, balisa);
    mensagensID =  append(mensagensID, iterator2);
  }
}
################################################################################

mensagensTs = ts(mensagens, start = 1, frequency = 1);
men = cbind(mensagensID, mensagens);

plot(cpuTS, type = "l");
par(new = T);
plot(men, pch=19, col = "red", cex=1);
