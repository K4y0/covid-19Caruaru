# limpar o cach??
rm(list = ls())

# working directory
setwd("~/documents/2020/covid-19/covid_dados")

# Semente
set.seed(1234)

# configura????o de exibi????o de decimais
options(digits="4", scipen = 9999); options(max.print=100000)

ip <- installed.packages()
for (i in c("deSolve","dfoptim"
            ,"readxl", "ggplot2")){
  if ( !(i %in% ip) ) { install.packages(i) }
  if (i=="Quandl") { library(i, character.only = T) ; Quandl.auth("WP2rt8HsRo3kjWsRkLY5") } 
  else library(i, character.only = T)
}
rm(list=c("ip","i"))

# Importar os dados a partir do primeiro caso de contaminados
dados <- read_xlsx("~/documents/2020/covid-19/covid_dados/casos_caruaru.xlsx",
                   sheet = 1, col_names = TRUE, col_types = "numeric")  

dados$data <- seq(as.Date("2020/03/01"), 
                  by = "day", 
                  length.out = length(dados$dia))

# Selecionando apenas os contaminados
Infectados <- dados[23:69,2]; colnames(Infectados) <- c("Infectados")


Infectados$data <- seq(as.Date("2020/03/23"), 
                       by = "day", 
                       length.out = length(Infectados$Infectados))


#Para c??lculo do valor de redu????o f
isolamento <- dados[23:45,c(3,4)]
isolamento <- na.omit(isolamento)

# Fator de reducao calculado com base no indice de isolamento
#f <- 0.9263; 
f <- round(1 - (0.6135 - mean(isolamento$indice_isolamento)), digits = 4)


# Gr??fico 1 - Curva de Contaminados, Para??ba-PB
infectados1 <- ggplot(data = Infectados, 
                      aes(x = data,
                          y = Infectados)) +
  geom_line(colour="black", size=0.50) +
  ggtitle("Curva de Contaminados, Caruaru-PE") + 
  labs(x = "", y = "Total de contaminados") + 
  theme(panel.background = 
          element_rect(fill = "white", 
                       colour = "grey25")); infectados1

# Gr??fico 2 - log(Curva de Contaminados), Para??ba-PB
infectados2 <- ggplot(data = Infectados, 
                      aes(x = data,
                          y = log(Infectados))) +
  geom_line(colour="black", size=0.50) +
  ggtitle("log(Cont??gio), Caruaru-PE") + 
  labs(x = "", y = "Total de contaminados") + 
  theme(panel.background = 
          element_rect(fill = "white", 
                       colour = "grey25")); infectados2

# par??metros para o modelo SIR

init_horizonte <- 1:(length(Infectados$Infectados))

# 0. Popula????o levada em considera????o
populacao = 361127 # IBGE 2019

# 2. Horizonte inicial e final da Simula????o 
horizonte <- seq(from = 1, to = 300, by = 1) 

# 1. Condicoes iniciais
condicoes_iniciais <- c(S = populacao, I = 1, R = 0)

# Par??metros Iniciais, para estima????o
# init_parameters = list(beta = 0.5220, gamma=0.4177)
init_parameters = list(beta = 0.5, gamma=0.5)

# Equa????es do Modelo SIR
SIR <- function(t,x,pars){
  with(as.list(pars),{
    
    S <- x[1]  # Suscet??vel
    I <- x[2]  # Infectados
    R <- x[3]  # Removidos
    
    N = S + I + R  # populacao total
    # beta = (c*p)*f # forca de infeccao
    
    # o modelo de 3 equacoes diferenciais
    dS <- -beta*S/N*I 
    dI <- beta*S/N*I - gamma*I
    dR <- gamma*I 
    
    resultado <- list(c(dS, dI, dR)) # saida da funcao
  }) }

# fun????o para estimar o beta e o gamma
# obs 
estima <- function(init_parameters) {
  # browser()
  names(init_parameters) <- c("beta", "gamma")
  out <- ode(y = condicoes_iniciais,
             times = init_horizonte,
             func = SIR, 
             parms = init_parameters)
  fit <- out[ , 3]
  sum((Infectados$Infectados - fit)^2)
}

# Estimando os par??metros do modelo com base nos dados reais
# Para os dados de 23/03 at?? 04/05
Opt <- optim(par=c(0.5, 0.5), fn = estima, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) 
Opt$message
Opt$convergence

# Par??metros estimados com base nos dados
Opt$par

Opt_par <- setNames(Opt$par, c("beta", "gamma"))


# Constru????o do Modelo 1

# parameters = list(c = 2, p = 0.261, f = 0.92, r=0.4177)
parameters = list(beta  = Opt_par["beta"],
                  gamma = Opt_par["gamma"])


R0 = ( Opt_par["beta"]) / Opt_par["gamma"]; R0



# Solu????o das Equa????es Diferenciais do Modelo SIR
res = lsoda(
  y = condicoes_iniciais,
  times = horizonte,
  func = SIR,
  parms = parameters)

modelo1 = as.data.frame(res)
modelo1$data <- seq(as.Date("2020/03/23"), 
                    by = "day", 
                    length.out = length(horizonte))

# N??mero M??ximo de Contaminados
modelo1[which.max(modelo1$I),]



# Constru????o do Modelo 2
# Modelo 2
parameters2 = list(beta = ((Opt_par["beta"])*f), 
                   gamma=Opt_par["gamma"])

R0 = ( parameters2$beta) / parameters2$gamma;
R0

# Solu????o das Equa????es Diferenciais do Modelo SIR
res = lsoda(
  y = condicoes_iniciais,
  times = horizonte,
  func = SIR,
  parms = parameters2)

modelo2 = as.data.frame(res)
modelo2$data <- seq(as.Date("2020/03/23"), by = "day", length.out = length(horizonte))

# N??mero M??ximo de Contaminados
modelo2[which.max(modelo2$I),]$data

# plotando o gr??fico das curvas
# plotando o gr??fico das curvas
curvas <- ggplot(data = modelo2, aes(x = data, y = I)) + geom_line(colour="blue", size=0.50) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
curvas + geom_line(data = modelo1, aes(x = data, y = I), colour="orange",size=0.50) +
  geom_vline(xintercept = modelo2[which.max(modelo2$I),]$data, size=0.2, colour="black", linetype = 2) +   
  geom_vline(xintercept = modelo1[which.max(modelo1$I),]$data, size=0.2, colour="black", linetype = 2) + 
  labs(x = "", y = "Total de contaminados") + 
  theme(panel.background = 
          element_rect(fill = "white", 
                       colour = "grey25")) +
  annotate("text", x = modelo1[which.max(modelo1$I),]$data, size=2.3, y = 7000, angle = 360, label = "Entre a 4a semana de Junho/\n 1a semana de Julho")+
  annotate("text", size=2.3, x = modelo2[which.max(modelo2$I),]$data, y = 3100, angle = 360, label = "Entre a 4a semana de Agosto/\n1a semana de Setembro")


##############################################################
########################### SEIR #############################
##############################################################

SEIR <- function(t, x, pars){
  # create state variables (local variables)
  S <- x[1]  # Suscet??vel
  E <- x[2]  # Exposed
  I <- x[3]  # Infectados
  R <- x[4]  # Removidos
  
  N = S + E + I + R  # populacao total
  # beta = (c*p)*f # forca de infeccao
  
  with(as.list (pars),{
    # compute derivatives
    dS = -beta*((S*I)/N) 
    dE = beta*((S*I)/N) - (sigma*E)
    dI = (sigma*E) - (gamma*I)
    dR = (gamma*I)
    
    # combine results
    results = c (dS, dE, dI, dR)
    list (results)}
  )}

# fun????o para estimar o beta e o gamma
# obs 
# 1. Condicoes iniciais
condicoes_iniciais_seir <- c(S = populacao, 
                             E = 3, I = 1, R = 0)

init_parameters_seir    <-  list(beta  = 0.69, 
                                 gamma      = 0.53, 
                                 sigma      = 0.1)

estima2 <- function(init_parameters_seir){
  names(init_parameters_seir) <- c("beta", "gamma", "sigma")
  out <- ode(y = condicoes_iniciais_seir,
             times = init_horizonte,
             func = SEIR, 
             parms = init_parameters_seir)
  fit <- out[ , 4]
  sum((Infectados$Infectados - fit)^2)
}


# Estimando os par??metros do modelo com base nos dados reais
# Para os dados de 23/03 at?? 04/05
Opt <- optim(par=c(0.5, 0.5, 0.5), 
             fn = estima2, method = "L-BFGS-B", 
             lower = c(0, 0, 0), 
             upper = c(5, 5, 5)
) 

Opt$message
Opt$convergence

# Par??metros estimados com base nos dados
Opt$par

Opt_par_seir <- setNames(Opt$par, c("beta", "gamma", "sigma"))


R0 <- Opt_par_seir["beta"] / Opt_par_seir["gamma"]; R0


seir <- lsoda(
  y     = condicoes_iniciais_seir,
  times = horizonte,
  func  = SEIR,
  parms = Opt_par_seir)
#parms = init_parameters_seir)

seir1 = as.data.frame(seir)
seir1$data <- seq(as.Date("2020/03/23"), 
                  by = "day", 
                  length.out = length(horizonte))
seir1[which.max(seir1$I),]$data



Opt_par_seir2 <- c(((Opt_par_seir["beta"])*f), 
                   Opt_par_seir["gamma"], 
                   Opt_par_seir["sigma"])

R0 <- Opt_par_seir["beta"]*f / Opt_par_seir["gamma"]; R0


seir2 <- lsoda(
  y     = condicoes_iniciais_seir,
  times = horizonte,
  func  = SEIR,
  parms = Opt_par_seir2)
#parms = init_parameters_seir)

seir2 = as.data.frame(seir2)
seir2$data <- seq(as.Date("2020/03/23"), 
                  by = "day", 
                  length.out = length(horizonte))

seir2[which.max(seir2$I),]$data



# plotando o gr??fico das curvas
curvas <- ggplot(data = seir2, aes(x = data, y = I)) + geom_line(colour="blue", size=0.50) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
curvas + geom_line(data = seir1, aes(x = data, y = I), colour="orange",size=0.50) +
  geom_vline(xintercept = seir2[which.max(seir2$I),]$data, size=0.2, colour="black", linetype = 2) +   
  geom_vline(xintercept = seir1[which.max(seir1$I),]$data, size=0.2, colour="black", linetype = 2) +
  labs(x = "", y = "Total de contaminados") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey25")) +
  annotate("text", x = modelo1[which.max(modelo1$I),]$data, size=2.3, y = 7000, angle = 360, label = "Entre a 4a semana de Junho/\n 1a semana de Julho")+
  annotate("text", size=2.3, x = modelo2[which.max(modelo2$I),]$data, y = 4500, angle = 360, label = "Entre a 2a semana de Setembro/\n3a semana de Setembro")




Resultado1 <- rbind("SIR 1" = modelo1[which.max(modelo1$I),],
                    "SIR 2" = modelo2[which.max(modelo2$I),])

Resultado2 <- rbind("SEIR 1" = seir1[which.max(seir1$I),],
                    "SEIR 2" = seir2[which.max(seir2$I),])
