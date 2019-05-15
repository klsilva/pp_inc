#Instala e carrega os pacotes necessários
pacotes <- c("splancs", "spatstat", "maptools", "knitr")

lapply(pacotes, install.packages)

lapply(pacotes, library, character.only = TRUE)

#Lendo o shapefile
BRA = readShapeSpatial("LimiteBrasil")

# dependendo do banco de dados é necessário utilizar:
y <- as(BRA, "SpatialPolygons")
y = as.owin(y)

#Lendo os locais de ocorrência dos eventos
focos = read.table("focos_brasil.txt", header = TRUE)
focos = as.matrix(focos)

#Preparando o mapa para usar no spatstat
BRA.spatstat = as.owin(BRA)
plot(BRA.spatstat, main = "")
dados.spatstat = as.ppp(focos, BRA.spatstat)
plot(dados.spatstat, cex = 1)


#Preparando o mapa para usar no splancs
BRA.splancs = as.matrix(cbind(BRA.spatstat$bdry[[54]]$x, BRA.spatstat$bdry[[54]]$y))

polymap(BRA.splancs)
pointmap(focos, add = TRUE)

#Quadrats

x11()
polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 15, 12)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(xlab = "Metros (m)", ylab = "Metros (m)")


#Gera os gráficos comparativos

x11()
par(mfrow = c(1, 2))

polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 10, 12)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(main = "Contagem de Eventos por Quadrats (10,12)", xlab = "Metros (m)", ylab =
        "Metros (m)")

polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 15, 12)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(main = "Contagem de Eventos por Quadrats (15,12)", xlab = "Metros (m)", ylab =
        "Metros (m)")

x11()
par(mfrow = c(1, 2))

polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 15, 12)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(main = "Contagem de Eventos por Quadrats (15,12)", xlab = "Metros (m)", ylab =
        "Metros (m)")

polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 20, 12)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(main = "Contagem de Eventos por Quadrats (20,12)", xlab = "Metros (m)", ylab =
        "Metros (m)")


x11()
par(mfrow = c(1, 2))

polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 15, 12)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(main = "Contagem de Eventos por Quadrats (15,12)", xlab = "Metros (m)", ylab =
        "Metros (m)")

polymap(BRA.splancs)
pointmap(focos, add = TRUE)
qx <- quadratcount(dados.spatstat, 24, 24)
plot(
  qx,
  add = TRUE,
  col = "red",
  cex = 1.5,
  lty = 2
)
title(main = "Contagem de Eventos por Quadrats (24,24)", xlab = "Metros (m)", ylab =
        "Metros (m)")


#Verificando a média dos quadrats

mean(qx)

#Construção da Tabela de Médias dos Quadrats

qx0 <- quadratcount(dados.spatstat, 15, 12)
qx1 <- quadratcount(dados.spatstat, 10, 12)
qx2 <- quadratcount(dados.spatstat, 20, 12)
qx3 <- quadratcount(dados.spatstat, 24, 24)

mqx0 <- mean(qx0)
mqx1 <- mean(qx1)
mqx2 <- mean(qx2)
mqx3 <- mean(qx3)
mediaqx <- (c(mqx0, mqx1, mqx2, mqx3))
parqx0 <- "15 x 12"
parqx1 <- "10 x 12"
parqx2 <- "20 x 12"
parqx3 <- "24 x 24"
parqx <- c(parqx0, parqx1, parqx2, parqx3)
tabela1 <- data.frame(mediaqx, parqx)
write.csv(tabela1, file = "Tabela 1.csv")

#O aumento do quadrats a partir deste ponto não apresenta melhora significativa na homogeinização dos quadrats e atrapalham a convergência assintótica para a distribuição Qui-Quadrado devido ao aumento de números zero.

# Cálculo do ICS

nquadrats <- length(as.vector(qx))
nquadrats
cont <- as.vector(qx)
s2 <- var(cont)
xbarra <- mean(cont)

ICS <- s2 / xbarra - 1
ICS

#Dados para a tabela de ICS com os outros quadrats

nquadrats0 <- length(as.vector(qx0))
nquadrats0
cont0 <- as.vector(qx0)
s20 <- var(cont0)
xbarra0 <- mean(cont0)

ICS0 <- s20 / xbarra0 - 1
ICS0

nquadrats1 <- length(as.vector(qx1))
nquadrats1
cont1 <- as.vector(qx1)
s21 <- var(cont1)
xbarra1 <- mean(cont1)

ICS1 <- s21 / xbarra1 - 1
ICS1

nquadrats2 <- length(as.vector(qx0))
nquadrats2
cont2 <- as.vector(qx2)
s22 <- var(cont2)
xbarra2 <- mean(cont2)

ICS2 <- s22 / xbarra2 - 1
ICS2

nquadrats3 <- length(as.vector(qx3))
nquadrats3
cont3 <- as.vector(qx3)
s23 <- var(cont3)
xbarra3 <- mean(cont3)

ICS3 <- s23 / xbarra3 - 1
ICS3

ICSC <- c(ICS0, ICS1, ICS2, ICS3)
write.csv(ICSC, file = "Tabela ICS.csv")

#O ICS indica agregação, testa-se agora se esta agregação é estatisticamente significativa

testequadrat0 <-
  quadrat.test(dados.spatstat, method = "MonteCarlo", alternative = "clustered")
testequadrat1 <-
  quadrat.test(dados.spatstat, method = "MonteCarlo", alternative = "two.sided")

# A partir da confirmação a partir do teste de que há agregração, decide-se estimar a intensidade por Kernel (apropriado para isto),

x11()
par(mfrow = c(2, 3))

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    250000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 250000))
title(xlab = "Metros (m)", ylab = "Metros (m)")

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    350000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 350000))
title(xlab = "Metros (m)", ylab = "Metros (m)")

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    450000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 450000))
title(xlab = "Metros (m)", ylab = "Metros (m)")

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    550000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 550000))
title(xlab = "Metros (m)", ylab = "Metros (m)")

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    650000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 750000))
title(xlab = "Metros (m)", ylab = "Metros (m)")

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    750000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 950000))
title(xlab = "Metros (m)", ylab = "Metros (m)")

x11()

lambda <-
  kernel2d(
    focos,
    BRA.splancs,
    470000,
    nx = 500,
    ny = 500,
    kernel = "quartic"
  )
image(lambda, col = terrain.colors(256), main = expression(tau == 470000))
pointmap(focos, add = TRUE, cex = 0.5)
title(xlab = "Metros (m)", ylab = "Metros (m)")

# Função K

#h definido como metade de um milhão

h <- seq(0, 500000, 100)
funcaoK <- khat(focos, BRA.splancs, h)
funcaoL <- sqrt(funcaoK / pi) - h

x11()
par(mfrow = c(1, 2))
plot(h,
     funcaoK,
     type = "l",
     xlab = "Distâncias em metros (m)",
     ylab = expression(hat(K)(h)))
lines(h, pi * (h ^ 2), lty = 2)
leg <- c("Dados observados", "Processo aleatório")
legend(0, 2500000000000, leg, lty = c(1, 2))
title(main = "Função K")

plot(h,
     funcaoL,
     type = "l",
     xlab = "Distâncias em metros (m)",
     ylab = expression(hat(L)(h)))
lines(h, rep(0, length(h)), lty = 2)
leg1 <- c("Dados observados", "Processo aleatório")
legend(0, 390000, leg1, lty = c(1, 2))
title(main = "Função L")

# Envelopes

envintervalo99 <- Kenv.csr(296, BRA.splancs, 199, h, quiet = F)
plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo99$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo99$lower / pi) - h,
      lty = 2,
      col = "blue")
legenv <- c("Dados Observados", "Limite Superior", "Limite Inferior")
legend(0,
       406825.1,
       legenv,
       lty = c(1, 2, 2),
       col = c("black", "red", "blue"))
title(main = "Envelope de Confiança - 99%")

envintervalo95 <- Kenv.csr(296, BRA.splancs, 39, h, quiet = F)
plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo95$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo95$lower / pi) - h,
      lty = 2,
      col = "blue")
legenv <- c("Dados Observados", "Limite Superior", "Limite Inferior")
legend(0,
       406825.1,
       legenv,
       lty = c(1, 2, 2),
       col = c("black", "red", "blue"))
title(main = "Envelope de Confiança - 95%")

envintervalo90 <- Kenv.csr(296, BRA.splancs, 19, h, quiet = F)
plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo90$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo90$lower / pi) - h,
      lty = 2,
      col = "blue")
legenv <- c("Dados Observados", "Limite Superior", "Limite Inferior")
legend(0,
       406825.1,
       legenv,
       lty = c(1, 2, 2),
       col = c("black", "red", "blue"))
title(main = "Envelope de Confiança - 90%")

envintervalo99.8 <- Kenv.csr(296, BRA.splancs, 999, h, quiet = F)
plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo99.8$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo99.8$lower / pi) - h,
      lty = 2,
      col = "blue")
legenv <- c("Dados Observados", "Limite Superior", "Limite Inferior")
legend(0,
       406825.1,
       legenv,
       lty = c(1, 2, 2),
       col = c("black", "red", "blue"))
title(main = "Envelope de Confiança - 99,8%")

#Plotar envelopes

x11()
par(mfrow = (c(2, 2)))


plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo90$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo90$lower / pi) - h,
      lty = 2,
      col = "blue")
title(main = "Envelope de Confiança - 90%")


plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo95$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo95$lower / pi) - h,
      lty = 2,
      col = "blue")
title(main = "Envelope de Confiança - 95%")


plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo99$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo99$lower / pi) - h,
      lty = 2,
      col = "blue")
title(main = "Envelope de Confiança - 99%")


plot(
  h,
  funcaoL,
  type = "l",
  xlab = "Distâncias",
  ylab = expression(hat(L)(h)),
  ylim = c(-100000, 400000)
)
lines(h, rep(0, length(h)), lty = 2)
lines(h,
      sqrt(envintervalo99.8$upper / pi) - h,
      lty = 2,
      col = 2)
lines(h,
      sqrt(envintervalo99.8$lower / pi) - h,
      lty = 2,
      col = "blue")
title(main = "Envelope de Confiança - 99,8%")
