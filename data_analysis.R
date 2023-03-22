###########################################
# Parte del Trabajo de Fin de Master titulado como: IMPACTO DEL HATCHING ASISTIDO EN TRANSFERENCIAS DE EMBRIONES CONGELADOS.
# Master universitario en Biotecnologia de la Reproduccion Humana Asistida. Universitat de Valencia, Facultad de Medicina y Odontologia.
# Realizado por Andres Chaves Lozano (2022-2023). andres.chaveslozano@gmail.com


# Lectura de dataframes

data_tableA <- read.table(file = "dataframeA.txt", header = FALSE,dec = ".", sep= ",")
data_tableB <- read.table(file = "dataframeB.txt", header = FALSE,dec = ".", sep= ",")



# Generar tabla de contingencia con los datos de los dataframes leidos.
# Ejemplo tabla de contingencia de la tasa de gestacion:
#
#                Gestacion       No gestacion
# Grupo 1            116              126
# 
#
# Grupo 2            153              151
# 
# H0: No existe asociacion estadistica entre las variables categoricas
# H1: Existe asociacion significativa

gestaciones <- c(as.numeric(data_tableA$V2[4]), as.numeric(data_tableB$V2[4]))
no_gestaciones <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[4])),
                     (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[4])))

cont_gestaciones <- data.frame(gestaciones, no_gestaciones)
colnames(cont_gestaciones) <- c("Gestaciones", "No gestaciones")
rownames(cont_gestaciones) <- c("Grupo A", "Grupo B")

# Visualizacion tabla de contingencia

cont_gestaciones

# Generar grafico de barras

barplot(as.matrix(cont_gestaciones), beside=TRUE, legend.text=TRUE,
        args.legend=list(x="topleft"), cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))

# Test estadistico X^2 de Pearson
chisq.gest = chisq.test(cont_gestaciones)



# Mismo proceso para el resto de parametros

# Total sacos. Tasa implantacion

implantacion <- c(as.numeric(data_tableA$V2[8]), as.numeric(data_tableB$V2[8]))
no_implantacion <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[8])),
                     (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[8])))


cont_implantacion <- data.frame(implantacion, no_implantacion)
colnames(cont_implantacion) <- c("Implantacion", "No implantacion")
rownames(cont_implantacion) <- c("Grupo A", "Grupo B")

cont_implantacion

barplot(as.matrix(cont_implantacion), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"), cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.impl = chisq.test(cont_implantacion)
chisq.impl


# Pruebas de embarazo positivas

prueba.pos <- c(as.numeric(data_tableA$V2[9]), as.numeric(data_tableB$V2[9]))
prueba.neg <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[9])),
                     (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[9])))


cont_prueba <- data.frame(prueba.pos, prueba.neg)
colnames(cont_prueba) <- c("Prueba de embarazo positiva", "Prueba de embarazo negativa")
rownames(cont_prueba) <- c("Grupo A", "Grupo B")

cont_prueba

barplot(as.matrix(cont_prueba), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"), cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.prueba = chisq.test(cont_prueba)
chisq.prueba


# Gestacion clinica

gestacionclinica <- c(as.numeric(data_tableA$V2[10]), as.numeric(data_tableB$V2[10]))
no_gestacionclinica <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[10])),
                     (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[10])))


cont_gestacionclinica <- data.frame(gestacionclinica, no_gestacionclinica)
colnames(cont_gestacionclinica) <- c("Gestaci�n cl�nica", "No gestaci�n cl�nica")
rownames(cont_gestacionclinica) <- c("Grupo A", "Grupo B")

cont_gestacionclinica

barplot(as.matrix(cont_gestacionclinica), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"),cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.gestclinic = chisq.test(cont_gestacionclinica)
chisq.gestclinic


# Gestacion bioquimica

gestacionbioq <- c(as.numeric(data_tableA$V2[11]), as.numeric(data_tableB$V2[11]))
no_gestacionbioq <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[11])),
                         (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[11])))


cont_gestacionbioq <- data.frame(gestacionbioq, no_gestacionbioq)
colnames(cont_gestacionbioq) <- c("Gestaci�n bioqu�mica", "No gestaci�n bioqu�mica")
rownames(cont_gestacionbioq) <- c("Grupo A", "Grupo B")

cont_gestacionbioq

barplot(as.matrix(cont_gestacionbioq), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"),cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.gestbioq = chisq.test(cont_gestacionbioq)
chisq.gestbioq


# Abortos clinicos

aborto <- c(as.numeric(data_tableA$V2[12]), as.numeric(data_tableB$V2[12]))
no_aborto <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[12])),
                         (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[12])))


cont_aborto <- data.frame(aborto, no_aborto)
colnames(cont_aborto) <- c("Aborto cl�nico", "No aborto cl�nico")
rownames(cont_aborto) <- c("Grupo A", "Grupo B")

cont_aborto

barplot(as.matrix(cont_aborto), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"),cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.aborto = chisq.test(cont_aborto)
chisq.aborto


# Gestaciones gemelares

gemelar <- c(as.numeric(data_tableA$V2[13]), as.numeric(data_tableB$V2[13]))
unica <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[13])),
               (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[13])))


cont_gemelar <- data.frame(gemelar, unica)
colnames(cont_gemelar) <- c("Gestaci�n gemelar", "Gestaci�n �nica")
rownames(cont_gemelar) <- c("Grupo A", "Grupo B")

cont_gemelar

barplot(as.matrix(cont_gemelar), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"),cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.gemelar = chisq.test(cont_gemelar)
chisq.gemelar


# Gestaciones ectopicas

ectopic <- c(as.numeric(data_tableA$V2[16]), as.numeric(data_tableB$V2[16]))
normal <- c((as.numeric(data_tableA$V2[3]) - as.numeric(data_tableA$V2[16])),
           (as.numeric(data_tableB$V2[3]) - as.numeric(data_tableB$V2[16])))


cont_ectopic <- data.frame(ectopic, normal)
colnames(cont_ectopic) <- c("Gestaci�n ect�pica", "Gestaci�n normal")
rownames(cont_ectopic) <- c("Grupo A", "Grupo B")

cont_ectopic

barplot(as.matrix(cont_ectopic), beside =TRUE, legend.text = TRUE,
        args.legend = list(x="topleft"), cex.names = 1.5, col=colorRampPalette(c("#F0FFFF", "#6CA6CD"))(2))


chisq.ectopic = chisq.test(cont_ectopic)
chisq.ectopic

