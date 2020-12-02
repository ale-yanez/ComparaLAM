# CONTROLES DEL MODELO langsotino amarillo norte
# Coeficiente de variación de los desvios Rt, No y prop_machos en el Reclutamiento
#  cv_Rt     cv_No     cv_Rm
   0.6        0.6      0.2

#  dt desove   dt CPUE	 dt_crucero	
   0.67         0.5        0.5 # Debe coindicir con el periodo de evaluación de área barrida del crucero de evaluación

# PARAMETROS DE CRECIMIENTO Y MORTALIDAD NATURAL Y CV (prior)
#_________________________________________________________________________________

#Loo,   k,        Lt(a=1)    cv(edad)     M
52.8    0.151      19       0.1         0.3    #machos
45.6    0.174      19       0.1         0.3    #hembras 
0.1     0.1        0.1      0.1         0.1     #cv prior si son estimados


# Stepness h
1

# q_crucero y cv
1.0  1000.05

# Selectividad flota (valores de partida)
# proxy A50 (tallas), rango,   machos
5     2

# proxy A50 (tallas), rango,   hembras
5     2

# Selectividad cruceros (valores de partida en tallas)
# proxy L50 (talla), rango,   machos
5     2

# proxy L50 (talla), rango,   hembras
5     2

# Número de bloques de selectividad flota (logistica) y años de inicio
#3
#1978 1993 2001
1
1985
  


# Número de bloques de selectividad crucero (logistica) y años de inicio
1 
1985  


# Número de bloques de capturabilidad CPUE y años de inicio
1
1985  


# Número de bloques de capturabilidad crucero y años de inicio
3
1985
2004
2014


#_________________________________________________________________
# FASES DE ESTIMACION 

# Fase de estimacion de capturabilidad flota & crucero
3      5 
# Fase estimación de la selectividad flota & crucero (nota: si la selec. crucero no se estima, se asume = 1)
6    6 
# Fase de estimacion de Lo & cv & M
4   5  -4
# Fase de estimacion mortalidad por pesca
3 
# Fase de estimacion dev_Rt y dev_No (dev_No<0 equilibrio)
2  2
# Fase de estimacion proporcion de machos en el Reclutamiento (pRm)
-6
# Fase de estimacion Fpbr (se sugiere sea la ultima)
6

# CTP
#____________________________________________________
#numero de PBRs a calcular
4
#30%    F45%    70%     71%(tasas de BDPR)
0.30	0.40	0.45    0.41
# Años a simular en el futuro
20
# Proyección de capturas ante distintos niveles de Reclutamiento (1.0 proporcional al reclutamiento medio)
1
# Opción para simular o estimar (0=simula, 1=estima)
#____________________________________________________
1
