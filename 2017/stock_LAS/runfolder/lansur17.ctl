# CONTROLES DEL MODELO LANGOSTINO_SUR

# Coeficiente de variación de los desvios Rt, No y prop_machos en el Reclutamiento
#_________________________________________________________________________________
#  cv_Rt     cv_No     cv_Rm
   0.6        0.6      0.2

#  dt desove   dt CPUE	 dt_crucero	
   0.67         0.5        0.5

# PARAMETROS DE CRECIMIENTO Y MORTALIDAD NATURAL Y CV (prior)
#_________________________________________________________________________________

#Loo,   k,        Lt(a=1)    cv(edad)     M
52.8    0.151     19.27      0.1         0.3    #machos
45.6    0.174     18.79      0.1          0.3    #hembras 
0.1     0.1        0.1       0.05         0.1     #cv prior si son estimados


# Stepness h
1

# q_crucero y cv
1.0  1000.05


# Selectividad flota (valores de partida)
# proxy A50 (tallas), s1 y s2  machos
25     1.5    35

# proxy A50 (tallas), s1 y s2  hembras
25     1.5    35

# Selectividad cruceros (valores de partida en tallas)
# proxy L50 (talla), s1 y s2  machos
24     1.5    30

# proxy L50 (talla), s1 y s2  hembras
24     1.5    30


# Penalizador modelo logistico/domo (ej. 0.1= domo, 100=logistico)
100

# Número de bloques de selectividad flota (logistica) y años de inicio
#3
#1978 1993 2001
1
1979 


# Número de bloques de selectividad crucero (logistica) y años de inicio
1
1979  

# Número de bloques de capturabilidad CPUE y años de inicio
1
1979

# Número de bloques de capturabilidad crucero y años de inicio
3
1979 
2004
2014



#_________________________________________________________________
# FASES DE ESTIMACION 

# Fase de estimacion de capturabilidad flota & crucero
4  5 
# Fase estimación de la selectividad flota & crucero (nota: si la selec. crucero no se estima, se asume = 1)
4  5 
# Fase de estimacion de Lo & cv & M
-4  5  -4
# Fase de estimacion mortalidad por pesca
2 
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
#40%    45%    100%     76%(tasas de BDPR)
0.30	0.40	0.45    0.48
# Años a simular en el futuro
15
# Proyección de capturas ante distintos niveles de Reclutamiento (1.0 proporcional al reclutamiento medio)
1.5
# Opción para simular o estimar (0=simula, 1=estima)
#____________________________________________________
1
