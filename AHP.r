# Código en R para calcular sensibilidad en el método analítico jerárquico AHP 
library(AHPtools)

# Nombres
crit_names <- c("C1_Coste","C2_Adec","C3_Easy","C4_Flex","C5_Soporte","C6_Integr")
alt_names  <- c("A1_Labway","A2_Zendo","A3_Oralims","A4_Labware","A5_Starlims","A6_Samplem")


#  MATRIZ DE COMPARACIÓN DE CRITERIOS
PCM_crit <- matrix(c(
  1,   1/3, 1,   1,   3,   2,
  3,   1,   2,   3,   8,   5,
  1,   1/2, 1,   1,   4,   2,
  1,   1/3, 1,   1,   3,   2,
  1/3, 1/8, 1/4, 1/3, 1,   1/2,
  1/2, 1/5, 1/2, 1/2, 2,   1
), nrow = 6, byrow = TRUE,
dimnames = list(crit_names, crit_names))

CR_crit <- CR(PCM_crit)
CR_crit        # para ver CR de la matriz de criterios

w_crit_raw <- CR_crit[[3]]
w_crit <- w_crit_raw / sum(w_crit_raw)   # normalizamos a suma 1
names(w_crit) <- crit_names
w_crit        



# COSTE 
w_cost <- c(0.299, 0.268, 0.222, 0.078, 0.070, 0.062)
names(w_cost) <- alt_names
w_cost <- w_cost / sum(w_cost)    # normalizamos 


#  ADECUACIÓN
PCM_adec <- matrix(c(
  1,   2,   1/2, 1/2, 1,   3,
  1/2, 1,   1/2, 1/3, 1/2, 2,
  2,   2,   1,   1,   2,   4,
  2,   3,   1,   1,   2,   4,
  1,   2,   1/2, 1/2, 1,   3,
  1/3, 1/2, 1/4, 1/4, 1/3, 1
), nrow = 6, byrow = TRUE,
dimnames = list(alt_names, alt_names))

CR_adec <- CR(PCM_adec)
CR_adec
w_adec <- CR_adec[[3]]; w_adec <- w_adec / sum(w_adec)
names(w_adec) <- alt_names


# EASY (facilidad de uso)
PCM_easy <- matrix(c(
  1,   1/2, 1/3, 1/4, 1,   2,
  2,   1,   1/3, 1/3, 2,   3,
  3,   3,   1,   1/5, 3,   4,
  4,   3,   5,   1,   4,   5,
  1,   1/2, 1/3, 1/4, 1,   2,
  1/2, 1/3, 1/4, 1/5, 1/2, 1
), nrow = 6, byrow = TRUE,
dimnames = list(alt_names, alt_names))

CR_easy <- CR(PCM_easy)
CR_easy
w_easy <- CR_easy[[3]]; w_easy <- w_easy / sum(w_easy)
names(w_easy) <- alt_names


# FLEXIBILIDAD
PCM_flex <- matrix(c(
  1,   3,   2,   1/2, 1/3, 2,
  1/3, 1,   1/2, 1/4, 1/5, 1/3,
  1/2, 2,   1,   1/7, 1/4, 1/2,
  2,   4,   7,   1,   1/2, 2,
  3,   5,   4,   2,   1,   3,
  1/2, 3,   2,   1/2, 1/3, 1
), nrow = 6, byrow = TRUE,
dimnames = list(alt_names, alt_names))

CR_flex <- CR(PCM_flex)
CR_flex
w_flex <- CR_flex[[3]]; w_flex <- w_flex / sum(w_flex)
names(w_flex) <- alt_names


#  SOPORTE 
PCM_supp <- matrix(c(
  1,   3,   1,   1/3, 1,   2,
  1/3, 1,   1/2, 1,   1/2, 1/2,
  1,   2,   1,   2,   1,   1,
  3,   1,   1/2, 1,   1/2, 1,
  1,   2,   1,   2,   1,   1,
  1/2, 2,   1,   1,   1,   1
), nrow = 6, byrow = TRUE,
dimnames = list(alt_names, alt_names))

CR_supp <- CR(PCM_supp)
CR_supp
w_supp <- CR_supp[[3]]; w_supp <- w_supp / sum(w_supp)
names(w_supp) <- alt_names


#  INTEGRACIÓN
PCM_int <- matrix(c(
  1,   5,   2,   1/3, 1/3, 3,
  1/5, 1,   1/5, 1/5, 1/5, 1/5,
  1/2, 5,   1,   1/4, 1/4, 2,
  3,   5,   4,   1,   2,   5,
  3,   5,   4,   1/2, 1,   4,
  1/3, 5,   1/2, 1/5, 1/4, 1
), nrow = 6, byrow = TRUE,
dimnames = list(alt_names, alt_names))

CR_int <- CR(PCM_int)
CR_int
w_int <- CR_int[[3]]; w_int <- w_int / sum(w_int)
names(w_int) <- alt_names


#  MATRIZ GLOBAL (ALTERNATIVAS × CRITERIOS)
W_alts <- cbind(
  C1_Coste  = w_cost,
  C2_Adec   = w_adec,
  C3_Easy   = w_easy,
  C4_Flex   = w_flex,
  C5_Soporte= w_supp,
  C6_Integr = w_int
)
rownames(W_alts) <- alt_names
colnames(W_alts) <- crit_names

W_alts      # pesos locales de cada LIMS por cada criterio
w_crit      # pesos de criteros


#  PESOS FINALES Y RANKING 
scores <- W_alts %*% w_crit      # producto matricial A · Wc
scores <- as.numeric(scores)
names(scores) <- alt_names

scores
sort(scores, decreasing = TRUE)  # ranking final



# CALCULO DE INCONSISTENCIA
check_consistency <- function(M, name = "Matriz") {
  n <- nrow(M)
  
  # Autovalores de la matriz
  eig <- eigen(M)
  # Valor propio dominante 
  lambda_max <- Re(eig$values[1])
  
  # Índice de consistencia de Saaty
  CI <- (lambda_max - n) / (n - 1)
  
  # Valores RI clásicos (Saaty) para n = 1..10
  RI_table <- c(
    "1"  = 0.00,
    "2"  = 0.00,
    "3"  = 0.58,
    "4"  = 0.90,
    "5"  = 1.12,
    "6"  = 1.24,
    "7"  = 1.32,
    "8"  = 1.41,
    "9"  = 1.45,
    "10" = 1.49
  )
  
  RI <- RI_table[as.character(n)]
  CR <- ifelse(RI > 0, CI / as.numeric(RI), NA)
  
  cat("\n============================\n")
  cat(" Consistencia -", name, "\n")
  cat("============================\n")
  cat("n           =", n, "\n")
  cat("lambda_max  =", lambda_max, "\n")
  cat("CI          =", CI, "\n")
  cat("CR          =", CR, "\n\n")
  
  invisible(list(lambda_max = lambda_max, CI = CI, CR = CR))
}

check_consistency(PCM_crit,  "Criterios")
check_consistency(PCM_adec,  "Adecuación")
check_consistency(PCM_easy,  "Easy")
check_consistency(PCM_flex,  "Flexibilidad")
check_consistency(PCM_supp,  "Soporte")
check_consistency(PCM_int,   "Integración")


# Función de sensibilidad
ahp_sensitivity <- function(W_alts, w_crit, crit_name,
                            grid = seq(0, 1, by = 0.05)) {
  # W_alts: matriz (alternativas × criterios) con pesos locales
  # w_crit: vector con pesos de criterios 
  # crit_name: nombre del criterio a variar
  # grid: valores que tomará el peso de ese criterio (entre 0 y 1)
  
  if (!(crit_name %in% names(w_crit))) {
    stop("El criterio elegido no existe en w_crit")
  }
  
  base <- w_crit
  others <- setdiff(names(base), crit_name)
  
  res_list <- list()
  
  for (alpha in grid) {
    # nuevo vector de pesos de criterios
    w_new <- base
    w_new[crit_name] <- alpha
    
    if (length(others) > 0) {
      prop <- base[others] / sum(base[others])
      w_new[others] <- (1 - alpha) * prop
    }
    
    # normalizar
    w_new <- w_new / sum(w_new)
    
    # calcular puntuaciones globales
    scores <- as.numeric(W_alts %*% w_new)
    names(scores) <- rownames(W_alts)
    
    ranks <- rank(-scores, ties.method = "min")  
    
    df <- data.frame(
      criterio      = crit_name,
      peso_criterio = alpha,
      alternativa   = rownames(W_alts),
      score         = scores,
      rank          = ranks
    )
    
    res_list[[length(res_list) + 1]] <- df
  }
  
  out <- do.call(rbind, res_list)
  rownames(out) <- NULL
  out
}



