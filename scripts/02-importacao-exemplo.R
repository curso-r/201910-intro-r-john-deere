library(tidyverse)



files <- list.files("dados/por-ano/", full.names = TRUE)

# Versão programática

for(arq in files) {
  
  if(arq == files[1]) {
    base <- read_rds(arq)
  } else {
    base <- rbind(base, read_rds(arq))
  }
  
}

# Versão "tidy" (avançado, mas fácil)

base2 <- purrr::map_dfr(files, read_rds)
