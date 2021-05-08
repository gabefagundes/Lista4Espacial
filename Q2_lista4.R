library(tidyverse)
library(leaflet)
library(sf)
library(spatstat)
library(splancs) # K-function
library(sp)
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models

library(rgdal)
library(raster)
library(spatstat)


library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)

library(httr)


# Questão 1 ---------------------------------------------------------------




# Questão 2 ---------------------------------------------------------------



# dados -------------------------------------------------------------------

neighborhoods_geojson <- "https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson"
neighborhoods_raw <- sf::read_sf(neighborhoods_geojson)
head(neighborhoods_raw)

base_url <- "https://phl.carto.com/api/v2/sql"
q <- "
  select *
  from shootings
  where year > 2018
"
shootings_geoJSON <- 
    httr::modify_url(
        url = base_url,
        query = list(q = q, format = "GeoJSON")
    )
shootings_raw <- sf::read_sf(shootings_geoJSON)


# CLEAN DATA   #### #### #### #### #### ####
neighborhoods <- neighborhoods_raw %>% 
    dplyr::select(label = mapname) 

shootings <- shootings_raw %>% 
    dplyr::filter(point_x > -80 & point_y > 25) # points in FL


# a 

ggplot()+
    geom_sf(data = neighborhoods)+
    geom_sf(data = shootings, col = "#eba123", alpha = 0.5)+
    theme_minimal()+
    ggtitle("Vítimas de Arma de Fogo")

# b

fatal = shootings %>%
    filter(fatal == 1)

ggplot()+
    geom_sf(data = neighborhoods)+
    geom_sf(data = fatal, col = "#ff003e", alpha = 0.5)+
    theme_minimal()+
    ggtitle("Vítimas Fatais")


y21 = shootings %>%
    filter(year == 2021)





# c

philadelphia <- read_csv("philadelphia.csv", 
                         col_types = cols(X1 = col_skip())) %>%
    as.points()

fcn.Ghatenv <- function(pts, poly, nsim, s, pmin=0.025,
                        pmax=0.975, quiet = F)
{
    #pts: eventos originais, padrao observado
    #poly: poligono onde eventos devem ser gerados
    #nsim: numero desejado de simulacoes
    #s: vetor com o grid do eixo de distancia
    # probabilidades para calcular enevelopes de confianca
    # retorna lista com 3 vetores, s, o envelope inferior e o superior
    n <- npts(pts) # numero de eventos
    len <- length(s)
    gup <- rep(0, length = len) #vetor para receber envelope sup
    glow <- rep(1e+34, length = len) #para receber envelope inf
    gmat <- matrix(0, nrow=len, ncol=(nsim + 1)) #para Ghat simulada
    gmat[,1] <- Ghat(pts, s)
    for(isim in (1:nsim)) {
        if(!quiet) cat("Doing simulation ", isim, "\n")
        gmat[,isim+1] <- Ghat(csr(poly, n), s)
        gup <- apply(gmat, 1, quantile, probs = pmax)
        glow <- apply(gmat, 1, quantile, probs = pmin)
    }
    list(s=s, lower = glow, upper = gup)
}

ggplot()+
    geom_sf(data = neighborhoods)+
    geom_sf(data = y21, col = "#00aa22", alpha = 0.5)+
    theme_minimal()+
    ggtitle("Vítimas em 2021")


y21_spdf = as_Spatial(y21)
phil_spdf = as_Spatial(neighborhoods)

d = seq(0, 0.015, length = 50)

gh = Ghat(y21_spdf@coords, s = d)


    
envred = fcn.Ghatenv(y21_spdf@coords, philadelphia, s = d, nsim = 200)


ggplot()+
    geom_line(aes(d, gh))+
    geom_line(aes(d, envred$lower), col = 'red', lty = 2)+
    geom_line(aes(d, envred$upper), col = 'red', lty = 2)+
    theme_minimal()

# Não podemos considerar com um PPH 


# d 

y21_f = y21 %>%
    filter(fatal == 1)

y21_f = as_Spatial(y21_f)


y21_nf = y21 %>%
    filter(fatal == 0)

y21_nf = as_Spatial(y21_nf)


k1 <- khat(y21_f@coords, philadelphia, d)
k2 <- khat(y21_nf@coords, philadelphia, d) 


Dr <- k1 - k2

env2 = Kenv.label(y21_f@coords, 
                  y21_nf@coords, 
                  philadelphia, 200, 
                  d, quiet = T)


ggplot()+
    geom_line(aes(d, Dr))+
    geom_hline(yintercept = 0)+
    geom_line(aes(d, env2$lower), col = 'red', lty = 2)+
    geom_line(aes(d, env2$upper), col = 'red', lty = 2)+
    geom_ribbon(aes(x = d, ymin = env2$lower, ymax = env2$upper), 
                alpha = 0.2, fill = 'red')
    theme_minimal()

#e     

y21_W = y21%>%
        filter(race == "W") %>%
        as_Spatial


y21_NW = y21 %>%
         filter(race != "W") %>%
         as_Spatial

ggplot()+
    geom_sf(data = neighborhoods)+
    geom_sf(aes(fill = race),data = y21, 
            alpha = 0.8, shape = 21,
            col = 'black')+
    theme_minimal()+
    ggtitle("Cor das Vítimas em 2021")+
    facet_wrap(~race, nrow = 3)


