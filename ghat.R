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
        19
        gmat[,isim+1] <- Ghat(csr(poly, n), s)
        gup <- apply(gmat, 1, quantile, probs = pmax)
        glow <- apply(gmat, 1, quantile, probs = pmin)
    }
    list(s=s, lower = glow, upper = gup)
}
