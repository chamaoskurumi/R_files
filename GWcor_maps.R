install.packages("spgwr"); library("spgwr")

GWcorLOR4reg <- gw.cov(LOR4reg, vars=736:765, bw=2500, longlat=FALSE)
spplot(GWcorLOR4reg$SDF, "mean.Mietechgr")
spplot(GWcorLOR4reg$SDF, "diff.Mietechgr")
spplot(GWcorLOR4reg$SDF, "cor.Mietechgr.FortzuegeR.")
spplot(GWcorLOR4reg$SDF, "mean.FortzuegeR")
spplot(GWcorLOR4reg$SDF, "diff.FortzuegeR")
spplot(GWcorLOR4reg$SDF, "cor.Mietechgr.FortzuegeR.")
spplot(GWcorLOR4reg$SDF, "mean.ZuzuegeDAR")
spplot(GWcorLOR4reg$SDF, "diff.ZuzuegeDAR")

