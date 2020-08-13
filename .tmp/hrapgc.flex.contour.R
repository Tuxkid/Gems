flex.contour <-
structure(function(df = cont.test.df[1:273, (1:3)], transpose = F, label.pos = (13:18)/20,
    levs.pc = c(50, 75, 90, 95, 99), lev.cex = 0.6, lev.font = 1, mark = F, 
    alone = F, ztransform = "logit")
{
# Flexibility in positioning the contour-level labels is the name of the game.
#
# df is dataframe such as produced by gee.
# Requires also is.not.na() in Gems & make.line.loess()
# Not generic yet. Uses only logit transformed % data
    xxs <- unique(df[, 1])
    yys <- unique(df[, 2])
    zzs <- df[, 3]
    zzs.mat <- matrix(zzs, nr = length(is.not.na(xxs)))
    x.use <- iff(transpose, yys, xxs)
    y.use <- iff(transpose, xxs, yys)
    if(transpose)
       zzs.mat <- t(zzs.mat)
    ztransform.fn <- get(ztransform)
    lev.pos <- ztransform.fn(levs.pc/100)    # (levs.pc are percentages)
    lev.txt <- paste(levs.pc)
    cont.fit <- contour(x.use, y.use, zzs.mat, save = T, levels = lev.pos)
    if(alone)
       plot(range(x.use), range(y.use), type = "n")
    if(length(label.pos < length(levs.pc)))
       label.pos <- rep(label.pos, length(levs.pc))
    for(i in 1:length(cont.fit)) {
       make.line.loess(cont.fit[[i]], lab = lev.txt[i], mark = mark, label.pos
           = label.pos[i], cex = lev.cex, lev.font = lev.font)
    }
}
, comment = "03/06/1998")
