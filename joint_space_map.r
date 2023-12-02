# Ying Bao's notes (April 2023):
# I based the code on an example posted on http://marketing-yogi.blogspot.com/2012/12/session-4-rcode-perceptual-maps.html
# I've modified the code to allow for additional inputs, changed some of the notation, and added a routine to compute
# predicted choices by projecting preferences onto the estimated coordinates


# Inputs:
# ratings: matrix of average ratings of brand attributes.  Rows are brands, and columns are attributes.  
# prefs: matrix of preference ratings across brands.  Again, rows should be brands, columns are individuals.
# cex: control text size on brand
# dim1: which dimension goes on the x-axis
# dim2: which dimension goes on the y-axis
# coord.scale: scaling factor for coordinates
# pref.scale: scaling factor for preference vector


Joint_Space_Map <- function(ratings, prefs, cex = 0.75, dim1 = 1, dim2 = 2,
                            coord.scale = 5, pref.scale = 2, firstchoice = TRUE) {
  
  fit = prcomp(ratings, scale.=TRUE)  # run principal components analysis on the average ratings
  
  vars = fit$sdev^2  # sdev is the square root of the eigenvalues, so square them to recover variances
  f.var = vars/sum(vars)
  
  inds = c(dim1,dim2)
  
  pr.data = data.frame(fit$rotation[,inds])
  colnames(pr.data) = c(paste("Dimension",dim1),paste("Dimension",dim2))
  
  attribnames = colnames(ratings)
  brdnames = rownames(ratings)
  
  # The transformation below will rescale the coordinates so that they will be bounded by -1 and 1
  
  fit1=fit
  fit1$x[,dim1]=fit$x[,dim1]/apply(abs(fit$x),2,sum)[dim1]
  fit1$x[,dim2]=fit$x[,dim2]/apply(abs(fit$x),2,sum)[dim2]
  
  # This code will construct the preference vectors and add them to the map
  pref = data.matrix(prefs) # make data compatible with the output of the pca function
  pref1 = pref %*% fit1$x[,inds]
  
  par(pty="s") # set square plotting region
  
  colnames(fit$rotation) = paste("Dimension ",1:length(brdnames),sep="")
  
  plot(fit$rotation[,inds],
       type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))
  
  abline(h=0); abline(v=0) # add axes to the plot
  
  for (i1 in 1:nrow(pref1)){
    segments(0,0, x1=pref1[i1,1]/pref.scale,y1=pref1[i1,2]/pref.scale, col="maroon2", lwd=1.25)
  }
  
  # this code adds attribute vectors as blue arrows
  for (i1 in 1:nrow(fit$rotation)){
    arrows(0,0, x1=fit$rotation[i1,dim1]*fit$sdev[dim1], y1=fit$rotation[i1,dim2]*fit$sdev[dim2], length = 0.1, col="blue", lwd=1.5);
    text(x=fit$rotation[i1,dim1]*fit$sdev[dim1]*1.2,y=fit$rotation[i1,dim2]*fit$sdev[dim2]*1.2, labels=attribnames[i1],col="blue", cex=cex)
  }
  
  points(x=fit1$x[,dim1]*coord.scale, y=fit1$x[,dim2]*coord.scale, pch=19, col="red")
  text(x=fit1$x[,dim1]*coord.scale, y=fit1$x[,dim2]*coord.scale, labels=brdnames,col="black", cex= cex, font=2, pos=4)
  
  # The code below computes predicted market shares using regression
  
  nd.share = 2  # I'm setting this to 2, since the interactive map can only be manipulated in 2 dimensions
  nresp = nrow(prefs)
  nprod = length(brdnames)
  p.coefs = matrix(0,nrow=nresp,ncol=nd.share+1)
  pbrand.share = rep(0,nprod)
  p.util = matrix(0,nrow=nresp,ncol=nprod)
  for(i in 1:nresp) {
    coords = fit1$x[,1:nd.share]*coord.scale
    res = lm(prefs[i,]~coords)
    p.coefs[i,] = res$coefficients
    p.util[i,] = res$fitted.values
    if(firstchoice) {
      pbrand.index = which.max(p.util[i,])
      pbrand.share[pbrand.index] = pbrand.share[pbrand.index] + 1
    } else {
      for(j in 1:nprod) {
        tutil = sum(exp(p.util[i,]))
        pbrand.share = pbrand.share + exp(p.util[i,])/tutil
      }
    }
    
  }
  pbrand.share = pbrand.share/nresp
  
  return(list(shares = pbrand.share, p.coefs = p.coefs, p.util = p.util, coords = fit1$x*coord.scale,
              f.var = f.var))

} 