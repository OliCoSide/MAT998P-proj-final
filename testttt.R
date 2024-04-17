brownianMotion <-
  function(n=10,xlim=c(-20,20),ylim=c(-20,20), steps=50)
  {
    x=rnorm(n) # random starting position
    y=rnorm(n)
    for (i in 1:steps) {
      plot(x,y,xlim = xlim,ylim = ylim)
      text(x,y)
      
      # iterate over particles
      for(k in 1:n){
        walk=rnorm(2); # random move of particle
        
        x[k]=x[k]+walk[1] # new position
        y[k]=y[k]+walk[2]
        
        # simple model (at most two rebounds) that prevents a particle
        # from moving past the limits
        if(x[k]<xlim[1]) x[k]=2*xlim[1]-x[k]
        if(x[k]>xlim[2]) x[k]=2*xlim[2]-x[k]
        if(y[k]<ylim[1]) y[k]=2*ylim[1]-y[k]
        if(y[k]>ylim[2]) y[k]=2*ylim[2]-y[k]
      }
    }
  }

pdf("frames.pdf")                # output device and file name
par(xaxs="i", yaxs="i", pty="s") # square plot region
par(mai=c(0.9,0.9,0.2,0.2))      # plot margins

brownianMotion(n=20, steps=400)  # 20 particles, 400 time steps