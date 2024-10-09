
IntegrateVector <- function(vector,dt=1,timeVec = NULL, 
                            interpolate = F, method = "cubic",
                            t0=0){
  #------------- Example
  ## requires: pracma and stats packages
  # dt=1
  # timeVec = NULL
  # interpolate = F
  # method = "cubic"
  # t0=0
  # t = seq(0,3*pi/4,by = pi/100)
  # y = sin(t)
  # stats::integrate(sin,t[1],t[length(t)])
  # IntegrateVector(y,dt = pi/100)
  # IntegrateVector(y,dt = pi/100,interpolate = T,method = "linear")
  # IntegrateVector(y,dt = pi/100,interpolate = T,method = "cubic")
  #---------------test
  # fy = ppfit(t,y,t,method = "linear")
  # ti = seq(0,3*pi/4,by = pi/1000)
  # yi = ppval(fy,ti)
  # 
  # plot(t,y,type = "b", main = "Piecewise polynomial approximation")
  # lines(ti, yi, col="red",lwd = 2)
  # 
  # t = seq(0,pi,by = pi/100)
  # y = sin(t)
  # yi = ppval(fy,t)
  # plot(t,y,type = "b", main = "Piecewise polynomial approximation")
  # lines(t, yi, col="red",lwd = 2)
  
  if(is.null(timeVec)){
    timeVec = seq(0,length(vector)-1,1)*dt+t0
  }
  if(interpolate & method == "cubic" & length(vector)<3){
    # At least three points needed for cubic interpolation.
    interpolate = F
  }
  
  
  if(interpolate){
    fy = pracma::ppfit(x = timeVec, y = vector, xi = timeVec, method = method)
    S = stats::integrate(f = function(x) pracma::ppval(fy,x), lower = timeVec[1],upper = timeVec[length(timeVec)])
    S = S$value
  }else{
    if(length(vector)>1){
      S = (sum(vector)-vector[1]/2-vector[length(vector)-1]/2)*dt
    }else{
      S = vector*dt
    }
  }
  return(S)
}