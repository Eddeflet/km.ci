"confi.fun" <-
function(abw,kap.mei,method)
{
    # Using the already calculated derivation this function calculates
    # the upper and lower boundary of a confidence band by substracting
    # resp. adding the derivation "abw".
    
    if(method=="linear")
    {
        lower <- kap.mei-abw
        upper <- kap.mei+abw
    }
    if(method=="log")
    {
        lower <- kap.mei^(1/abw)
        upper <- kap.mei^abw
    }
    if(method=="arcsin")
    {
      temp <- asin(sqrt(kap.mei))-abw
      lower <- sin(temp*(temp >= 0))^2
      temp <- asin(sqrt(kap.mei))+abw
      upper <- sin(temp*(temp <= pi/2)+pi/2*(temp > pi/2))^2
    }
    return(list(lower=lower,upper=upper))
}

