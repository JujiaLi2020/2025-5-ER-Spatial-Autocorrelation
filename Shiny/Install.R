
### Test Locally
setwd("C:/Users/julia/Box/Personal/Research/2025-5-ER-Spatial-Autocorrelation/Shiny")

shiny::runApp()




### Deploy
library(rsconnect)

rsconnect::setAccountInfo(name='v6fozf-joshua-lee',
                          token='A67B9E661A6E900BF8663140D033E6CF',
                          secret='6tQdHFUyeHJqErogassV6PnJnYbLMaWKrfNdmaR6')

rsconnect::deployApp("C:/Users/julia/Box/Personal/Research/2025-5-ER-Spatial-Autocorrelation/Shiny")

