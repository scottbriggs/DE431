HHMMSS <- function (HHdd)
{
    HHdd <- decimal_hours
    intpart1 <- trunc(HHdd)
    fracpart1 <- HHdd - intpart1
    hh = intpart1
    temp = fracpart1 * 60
    mm <- trunc(temp)
    ss = trunc((temp - mm) * 60)
    
    c(hh, mm, ss)
}

HHdd <- function (hr, min, sec)
{
    decimal_hours <- hr + min  /60 + sec / 3600
}
