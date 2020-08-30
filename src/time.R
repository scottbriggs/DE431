HHMMSS <- function (HHdd)
{
    # Converts a decimal hours to hours, minutes, and seconds
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
    # Converts hours,  minutes, and seconds to decimal hours.
    decimal_hours <- hr + min  /60 + sec / 3600
}
