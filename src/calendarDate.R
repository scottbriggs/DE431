
calendarDate <- function(jd)
{
    jd <- jd + 0.50
    
    jd_int <- trunc(jd)
    jd_frac <- jd - jd_int
    
    a <- 0
    
    if (jd_int < 2299161)
    {
        a <- jd_int
    } else
    {
        alpha <- trunc((jd_int - 1867216.25) / 36524.25)
        a <-jd_int + 1 + alpha - trunc(alpha / 4)
    }
    
    b <- a + 1524
    c <- trunc((b - 122.1) / 365.25)
    d <- trunc(365.25 * c)
    e <- trunc((b - d) / 30.6001)
    
    day <- b - d - trunc(30.6001 * e) + jd_frac
    month <- 0
    
    if (e < 14)
    {
        month <- e - 1
    }else if (e == 14 | e == 15)
    {
        month <- e - 13
    }
    
    year <- 0
    
    if (month > 2)
    {
        year <- c - 4716
    }else if (month == 1 | month == 2)
    {
        year <- c - 4715
    }
    
    c(year, month, day)
}