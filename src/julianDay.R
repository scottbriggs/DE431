
julianDay <- function (year, month, day)
{
    # Year is an integer
    # Month is an integer from 1 - 12 indicating the year - January - December
    # Day is a decimal number indicating the day and fraction of a day
    
    # Determine if the date is in the Julian or Gregorian calendar
    y = year
    m = month
    d = day
    
    if (m < 2)
    {
        m = m + 12
        y = y - 1
    }
    
    b = 0
    
    # Julian Calendar
    if (y < 1582 | (y == 1582 & m < 10) | (y == 1582 & m ==10 & y < 15 ))
    {
        b = 0
    # Gregorian Calendar
    }else {
        a <- trunc(y / 100)
        b <- 2- a + trunc(a/4)
    }
    
    jd <- trunc(365.25 * (y + 4716)) + trunc(30.6001 * (m + 1)) + d + b - 1524.5
}

