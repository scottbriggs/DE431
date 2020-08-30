julianDay <- function (year, month, day)
{
    # Calculate the julian day from a calendar date - Julian or Gregorian calendar is acceptable.
    # The julian day starts on January 1, -4712.
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

calendarDate <- function(jd)
{
    # Calculate the calendar date given a julian day number. Works for julian and calendar dates
    # from January 1, -4712.
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

dayOfWeek <- function(year, month, day)
{
    # Determines the day of the week given a calendar date. Works for Julian and Gregorian calendars.
    d <- trunc(day)
    jd <- julianDay(year, month, d)
    
    dayNum <- (jd + 1.5) %% 7
    
    dayStr <- ""
    
    if (dayNum == 0) {
        dayStr <- "Sunday"
    } else if (dayNum == 1){
        dayStr <- "Monday"
    } else if (dayNum == 2){
        dayStr <- "Tuesday"
    } else if (dayNum == 3){
        dayStr <- "Wednesday"
    } else if (dayNum == 4){
        dayStr <- "Thursday"
    } else if (dayNum == 5){
        dayStr <- "Friday"
    } else {dayStr = "Saturday"}
    
    return(dayStr)
}

dateOfEaster <- function(year)
{
    # Calculates the date of Christian Easter
    month <- 0
    day <- 0
    
    # Julian Calendar
    if (year < 1583)
    {
        a <- year %% 4
        b <- year %% 7
        c <- year %% 19
        d <- (19 * c + 15) %% 30
        e <- (2 * a + 4 * b - d + 34) %% 7
        temp <- d + e + 114
        month <- trunc(temp/31)
        day <- (temp %% 31) + 1
        # Gregorian Calendar
    } else {
        a <- year %% 19
        b <- trunc(year / 100)
        c <- year %% 100
        d <- trunc(b / 4)
        e <- b %% 4
        f <- trunc((b+8) / 25)
        g <- trunc((b - f + 1) / 3)
        h <- (19 * a + b - d - g + 15) %% 30
        i <- trunc(c / 4)
        k <- c %% 4
        l <- (32 + 2 * e + 2 * i - h - k) %% 7
        m <- trunc((a + 11 * h + 22 * l) / 451)
        temp <- h + l - 7 * m + 114
        month <- trunc(temp / 31)
        day <- (temp %% 31) + 1
    }
    
    monthStr <- ""
    
    if (month == 3) {
        monthStr <- "March"
    } else { monthStr <- "April" }
    
    c(monthStr, day)
}
