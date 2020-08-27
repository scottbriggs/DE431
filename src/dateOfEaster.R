
dateOfEaster <- function(year)
{
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

