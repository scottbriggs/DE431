
dayOfWeek <- function(year, month, day)
{
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