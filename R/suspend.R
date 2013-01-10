suspend_console <- function(){

   # My first approach to suspending the console
   # was to call Sys.sleep with .Machine$integer.max, however
   # this fails on windows because its implementating causes
   # overflow.

   # So we sleep for a day, and then loop.
   allday <- 24*60*60
   while(TRUE) Sys.sleep(allday)
}
