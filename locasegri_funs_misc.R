
# Generates a time stamp for files
time_Stamp = function () { return( paste0(substr(Sys.time(), start=12, stop=13), 
                                          substr(Sys.time(), start=15, stop=16),
                                          substr(Sys.time(), start=18, stop=19)) ) }
                                          
# Generates a date and time stamp for file names
datetime_Stamp = function () { return( 
  paste0(substr(Sys.time(), start=1, stop=10),
         '-', 
         substr(Sys.time(), start=12, stop=13), 
         substr(Sys.time(), start=15, stop=16),
         substr(Sys.time(), start=18, stop=19)) ) }

# INPUT: a (T x K) tibble
# OUTPUT: a K list in which each element is a string with the five num of column of table
# Ex: OUTPUT$colI = '-10; -4; 0; 3; 12'
five_Num_Str = function(table) {
	 imap(table,
        function(x, varName) {
          c(str_c(round(fivenum(x), 2), collapse='; '))
      })
}

# INPUT: a (T x K) tibble
# OUTPUT: a list that contains a string with the mean and 5 numbers (min, 25th, median, 75th, max) of each
# column of the input tibble 
# Ex: OUTPUT$colI = 'Mean: 0.22, -10; -4; 0; 3; 12'
desc_Stats <- function(table, naRm=FALSE, decimals=3, scNotation=FALSE) {
  formato = if_else(scNotation==TRUE, 'e', 'f') 
  imap(table,
        function(x, varName) {
          fig <- list('mean' = mean(x, na.rm=naRm),
                    'var' = var(x, na.rm=naRm),
                    'fiveNum' = fivenum(x))
          figStr <- map(fig, function(x) { formatC(x, digits=decimals, format=formato) })
          
          glue::glue("Mean: {figStr$mean}-Var: {figStr$var}-5Num: {str_c(figStr$fiveNum, collapse=';')}")
        
        })
}

save2Excel <- function(obj, sName, file) {
  write.xlsx2(obj, file, 
              sheetName=sName, append=TRUE)
}
