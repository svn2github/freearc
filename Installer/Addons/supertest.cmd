:: This shell script allows you to test novel FreeArc compression algorithms
:: on your data. Just run it in the root of directory containing large number
:: of files. Replace "d:\" with the name of directory having enough free space
:: to archive whole source directory, and look at log files created to check
:: that they end with "All OK" line

arc create d:\a -t -mlzp       -r      -i2 >d:\log+lzp
arc create d:\a -t -mlzp:4:h13 -r      -i2 >d:\log+lzp-4
arc create d:\a -t -mdelta     -r      -i2 >d:\log+delta
arc create d:\a -t -mdict      -r      -i2 >d:\log+dict
arc create d:\a -t -mdict:p    -r      -i2 >d:\log+dict-p
arc create d:\a -t -mtor       -r      -i2 >d:\log+tor5
arc create d:\a -t -mtor:3     -r      -i2 >d:\log+tor3
arc create d:\a -t -mrep       -r      -i2 >d:\log+rep512
arc create d:\a -t -mrep:32    -r      -i2 >d:\log+rep32

arc create d:\a -t -mlzp       -r -s-  -i2 >d:\log-lzp
arc create d:\a -t -mlzp:4:h13 -r -s-  -i2 >d:\log-lzp-4
arc create d:\a -t -mdelta     -r -s-  -i2 >d:\log-delta
arc create d:\a -t -mdict      -r -s-  -i2 >d:\log-dict
arc create d:\a -t -mdict:p    -r -s-  -i2 >d:\log-dict-p
arc create d:\a -t -mtor       -r -s-  -i2 >d:\log-tor5
arc create d:\a -t -mtor:3     -r -s-  -i2 >d:\log-tor3
arc create d:\a -t -mrep       -r -s-  -i2 >d:\log-rep512
arc create d:\a -t -mrep:32    -r -s-  -i2 >d:\log-rep32
