BEGIN { FS = "\t" }

$1 !~ /^</  { A[$1][$4][$5]++; }

END {
    for (i in A) {
        for (j in A[i]) {
            for (k in A[i][j]) {
                print ("i-> " i,"| j-> " j,"| k-> " k, "| Valor-> " A[i][j][k])
            }
        }
    }
}
