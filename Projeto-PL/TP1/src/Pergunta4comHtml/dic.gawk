BEGIN {
    FS = "\t"
    s = ""
    print "<html><head><style>\n" > "tabelaDic.html"
    print "table { border-collapse: collapse; width 100%; }\n" > "tabelaDic.html"
    print "th, td { text-align: left; padding: 8px; }\n" > "tabelaDic.html"
    print "tr:nth-child(even){ background-color: #f2f2f2 }\n" > "tabelaDic.html"
    print "th { background-color: #3A80F2; color: white; }\n" > "tabelaDic.html"
    print "</style><meta charset='UTF-8'> </head><body>\n" > "tabelaDic.html"
}

$1 !~ /^</  { A[$1][$4][$5]++; }

END {
    print "<table><tr> <th>Palavra</th> <th>Radical</th>  <th>Part Of Speech</th> </tr>" > "tabelaDic.html"

    for (i in A) {
        for (j in A[i]) {
            for (k in A[i][j]) {
                if (N >= 1) {
                    s = s ", " k
                } else {
                    s = k
                }
                N++
            }
            print "<tr><td>" i "</td> <td>" j "</td> <td>" s "</td></tr>" > "tabelaDic.html"
            s = ""
            N = 0
        }
    }

    print "</table></body></html>" > "tabelaDic.html"
}
