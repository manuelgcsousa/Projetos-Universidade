BEGIN { FS = "\t"; S = "" }

$1 ~ /<mwe.*/               { c = 1; next; }
c == 1 && ($1 !~ /<\/mwe>/) { S = $1; c++; next; }
c > 1 && ($1 !~ /<\/mwe>/)  { S = S " " $1; }
$1 ~ /<\/mwe>/              { c = 0; dic[S]++; S = ""; }

END {
    for (i in dic) {
        print "Expressão: " i, dic[i] > "test.txt";
        sum += dic[i];
    }
    print sum;
}
