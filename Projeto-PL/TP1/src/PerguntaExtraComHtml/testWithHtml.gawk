BEGIN {
    FS = "\t";
    f = "<li><a href ='%s'> %s </a> </li> \n";
    f2 = "<html><body><p> %s ";
    print "<h1 style='color:blue;'> <i> Lista de Extratos </i> </h1> \n" > "indicegeral.html";
    print "<html><head><title>Lista de Extratos</title><meta charset='UTF-8'> </head> \n" > "indicegeral.html";
}

$1 ~ /<ext.*/ {
    split($0, a, / /);
    gsub(/n=/, "", a[2]);
    printf (f,"Extrato " a[2] ".html","Extrato " a[2]) > "indicegeral.html";
    printf "<html><head><meta charset='UTF-8'> </head> \n"> "Extrato " a[2] ".html";
    printf ("<h1> <i> <b style = 'color:red'> %s </b> </i> </h1> \n", "Extrato " a[2]) > "Extrato " a[2] ".html";
    print "<html><body>" > "Extrato " a[2] ".html"
}

$1 ~ /<p.*/      { print "<p><t>" > "Extrato " a[2] ".html"}

$1 !~ /<.*/      { printf ($1 " ") > "Extrato " a[2] ".html" }

$1 ~ /<\/p>/     { print "</p>" > "Extrato " a[2] ".html" }

$1 ~ /<\/ext>/   { print ("</body></html>") > "Extrato " a[2] ".html" }

END {}
