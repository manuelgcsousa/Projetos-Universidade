BEGIN {}

$1 ~ /<\/ext>/  { e++ }
$1 ~/<\/p>/     { p++ }
$1 ~/<\/s>/     { f++ }

END {
    print "Número de extratos: " e "\n" "Número de parágrafos: " p "\n" "Número de frases: " f
}
