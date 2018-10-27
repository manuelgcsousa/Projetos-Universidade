BEGIN { FS = "\t" }

$5 == "V"  { A[$4]++ }

END {
    for (i in A) {
        print i;
        print A[i];
    }
}
