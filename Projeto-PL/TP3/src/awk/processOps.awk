BEGIN { FS = "," }

{ if (NR == 1) next; }

{ print("operacao {\n\t"$1";\n\t"$2";\n\t"$3";\n}\n"); } 

END { }
