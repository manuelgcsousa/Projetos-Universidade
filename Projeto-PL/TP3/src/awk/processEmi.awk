BEGIN { FS = "," }

{ if (NR == 1) next; }

{ print("emigrante {\n\t"$1";\n\t"$2";\n\t"$3";\n\t"$4";\n\t"$5";\n\t"$6";\n\t"$7";\n\t"$8";\n}\n"); } 

END { }
