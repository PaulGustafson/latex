inf = open('nbd3.tex')
outf = open('test2_gen.py', 'w')
outf.truncate()
outf.write('o = open("nbd3_gen.tex", "w")\n')
outf.write('o.truncate()\n')

for line in inf:
    line = line[:-1]
    line = line.replace('\\','\\\\')
    outf.write("o.write('" + line + "\\n') \n")

outf.write('o.close()\n')
inf.close()
outf.close()
