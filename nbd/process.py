inf = open('test.py')
outf = open('test2.py', 'w')
outf.truncate()

for line in inf:
    line = line[:-1]
    line = line.replace('\\','\\\\')
    outf.write("print '" + line + "' \n")





