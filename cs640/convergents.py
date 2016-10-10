a = 6
b = 256


#continued fraction expansion
def cfExp(p,q, cfE):
    if q == 0 :
        cfE = [p];
    else:
        cfExp(q, p - q*(p/q), cfE)
        cfE.insert(0, p/q)

cfE = []
cfExp(a, b, cfE)
print cfE

#Old convergents, initial conditions
p2 = 0
q2 = 1
p1 = 1
q1 = 0


for a in cfE:
    p = a*p1 + p2 
    q = a*q1 + q2
    print("\\frac{" + str(p) + "}{"  + str(q) + "}, ")
    p2 = p1
    q2 = q1
    p1 = p
    q1 = q



    

