# Composes e2 vertex into e1
def compose(e1, e2):
    x = e1.pop();
    e2.pop();
    ans = ev(x) + reassociateCCW([cat(e1)] + e2)
    e1.extend(e2)
    return ans

# Tensors objects at i and i + 1
def tensor(edges, i):
    i = i % len(edges)  # fix negative indices bug
    b = edges.pop(i)
    c = edges.pop(i)
    edges.insert(i, cat([b, c]))
    if (i > 0):
        return w(cat(edges[0:i]), b, c)
    return ''
                                 
# Argument: list of strings labelling edges incident to a vertex
# Returns: LaTeX of associators used for 1 counterclockwise 
#          rotation of the reference frame
# Side Effect: rotates input list
# Example: (a,b,c) \mapsto (c,a,b)
def rotateCCW(edges):
    edges.insert(0, edges.pop())
    return reassociateCCW(edges)

def reassociateCCW(edges):
    if (len(edges) < 3):
        return ""
    return wi(edges[0], cat(edges[1:-1]), edges[-1]) + reassociateCCW(edges[0:-1])

# Argument: list of strings labelling edges incident to a vertex
# Returns: LaTeX of associators used for 1 clockwise rotation
#          of the reference frame
# Side Effect: rotates input list
# Example: (a,b,c) \mapsto (b,c,a)
def rotateCW(edges):
    ans =  reassociateCW(edges, 2)
    edges.append(edges.pop(0))
    return ans

def reassociateCW(edges, n):
    if (n >= len(edges)):
        return ""
    return w(edges[0], cat(edges[1:n]), edges[n]) + reassociateCW(edges, n + 1)


def cat(edges):
    ans = ""
    for e in edges:
        ans = ans + e
    return ans

def wi(a,b,c):
    return "\omega^{-1}(" + a + ", " + b + ", " + c + ") "

def w(a,b,c):
    return "\omega(" + a + ", " + b + ", " + c + ") "

def ev(g):
    return '\ev_{' + g + '} ' #w((g)^{-1}. g, (g)^{-1})

def coev(g):
    return '\coev_{' + g + '} ' 


def show(edges):
    print 'Edges: [%s]' % ', '.join(edges)
    pass

def doc(msg):
    print '\n***' + msg
    #pass

def newFactor(f):
     if (len(f) > 0):
        print f
    # pass
    
    
## Fig 4

doc("Add a coev vertex in the upper left corner")
newFactor(coev('g'))

doc('Rotate main vertex edges for composition')
edges = ['h','g','h^{-1}', 'hg^{-1}h^{-1}']
show(edges)
newFactor(rotateCCW(edges) + rotateCCW(edges))
show(edges)


doc("Apply composition of ['h^{-1}', 'hg^{-1}h^{-1}', 'h', 'g'] with (g g^-1)")
edges.append('1')
e2 = ['g', 'g^{-1}', '1']
newFactor(compose(edges, e2))
show(edges)

## Fig 6

doc('Make new node for gh')
newFactor(coev('g'))
newFactor(coev('h'))
e1 = ['g','g^{-1}','1']
e2 =  ['h^{-1}', 'h','1']
newFactor(compose(e1, e2))
#show(e1)

doc('Rotate and tensor new node for composition')
newFactor(rotateCW(e1))
show(e1)
newFactor(tensor(e1,0))
newFactor(tensor(e1,-2))
newFactor(rotateCW(e1))
show(e1)

doc('Rotate and tensor old node')
newFactor(rotateCCW(edges))
show(edges)
newFactor(tensor(edges,0))
show(edges)
newFactor(tensor(edges,-3))
newFactor(rotateCCW(edges))
show(edges)
newFactor(compose(edges, e1))
show(edges)
newFactor(rotateCCW(edges))
show(edges)

