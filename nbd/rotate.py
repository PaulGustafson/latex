# Composes e2 vertex into e1
def compose(e1, e2):
    x = e1.pop();
    e2.pop();
    ans = ev(x) + reassociateCCW([cat(e1)] + e2)
    e1.extend(e2)
    return ans

def tensor(edges, i):
    c = edges.pop(i)
    b = edges.pop(i)
    edges.insert(i, cat([b, c]))
    return w(cat(edges[0:i]), b, c)
                                 
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

## Fig 4

# Add a coev
print(coev('g'))

# Rotate main vertex edges for composition
edges = ['h','g','h^{-1}', 'hg^{-1}h^{-1}']
print(rotateCCW(edges) + rotateCCW(edges))
show(edges)


## Fig 5

# Apply composition of ['h^{-1}', 'hg^{-1}h^{-1}', 'h', 'g'] with (g g^-1)
edges.append('g')
edges.append('g^{-1}')

# After composition: (edges) \otimes (g g^{-1}), fix this
print(wi(cat(edges[0:-2]), 'g', 'g^{-1}'))
show(edges)

## Fig 6

print(rotateCCW(edges))
print(rotateCCW(edges))
show(edges)
# make new node for gh
print(coev('g'))
print(coev('h'))
e1 = ['g','g^{-1}','1']
e2 =  ['h^{-1}', 'h','1']
print(compose(e1, e2))
print(e1)
print(rotateCW(e1))
print(e1)
print(w(cat(e1[0:-2]),e1[-2], e1[-3]))
print(tensor(e1,-1))
print(e1)
print(tensor(edges, -1))
show(edges) # [g, g^{-1}, h^{-1}, hg^{-1}h^{-1}, hg]
print(w(edges[0], edges[1], edges[2]))
a = edges.pop(1)
b = edges.pop(1)
edges.insert(1, cat([a, b]))
print(compose(edges, e1))
show(edges)

