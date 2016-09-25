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
    return '\ev_{' + g + '}' #w((g)^{-1}. g, (g)^{-1})

def coev(g):
    return '\coev_{' + g + '}' 



## Fig 4

# Add a coev
print(coev('g'))

# Rotate main vertex edges for composition
edges = ['h','g','h^{-1}', 'hg^{-1}h^{-1}']
print(rotateCCW(edges) + rotateCCW(edges))
print(edges) 


## Fig 5

# Apply composition of ['h^{-1}', 'hg^{-1}h^{-1}', 'h', 'g'] with (g g^-1)
edges.append('g')
edges.append('g^{-1}')

# After composition: (edges) \otimes (g g^{-1}), fix this
print(wi(cat(edges[0:-2]), 'g', 'g^{-1}'))
print(edges)

## Fig 6

# uncompose leftside (g^{-1} h^{-1})
print(rotateCCW(edges))
print(edges)

print(coev('g'))
print(coev('h'))

# Should I 
