# Argument: list of strings
# Returns: LaTeX of associators used for 1 left rotation
# Side Effect: rotates input list
# Example: (a,b,c) \mapsto (c,a,b)
def rotateL(edges):
    edges.insert(0, edges.pop())
    return reassociateL(edges)

def reassociateL(edges):
    if (len(edges) < 3):
        return ""
    return wi(edges[0], cat(edges[1:-1]), edges[-1]) + reassociate(edges[0:-1])

# Argument: list of strings
# Returns: LaTeX of associators used for 1 right rotation
# Side Effect: rotates input list
# Example: (a,b,c) \mapsto (b,c,a)
def rotateL(edges):
    ans =  reassociateR(edges)
    edges.append(edges.pop(0))
    return ans

def reassociateL(edges):
    if (len(edges) < 3):
        return ""
    return wi(edges[0], cat(edges[1:-1]), edges[-1]) + reassociate(edges[0:-1])




def wi(a,b,c):
    return "\omega^{-1}(" + a + ", " + b + ", " + c + ") "


def cat(edges):
    ans = ""
    for e in edges:
        ans = ans + e
    return ans


# Rotate main vertex edges for composition
edges = ['h','g','h^{-1}', 'hg^{-1}h^{-1}']
print(rotateL(edges) + rotateL(edges))
print(edges)

# After composition: (edges) \otimes (g g^{-1})
print(wi(cat(edges), 'g', 'g^{-1}'))
