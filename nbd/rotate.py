def reassociate(edges):
    if (len(edges) < 3):
        return ""
    return "\omega(" + edges[0] + ", " + cat(edges[1:-1]) + ", " + edges[-1] + ") " + reassociate(edges[0:-1])

def cat(edges):
    ans = ""
    for e in edges:
        ans = ans + e
    return ans

               
print(reassociate(['a','b','c','d']))
