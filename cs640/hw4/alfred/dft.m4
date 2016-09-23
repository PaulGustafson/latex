include(perm.m4)
H  = [0.707,0.707,0.707,-0.707]
RA = [1,0,0,i]
RB = [1,0,0,i*0.707]

N=3
print "A state with period 4"
|000)+2|001)+3|010)+4|011)+|100)+2|101)+3|110)+4|111)
normalize
show
G(2,H)
G([2],1,RA)
G([2],0,RB)
G(1,H)
G([1],0,RA)
G(0,H)
swap(0,2)
print "Every other sample is 0 in the spectrum of the DFT"
show






