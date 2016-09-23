divert(-1)
# Three macros are provided that allow to permute quantum wires
# swap(2,3) swaps the content of qubit 2 and 3
# cyc(1,2,3) implements the permutation 1->2->3->1 of quantum wires
# perm((1,2,3)(4,5,6)) allows to write down any permutation of 
#      quantum wires in cycle notation
define(`swap',`#swap($1,$2) 
 G([$1],$2, [0,1,1,0]) 
 G([$2],$1, [0,1,1,0]) 
 G([$1],$2, [0,1,1,0])')dnl
define( `cyc',`ifelse($#,0, , $#,1, , `cyc(shift($@))
swap($1,$2)')')dnl
define(`perm',`patsubst($1,`(',`cyc(')')dnl
divert(0)

