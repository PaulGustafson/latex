divert(-1)
include(perm.m4)
# for loop macros
define(for,`undefine(`_break')define(`$1',`$2')_for(`$1',`$2',`$3',`$4')')dnl
define(_for,`$4`'ifelse($1,`$3',,`ifdef(`_break',,`define(`$1',incr($1))_for(`$1',`$2', `$3',`$4')')')')dnl
define(`break',`define(`_break')')dnl
# newline
define(`nl',`
')
# bit reversal br(num,len), reverses numbers with len bits. 
define(`br',`dnl 
undefine(`_num')define(`_num', $1)dnl
undefine(`_len')define(`_len', $2)dnl
undefine(`_res')define(`_res',  0)dnl
undefine(`i')dnl
for(`i', 0, eval(_len-1),`define(`_res', eval( (_res <<1) | (_num & 1) ))define(`_num', eval( _num >> 1 ))')_res')

define(`bitrev',# bit reversal 
`for(`k',0,eval((1<<$1)-1),`ifelse(eval(br(k,$1)>k),1,`swap(k,br(k,$1))
')')dnl
# end of bit reversal
')
divert(0)

bitrev(4)





