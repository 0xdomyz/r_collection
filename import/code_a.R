#Usage
#--------
#rscript import\code_a.R
#rscript import\code_p.R

import::here('code_p.R', fun_p, .directory=here::here('import'))
import::here('code_k.R', fun_k, .directory=here::here('import', 'folder'))

fun_p()