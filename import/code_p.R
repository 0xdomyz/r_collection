import::here('code_k.R', fun_k, .directory=here::here('import', 'folder'))
#import::from('code_k.R', fun_k, .directory=here::here('import', 'folder'))

fun_p = function(){
    print('fun_p')
    fun_k()
}

if (identical(environment(), globalenv())){
    fun_p()
}


