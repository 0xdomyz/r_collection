fun_k = function(){
    print('fun_k')
}

if (identical(environment(), globalenv())){
    fun_k()
}
