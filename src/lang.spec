
int max(x:int, y:int){
    if( lt(x,y) ){
        return x      
    }else{
        return y
    }
};


int arith(a:int, b:int, c:int){
    int t = add(a,b);
    t = mul(t,c);
    t = sub(t,a);
    t = div(t,2);
    if( lt(t,5) ){
        t = add(t,1)
    }else{
    };
    return t
};




int main(a:int, b:int){ a = add(a,1); b = add(b,1); if(lt(a,b)){a = mul(a,2); return a}else{b = mul(b,2); return b}}




int main(a:int, b:int){ int i = a; int flag = true; int sum = 0; while(lt(i,b)){ if(flag){ sum = add(sum,i); flag = false }else{ sum = mul(sum,i); flag = true }; i = add(i,1) }; return sum }


int main(a:int, b:int){ if(lt(a,b)){ a = add(a,1) }else{ a = add(a,2) }; return a }


int main(a:int){ while(lt(a,10)){ a = add(a,5) }; return a }

int main(a:int, b:int){ bool flag = true; int  sum = 0; while( lt(a,b) ) { if( flag ) { sum = add(sum,2); flag = false } else { sum = mul(sum,2); flag = true }; a = add(a, 1) }; return sum }


int main(a:int, b:int){ int x = a; int y = b; int z = 0; while(lt(x,y)){ if(lt(x,50)){ z = add(z,x) }else{ z = mul(z,y) }; x = add(x,5); int t = add(x,y) }; return z }