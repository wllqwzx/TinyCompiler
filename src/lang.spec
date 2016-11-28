
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

int main(){
    int t = max(3,5);
    int i = arith(2,4,t);
    i = add(i,1);
    int j = 10;
    while( or(0<j,i<j) ){
        j = sub(j,1)
    };
    print(j);
    return 0
}


int main(a:int, b:int){ a = add(a,1); b = add(b,1); if(lt(a,b)){return a}else{return b}}