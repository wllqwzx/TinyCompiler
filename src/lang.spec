
int max(x:int, y:int){
    if( lt(x,y) ){
        x      
    }else{
        y
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
        pass
    };
    t
};

void main(){
    int t = max(3,5);
    int i = arith(2,4,t);
    i = add(i,1);
    int j = 10;
    while( or(0<j,i<j) ){
        j = sub(j,1)
    };
    print(j)
}


