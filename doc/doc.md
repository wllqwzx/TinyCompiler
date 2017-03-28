
# TinyCompiler

A tiny compiler with IR optimization.
----------

## 目录
> ##### 1. 项目的内容及意义
> ##### 2. 具体的解决方案
> ##### 3. 测试与结果分析
> ##### 4. 代码编译与运行

----------
## 1. 项目的内容及意义

在编译器将源代码编译为目的码的过程中，会先将源代码转换为一个或多个的中间表示，以方便编译器进行优化，并产生出目的机器的机器语言，它类似于汇编代码但更加抽象，不受具体机器指令集的约束。现代编译器的编译过程中大多包含中间表示，例如LLVM IR，Java Bitcode等。

在本项目中，我们首先设计了一种简单的编程语言，它包含基本的赋值，条件分支，循环，函数定义与调用等，支持int与bool两种数据类型。我们采用OCaml作为实现语言，使用lex与yacc产生词法分析器与语法分析器，之后我们又定义了一种中间表示，并将抽象语法树编译到了这种中间表示。由于生成的中间表示包含很多冗余代码，我们尝试对这种中间表示进行优化。

在优化部分，我们首先将中间代码转换成static single assignment(SSA) 的形式，SSA是代码的一种性质，它要求所有变量只被定义一次。SSA形式的中间代码具有很多优点，它使得后面的优化部分的实现变得简单，并且也降低了时间复杂度。之后我们在SSA形式的中间代码上进行了四种优化：死代码消除，常量传播，拷贝传播与常量折叠。最后我们消除了代码中的phi函数将SSA形式的中间代码转回了一般形式的中间代码。从后面的例子中我们可以看到，这些优化取得了较好的结果，对于某些中间代码，优化之后大大减少了代码的长度。

下面是我们在本项目中主要完成的工作：
> * lexer
> * parser
> * IR generator
> * static single assignment IR
> * dead code elimination
> * constant propagation
> * copy propagation
> * constant folding
> * eliminate phi function


----------


## 2. 具体解决方案
### 2.1 语言定义与抽象语法树 

我们首先定义了一种简单的语言，下面是我们定义的语言的concrete syntex:
```
program 		::= function_def; function_def; ...; function_def

function_def 	::= type name(param,){ statement; statement; 
                    ...;statement}  		 	 
                   
type 			::= Int_type | bool_type

name			::= string

param			::= name : type

statement		::= type name = expression
				  | name = expression
				  | if(expression){statement; statement; 
				    ...; statement}else{statement; statement; ...;
				    statement}
				  | while(expression){statement; statement; 
				    ...; statement}
				  | print(expression)
				  | return expression
				  
expression		::= true
			      | false
			      | number
			      | lt(expression, expression)
			      | and(expression, expression)
			      | or(expression, expression)
			      | add(expression, expression)
			      | sub(expression, expression)
			      | mul(expression, expression)
			      | div(expression, expression)
			      | varname
			      | varname = funcname(expression, expression, 
			        ..., expression)
```
为了更加直观，我们写出几个由上面定义的语言写出的程序：
```
int max(a:int, b:int){
	if(lt(a,b)){
		return b
	}else{
		return a
	}
};

int main(a:int, b:int){ 
	bool flag = true; 
	int  sum = 0; 
	while( lt(a,b) ) { 
		if( flag ) { 
			sum = add(sum,2); 
			flag = false 
		} else { 
			sum = mul(sum,5); 
			flag = true 
		}; 
		a = add(a, 1) 
	}; 

	int res = max(sum,50);
	return res
}
```

接下来我们定义了abstract syntex，我们用OCaml中的代数数据类型来表示它：
```OCaml
type basetype =
  | Int_ty 
  | Bool_ty

type expression =
  | Const_bool_exp  of bool
  | Lt_exp          of expression * expression
  | And_exp         of expression * expression
  | Or_exp          of expression * expression
  | Const_int_exp   of int
  | Sub_exp         of expression * expression
  | Add_exp         of expression * expression
  | Mul_exp         of expression * expression
  | Div_exp         of expression * expression
  | Var_exp         of string
  | Call_exp        of string * expression list

type statement =
  | Var_def_stat  of basetype * string * expression
  | Var_set_stat  of string * expression
  | If_stat       of expression * statement list * statement list
  | While_stat    of expression * statement list
  | Print_stat    of expression
  | Return_stat   of expression

type param =
  | A_param of string * basetype

type function_def = 
  | A_func of basetype * string * param list * statement list

type program =
  | A_program of function_def list
```

### 2.2 中间表示
我们定义了一种简单的中间表示，它包括基本的运算，条件跳转，无条件跳转，变量赋值，函数调用等，为了直观的描述这种中间表示，我们给出一个源代码到中间代码的具体例子：
```
源代码：
int main(a:int){
	if(lt(a,10)){
		a = add(a,5);
		int b = mul(a,2);
		int c = add(a,b)
	}else{
		a = sub(a,10);
		int b = mul(a,2);
		int c = add(a,b)
	};
	return a
}

对应中间代码：
_B5:
pop a
_t1 = a
_t2 = 10
_t3 = _t1 < _t2
ifz _t3 goto _B2:
goto _B1

_B1:
_t4 = a
_t5 = 5
_t6 = _t4 + _t5
a = _t6
_t7 = a
_t8 = 2
_t9 = _t7 * _t8
b = _t9
_t10 = a
_t11 = b
_t12 = _t10 + _t11
c = _t12
goto _B3

_B2:
_t13 = a
_t14 = 10
_t15 = _t13 - _t14
a = _t15
_t16 = a
_t17 = 2
_t18 = _t16 * _t17
b = _t18
_t19 = a
_t20 = b
_t21 = _t19 + _t20
c = _t21
goto _B3

_B3:
goto _B4
_B4:
_t22 = a
ret _t22
```
为了简化中间代码生成程序，以及使后续的处理更加简单，我们生成的中间代码中出现了一些冗余的赋值语句，但这并不会对结果造成影响，因为在后面的优化过程中这些冗余都会被删除掉，除此之外，为了使静态单赋值更易于实现，我们在保持语义不变的情况下增加了一些基本块的数量，使中间代码满足edge splitting的性质，即不会出现同时具有多个后继与前驱的基本块。


### 2.3 静态单赋值IR
构建静态单赋值形式的IR是本次课程项目的核心工作，将一般形式的IR转化为静态单赋值形式的IR需要完成以下几个任务：

> * 构建IR的CFG
> * 根据CFG构建支配树
> * 根据支配树和CFG构建CFG中每个节点的支配边界
> * 插入phi函数
> * 对变量进行重命名
> * 消除phi函数

下面我们对每个部分的具体实现方法进行简要的说明：
#### 2.3.1 构建IR的CFG

由源代码直接转化过来的IR是线性的，我们需要对其进行结构化，划分出一些基本块，并记录每条基本块之间的边。

构建CFG的过程比较简单，我们先创建一个临接表，然后把IR扫描一边，将每一个基本快放到一个list中，并用其对应的label ID索引，然后每遇到一个跳转语句就往临接表中添加一条边。这样CFG就构建完成了。

#### 2.3.2 根据CFG构建支配树

构建支配树是一个复杂的过程，常用的高效的算法是lengaure tarjan algorithm，但此算法实现较复杂，因此本项目中采用的是虎书407页给出的一种易于实现单效率相对较低的算法。下面给出该算法的具体思路：
```
1.首先对于每个节点求出该节点的支配节点(dominator)：
	另DOM[n]为支配节点n的节点集合，
	对于起始节点s0, DOM[s0] = {s0},
	对于其他节点n, DOM[n] = {n} + {n的所有前驱节点p的DM[p]的交集}，
	迭代此过程直到求出所有节点的DOM.

2.接下来求每个节点的直接支配节点：
	由于每个节点的支配节点都已经求出，因此对当前节点的所有支配节点的支配节点求交集，得到的集合一定只有一个元素，因为根据虎书408页的定理，一个节点的任意两个支配节点必有一个支配另一个。所以该元素就是当前节点的直接支配节点。

3.根据直接支配点构建支配树。
```
#### 2.3.3 根据支配树和CFG构建CFG中每个节点的支配边界
一个节点的支配边界指是一些节点的集合，且对该集合中的任意节点，当前节点支配它的某些前驱，但不严格支配该节点。支配边界会在下面插入phi函数的过程中被用到。下面给出此工程中求支配边界所用到的算法的伪代码：
```
另DF[n]为节点n的支配边界：

computeDF[n] = 
	S = {}
	对节点n的所有后继节点y:
		如果y的直接支配节点不是n：
			那么：S <- S + {y}
	对n在支配树中的所有子节点c:
		computeDF[c]
		对DF[c]的所有元素w:
			如果n不支配w:
				S <- S + {w}
	DF[n] <- S
```

#### 2.3.4 插入phi函数
插入phi函数的算法参考了虎书435页的内容，但直接按照此算法会插入大量的冗余phi函数，因此我们对其进行了优化，我们使用semi-pruned SSA，即如果一个变量的定义和使用都在同一个基本块内，那么不用为此变量插入phi函数。下面是伪代码：
```
1.查找所有变量被定义的节点：
	建立一个变量到节点集合的hash table：defSite;
	对于每一个节点n：
		对节点n中所有被定义的变量x:
			将n加入到defSite[x]中

2.查找所有变量被使用的节点：
	建立一个变量到节点集合的hash table：useSite;
	对于每一个节点n：
		对节点n中所有被使用的变量x:
			将n加入到useSite[x]中

3.插入phi函数：
	对defSite中的每一个变量x:
		令S_def = defSite[x]
		令S_use = useSite[x]
		如果S_def中只有一个元素，且S_use为空或所有元素都与该元素相同，那么：
			跳过变量x,不为x插入phi函数
		否则：
			当S_def为非空时：
				从S_def中移除任意元素n
				对DF[n]中的所有元素y：
					如果y节点还未插入变量x的phi函数：
						把 x = Phi(x,x,..x) 插入到节点y的开始位置，并且
						Phi中x的数目与节点y的前驱节点数量相同。
					如果x没有在y中被定义过：
						将y加入到S_def中
```

#### 2.3.5 对变量进行重命名
对变量进行重命名之后，IR就会满足每个变量只被定义一次了，下面是本项目中所使用的算法的伪代码：
```
1.初始化：
	对所有变量x:
		令count[x] = 0
		令stack[x] = {}
		push 0 onto stack[x]
2.对节点n中的变量进行重命名：
	reName(n) = 
		对n中的任意一个语句s：
			如果s不是phi函数：
				对s中所有被使用的变量x：
					令i=top(stack[x])
					将s中使用x的地方变为使用xi
			对s中所有被定义的变量a：
				count[a] = count[a] + 1
				i = count[a]
				push i onto stack[a]
				将s中定义a的地方改为定义ai
		对n在CFG中的所有后继节点y：
			假设n是y的第j个前驱
			对y中的所有phi函数：
				假设phi函数的第j个参数为a
				令i=top(stack[a])
				将phi函数的第j个参数a改为ai
		对n在支配树中的所有儿子节点t：
			reName(t)
		对原始的s中所有被定义的变量a：
			pop(stack[a])
```

#### 2.3.6 消除phi函数
消除phi函数真正执行的时刻是优化之后，但它属于与静态单赋值相关的算法的一部分，因此放在了这里。我们采用下面的思路来消除phi函数：
```
	对所有的节点n：
		对节点n中的所有phi函数xi0 = phi(xi1,xi2,..xin):
			假设n的第j个前驱是y
			把xi0 = xij插入到y节点跳转语句的前面，其他语句的后面
			将phi函数从当前节点删除
```
### 2.4 死代码消除
SSA形式的IR有一些很好的性质，如：每个变量只会被定义一次，这些性质使得一些优化存在时间复杂度相对较低且实现难度也较低的算法。下面我们给出项目中使用的在SSA形式的IR上进心死代码消除的算法的伪代码：
```
1.统计每个变量的使用次数
	对所有的语句s：
		对s中所有的变量x：
			若use[x]未被初始化：
				将use[x］初始化为0
			若x在s中被使用：
				use[x] = use[x] + 1

2.执行死代码消除
	令W为SSA形式的IR中所有的变量
	当W不为空集时：
		从W中移除一个变量v
		如果v的使用次数为0：
			令s为变量v的定义语句
			把s从所在的基本块中删除
			对s中所有被使用的变量x：
				把x的使用次数减1
				把x加入到W中
```


### 2.5 常量传播, 拷贝传播, 常量折叠
与死代码消除类似，在SSA形式的IR上常量传播，拷贝传播，常量折叠的实现也变得非常简单。常量传播只需将所有形如x = c，或x = phi(c,c...)的语句删除，并用常量c来代替代码中的x；拷贝传播与常量折叠类似，拷贝传播将所有形如 x = v 和 x = phi(v,v..v) 的语句删除，并用v来代替x；常量折叠指的是将所有形如 x = c1 op c2 的语句删除，并计算出 c1 op c2 的结果，用此结果替代所有使用x的地方。下面我们给出对应算法的伪代码：


```
1.常量传播：
	令W为程序中的所有语句的集合
	当W为非空时：
		从W中移除一条语句s
		如果s是 x = phi(c,c,..c) 形式的:
			把s变为 x = c
		如果s是 x = c 形式的：
			把s从程序中删除
			对程序中所有使用x的语句t：
				把t中的x替换为c
				将t添加到W中去
				
2.拷贝传播
	令W为程序中的所有语句的集合
		当W为非空时：
			从W中移除一条语句s
			如果s是 x = phi(y) 形式的:
				把s变为 x = y
			如果s是 x = y 形式的：
				把s从程序中删除
				对程序中所有使用x的语句t：
					把t中的x替换为y
					将t添加到W中去
					
3.常量折叠
	令W为程序中的所有语句的集合
		当W为非空时：
			从W中移除一条语句s
			如果s是 x = c1 op c2 形式的:
				计算c1 op c2的值，假设值为c
				对程序中所有使用x的语句t：
					把t中的x替换为c
					将t添加到W中去
```

----------
## 3. 测试与结果分析
这一节我们给出了用之前定义的语言写出的几段程序，然后分别编译，输出中间结果及最重优化后的代码，我们将看到对于不同的程序优化后的代码都比原来的代码有一定程度的减少，不同的代码优化效果不同，这与代码中各种可优化语句的你里有关。接下来我们分分别用几个短代码来测试各种不同优化的效果，最后用一个稍微长一点的代码测试综合效果。

### 3.1 测试死代码消除
对于下面这段程序，if的两个分支中的b，c的计算没有被使用过，是死代码：
```
int main(a:int){
	if(lt(a,10)){
		a = add(a,5);
		int b = mul(a,2);
		int c = add(a,b)
	}else{
		a = sub(a,10);
		int b = mul(a,2);
		int c = add(a,b)
	};
	return a
}
```
其编译后的IR为：
```
_B5:
pop a
_t1 = a
_t2 = 10
_t3 = _t1 < _t2
ifz _t3 goto _B2:
goto _B1
_B1:
_t4 = a
_t5 = 5
_t6 = _t4 + _t5
a = _t6
_t7 = a
_t8 = 2
_t9 = _t7 * _t8
b = _t9
_t10 = a
_t11 = b
_t12 = _t10 + _t11
c = _t12
goto _B3
_B2:
_t13 = a
_t14 = 10
_t15 = _t13 - _t14
a = _t15
_t16 = a
_t17 = 2
_t18 = _t16 * _t17
b = _t18
_t19 = a
_t20 = b
_t21 = _t19 + _t20
c = _t21
goto _B3
_B3:
goto _B4
_B4:
_t22 = a
ret _t22
```
死代码消除之后：
```
 _B5:
pop a_1
_t1_1 = a_1
_t2_1 = 10
_t3_1 = _t1_1 < _t2_1
ifz _t3_1 goto _B2:
goto _B1

 _B1:
_t4_1 = a_1
_t5_1 = 5
_t6_1 = _t4_1 + _t5_1
a_2 = _t6_1
_t8_1 = 2
goto _B3

 _B2:
_t13_1 = a_1
_t14_1 = 10
_t15_1 = _t13_1 - _t14_1
a_3 = _t15_1
_t17_1 = 2
goto _B3

 _B3:
a_4 = Phi(a_2, a_3)
goto _B4

 _B4:
_t22_1 = a_4
ret _t22_1
```
我们可以看到基本块B1,B2中的计算b,c部分的代码被消除了。所以在此例子中死代码消除优化能够正常运行，且能够完成其对应的任务。

### 3.2 测试拷贝传播
在下面这个程序中，最返回值e的值来源于a，中间的几个赋值都是冗余的，因此我们希望优化之后的代码应该直接返回a，而中间不进行任何多余的赋值。

```
int main(a:int){
	int b = a;
	int c = b;
	int d = c;
	int e = d;
	return e
}
```
生成的中间代码为：
```
_B2:
pop a
_t1 = a
b = _t1
_t2 = b
c = _t2
_t3 = c
d = _t3
_t4 = d
e = _t4
goto _B1
_B1:
_t5 = e
ret _t5
```
拷贝传播优化之后：
```
 _B2:
pop a_1
goto _B1

 _B1:
ret a_1
```
我们可以看到这段代码的优化效果比较明显，所有中间冗余的赋值都被消除了，将参数从栈中读取出来后，代码直接将其返回。因此这段代码的优化结果和预期的一致。

### 3.3 测试常量折叠
在下面这段代码中，a,b,c,d的值是确定的，其中 a = 7, b = 5, c = 12, d = 19, 最终只有d被使用，因此我们期望代码优化后能够直接返回d的值19，而消除中间的各种计算语句。

```
int main (){
	int b = add(2,3);
	int a = add(3,4);
	int c = add(a,b);
	int d = add(c,a);
	return d
}
```
其中间代码为：
```
_B2:
_t1 = 2
_t2 = 3
_t3 = _t1 + _t2
b = _t3
_t4 = 3
_t5 = 4
_t6 = _t4 + _t5
a = _t6
_t7 = a
_t8 = b
_t9 = _t7 + _t8
c = _t9
_t10 = c
_t11 = a
_t12 = _t10 + _t11
d = _t12
goto _B1
_B1:
_t13 = d
ret _t13
```
优化之后的代码为：
```
 _B2:
goto _B1

 _B1:
ret 19
```
从优化后的结果我们可以看到：这段代码的优化效果也是比较明显的，通过常量折叠，我们消除了一些计算，在编译阶段就将常量之间的计算求出来并将结果传递到了相应的位置。

### 3.4 测试静态单赋值
我们通过下面这段代码来测试将IR转化到静态单赋值形式以及消除phi函数的过程是否正确：
```
int main(a:int, b:int){ 
	int x = a; 
	int y = b; 
	int z = add(a,b); 
	while(lt(x,y)){ 
		if(lt(x,50)){ 
			z = add(z,x) 
		}else{ 
			z = mul(z,y) 
		}; 
		x = add(x,5); 
		int t = add(x,y) 
	}; 
	return z 
}
```
下面为转换后的静态单赋值形式的IR，我们可以看到，每个变量只被定义一次，且变量z_2, x_2, z_5都被正确的插入了phi函数：
```

 _B9:
pop a_1
pop b_1
_t5_1 = a_1 + b_1
goto _B1

 _B1:
z_2 = Phi(z_5, _t5_1)
x_2 = Phi(_t20_1, a_1)
_t8_2 = x_2 < b_1
goto _B2

 _B2:
ifz _t8_2 goto _B4:
goto _B3

 _B3:
_t11_1 = x_2 < 50
ifz _t11_1 goto _B6:
goto _B5

 _B5:
_t14_1 = z_2 + x_2
goto _B7

 _B6:
_t17_1 = z_2 * b_1
goto _B7

 _B7:
z_5 = Phi(_t14_1, _t17_1)
_t20_1 = x_2 + 5
goto _B1

 _B4:
goto _B8

 _B8:
ret z_2

```
进行优化之后再消除phi函数之后的代码为：
```
 _B9:
pop a_1
pop b_1
_t5_1 = a_1 + b_1
z_2 = _t5_1
x_2 = a_1
goto _B1

 _B1:
_t8_2 = x_2 < b_1
goto _B2

 _B2:
ifz _t8_2 goto _B4:
goto _B3

 _B3:
_t11_1 = x_2 < 50
ifz _t11_1 goto _B6:
goto _B5

 _B5:
_t14_1 = z_2 + x_2
z_5 = _t14_1
goto _B7

 _B6:
_t17_1 = z_2 * b_1
z_5 = _t17_1
goto _B7

 _B7:
_t20_1 = x_2 + 5
z_2 = z_5
x_2 = _t20_1
goto _B1

 _B4:
goto _B8

 _B8:
ret z_2
```
从上面的代码中我们可以看到，phi函数被正确的消除，例如 z_5 = Phi(_t14_1, _t17_1)，这个语句，被转化为 z_5 = _t14_1 与 z_5 = _t17_1，并分别插入到了其前驱节点B5与B6中了。


从上面几个例子我们可以看出，中间代码IR经过转化为SSA IR与不同优化之后代码体积得到了一定的缩减，我们的优化算法是有效果的。



----------

## 4. 代码编译与运行
本项目放在了Github上，地址为：https://github.com/kcviuas/TinyCompiler。本项目是用OCaml语言编写的，因此需要先安装配置OCaml环境才能编译运行，下面我们分别们给出Ubuntu和MacOS下OCaml的安装，环境配置和代码编译与运行的方法：

### Ubuntu:
```
添加软件源
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update

安装ocaml, opam(OCaml包管理器), m4
sudo apt-get install ocaml
sudo apt-get install opam
sudo apt-get install m4
 
 
初始化opam 
opam init
eval `opam config env`
 
安装OCaml的核心库, menhir(parser generator)等
sudo opam install core
sudo opam install core_extended
sudo opam install core_bench
sudo opam install menhir


-------------

下载源代码：
git clone https://github.com/kcviuas/TinyCompiler.git

进入工程目录并编译：
cd TinyCompiler/
make


-------------

运行代码：

编译之后会在工程目录下产生一个 compile.native 的二进制可执行文件，该文件就是编译器，
在工程目录下面提供了5个测试样例，分别在test1, test2, ... test5目录下面，下面我们以test1为例：

test下的test1.lang是本项目所定义的语言的源代码文件，运行下面这行命令会在test目录下产生一个名为test1.lang.FINAL的文件，它是源文件经过编译和优化之后产生的最终结果(中间代码)：
./compile.native test/test1.lang


如果想看每一步优化之后的结果，可以在命令中添加 -all 选项：
./compile.native test/test1.lang -all
这样就回输出一些中间结果：
test1.lang.IR(编译后的中间代码)
test1.lang.SSA(SSA形式的中间代码)
test1.lang.ELIM(执行死代码消除之后的结果)
test1.lang.OPT(执行3个优化之后的结果)
test1.lang.FINAL(消除phi函数之后的最终结果)。
```

### MacOS
```
安装ocaml, opam(OCaml包管理器)
brew install ocaml
brew install opam
 
 
初始化opam 
opam init
eval `opam config env`
 
安装OCaml的核心库, menhir(parser generator)等
opam install core
opam install core_extended
opam install core_bench
opam install menhir


-------------

下载源代码：
git clone https://github.com/kcviuas/TinyCompiler.git

进入工程目录并编译：
cd TinyCompiler/
make


-------------

运行代码：

编译之后会在工程目录下产生一个 compile.native 的二进制可执行文件，该文件就是编译器，
在工程目录下面提供了5个测试样例，分别在test目录下面，下面我们以test1为例：

test下的test1.lang是本项目所定义的语言的源代码文件，运行下面这行命令会在test1目录下产生一个名为test1.lang.FINAL的文件，它是源文件经过编译和优化之后产生的最终结果(中间代码)：
./compile.native test/test1.lang


如果想看每一步优化之后的结果，可以在命令中添加 -all 选项：
./compile.native test/test1.lang -all
这样就回输出一些中间结果：
test1.lang.IR(编译后的中间代码)
test1.lang.SSA(SSA形式的中间代码)
test1.lang.ELIM(执行死代码消除之后的结果)
test1.lang.OPT(执行3个优化之后的结果)
test1.lang.FINAL(消除phi函数之后的最终结果)。
```


