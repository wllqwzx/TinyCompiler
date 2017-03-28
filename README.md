
# TinyCompiler

A tiny compiler with IR optimization.
----------

## content:
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


## 编译与运行
本项目是用OCaml语言编写的，因此需要先安装配置OCaml环境才能编译运行，下面我们分别们给出Ubuntu和MacOS下OCaml的安装，环境配置和代码编译与运行的方法：

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


