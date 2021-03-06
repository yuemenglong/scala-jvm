# scala-jvm
一个用scala写的jvm的简单实现
## 内容包括：
* class文件的解析和jar包的解析
* 所有基本数据类型
* 所有201条jvm指令的实现
* 常量池
* 堆
* 栈
* 局部变量表
* 接口与继承
* 反射机制
* 线程
* 锁同步机制
* 垃圾回收机制
* 方法的动态链接
* 类的运行时动态加载
* 部分native方法
## 实现原理
### 基本数据类型
直接使用scala的基本数据类型
### 对象
简化成每个对象就是一个经过封装的Map[String, Any]，读写字段就等于对Map做get和set操作
### 数组
java的数组比较特殊，没有.class文件与数组对应，需要在运行时动态产生并缓存
### 堆
为简单起见，没有分年轻代，老年代，全部用一个Map存起来
### 栈
用一个数组模拟，一个上下文内的所有栈帧共享一个栈，每个栈帧有自己独立的pc(程序计数器)和vt(局部变量表)
### 接口与继承
其实只要实现了一套方法与字段的查找算法就等于实现了接口与继承的功能
### 线程
并没有将每个java线程映射到一个操作系统线程上去，而是通过切换上下文实现程序流程的切换，相当于用户态线程或者协程的概念
### 锁同步
因为没有真正的线程而是协程实现，锁的实现就比较简单了，通过一个引用计数器+线程id的对象进行临界区的准入判断
### 垃圾回收
由于没有分年轻代与老年代，使用扫描标记算法，遍历整个堆上的对象，把没有引用的对象remove出堆。
为了简单起见，若干个指令周期后进行垃圾回收
## 不能做什么
由于除了反射、线程等相关的外，还有大量native方法没有实现(主要因为懒)，因此依赖native方法的事儿都做不了，比如输入输出、文件读写、网络调用等(这些本质上属于库函数的范畴而不是语言的内容)。
数值计算、面向对象等只依赖虚拟机指令的操作都能完成
## demo

    def main(args: Array[String]): Unit = {
        Vm.rt.clazzpath(s"${sys.env("JAVA_HOME")}/jre/lib/rt.jar") // 引入java运行时包
        Vm.rt.clazzpath("target/test-classes")                     // 引入自己写的class文件
        val lib = Vm.rt.load("io/github/yuemenglong/jvm/Java")     // 加载main方法对应的类
        Vm.run(lib.main())                                         // 执行main方法
    }
## output
可以看到类似下面的输出，包括线程号，字节码指令，堆栈和局部变量表的变化等，方便学习研究

        [Java:main] [0] iconst_1
            [3|Pc:main] 1
            [Local-None]
            [Stack-Size] 1
            [Stack] 1
        [Java:main] [1] istore_1
            [3|Pc:main] 2
            [Local] [1] 1
            [Stack-Size] 0
            [Stack-None]
        [Java:main] [2] bipush 10
            [3|Pc:main] 4
            [Local] [1] 1
            [Stack-Size] 1
            [Stack] 10
        [Java:main] [4] iload_1
            [3|Pc:main] 5
            [Local] [1] 1
            [Stack-Size] 2
            [Stack] 10
            [Stack] 1
        [Java:main] [5] iadd
            [3|Pc:main] 6
            [Local] [1] 1
            [Stack-Size] 1
            [Stack] 11
        [Java:main] [6] i2d
            [3|Pc:main] 7
            [Local] [1] 1
            [Stack-Size] 1
            [Stack] 11.0
        [Java:main] [7] dstore_2
            [3|Pc:main] 8
            [Local] [1] 1
            [Local] [2] 11.0
            [Stack-Size] 0
            [Stack-None]
        [Java:main] [8] dload_2
            [3|Pc:main] 9
            [Local] [1] 1
            [Local] [2] 11.0
            [Stack-Size] 1
            [Stack] 11.0
        [Java:main] [9] ldc2_w [Double] 2.0
            [3|Pc:main] 12
            [Local] [1] 1
            [Local] [2] 11.0
            [Stack-Size] 2
            [Stack] 11.0
            [Stack] 2.0
        [Java:main] [12] invokestatic [Methodref] io/github/yuemenglong/jvm/Java (DD)D sub
            [3|Pc:sub] 0
            [Local] [0] 11.0
            [Local] [2] 2.0
            [Stack-Size] 0
            [Stack-None]
        [Java:sub] [0] dload_0
            [3|Pc:sub] 1
            [Local] [0] 11.0
            [Local] [2] 2.0
            [Stack-Size] 1
            [Stack] 11.0
