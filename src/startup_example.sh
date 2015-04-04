#!/bin/sh


ROOT=~/erlang/Erlang-and-OTP-in-Action-Source/redeem_code

ulimit -n 1048576

erl -pa ../ebin ../deps/*/ebin \
    +K true \
    -P 10240000 \
    -sbt db \
    -sub true \
    -name redeem@127.0.0.1 \
    -s common startup \
    -root_path $ROOT/

    #-s httpecho_app startup \
# -K true | false 是否开启kernel poll，就是epoll；
# -P Number erlang节点系统的最大并发进程数；
# -e Number ETS表的最大数量；
# -s 要执行的命令
# -sbt db 调度器默认选择
# -sub true 采用sub调度方式，R17及以上版本erlang支持
# -root_path 执行根目录
# -config  加载的配置文件
# -d 默认情况下erlang进程遇到内部错误，比如oom,会产生一个crash dump和core dump，+d让节点只产生后者；

# -noshell (该参数关闭终端，但依然不会后台运行，有输出时会直接打印到当前屏幕
# -noinput 禁止终端下的输入
# -hidden 隐藏运行
# -detached 后台方式运行   启用此参数，程序会以后台方式运行，你可以通过进程号将其调入到前台。在加入此参数运行服务器后，查看当前正在运行的程序，你会发现这个参数自动被分解成了 -noshell 和 -noinput, 所以，在加入-detached后，就不需要再加这两个参数了。你可以通过进程号将其调入到前台。在加入此参数运行服务器后，查看当前正在运行的程序，你会发现这个参数自动被分解成了 -noshell 和 -noinput, 所以，在加入-detached后，就不需要再加这两个参数了。