# Building and Installing LFE

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 18.04.1 LTS x86-64)](#building-under-ubuntu-server-ubuntu-18041-lts-x86-64)
  * [Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)](#building-under-arch-linux-kernel-41613-2-arch-x86-64)
* **[Running](#running)**
  * [Running on OpenBSD/amd64 6.3](#running-on-openbsdamd64-63)
  * [Running on Ubuntu Server (Ubuntu 18.04.1 LTS x86-64)](#running-on-ubuntu-server-ubuntu-18041-lts-x86-64)
  * [Running on Arch Linux (kernel 4.16.13-2-ARCH x86-64)](#running-on-arch-linux-kernel-41613-2-arch-x86-64)

## Building

### Building under OpenBSD/amd64 6.3

```
$ mkdir lfe
$ cd lfe/
$ git clone https://github.com/rvirding/lfe.git
Cloning into 'lfe'...
...
```

```
$ cd lfe/
$ gmake clean && gmake clean
which: mandb: Command not found.
escript get_comp_opts.escript
which: mandb: Command not found.
rm -rf ebin/*.beam erl_crash.dump comp_opts.mk
which: mandb: Command not found.
escript get_comp_opts.escript
which: mandb: Command not found.
rm -rf ebin/*.beam erl_crash.dump comp_opts.mk
```

```
$ gmake install PREFIX=/home/<username>/ MANINSTDIR=/home/<username>/man
which: mandb: Command not found.
escript get_comp_opts.escript
which: mandb: Command not found.
gmake  erlc-lfec
gmake[1]: Entering directory '/home/<username>/lfe/lfe'
which: mandb: Command not found.
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_eval.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_init.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_ms.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_trans.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_qlc.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_gen.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_export.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_lib.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_bits.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_edlin_expand.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_parse.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfescript.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_record.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_codegen.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_include.erl
src/lfe_macro_include.erl:306: Warning: variable 'Line' is unused
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_pmod.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_internal.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_abstract_code.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_env.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_comp.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_format.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_doc.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_lint.erl
src/lfe_lint.erl:1501: Warning: function add_errors/3 is unused
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_types.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_write.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_pretty.erl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_shell.erl
erlc -o src src/lfe_scan.xrl
erlc -I include -o ebin -DERLANG_VERSION=\"19.3\" -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_scan.erl
cc -o bin/lfeexec c_src/lfeexec.c
/tmp/lfeexec-2e4b5f.o: In function `main':
c_src/lfeexec.c:(.text+0x7f): warning: sprintf() is often misused, please use snprintf()
cp src/lfe.app.src ebin/lfe.app
bin/lfe bin/lfec -I include -o ebin -pa ../lfe +debug-info src/clj.lfe
bin/lfe bin/lfec -I include -o ebin -pa ../lfe +debug-info src/cl.lfe
rm src/lfe_scan.erl
gmake[1]: Leaving directory '/home/<username>/lfe/lfe'
rm -Rf /home/<username>//lib/lfe/ebin
install -m755 -d /home/<username>//lib/lfe/ebin
install -m644 \
        ebin/lfe.app \
        ebin/lfe.beam ebin/lfe_eval.beam ebin/lfe_init.beam ebin/lfe_ms.beam ebin/lfe_trans.beam ebin/lfe_qlc.beam ebin/lfe_io.beam ebin/lfe_gen.beam ebin/lfe_macro_export.beam ebin/lfe_lib.beam ebin/lfe_bits.beam ebin/lfe_edlin_expand.beam ebin/lfe_parse.beam ebin/lfescript.beam ebin/lfe_macro_record.beam ebin/lfe_codegen.beam ebin/lfe_macro_include.beam ebin/lfe_pmod.beam ebin/lfe_internal.beam ebin/lfe_abstract_code.beam ebin/lfe_env.beam ebin/lfe_comp.beam ebin/lfe_io_format.beam ebin/lfe_doc.beam ebin/lfe_lint.beam ebin/lfe_types.beam ebin/lfe_io_write.beam ebin/lfe_macro.beam ebin/lfe_io_pretty.beam ebin/lfe_shell.beam ebin/lfe_scan.beam \
        ebin/clj.beam ebin/cl.beam \
        /home/<username>//lib/lfe/ebin
install -m755 -d /home/<username>//lib/lfe/bin
install -m755 \
        bin/lfe \
        bin/lfec \
        bin/lfedoc \
        bin/lfescript \
        /home/<username>//lib/lfe/bin
install -m755 -d /home/<username>//bin
ln -sf /home/<username>//lib/lfe/bin/* /home/<username>//bin/
install -m644 doc/man/*.1 /home/<username>/man/man1/
install -m644 doc/man/*.3 /home/<username>/man/man3/
install -m644 doc/man/*.7 /home/<username>/man/man7/
```

### Building under Ubuntu Server (Ubuntu 18.04.1 LTS x86-64)

```
$ mkdir lfe
$ cd lfe/
$ git clone https://github.com/rvirding/lfe.git
Cloning into 'lfe'...
...
```

```
$ cd lfe/
$ make clean && make clean
rm -rf ebin/*.beam erl_crash.dump comp_opts.mk
escript get_comp_opts.escript
rm -rf ebin/*.beam erl_crash.dump comp_opts.mk
```

```
$ make install PREFIX=/home/<username>/ MANINSTDIR=/home/<username>/man
escript get_comp_opts.escript
make  erlc-lfec
make[1]: Entering directory '/sda2/home/<username>/lfe/lfe'
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_shell.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_pretty.erl
src/lfe_io_pretty.erl:23: Warning: export_all flag enabled - all functions will be exported
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_write.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_lint.erl
src/lfe_lint.erl:1501: Warning: function add_errors/3 is unused
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_doc.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_format.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_comp.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_ms.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_gen.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_abstract_code.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_internal.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_pmod.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_include.erl
src/lfe_macro_include.erl:306: Warning: variable 'Line' is unused
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_codegen.erl
src/lfe_codegen.erl:33: Warning: export_all flag enabled - all functions will be exported
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfescript.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_parse.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_record.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_edlin_expand.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_bits.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_lib.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_export.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_env.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_qlc.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_trans.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_types.erl
src/lfe_types.erl:37: Warning: export_all flag enabled - all functions will be exported
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_init.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_eval.erl
erlc -I include -o ebin -DERLANG_VERSION=\"20.2.2\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io.erl
erlc -o src src/lfe_scan.xrl
Compiler function leex:compile/3 failed:
{undef,[{leex,compile,
              ["/sda2/home/<username>/lfe/lfe/src/lfe_scan",
               "/sda2/home/<username>/lfe/lfe/src/lfe_scan",
               {options,[],"/sda2/home/<username>/lfe/lfe/src",undefined,
                        [],1,false,999,[],[],
                        "/sda2/home/<username>/lfe/lfe"}],
              []},
        {erl_compile,compile_file,4,[{file,"erl_compile.erl"},{line,304}]},
        {erl_compile,compile3,3,[{file,"erl_compile.erl"},{line,285}]},
        {erl_compile,'-compiler_runner/1-fun-1-',1,
                     [{file,"erl_compile.erl"},{line,92}]}]}
Makefile:81: recipe for target 'src/lfe_scan.erl' failed
make[1]: *** [src/lfe_scan.erl] Error 1
make[1]: Leaving directory '/sda2/home/<username>/lfe/lfe'
Makefile:94: recipe for target 'compile' failed
make: *** [compile] Error 2
```

### Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)

```
$ mkdir lfe
$ cd lfe/
$ git clone https://github.com/rvirding/lfe.git
Cloning into 'lfe'...
...
```

```
$ cd lfe/
$ make clean && make clean
escript get_comp_opts.escript
rm -rf ebin/*.beam erl_crash.dump comp_opts.mk
escript get_comp_opts.escript
rm -rf ebin/*.beam erl_crash.dump comp_opts.mk
```

```
$ make install PREFIX=/home/<username>/ MANINSTDIR=/home/<username>/man
escript get_comp_opts.escript
make  erlc-lfec
make[1]: Entering directory '/home/<username>/lfe/lfe'
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_shell.erl
src/lfe_shell.erl:366: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_pretty.erl
src/lfe_io_pretty.erl:23: Warning: export_all flag enabled - all functions will be exported
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro.erl
src/lfe_macro.erl:606: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
src/lfe_macro.erl:626: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_write.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_types.erl
src/lfe_types.erl:37: Warning: export_all flag enabled - all functions will be exported
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_lint.erl
src/lfe_lint.erl:1501: Warning: function add_errors/3 is unused
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_doc.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io_format.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_comp.erl
src/lfe_comp.erl:96: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_gen.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_abstract_code.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_internal.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_pmod.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_include.erl
src/lfe_macro_include.erl:306: Warning: variable 'Line' is unused
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_codegen.erl
src/lfe_codegen.erl:33: Warning: export_all flag enabled - all functions will be exported
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfescript.erl
src/lfescript.erl:62: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
src/lfescript.erl:188: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_parse.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_record.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_edlin_expand.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_bits.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_lib.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_macro_export.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_env.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_io.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_qlc.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_trans.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_ms.erl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_init.erl
src/lfe_init.erl:71: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_eval.erl
src/lfe_eval.erl:736: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
src/lfe_eval.erl:793: Warning: erlang:get_stacktrace/0: deprecated; use the new try/catch syntax for retrieving the stack backtrace
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe.erl
erlc -o src src/lfe_scan.xrl
erlc -I include -o ebin -DERLANG_VERSION=\"21.0.9\" -DNEW_BOOL_GUARD=true -DNEW_RAND=true -DNEW_REC_CORE=true -DHAS_FULL_KEYS=true -DHAS_MAPS=true -W1 +debug_info src/lfe_scan.erl
cc -o bin/lfeexec c_src/lfeexec.c
cp src/lfe.app.src ebin/lfe.app
bin/lfe bin/lfec -I include -o ebin -pa ../lfe +debug-info src/cl.lfe
bin/lfe bin/lfec -I include -o ebin -pa ../lfe +debug-info src/clj.lfe
rm src/lfe_scan.erl
make[1]: Leaving directory '/home/<username>/lfe/lfe'
rm -Rf /home/<username>//lib/lfe/ebin
install -m755 -d /home/<username>//lib/lfe/ebin
install -m644 \
        ebin/lfe.app \
        ebin/lfe_shell.beam ebin/lfe_io_pretty.beam ebin/lfe_macro.beam ebin/lfe_io_write.beam ebin/lfe_types.beam ebin/lfe_lint.beam ebin/lfe_doc.beam ebin/lfe_io_format.beam ebin/lfe_comp.beam ebin/lfe_gen.beam ebin/lfe_abstract_code.beam ebin/lfe_internal.beam ebin/lfe_pmod.beam ebin/lfe_macro_include.beam ebin/lfe_codegen.beam ebin/lfescript.beam ebin/lfe_parse.beam ebin/lfe_macro_record.beam ebin/lfe_edlin_expand.beam ebin/lfe_bits.beam ebin/lfe_lib.beam ebin/lfe_macro_export.beam ebin/lfe_env.beam ebin/lfe_io.beam ebin/lfe_qlc.beam ebin/lfe_trans.beam ebin/lfe_ms.beam ebin/lfe_init.beam ebin/lfe_eval.beam ebin/lfe.beam ebin/lfe_scan.beam \
        ebin/cl.beam ebin/clj.beam \
        /home/<username>//lib/lfe/ebin
install -m755 -d /home/<username>//lib/lfe/bin
install -m755 \
        bin/lfe \
        bin/lfec \
        bin/lfedoc \
        bin/lfescript \
        /home/<username>//lib/lfe/bin
install -m755 -d /home/<username>//bin
ln -sf /home/<username>//lib/lfe/bin/* /home/<username>//bin/
Updating man page database ...
/usr/bin/mandb /home/<username>/man
Processing manual pages under /home/<username>/man...
Updating index cache for path `/home/<username>/man/man3'. Wait...done.
Checking for stray cats under /home/<username>/man...
Processing manual pages under /home/<username>/man/cat7...
Processing manual pages under /home/<username>/man/cat1...
Processing manual pages under /home/<username>/man/cat3...
3 man subdirectories contained newer manual pages.
0 manual pages were added.
0 stray cats were added.
0 old database entries were purged.
install -m644 doc/man/*.1 /home/<username>/man/man1/
install -m644 doc/man/*.3 /home/<username>/man/man3/
install -m644 doc/man/*.7 /home/<username>/man/man7/
```

## Running

### Running on OpenBSD/amd64 6.3

```
$ lfe
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/rvirding/lfe
   \     l    |_/    |
    \   r     /      |   LFE v1.3-dev (abort with ^G)
     `-E___.-'

lfe>
```

```
$ lfec
Usage: lfec [options] file ...

Options:
-h             Print usage and exit
-I name        Name of include directory
-o name        Name of output directory
-pa path       Add path to the front of LFE's code path
-pz path       Add path to the end of LFE's code path
-v             Verbose compiler output
-Werror        Make all warnings into errors
-Wnumber       Set warning level (ignored)
-D             Equivalent to +debug-print
-L             Equivalent to +to-lint
-E             Equivalent to +to-exp
-S             Equivalent to +to-asm
--             No more options, only file names follow
+term          Term will be added to options

Terms include:

+binary, +no-docs, +to-exp, +to-lint, +to-core0, +to-core, +to-kernel, +to-asm
+{outdir, Dir}, +report, +return, +debug-print
```

```
$ lfedoc
Usage: lfedoc [options] file

Options:
-h             Print usage and exit
-I name        Name of include directory
-pa path       Add path to the front of LFE's code path
-pz path       Add path to the end of LFE's code path
-v             Verbose compiler output
-Werror        Make all warnings into errors
-Wnumber       Set warning level (ignored)
-D             Equivalent to +debug-print
--             No more options, only file names follow
+term          Term will be added to options

Terms include:

+binary, +no-docs, +{outdir, Dir}, +report, +return, +debug-print
```

```
$ lfescript
lfescript: Missing filename
```

### Running on Ubuntu Server (Ubuntu 18.04.1 LTS x86-64)

```
$
```

### Running on Arch Linux (kernel 4.16.13-2-ARCH x86-64)

```
$ lfe
Erlang/OTP 21 [erts-10.0.8] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/rvirding/lfe
   \     l    |_/    |
    \   r     /      |   LFE v1.3-dev (abort with ^G)
     `-E___.-'

lfe>
```

```
$ lfec
Usage: lfec [options] file ...

Options:
-h             Print usage and exit
-I name        Name of include directory
-o name        Name of output directory
-pa path       Add path to the front of LFE's code path
-pz path       Add path to the end of LFE's code path
-v             Verbose compiler output
-Werror        Make all warnings into errors
-Wnumber       Set warning level (ignored)
-D             Equivalent to +debug-print
-L             Equivalent to +to-lint
-E             Equivalent to +to-exp
-S             Equivalent to +to-asm
--             No more options, only file names follow
+term          Term will be added to options

Terms include:

+binary, +no-docs, +to-exp, +to-lint, +to-core0, +to-core, +to-kernel, +to-asm
+{outdir, Dir}, +report, +return, +debug-print
```

```
$ lfedoc
Usage: lfedoc [options] file

Options:
-h             Print usage and exit
-I name        Name of include directory
-pa path       Add path to the front of LFE's code path
-pz path       Add path to the end of LFE's code path
-v             Verbose compiler output
-Werror        Make all warnings into errors
-Wnumber       Set warning level (ignored)
-D             Equivalent to +debug-print
--             No more options, only file names follow
+term          Term will be added to options

Terms include:

+binary, +no-docs, +{outdir, Dir}, +report, +return, +debug-print
```

```
$ lfescript
lfescript: Missing filename
```

---
:cd:
