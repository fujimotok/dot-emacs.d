# NativeCompile a.k.a GCCEmacs for Windows
[参考](https://misohena.jp/blog/2023-07-31-setup-emacs-29-1-for-windows.html)

# ディレクトリ構成
bin/* : emacs.exeのあるディレクトリに置く必要があるファイル。scoopで入れてるなら、~/scoop/emacs/current/bin
lib/* : native-comp-driver-options で "-B" オプションで指定する必要があるファイル

# NativeCompileが有効かどうか
- (native-comp-available-p) が t を返せば有効
- .emacs.d/eln-cache にelnができてたら有効
- *warning* で `as no such file or directory` とか出てるならちゃんと -B オプションが効いてない

# msys2からコピーしてくる
msys64 がルートとして表現

## native-comp-driver-options で指定のdirectoryに置くもの
msys64/mingw64/bin/as.exe
msys64/mingw64/bin/ld.exe
msys64/mingw64/lib/crtbegin.o
msys64/mingw64/lib/crtend.o
msys64/mingw64/lib/dllcrt2.o
msys64/mingw64/lib/libadvapi32.a
msys64/mingw64/lib/libgcc_s.a
msys64/mingw64/lib/libkernel32.a
msys64/mingw64/lib/libmingw32.a
msys64/mingw64/lib/libmingwex.a
msys64/mingw64/lib/libmoldname.a
msys64/mingw64/lib/libmsvcrt.a
msys64/mingw64/lib/libpthread.a
msys64/mingw64/lib/libshell32.a
msys64/mingw64/lib/libuser32.a
msys64/mingw64/lib/gcc/x86_64-w64-mingw32/*.*.*/libgcc.a

## emacs.exe のあるdirectoryに置くもの
msys64/mingw64/bin/libgccjit-0.dll
msys64/mingw64/bin/libisl-23.dll
msys64/mingw64/bin/libmpc-3.dll
msys64/mingw64/bin/libmpfr-6.dll
