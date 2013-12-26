
Download git repo of [ecl](http://ecls.sourceforge.net/download.html)

```shell
    git clone git://git.code.sf.net/p/ecls/ecl ecl
    git clone git://git.code.sf.net/p/ecls/ecl-doc ecl-doc
```

Add the following Raspbian package

```shell
    sudo apt-get install rlwrap # (not explicitly needed but handy)
    sudo apt-get install libffi-dev
    sudo apt-get install libreadline-fev
    sudo apt-get install libgc-dev
```

Run configure with the following options

```shell
    ./configure --prefix=/usr/local \
    --enable-boehm=auto \
    --enable-gengc=yes \
    --enable-precisegc=yes \
    --enable-unicode=yes \
    --with-profile=yes \
    --with-__thread \
    --with-unicode-names=yes \
    --with-asdf \
    --with-profile \
    --with-dffi=auto \
```

Install on Raspbian system (takes a long time to compile)

```shell
    echo "START buid $(date)" >> build-time.txt
    make clean; make;sudo make install
    echo "END build $(date)" >> build-time.txt
```

Confirm ecl's install location and change your shell to it.

