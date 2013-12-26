Download git repo of [ecl](http://ecls.sourceforge.net/download.html)

```shell
    git clone git://git.code.sf.net/p/ecls/ecl ecl
    git clone git://git.code.sf.net/p/ecls/ecl-doc ecl-doc
```

Add the following Raspbian package

```shell
    sudo apt-get install libreadline-dev
    sudo apt-get install libgc-dev
    sudo apt-get install libffi-dev
```

Run configure with the following options

```shell
    cd ecl # if you're not already in the ecl source directory
    ./configure --prefix=/usr/local \
    --enable-unicode=yes \
    "CFLAGS=-mcpu=arm1176jzf-s -DAO_USE_PTHREAD_DEFS"
```

Install on Raspbian system (takes a long time to compile)

```shell
    make
    sudo make install
```

Confirm ecl's install location and change your shell to it.

