If you are OK with version 11 of ecl then you can do

```shell
    sudo apt-get install ecl
```

If you want to compile the current (i.e. 13.5.1 at time of writing this document) you'll
to do a little bit more.

I added the the following Raspbian packages to my Raspbian deployment based on looking at what
was required to install the old version 11 debian package.

```shell
    sudo apt-get install libgmp-dev
    sudo apt-get install libgmp3-dev
    sudo apt-get install libatomic-ops-dev
    sudo apt-get install libncurses5-dev
    sudo apt-get install libreadline-dev
    sudo apt-get install libffi-dev
```


Download git repo of [ecl](http://ecls.sourceforge.net/download.html)

```shell
    git clone git://git.code.sf.net/p/ecls/ecl ecl
    cd ecl # we'll be compiling in this directory.
```


Run configure with the following options

```shell
    ./configure --enable-unicode=yes "CFLAGS=-mcpu=arm1176jzf-s -DAO_USE_PTHREAD_DEFS"
```

I found a CLAGS line suggested at [Holger's Blog](http://blog.hdurer.net/posts/2012/07/23_building-my-own-ecl-for-and-on-the-raspberry-pi.html). Article is dated as 2012. I tried to compile without it but ran int to problems when it came to the GC modules being compiled.

Next it was the usual Unix build and install sequence (takes a long time to compile).

```shell
    make; sudo make install
```

Confirm ecl's install location, run some toy lisp code then changed my shell to use ecl (e.g. 
chsh rsdoiel /usr/local/bin/ecl).



