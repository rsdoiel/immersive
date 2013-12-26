If you are OK with version 11 of ecl then you can do

```shell
    sudo apt-get install ecl
```

If you want to compile the current (i.e. 13.5.1 at time of writing this document) you'll
need to follow my notes below.


Download git repo of [ecl](http://ecls.sourceforge.net/download.html)

```shell
    git clone git://git.code.sf.net/p/ecls/ecl-doc ecl-doc
    cd ecl # we'll be compiling in this directory.
```

I added the the following Raspbian packages to my Raspbian deployment.

```shell
    sudo apt-get install libgmp-dev
    sudo apt-get install libgmp3-dev
    sudo apt-get install libncurses5-dev
    sudo apt-get install libreadline-dev
    sudo apt-get install libgc-dev
    sudo apt-get install libffi-dev
```

Run configure with the following options

```shell
    cd ecl # if you're not already in the ecl source directory
    ./configure --prefix=/usr/local
```

I found a CLAGS line suggested at [Holger's Blog](http://blog.hdurer.net/posts/2012/07/23_building-my-own-ecl-for-and-on-the-raspberry-pi.html). Article is
dated as 2012. It sounds like this is needed to get threading support on an older version of ecl (13.5.1 was announced 2013-05-28). I didn't use it as I wasn't particularly concerned about threads for my tests.

Install on Raspbian system (takes a long time to compile)

```shell
    make; sudo make install
```

Confirm ecl's install location, run some toy lisp code then changed my shell to use ecl (e.g. 
chsh rsdoiel /usr/local/bin/ecl).



