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

I found a CLAGS line suggested at [Holger's Blog](http://blog.hdurer.net/posts/2012/07/23_building-my-own-ecl-for-and-on-the-raspberry-pi.html). Article is dated as 2012-07-23. I tried to compile without it but ran in to problems.


Using the ecl gc system was the way to go.

Next it was the usual Unix build and install sequence (takes a while time to compile on Raspberry Pi Model B,
aprox. 90 minutes).


```shell
    make; sudo make install
```

Confirm ecl's install location, run some toy lisp code then changed my shell to use ecl (e.g. 
chsh rsdoiel /usr/local/bin/ecl).

Next add ECL to the list of shells available on the system. This is done by modifying
/etc/shells. Since I installed ecl in /usr/local on my machine I added the line "/usr/local/bin/ecl" to
/etc/shells. You can then use the _chsh_ command to set your shell to ecl.
 

# After compile

Log into the account you're planning to use to run your Lisps from.  Then use the Unix command _chsh_
to switch your shell to ecl. Log out then log in again and you should find yourself at the ecl prompt.

Using the "ed" command edit your init file (e.g. .eclrc file for _ecl_ lisp) to load "user-shell.lisp".
This adds two commands - (bash) and (shell).  We can use these for the next steps of bootstraping things.

## Adding Quicklisp

Quicklisp instructions at [http://www.quicklisp.org/beta/](http://www.quicklisp.org/beta/).
We'll rely on _ecl_'s built in [asdf](http://common-lisp.net/project/asdf/#downloads). You will
need to make sure your account and _rwx_ privilleges for those directories and files.

```shell
   ;; You can create user-shell.lisp by using the (ed "user-lisp.lisp") command and
   ;; pasting in user-lisp.lisp from the git repo.
   (ed "user-shell.lisp")
   (load "user-shell.lisp")
   
   ;; Now get Quicklisp and install it locally to the account
   (shell "curl -O http://beta.quicklisp.org/quicklisp.lisp")
   ;; Now load quicklisp.lisp to go through install steps
   (load "quicklisp.lisp")
   ;; Follow the quicklisp instructions from here.
```



## Misc 

Early on in my compilation trials I had the debian package libgc-dev installed. This turned out to cause problems.
I removed the pkg (sudo apt-get remove libgc-dev libgc1c2 and that solved that problem.)
