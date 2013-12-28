# Dev setup

## Step 1

Here is a script I used to setup my dev environment on a fresh Raspbbery Pi. You can
run it from your user account. I recommend making sure the *Pi* or *root* accounts remain
available incease you break something while doing development.

The following will only need to be done once to get the right Raspbian Packages installed.

```shell
    # Basic tools
    sudo apt-get update
    sudo apt-get install git-core curl build-essential openssl libssl-dev
    # Helpful libraries
    sudo apt-get install libgmp-dev
    sudo apt-get install libgmp3-dev
    sudo apt-get install libatomic-ops-dev
    sudo apt-get install libncurses5-dev
    sudo apt-get install libreadline-dev
    sudo apt-get install libffi-dev
```

Now fetch and build _ecl_.


```shell
  cd # go to the $HOME directory
  git clone git://git.code.sf.net/p/ecls/ecl
  cd ecl # change to the ecl directory cloned in $HOME
  ./configure --prefix=$HOME --enable-unicode=yes \
      --with-clx=yes \
      --with-asdf=yes \
      --with-x=yes \
      "CFLAGS=-mcpu=arm1176jzf-s -DAO_USE_PTHREAD_DEFS"  
```


It takes about ninety minutes to compile on my Raspberry Pi Model B.  Thirty
minitures to compile the *C* code and the rest for compiling the *Lisp* itself.

We can now execute _ecl_ from the command line and get a working Lisp REPL.

## Step 2

Assembling a rich Lisp environment requires adding ASDF and Quicklisp. _ecl_ ships
with ASDF v2. ASDF v3 is now out so we want to upgrade to that. Finally we also
want install Quicklisp to have a modern way to fetch and integrate remote packages.


