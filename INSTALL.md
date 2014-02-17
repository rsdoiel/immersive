immersive
=========

The goal is to get an immersive Lisp environment up and running on a Raspberry Pi Model B.
This would start out with a simple replacement shell based on *ecl*'s REPL.  Eventually
it would be cool to a graphical UI going. That seems a long way off right now so the 
first step is setting things up so I can hack happily in Lisp.

My choices

+ ecl for Lisp and a REPL
+ vim for editor (EMACS/SLIME seems like overkill on a Raspberry Pi)
+ Some sort of readline support (e.g. *rlwrap*)
+ Quicklisp for packaging
+ Basic mapping of common Unix commands. E.g.
    - pwd
    - cd
    - mkdir
    - git
    - ls
    - bash
    - shell

# Dev setup

## Step 1 - build and install *ecl*

Here is a script I used to setup my dev environment on a fresh Raspbbery Pi. You can
run it from your user account. I recommend making sure the *pi* or *root* accounts remain
available incease you break something while doing development.
 
The following will only need to be done once to get the right Raspbian Packages installed.

```shell
    # Basic tools
    sudo apt-get update
    sudo apt-get install git-core curl build-essential openssl libssl-dev
    sudo apt-get install autoconf # used when we get to adding support for stumpwm
    # Helpful libraries
    sudo apt-get install libgmp-dev
    sudo apt-get install libgmp3-dev
    sudo apt-get install libatomic-ops-dev
    sudo apt-get install libncurses5-dev
    sudo apt-get install libreadline-dev
    sudo apt-get install libffi-dev
    # We'l use rlwrap to add command editing and history to ecl
    sudo apt-get install rlwrap
```

Next fetch and build *ecl*.

```shell
    sudo mkdir -p /usr/local/src
    sudo chown $USER /usr/local/src
    cd /usr/local/src
    git clone git://git.code.sf.net/p/ecls/ecl
    cd ecl # change to the ecl directory cloned in /usr/local/src/ecl
    ./configure --prefix=/usr/local --enable-unicode=yes \
      --with-clx=yes \
      --with-asdf=yes \
      --with-x=yes \
      "CFLAGS=-mcpu=arm1176jzf-s -DAO_USE_PTHREAD_DEFS"
    make
    sudo make install
```

It takes about ninety minutes to compile on my Raspberry Pi Model B without the clx and X
options.  Thirty minutes just to compile the *C* code and the rest for compiling
the *Lisp*. Adding the clx and X options made it take about twice that.

Normally @/usr/local/bin@ is in your @$PATH@. If not you may need to add it.

```shell
    export PATH="/usr/local/bin:$PATH"
```

We can now execute *ecl* from the command line at @/usr/local/bin/ecl@. Your Lisp
is now installed.

## Step 2 - adding Adding Quicklisp

1. Download quicklisp.lisp
2. Launch _ecl_ and load quicklisp.lisp
3. exit _ecl_ and clone the immersive repo in _quicklisp/local-projects_


```shell
    curl -O http://beta.quicklisp.org/quicklisp.lisp
    ecl
    (load "quicklisp.lisp")
    (quicklisp-quickstart:install)
    (ql:add-to-init-file)
    (quit)
```

## Step 3 - loading immersive


1. Restart _ecl_ then use _ql:quickload_ to add _immersive_
2. switch to the immersive namespace and play with immersive
3. go back to the cl-user namespace and quit

```shell
    cd quicklisp/local-projects
    git clone https://github.com/rsdoiel/immersive.git
    cd
    ecl
```

Now load _immersive_ and expose the symbols to COMMON-LISP-USER.

```lisp
    (ql:quickload "immersive")
    (use-package :immersive)
    ;; play around with lisp and immersive
```

In _.eclrc_ I've added these two lines lines so _immersive_ is ready when I start _ecl_.


Immersive's extensions to *ECL*'s repl should now be available. Note this doesn't give you
command line history and editing. For that we need to use something like *rlwrap*.

## Optional, Step 4 - add commnad line editing and history with rlwrap

This part is a kludge but it helpful until *immersive* has its own repl.  In /etc/profile.d/ 
folder I create a shell script to be included by /etc/profile.  I'm calling it ecl-user.sh for
lack of a better name. It's job is to find out where *ecl* is installed, setup of the
break characters for *rlwrap* and create a new alias _ecl_ that calls *ecl* via *rlwrap*.

Here's what my */etc/profile.d/ecl-user.sh* looks like.

```shell
    #!/bin/bash
    export ECLS=$(which ecl)
    export ECLS_BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
    alias ecl="rlwrap -b \$ECLS_BREAK_CHARS $ECLS"
```

