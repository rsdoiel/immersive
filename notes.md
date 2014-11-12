
Been thinking about what a proof of concept *worker-node* would look like in for immersive. It would be designed as a function evaluator without direct access to disc or OS services (e.g. opening outbound sockets).  It would receive a Lisp Experssion over https and the evaluated results.  The https service would require authentication for initial access and the lisp environment would persist only for the durration of the request and response cycle. In effect it would be
a read, eval, print transaction. The http content type supported would only be application/x-common-lisp.

Immersive would also support a *storage-node* which sole task would be to store lisp expressions based on a unique url as a key (similar to JSON data stores). The *storage-node* would be the service responsible for persistence.

Immersive would have a *conductor-node* which would route requests to *worker-node*s or *storage-nodes*.  The *conductor-node* would provide a _repl_.

```
    *conductor-node* --> s-exprs --> *worker-node* --+
    *conductor-node* <-- s-exprs --------------------+

    *conductor-node* --> s-exprs --> *storage-node* --+
    *conductor-node* <-- s-exprs --------------------+
```

https would be the transmition protocol between nodes. Access could
be restricted by traditional http authentication mechanisms.

*worker-node* would not persist data so vulnerabilities would be limited to the in-memory running lisp service.

*storage-node* would not evaluate lisp or store experssions in a bucket.  You would need explicit permissions to access the buckets. Ideally the *storage-node* could be defined as a thin layer over somthing like Google Datastore, MongoDB
or other key/value datastorage service. The *storage-node* would support create, replace, delete and exists through GET (read), POST (create/replace), DELETE (delete), and HEAD (expression is stored exists) http methods.

Between the worker nodes, storage nodes and conductor nodes messaging would be valid URLs and s-expressions.

The prototype of this arhitecture could be done in a simpler repl (e.g. immersive-shell). Eventually a GUI version could be imeplemented and finally the clustered version splitting worker and storage nodes among a group of RPi.


----
Need to look at SBCL and see how that now compiles and works on RPi. May need a working copy of clisp to compile.  Immersive needs to work on one CL before I worry about multiple CL.

Alternative approach is to take a Golang based Lisp snd evolve it both towards CL and towards the environment in my head.

-----

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


## Building a SD card image, simple OS, something to host Immersive

MagPi issues 15 (starting page 28) and 20 (starting page 40) have articles on how to build an OS on Raspberry Pi-- "Baking your own Pi filling" by Martin Kalitis.  These articles take you through using a Ubuntu/Debian system to cross compile and build an SD card image.

Cambridge University has a free course material introducing ARM assembly for Rasberry Pi.  Course material is titled [Baking Pi – Operating Systems Development](https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/os/) by [Alex Chadwick](mailto:awc32@cam.ac.uk).  This provides a feel for basic hardware control at the assembly level. Also
includes links to ARM Refeence material and other [Downloadable materials](http://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/os/downloads.html)


Two academic OSes also offer interesting possibilty. [Minix 3](http://minix3.org) is in the process of being ported to Raspberry Pi as well as [xv6](http://pdos.csail.mit.edu/6.828/2012/xv6.html). While the latter is explicitly
targetted at teaching OS implementation (as early Minix) it is a simple enough platform when available to 
Raspberry Pi could host immersive.  Minix 3 also offers interesting possibitilies with its microkernel architecture. In that approach Immersive would just be another "service" available to the Kernel. RISC processors seem well suited
to hosting Lisps.

Finally [LLVM](http://llvm.org) is tantilizing as a means of leveraging existing compiler tools for machine level optimization. The trouble is the virtual machine presented in register oriented rather than stack orient. Not sure that is a show
stopper given recent as well as ancient computer history.





## Random Ideas about what immersive might become

+ Cast a Posix friendly, Unix like set a functions supporting easy composition in Lisp that allow clear interaction with the host operating system (or kernel)
    - ls function would lists files like Unix's *ls* command but in Lisp it should return the listing as a set of file paths on the file system; command line options should map sensibly to optional lisp key parameters or optional parameters (e.g. -l might be :details true other something)
    - cp would be a function that copies files; while the side effect would be the desired result like print type command, it would return a status object indicating the successful copy as well as confirming the filename of the new copy created
    - cd function would change the working directory in the REPL, its purpose, like _cp_ would be the side effect but the function would return the new value of the working directory; it should support relative path designation like Unix (e.g. ../../, ./)
    - Pipes would be replaced by functional composition
    - Where sensible output would be delivered as Lisp data expressions
    






