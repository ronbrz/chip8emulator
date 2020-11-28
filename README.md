# chip8emu

A chip8 emulator written in clojure, using [quil](www.quil.info) as the graphics library

## Installation

To compile to a jar file:
```
lein compile
lein uberjar
```

## Usage

With a generated jar file:
``` sh
$ java -jar chip8emu-0.1.0-standalone.jar [args]
```

To just run within directory:

``` sh
$ lein run [args]
```

For example, to run VBRIX included in the games directory use

``` sh
$ lein run VBRIX
```

## Resources
Wonderful reference: 
http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#00FD

## TODO
* get sound working
* 60hz timers working correctly 
