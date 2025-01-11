# FoenixMonitor
A monitor/debugger for the Foenix F256 computers in classic mode

**This software is under developement and is missing quite a bit.**

### Installing the program
There are four files that reside in the root directory that are needed to run the program:
```
fmon.bas
fmon.bin
fmonWedge.bin
kernelCopy.bin
```
Load ```fmon.bin``` and run in SuperBASIC. This will load and install the other programs in the order needed to function. Once the basic program has been executed, the monitor can either be started by using a ```CALL $7e00``` command, or by pressing the ```RESTORE``` key.

### Commands
The following list is the current list of functioning commands.
All numbers are in hexadecimal format.
```
d [xxxx]  -  Disassemble 16 lines of code starting at xxxx. If no address is given, the program will continue from the previous list.
m [xxxx]  -  Display 256 byte of memory starting at xxxx. If no address is given, the program will continue from the previous list.
r         -  Display the computer's registers at the time of entering the monitor
x         -  Exit the program. This can also be done by pressing the F1 key. The RESTORE key remains active
```
