---
title: Serial Port Tools on Linux
tags: uart
---

This page lists a set of tips that I have collected over the years using serial
port for interacting with embedded boards.

## Minicom

1. Use `minicom -w` to start minicom with line wrapping enabled.

2. Add yourself to the 'dialout' group to open Serial ports without using
   `sudo`. Remember to logout and login for the newly added group membership to
   take effect.

    ~~~{.bash}
    $ sudo adduser vmandela dialout
    ~~~

3. Use `minicom -C log.txt` to the UART port output.

4. Hit `Ctrl+a`,`N` to toggle timestamps in minicom. I normally use
   [GrabSerial](#grabserial) instead if I need timing information from uart logs.
   
### Minicom config files

1. The system wide configuration file for minicom is in
   `/etc/minicom/minirc.dfl`. You can override the system configuration by
   creating a config file at `~/.minirc.dfl`.
   
2. To open a specific port by name(say usb0), create a file `~/.minirc.usb0` and
   include all the required configuration parameters in it.
   
   ~~~
   $ cat ~/.minirc.usb0
   pu port             /dev/ttyUSB0
   pu baudrate         115200
   pu bits             8
   pu parity           N
   pu stopbits         1
   ~~~
   
   Now you can open `/dev/ttyUSB0` by doing
   
   ~~~
   $ minicom usb0
   ~~~
   
   instead of
   
   ~~~
   $ minicom -D /dev/ttyUSB0
   ~~~

## Use Byobu for multiple serial ports

For boards which have multiple UART outputs, I use byobu + tmux to open all the UART ports
in one command. The boards usually have multiple UART's multiplexed on to a single USB port
using an [FTDI chip](https://www.ftdichip.com/Products/ICs/FT4232H.htm).

~~~{.bash}
$ cat ~/.byobu/windows.tmux.evm1
new-session -n port0 -c /home/vmandela/work/ minicom -c on -w -C port0.txt usb0;
new-window -n port1 -c /home/vmandela/work/ minicom  -c on -w -C port1.txt usb1;
new-window -n port2 -c /home/vmandela/work/ minicom  -c on -w -C a53.txt usb2;
rename-session EVM1-UART
$ BYOBU_WINDOWS=evm1 byobu
~~~

## GrabSerial

[Grabserial](https://github.com/tbird20d/grabserial) is an amazing tool for capturing and timestamping
UART logs. I used it extensively when performing boot time optimizations. Grabserial provides a running
time stamp as well as delta from the previous line. This makes it easy to identify functionality
consuming most time.

You can install `grabserial` from ubuntu repositories. 

~~~
$ sudo apt install grabserial
~~~

It is also available in the NixOS repositories.

~~~
$ nix-env -iA nixpkgs.grabserial
~~~

The below command captures logs from ttyUSB5 until 120 seconds or receiving the string "test(s)"

~~~
$ grabserial -d /dev/ttyUSB5 -q 'test\(s\)' -e 120 | tee /home/vmandela/work/usb5.txt
~~~
    
Add the `-t` command line option to enable timestamping.

## XModem File transfer

One of the issues with `minicom` is the lack of scripting facilities. On
Windows, [TeraTerm](https://ttssh2.osdn.jp/index.html.en) offers scripting with
its own [TTL](https://ttssh2.osdn.jp/manual/en/macro/syntax/index.html)
language. I did not find a viable alternative on Linux yet and had to use
python to automate booting of an board over UART. Here is a python script that
boots a [AM6 EVM](http://www.ti.com/tool/TMDX654GPEVM) over UART. The script is
based on the example at [xmodem pypi page](https://pypi.org/project/xmodem/).

~~~{.py}
#!/usr/bin/env python
import serial
import sys
from xmodem import XMODEM
import logging
import subprocess

# Uncomment for logs on XMODEM block transfer
# logging.basicConfig(level=logging.DEBUG)

uart_port='/dev/ttyUSB5'
print "Port used is " + uart_port

bin_loc='test.bin'
port = serial.Serial(uart_port,
                     115200, timeout=1) # or whatever port you need

def getc(size, timeout=1):
    return port.read(size) or None

def putc(data, timeout=1):
    return port.write(data)  # note that this ignores the timeout

modem = XMODEM(getc, putc)

# Wait for character indicating the EVM is ready to receive the binary
print "Waiting for C"
rcv = 'd'
while rcv != 'C':
    rcv = port.read(1)
    if rcv:
        print rcv

print "Sending file"
bin = open(bin_loc, 'rb')
modem.send(bin)
bin.close()
port.close()
~~~
