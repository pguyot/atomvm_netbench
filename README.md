atomvm_netbench
=====

Network benchmarking and testing application for AtomVM

Usage
-----

The application is composed of a test controller, typically running on a
desktop using Erlang/OTP, and a test device, typically a micro controller
running AtomVM.

The test device needs to connect to a wifi network. You will need to copy
config.erl-template and fill in the wifi network details.

Flashing and running on device
------------------------------

You can compile and flash the device with:

```shell
rebar3 atomvm pico_flash
```

or

```shell
rebar3 atomvm esp32_flash -p PORT
```

where PORT is the tty device of your device (e.g. `/dev/tty.usbserial-11524D9FA8`)

Connect to the device using a serial console for the Pico (you may need to
restart) or idf.py monitor. It will print out its IP address.

For example a Pico-W will output:

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    Starting AtomVM revision 0.5.0
    Found startup beam atomvm_netbench.beam
    Starting atomvm_netbench.beam...---
    Connecting to wifi access point
    Device will listen on {192,168,1,197} port 40888
    Waiting for connection from test controller

Remarks: this code requires AtomVM master branch or 0.6.0-alpha.2 or higher,
even if it says 0.5.0.

Running test controller
-----------------------

Once the device is running, you can connect to it from a computer running
Erlang/OTP. Make sure the test controller host is on the same network as the
device.

You can compile the test controller with:

```shell
rebar3 escriptize
```

You can run it with

```shell
./_build/default/bin/atomvm_netbench IP_ADDR
```

For example, if the IP address printed by the device was `{192,168,1,197}`

```shell
./_build/default/bin/atomvm_netbench 192.168.1.197
```

Running test device on Unix
---------------------------

To check implementation of tests, the "test device" can be a local Unix machine
running AtomVM or OTP.

For the device using AtomVM:
```shell
rebar3 atomvm packbeam
../AtomVM/build/src/AtomVM _build/default/lib/atomvm_netbench.avm ../AtomVM/build/libs/atomvmlib.avm
```

For the device using Erlang/OTP:
```shell
rebar3 escriptize
./_build/default/bin/atomvm_netbench --device
```

For the controller:
```
rebar3 escriptize
./_build/default/bin/atomvm_netbench 127.0.0.1
```

Tests
-----

Three tests are currently implemented:
- udp echo test: the device echoes an UDP packet sent from the controller,
  ten times.
- tcp echo test: the device connects to a TCP Server running on the controller,
  and echoes the messages sent from the host, ten times.
- tcp priv server test: the device runs as a TCP server and sends a file from
  priv to each client, while the controller connects ten times in parallel
