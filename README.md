# monstick firmware

Firmware for Monstick devices.

Written using [Ivory Tower framework](http://ivorylang.org/)

## Building

```bash
nix-shell
make <TARGET>
```

Run `make` to build all applications.
Specific application can be built with `make APP`
loaded with `make APP-load` and `make APP-run`.

To load Blink test application run

```bash
make blink-test-load
```

to also issue run and start application after loading use::

```bash
make blink-test-run
```

to just run gdb with new binary without loading::

```bash
make blink-test-gdb
# issuing 'load' in gdb         == blink-test-load
# running both 'load' and 'run' == blink-test-run
```

### Apps

LoraWAN
: Periodically send temperature / humidity readings over LoraWAN.
Sample is packed using Cayenne Low Power protocol using
[ivory-tower-cayenne](https://github.com/hexamon-tech/ivory-tower-cayenne)
library.

Logger
: Log sensor readings to UART

### Tests

Blink
: Blinks red and green LEDs

Heater
: Allows control of SI7006 internal heater via UART '0'-'9' characters

UARTBridge
: Bridge debug UART to radio modem UART

RadioCommand
: Raw radio command example

### Flashing

Manually with BlackMagic Probe::

```bash
arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/lorawan/image
```
