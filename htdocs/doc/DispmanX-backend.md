**Obsolete: unfortunately, this backend was not merged, and it is not maintained anymore. It is recommended to use regular Linux backends (like X or GTK) on Raspberry Pi instead.**

A new link:pass:[CastleWindow backends][CastleWindow backend], right now available only in the fork https://github.com/JPGygax68/castle-engine . It allows to use Castle Game Engine on the Raspberry Pi without X or Wayland.

THIS BACKEND IS EXPERIMENTAL!

Please report all questions and problems to:
Jean-Pierre Gygax <gygax@practicomp.ch> ("JPNotADragon" on the 
"castle-engine" Discord network).

To use this backend, define the preprocessor symbols 
CASTLE_ENGINE_RASPBERRY_PI and CASTLE_WINDOW_DISPMANX. You can do this in 
your personal Freepascal configuration file `~/.fpc.cfg`. For example:

```
  #include /etc/fpc.cfg
  -dCASTLE_ENGINE_RASPBERRY_PI
  -dCASTLE_WINDOW_DISPMANX
  -Fl/opt/vc/lib/
```

The first symbol is not actually related to the backend, but switches the
engine from OpenGL to OpenGL ES (version 2), which is necessary since
the Pi only supports the latter in hardware. The `-Fl` option tells
Freepascal where to find the Broadcom libraries (if you have admin access,
you can also add this search path globally in `/etc/fpc.cfg`).

What this backend does
----------------------

This backend makes it possible to use the Castle Game Engine on the 
Raspberry Pi under Linux (tested with Raspbian) without X or Wayland.

This is possible because Broadcom (the manufacturer of the System-on-Chip)
provides a low-level graphics API called DispmanX, which, in cooperation
with several other Broadcom-provided libraries, makes it possible to
access the Pi's VideoCore 4 chip, configure the connected display and 
then draw on it via GLESv2 (using EGL for housekeeping and presentation tasks).

For input, the DispmanX backend relies on the `/dev/input/eventX` files
provided by Linux. By listening to all of those device files, the backend 
gets informed about touchpad, mouse, and keyboard input (read about limitations
below), which it translates and relays to the engine as all backends do.

Notes and limitations:
---------------------

- I have only ever used this backend on my FT5406 7" touch display (the
  "official" display for the Raspberry Pi). It should work on other
  displays - please test and report!

- Touch input is supposed to work, but wasn't formally tested and green-lighted (the `draw_toy` example app does not seem to work correctly, and we haven't yet established why that is).

- Mouse input has yet to be reviewed by the engine author. Currently,
  the mouse pointer starts at the center of the screen, but is totally
  unconstrained in all directions. This will most probably change soon.

- Keyboard input is somewhat limited in that, as is the case for all 
  backends except GTK and Windows, it does not use keymaps, so that text 
  input is in effect limited to US ASCII. (We have a couple of ideas on how 
  to address this, so please inform us if that particular limitation is a 
  concern to you).

- This backend does not currently take any of the configuration options
  (such as multisampling) into account. (Though it does enable the maximum
  level of multisampling available on the Pi, which is 4 - there's just no
  way to select a different value right now.)

Tip
---

Even though a CGE game built with the DispmanX backend *can* apparently be launched while the X desktop is active, that is probably not a good idea, since X will then continue to receive all the same input that your game does. One of the ways to stop the X server (Raspbian) is:
```
sudo service lightdm stop
```
