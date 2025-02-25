# touchpad-scroll-mode

A minor mode which implements scrolling momentum for touchpads. Tested on Linux and Windows.

## Usage

Download `touchpad.el` from this repo.

For pixel scrolling:
```lisp
(pixel-scroll-precision-mode)
(load "touchpad")
(touchpad-scroll-mode)
(setq touchpad-pixel-scroll t)
```
And line scrolling:
```lisp
(load "touchpad")
(touchpad-scroll-mode)
```

## Why?

Emacs has a lot of packages which implement improved scrolling behaviour.
However, I couldn't find any that implemented scrolling momentum.
In particular, scrolling packages generally fell into one of two categories:
- Packages which interpolate between the current position and a fixed target position,
such as [good-scroll](https://github.com/io12/good-scroll.el).
- Packages which implement true pixel scrolling, but without momentum,
such as [ultra-scroll](https://github.com/jdtsmith/ultra-scroll)
and the builtin [pixel-scroll-precision-mode](https://www.gnu.org/savannah-checkouts/gnu/emacs/manual/html_node/efaq/New-in-Emacs-29.html).

However, neither category allows for "flicking" and "catching" the content
with two-finger touchpad scrolling like is possible in other applications.
(Note: if using the deprecated Synaptics Touchpad input driver on Linux,
such behaviour is indeed possible with a package from the second category,
as scroll momentum is implemented directly in the driver
by sending scroll events to applications even after the touchpad has been released.)

In fact, it turns out that momentum-based scrolling doesn't require pixel scrolling at all -
keeping a hidden "true" position and displaying the buffer content rounded to the nearest line
still allows for a smooth scrolling experience, even without pixel scrolling support.

## Bonus

A crude implementation of touchscreen scrolling is also provided,
as it reuses the same underlying framework to operate.
