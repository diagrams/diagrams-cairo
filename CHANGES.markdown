1.2.0.1 (22 August 2014)
------------------------

- Allow lens-4.4
- Allow cairo-0.13 and pango-0.13

1.2 (27 May 2014)
-----------------

* **New features**

    - Much improved text support using the `pango` library instead of
      cairo's "toy" text API.

    - Support for linear and radial gradients on strokes and fills.

* **Dependency/version changes

  - Allow `opt-parse-applicative-0.9`
  - Allow `lens-4.2`
  - Allow `mtl-2.2`
  - Allow `transformers-0.4`

1.1.0.2 (19 March 2014)
----------------------

- Allow `lens-4.1`

1.1.0.1 (18 March 2014)
-----------------------

- Allow `optparse-applicative-0.8`

1.1 (8 March 2014)
------------------

* **New features**

    - It is now possible to directly output animated GIFs, using the
      `gifMain` function.

* **Dependency/version changes**

    - allow `diagrams-core-1.1` and `diagrams-lib-1.1`
    - allow `unix-2.7`
    - allow `vector-0.10`

* **Bug fixes**

    - Don't explicitly draw final segment of a loop if it is straight
      ([#38](https://github.com/diagrams/diagrams-cairo/issues/38))

1.0.1.2 (6 February 2014)
-------------------------

    - Require diagrams-lib >= 1.0.1

1.0.1.1: 30 January 2014
------------------------

    - Work around bug in GHC 7.4.2, which can't derive Generic for associated
      data types.

1.0.1: 26 January 2014
----------------------

    - Add `Hashable (Options Cairo R2)` instance

1.0: 25 November 2013
---------------------

    - Re-implement via new backend `RTree` interface.
    - Use new command-line interface from `diagrams-lib`.
    - Export `B` as an alias for `Cairo` token.

0.7: 9 August 2013
------------------

* **New features**

    - New `renderCairo` function for more convenient use of the cairo
      backend.
    - Lots of Haddock documentation improvements.

* **New instances**

    - `Show` instance for `Options Cairo R2`.

0.6: 11 December 2012
---------------------

* **New features**

    - New `--list` option for `multiMain` to list all available diagrams

    - Major documentation improvements

    - New modules:

        + `Diagrams.Backend.Cairo.Ptr`, for rendering directly to buffers
          in memory

        + `Diagrams.Backend.Cairo.List`, for rendering to a list of lists
          of pixels.

* **API changes**

    - Removal of `StyleParam` from `Diagrams.Backend.Cairo.Text`, change
      functions in that module to accept `Style R2`.  Usage can be fixed
      by applying these style functions to `mempty`.

    - GTK rendering has been split out into a new package, diagrams-gtk.

	+ The `Diagrams.Backend.Cairo.Gtk` module is now
	  `Diagrams.Backend.Gtk` in the `diagrams-gtk` package.

	+ The `CairoOptions` record has a new boolean `cairoBypassAdjust`
	  option; when set, the backend should bypass calling `adjustDia2D`.

	+ The GTK output type is gone.

	+ There is a new `RenderOnly` output type, for when you don't
	  care about the `IO` action but only want the cairo `Render` action.

* **Dependency/version changes**

    - Upper bounds relaxed to allow
      `base`-4.6, `unix`-2.6, `cmdargs`-0.10, `split`-0.2.*, `mtl`-2.1

    - Add a dependency on `time`, and conditional compilation to use
      either ClockTime or UTCTime depending on the version of the
      `directory` package

    - Add dependency on `colour`

    - Lower bound on `cairo` raised to 0.12.4

* **Bug fixes**

    - Fixed looped compile mode, which was repeatedly trying to compile
      when the code contained errors, instead of trying once and then
      waiting for a change.

    - Fix a bug where default attributes were not being set when using
      the "bypass" mode used by the gtk backend. ([\#16](https://github.com/diagrams/diagrams-cairo/pull/16))

0.5.0.2 : 13 May 2012
---------------------

* Allow building under `mtl` 2.1.*

0.5.0.1 : 9 March 2012
----------------------

* Remove statement in package description that a development version
    of `gtk2hs` must be used with GHC 7.4; this is no longer true as of
    the 0.12.3 release of `gtk2hs`.

0.5: 9 March 2012
-----------------

* **New features**
    - New `Diagrams.Backend.Cairo.Text` module by Michael Sloan, with
      functions for creating appropriately sized text objects by
      querying cairo for the size, and related supporting functions.
    - Basic support for animation with `animMain` function, by
      generating frames sampled at regular intervals.
    - Proper vertical alignment of default text based on font
      parameters (Michael Sloan).
    - Requesting just a width or height now causes the other to be
      computed appropriately.

* **API changes**
    - Move `Diagrams.Backend.Cairo` to
      `Diagrams.Backend.Cairo.Internal` and export everything.
      `Diagrams.Backend.Cairo` now just re-exports selected functions
      from `Internal`.  This allows anyone who wants access to the
      helper/utility functions to import `Internal`.

* **Dependency/version changes**
    - relax `cmdargs` upper bound
    - GHC 7.4.1 compatibility: update `base`, `filepath`, and
      `old-time` upper bounds

* **Bug fixes**
    - [\#54](http://code.google.com/p/diagrams/issues/detail?id=54): Generate warning for missing image files (Ian Ross).

0.4: 22 October 2011
--------------------

* New features:
    + Support for drawing directly to Gtk widgets
    + Support for path fill rule attribute

* New/improved examples

* Improved documentation

* Bug fixes:
    + Warning for unsupported image types (#41)

0.3: 18 June 2011
-----------------

* Some new/improved examples

* New features:
    + simple text support
    + simple support for external PNG images

0.2: 3 June 2011
----------------

* add `Typeable` and other instances for `Cairo` type

* generalize `Result` type to `(IO (), Render ())`, so programs that
    don't want to generate a file but just want a `Render` operation
    (*e.g.* to use to paint a gtk window) can use the second component.

* add support for opacity attribute and path clipping

0.1.2: 18 May 2011
------------------

* link to new website

0.1.1: 18 May 2011
------------------

* fix tic-tac-toe example

0.1: 17 May 2011
----------------

* initial preview release
