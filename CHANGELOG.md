## [v1.4.1](https://github.com/diagrams/diagrams-cairo/tree/v1.4.1) (2018-05-17)

- Allow `base-4.11` and `lens-4.16`
- Add `Semigroup` instance for `Render Cairo V2 Double`
- Drop GHC 7.8 support

## [v1.4](https://github.com/diagrams/diagrams-cairo/tree/v1.4) (2016-10-26)

- No significant changes, just bumping version for diagrams 1.4
  release

- Allow `vector-0.12` (Hackage revision 1)
- Allow `base-4.10` and `optparse-applicative-0.14` (Hackage rev 2)

## [v1.3.1.2](https://github.com/diagrams/diagrams-cairo/tree/v1.3.1.2) (2016-08-22)

- Require `optparse-applicative-0.13` and fix compilation error

## [v1.3.1.1](https://github.com/diagrams/diagrams-cairo/tree/v1.3.1.1) (2016-08-16)

- Allow `optparse-applicative-0.13`

## [v1.3.1](https://github.com/diagrams/diagrams-cairo/tree/v1.3.1) (2016-06-16)

- Bump upper bounds to allow:
    - `base-4.9`
    - `data-default-class-0.1`
    - `transformers-0.5.x`

- New module `Diagrams.Backend.Cairo.Text` with better text support
  (based on `pango`)

## [v1.3.0.6](https://github.com/diagrams/diagrams-cairo/tree/v1.3.0.6) (2016-05-01)

- allow `lens-4.14`
- New module `Diagrams.Backend.Cairo.Text` (should have been minor
  version bump; if you want to depend on this module please use
  `diagrams-cairo-1.3.1` or later).

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.3.0.5...v1.3.0.6)

## [v1.3.0.5](https://github.com/diagrams/diagrams-cairo/tree/v1.3.0.5) (2015-09-29)

  - Allow `optparse-applicative-0.12`

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.3.0.4...v1.3.0.5)

## [v1.3.0.4](https://github.com/diagrams/diagrams-cairo/tree/v1.3.0.4) (2015-09-17)

  - Allow `lens-4.13`

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.3.0.3...v1.3.0.4)

## [v1.3.0.3](https://github.com/diagrams/diagrams-cairo/tree/v1.3.0.3) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.3.0.2...v1.3.0.3)

## [v1.3.0.2](https://github.com/diagrams/diagrams-cairo/tree/v1.3.0.2) (2015-05-26)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.3.0.1...v1.3.0.2)

## [v1.3.0.1](https://github.com/diagrams/diagrams-cairo/tree/v1.3.0.1)(2015-04-25)

**Fixed bug:**

- Reflection should be included in transformation returned by adjustDia [\#63](https://github.com/diagrams/diagrams-cairo/issues/63)

## [v1.3](https://github.com/diagrams/diagrams-cairo/tree/v1.3)(2015-04-19)

- allow `lens-4.9`
- update for `diagrams-1.3`

## [v1.2.0.7](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.7) (2015-04-04)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2.0.5...v1.2.0.7)

**Fixed bugs:**

- loop when rendering text [\#59](https://github.com/diagrams/diagrams-cairo/issues/59)

**Merged pull requests:**

- remove Text module [\#60](https://github.com/diagrams/diagrams-cairo/pull/60) ([bergey](https://github.com/bergey))

## [v1.2.0.6] () (2015-01-12)

- allow `lens-4.7`

## [v1.2.0.5](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.5) (2014-12-25)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2.0.4...v1.2.0.5)

## [v1.2.0.4](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.4) (2014-11-17)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2.0.3...v1.2.0.4)

**Closed issues:**

- expose gifRender [\#55](https://github.com/diagrams/diagrams-cairo/issues/55)

**Merged pull requests:**

- Bump lens upper version bounds [\#58](https://github.com/diagrams/diagrams-cairo/pull/58) ([RyanGlScott](https://github.com/RyanGlScott))

- Update for new measure and size spec. [\#57](https://github.com/diagrams/diagrams-cairo/pull/57) ([cchalmers](https://github.com/cchalmers))

- Diagram B [\#56](https://github.com/diagrams/diagrams-cairo/pull/56) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- port to linear instead of vector-space [\#54](https://github.com/diagrams/diagrams-cairo/pull/54) ([bergey](https://github.com/bergey))

## [v1.2.0.3](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.3) (2014-10-08)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2.0.2-1...v1.2.0.3)

**Fixed bugs:**

- --loop disables generating multiple files [\#50](https://github.com/diagrams/diagrams-cairo/issues/50)

**Merged pull requests:**

- loop using fsnotify [\#53](https://github.com/diagrams/diagrams-cairo/pull/53) ([bergey](https://github.com/bergey))

## [v1.2.0.2-1](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.2-1) (2014-09-08)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2.0.2...v1.2.0.2-1)

## [v1.2.0.2](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.2) (2014-09-07)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2.0.1...v1.2.0.2)

## [v1.2.0.1](https://github.com/diagrams/diagrams-cairo/tree/v1.2.0.1) (2014-08-22)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.2...v1.2.0.1)

**Closed issues:**

- Problems with yellow color in gif animation [\#51](https://github.com/diagrams/diagrams-cairo/issues/51)

## [v1.2](https://github.com/diagrams/diagrams-cairo/tree/v1.2) (2014-06-02)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.1.0.1...v1.2)

**New features**

- Much improved text support using the `pango` library instead of
  cairo's "toy" text API.

- Support for linear and radial gradients on strokes and fills.

**Fixed bugs:**

- textLineBoundedIO sometimes produces incorrect envelopes [\#19](https://github.com/diagrams/diagrams-cairo/issues/19)

**Closed issues:**

- Mac OS X/XQuartz: Can't render single letter [\#43](https://github.com/diagrams/diagrams-cairo/issues/43)

**Merged pull requests:**

- Pango [\#49](https://github.com/diagrams/diagrams-cairo/pull/49) ([bergey](https://github.com/bergey))

- fix text scaling [\#48](https://github.com/diagrams/diagrams-cairo/pull/48) ([byorgey](https://github.com/byorgey))

- DImage implemented [\#47](https://github.com/diagrams/diagrams-cairo/pull/47) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Units [\#46](https://github.com/diagrams/diagrams-cairo/pull/46) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Gradient [\#37](https://github.com/diagrams/diagrams-cairo/pull/37) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1.0.1](https://github.com/diagrams/diagrams-cairo/tree/v1.1.0.1) (2014-03-19)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.1...v1.1.0.1)

## [v1.1](https://github.com/diagrams/diagrams-cairo/tree/v1.1) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.0.1.2...v1.1)

**New features**

- It is now possible to directly output animated GIFs, using the
  `gifMain` function

**Closed issues:**

- Color conversions have huge impact on performance [\#44](https://github.com/diagrams/diagrams-cairo/issues/44)

**Merged pull requests:**

- Fix documentation typo \(subtable -\> suitable\) [\#45](https://github.com/diagrams/diagrams-cairo/pull/45) ([robx](https://github.com/robx))

## [v1.0.1.2](https://github.com/diagrams/diagrams-cairo/tree/v1.0.1.2) (2014-02-06)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.0.1.1...v1.0.1.2)

**Closed issues:**

- Build failure for GHC 7.4 [\#42](https://github.com/diagrams/diagrams-cairo/issues/42)

## [v1.0.1.1](https://github.com/diagrams/diagrams-cairo/tree/v1.0.1.1) (2014-01-30)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.0.1...v1.0.1.1)

## [v1.0.1](https://github.com/diagrams/diagrams-cairo/tree/v1.0.1) (2014-01-26)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v1.0...v1.0.1)

**Merged pull requests:**

- Add Hashable instance for Options Cairo R2 [\#41](https://github.com/diagrams/diagrams-cairo/pull/41) ([byorgey](https://github.com/byorgey))

- Making animated GIFs directly from diagrams using Cairo. [\#40](https://github.com/diagrams/diagrams-cairo/pull/40) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.0](https://github.com/diagrams/diagrams-cairo/tree/v1.0) (2013-11-25)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v0.7...v1.0)

**New Features**

- Re-implement via new backend `RTree` interface.

- Use new command-line interface from `diagrams-lib`.

- Export `B` as an alias for `Cairo` token.

**Fixed bugs:**

- opacity does not affect text [\#15](https://github.com/diagrams/diagrams-cairo/issues/15)

**Merged pull requests:**

- Backend tree conversion [\#36](https://github.com/diagrams/diagrams-cairo/pull/36) ([byorgey](https://github.com/byorgey))

- Lens [\#35](https://github.com/diagrams/diagrams-cairo/pull/35) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v0.7](https://github.com/diagrams/diagrams-cairo/tree/v0.7) (2013-08-09)

[Full Changelog](https://github.com/diagrams/diagrams-cairo/compare/v0.6...v0.7)

**New features**

- New `renderCairo` function for more convenient use of the cairo
  backend.

- Lots of Haddock documentation improvements.

**Fixed bugs:**

- Lines should not be filled [\#32](https://github.com/diagrams/diagrams-cairo/issues/32)

**Merged pull requests:**

- Updates for new trail API. [\#31](https://github.com/diagrams/diagrams-cairo/pull/31) ([byorgey](https://github.com/byorgey))

## [v0.6](https://github.com/diagrams/diagrams-cairo/tree/v0.6) (2012-12-12)

**New features**

- New `--list` option for `multiMain` to list all available diagrams

- Major documentation improvements

- New modules:

  + `Diagrams.Backend.Cairo.Ptr`, for rendering directly to buffers
    in memory

  + `Diagrams.Backend.Cairo.List`, for rendering to a list of lists
  of pixels.

**API changes**

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

**Fixed bugs:**

- old-time and 7.6 [\#21](https://github.com/diagrams/diagrams-cairo/issues/21)

- Use of deprecated 'try' function in Diagrams.Backend.Cairo.Internal [\#12](https://github.com/diagrams/diagrams-cairo/issues/12)

- Better error message when image file does not exist [\#11](https://github.com/diagrams/diagrams-cairo/issues/11)

- Path doesn't get resized correctly when no transformations are applied to it [\#6](https://github.com/diagrams/diagrams-cairo/issues/6)

- Make Cairo backend smarter about missing attributes [\#4](https://github.com/diagrams/diagrams-cairo/issues/4)

- Attribute transformations are not handled correctly when rendering [\#3](https://github.com/diagrams/diagrams-cairo/issues/3)

**Closed issues:**

- vector-space-0.8.5 breaks compilation [\#27](https://github.com/diagrams/diagrams-cairo/issues/27)

- ‘cairoBypassAdjust’ undocumented [\#23](https://github.com/diagrams/diagrams-cairo/issues/23)

- Improve diagrams-cairo documentation [\#10](https://github.com/diagrams/diagrams-cairo/issues/10)

**Merged pull requests:**

- Additional rendering functions, for Ptr Word8 and \[\[Colour a\]\] [\#25](https://github.com/diagrams/diagrams-cairo/pull/25) ([haasn](https://github.com/haasn))

- `old-time` to `time` [\#22](https://github.com/diagrams/diagrams-cairo/pull/22) ([fryguybob](https://github.com/fryguybob))

- split out gtk rendering into a separate package [\#20](https://github.com/diagrams/diagrams-cairo/pull/20) ([byorgey](https://github.com/byorgey))

- use requiredScaleT in place of deprecated adjustSize [\#17](https://github.com/diagrams/diagrams-cairo/pull/17) ([byorgey](https://github.com/byorgey))

- call setDefault2DAttributes even when bypassing size adjustments [\#16](https://github.com/diagrams/diagrams-cairo/pull/16) ([byorgey](https://github.com/byorgey))

## [v0.5.0.2]() (13 May 2012)

* Allow building under `mtl` 2.1.*

## [v0.5.0.1]() (9 March 2012)

* Remove statement in package description that a development version
    of `gtk2hs` must be used with GHC 7.4; this is no longer true as of
    the 0.12.3 release of `gtk2hs`.

## [v0.5]() (March 2012)

**New features**

- New `Diagrams.Backend.Cairo.Text` module by Michael Sloan, with
  functions for creating appropriately sized text objects by
  querying cairo for the size, and related supporting functions.

- Basic support for animation with `animMain` function, by
  generating frames sampled at regular intervals.

- Proper vertical alignment of default text based on font
  parameters (Michael Sloan).

- Requesting just a width or height now causes the other to be
  computed appropriately.

**API changes**

- Move `Diagrams.Backend.Cairo` to
  `Diagrams.Backend.Cairo.Internal` and export everything.
  `Diagrams.Backend.Cairo` now just re-exports selected functions
  from `Internal`.  This allows anyone who wants access to the
  helper/utility functions to import `Internal`.

**Dependency/version changes**

- relax `cmdargs` upper bound

- GHC 7.4.1 compatibility: update `base`, `filepath`, and
  `old-time` upper bounds

**Bug fixes**

- [\#54](http://code.google.com/p/diagrams/issues/detail?id=54): Generate warning for missing image files (Ian Ross).

## [v0.4]() (22 October 2011)

* New features:

  + Support for drawing directly to Gtk widgets

    + Support for path fill rule attribute

* New/improved examples

* Improved documentation

* Bug fixes:

    + Warning for unsupported image types (#41)

## [v0.3]() (18 June 2011)

* Some new/improved examples

* New features:

    + simple text support

    + simple support for external PNG images

## [v0.2]() (3 June 2011)

* add `Typeable` and other instances for `Cairo` type

* generalize `Result` type to `(IO (), Render ())`, so programs that
    don't want to generate a file but just want a `Render` operation
    (*e.g.* to use to paint a gtk window) can use the second component.

* add support for opacity attribute and path clipping

## [v0.1.2]() (18 May 2011)

* link to new website

## [v0.1.1]() (18 May 2011)

* fix tic-tac-toe example

## [v0.1]() (17 May 2011)

* initial preview release


\* *This Change Log was automatically generated by (and hand edited) [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
