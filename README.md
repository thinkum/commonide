CommonIDE from Thinkum Labs
===========================


## Overview

[Project]

[Availability]

[QA]

**See also:** Garnet KR


## Design Notes - CommonIDE Project


### CommonIDE - General Concerns

- GTK+ - Portability (PC Desktop Environment, and some touchscreen environments)

- Glade - Support

- Integration with Common Lisp implementations - FFI APIs; Other
  Implementation-Specific, Host-Specific, and Machine-Specific concerns
  in applications of Common Lisp as an object-oriented systems
  programming language - in UNIX and other environments.

- QA


### Common IDE - Application Concepts

The following general concepts have been addressed, to an extent, with
the design of the CommonIDE UI layout in its present revision:

- IDE Windows

- Project Management Windows

- IDE Editor Views

- Shell Views (REPL, UNIX Command-Line Shell, Other)

- IDE Preferences

- Project Management Preferences

- Project Configuration Support, for "New Project" Creation


The following concerns should be furthermore developed, in subsequent
revisions.

- Define all terminal emulator window prototypes - focusing on
  REPL and Shell terminal usage cases. Ideally, this may extend
  VTE, if a methodology may be developed for linking onto VTE
  from within non-C++ programs. GIR may be of some use, at
  that -- similar to how it's been applied for using VTE from
  GNOME Vala programs. It should also extend of the existing
  command completion facilities available in GNOME 3 and
  subsequent.

- Test applications of the GtkListStore, GtkEntryCompletion,
  and GtkEntryBuffer implementation, for operations of
  programmable completion in the cmd entry buffer - this, for
  each of the `_IDE_Main` and `_Project_Main` windows. The
  programmed completion implementation should be reusable, to
  some extent, for the REPL and/or Shell terminal usage cases.

- Define a normal semantics for the IDE and Project Management
  'Mode' and 'Shell' drop-up menus

- Test application of the GtkTextTagTable and GtkTextTag
  implementations, as in a manner after font-locking in Emacs.
  Define a normal syntax for runtime definition of such text
  tag objects.

- Test the vscroll, hscroll controller semantics

- Refinement of the initial prototype window layouts

**Editor Note:** The previous text was adapted from this project's
source changelog, revision `acae1ef` (14 June, 2019 @ 02:12:41 GMT)
minus some text of that changelog entry, as concerning methodologies for
concurrent applications of GTK+ within Common Lisp environments.


The following concerns may also be addressed, in the CommonIDE project:

- Project modeling for support of systems management in development
  environments.

- Integration of GTK+ and WebKit - APIs, and in implementations.

- Extensions onto GTK+ in the Clutter framework.

- Support for internationalization and accessibility, in Glade and
  broader GTK+ applications (support in the application, and development
  support for application users)

- Component build tooling - concerning pkgsrc, FreeBSD Ports, Debian
  Packages, and other distribution frameworks for software
  components. Furthermore, concerning site-local infrastructure
  systems.

- Support for multi-modal "Source Text Editing" and "Visual Editing",
  broadly, for text in XML and other structured notations (Markdown,
  TeX, ....)

- Shell/Terminal/Console interaction during application development, in
  parallel to direct interaction with the CommonIDE UI system via the
  CommonIDE main application windows.

- General application runtime concerns - to include: Crash capture for
  applications, with obfuscation of user-private data in application
  crash reports, before message delivery over arbitrary, untrusted
  networks; Application signing, onto any single trust infrastructure;
  Support for arbitrary QA workflows, in application development
  environments; Extensibility and Portability, as concerns of a manner
  that may be principally primary to some concerns of Manageability,
  _per se_.

- Updates (GNOME 3 GTK Builder compatibility) and integration of the
  nearly _extant_ [**libglade** source repository - `thinkum-contrib`
  fork][libglade-src]. (**Ed. Note:** **libglade** appears to have been
  been removed from the GNOME Gitlab and GNOME GitHub source
  repositories, at some time corresponding to GNOME 3)

- Evaluation of the GTK+ **GtkSocket** and **GtkPlug** APIs, in GNOME 3,
  pursuant towards a possibility of reusing GNOME Dia UI components for
  model editing, in CommonIDE.

- Formal development support for Glade UI layout templates, in UI design
  and UI implementation. Note the concerns with regards to constraints
  for ID and Name values in Glade XML, as denoted below in this
  document.

- Integration with LLVM JIT and LLVM C APIs, for purposes of static
  analysis and programmed reflection onto C source code, with GTK+,
  **Glade**/**libglade** and CommonIDE.

  Concerning differences in call stack conventions in Common Lisp
  implementations, contrasted to any bytecode as may be produced with
  LLVM, any bytecode generated with the LLVM-C and LLVM JIT APIs may
  only be usable, in CommonIDE, for applications expressly in the
  external C environment. Regardless, even within this ABI-related
  limitation, it may serve to provide some support for integration with
  C program systems, in CommonIDE.

  More specifically, such integration could serve to be of some use when
  validating a Glade XML UI description, in comparison to the set of
  symbols that would be assumed to be available within the external C
  program environment, in any applications as may be developed with
  CommonIDE.

  Ideally, such integration could also serve to provide for a more
  comprehensive support for interactive testing of UI layouts designed
  with Glade.

  Of course, the Glade editor -- at present -- provides a manner of
  broad support for interaction with UI preview windows, in the Glade
  editor environment, Insofar as for supporting development and
  application of individual Glade UI layout descriptions in complete
  GTK+ applications, some further support may be developed for support
  of application testing with Glade UI layout descriptions in CommonIDE
  -- pursuant of some careful integration with the LLVM compiler
  framework.



### CommonIDE - Concepts of Design - Generalized Widgets

**Concept:** Widget Signatures
- **Concept:** Widget Signature Definition
- **Concept:** Widget Signature Implementation
- **Concept:** Widget Initialization

**Concept:** Towards a Generalized Interface for Development of
Device-Neutral Widgets with Platform-Specific Implementations

- Ed. NB: Juxtapose to some features of CLIM
    - CLIM Streams model (for graphical applications in desktop environments?)
    - CLIM Input Recording - Input Capture and Playback as a Generalized "Undo"
    - CLIM Abstract Widget Model - as a highlight of the generalized
      CLIM/CLIM2 Framework
    - Generalized Accept/Present Flow in CLIM (Ed. NB, see also: Expect)
    - Fonts in CLIM
    - Pictures in CLIM
    - Geometry and CLIM
    - Portability in CLIM/CLIM2 - Ports, Sheets, Grafts ... and Applications

- Ed. NB: Juxtapose also to Garnet
    - In context, Garnet Gem and Garnet Opal systems - support for
      specific presentational environments with generalized _device_
      definitions
        - X Window System instance as _device_ with Gem/Opal
        - gworld and the Macintosh PC Desktop [Context:TechAnachronism]
          (post-NeXT) with Gem/Opal
    - The underlying KR object system in Garnet
    - Fonts in Garnet
    - Pictures and Cursors in Garnet
    - Geometry and Garnet (Ed. NB: cf. _Garnetdraw_ demo)
    - Garnet Gadgets, Aggregadgets, Aggregraphs

- Ed. NB: This proposal is directed for development with GTK+ (Desktop)
  and ncurses (Generalized Console Environment) APIs, if not furthermore
  for application of user interface toolkits on mobile platforms
  (e.g Android w/ JNI, iOS w/ FFI and Apple Frameworks as may be available
  in XCode development, and X with Hildon on Maemo [Context:TechAnachronism])


### CommonIDE - Concepts of Design - Editor Views

(...)


### CommonIDE - Concepts of Design - Project Views

(...)


### CommonIDE UI Builder - General Concerns - Naming in Glade Applications

The following assumptions (...)

- Widget ID values must be unique, within any single Glade interface
  definition context (e.g a single *.glade file or XML stream).

- Widget Name values may be relevant principally for applications of
  **libglade** -- in a sense of application implementations, i,e "End User"
  applications --  and must be unique within a similar definition
  context.

- The set of Widget ID values and Widget Name values represents, in
  each, a distinct naming context. Widget ID and Widget Name values
  should be unique within each respective context, but need not be
  unique within a set union of those contexts.


### CommonIDE UI Builder - Tool Support - Glade XML Grammar

**Ed. Note:** A RELAX NG grammar for Glade 2.0 XML notation  has been
added to the [**libglade** fork, at `thinkum-contrib`][libglade-src]

As a manner of formalization for the Glade XML grammar, the Glade
project defines an XML DTD for the Glade XML format. This formal DTD has
a system identifier and XML namespace as follows:

> `http://glade.gnome.org/glade-2.0.dtd`

This DTD may be available via local XML catalogs.

While it represents a formal definition of the Glade XML format, but it
would not be supported for XML validation in any XML editor environments
that do not support XML grammars in a DTD notation -- such as Emacs
nXML, which supports the Relax NG XML notation syntax, specifically,
with extension for Schematron declarations.

The Glade XML 2.0 DTD would be supported in Emacs pSGML and other XML
editor environments that support XML grammars in XML DTD notation.

Subsequent of the definition of the Glade XML 2.0 DTD, it should be
possible to derive a Relax NG schema for Glade XML files. Subsequent to
the production of any such derived schema -- if not furthermore, in any
update to the original XML DTD, itself -- the schema may be annotated,
subsequently, with any small number of _value constraint_ declarations,
using a Schematron declaration syntax.

This could serve to support validation of Glade XML files with any one
or more tools for application development environments. Such XML
validation -- pursuant of any extensional declarations in Schematron
notation -- would be operable at a certain granularity beyond the
constraints provided of the original XML DTD type grammar, in
itself. For instance, this may serve to support validation of widget ID
and widget Name values for applications of Glade, with the validation
being performed in manner principally independent to the Glade
application environment, in itself.

Assuming a support for Schematron declarations in libxml2, furthermore
considering the reuse of libxml2 in Glade, such tooling could be
developed as principally extensional to libxml2.

Of course, insofar as the simple availability of a Relax NG schema for
Glade XML, this would serve to provide some support for validation
within XML editor tools supporting the Relax NG notation. As such, this
could serve to support validation in the editor environment, for
applications of Emacs nXML.


### CommonIDE UI Builder - Support for Application Development Workflows

The CommonIDE UI Builder environment should serve to support a workflow
such as may be formally denoted of three principal stages of development,
insofar as may pertain to prototyping, testing, and application of
graphical applications using GTK+

1. Creation of a UI Builder Layout

    - in extension to **libglade**

        - TBD: UI Builder support for Mobile Applications

    - supporting creation from Layout/Source Template (syntax TBD,
      extensional to the Glade XML DTD version 2.0 and the glib/GTK+
      APIs) or creation from scratch (syntax/validation support TBD, in
      the UI Builder editor environment and in shell console
      environments)

2. Development of the UI Builder Layout

    - Note concerns with regards to name and id value templates, for
      id references subsequent of template application (layout creation)
      i.e during layout development, and - subsequently - for name
      references in testing and normal runtime environments.

        - This, in itself, may not be singularly addressed with widget
          type definitions.

        - Note that any widget name templates may be relevant for
          evaluation in application environments, while widget ID
          templates may be relevant principally for evaluation within
          the **libglade** environment. If a widget ID template, in
          applications, may be derived from a widget name, in
          applications, this may serve to present some concerns with
          regards to widget ID consistency within the UI layout editor
          environment (TBD: Verifying this set of assumptions, with
          regards to application of widget ID and widget name values in
          the Glade editor, and in applications extending **libglade**)

    - Note and linking between UI layout elements, program source
      elements, and application data elements, such as vis a vis:

        - Function names for callback event declarations in the Glade
          interface definition. These function names would be, in
          effect, closed onto the set of symbols defined within an
          application's distribution -- such that should match the set of
          symbols defined in the application development _environment_

        - Widget names, with similar closure.

        - Stylesheet (??) declarations

        - Translatable Strings and Translation Specifications

            - See also: The Okapi project

        - Desktop resource definitions, _vis a vis_ effective standards
          published at freedesktop.org (FDO) insofar as may pertain to
          development and distribution of desktop applications

3. Application of the UI Builder Layout, in any one or more discrete
   application environments

    - Note specializations for support of editor development for
      CommonIDE - principally in extension to GTK+, however adapted for
      a single application environment.

    - Note general concerns for development of application-specific
      widget types, principally in extension to GTK+

    - Note general concerns for development of reusable widget
      libraries, principally in extension to GTK+


[libglade-src]: https://github.com/thinkum-contrib/libglade

<!--  LocalWords:  Thinkum CommonIDE GTK FFI APIs UI REPL VTE GIR Vala
 -->
<!--  LocalWords:  GtkListStore GtkEntryCompletion GtkEntryBuffer cmd
 -->
<!--  LocalWords:  GtkTextTagTable GtkTextTag runtime vscroll hscroll
 -->
<!--  LocalWords:  changelog WebKit pkgsrc FreeBSD untrusted DTD NG src
 -->
<!--  LocalWords:  Extensibility libglade namespace nXML Schematron TBD
 -->
<!--  LocalWords:  pSGML libxml vis se Stylesheet freedesktop FDO Dia
 -->
<!--  LocalWords:  thinkum contrib Gitlab GtkSocket GtkPlug LLVM JIT
 -->
<!--  LocalWords:  CLIM presentational gworld TechAnachronism NeXT JNI
 -->
<!--  LocalWords:  Garnetdraw ABI bytecode Aggregadgets Aggregraphs
 -->
<!--  LocalWords:  ncurses toolkits XCode Hildon Maemo
 -->

<!-- Local Variables: -->
<!-- ispell-buffer-session-localwords: ("Aggregadgets" "Aggregraphs" "Garnetdraw" "Hildon" "JNI" "Maemo" "NeXT" "TechAnachronism" "XCode" "bytecode" "gworld" "ncurses" "presentational" "toolkits") -->
<!-- End: -->
