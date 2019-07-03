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
- **Concept:** Widget Presentation Cycle - Allocation, Initialization,
  Finalization, and Deallocation


**Concepts - Misc**

- Input/Editor Focus, Cognitive Focus, and Contextual Interface Definitions
    - NB: "Wiget Framing" for input fields (Generally Forms-Oriented Semantics)
    - NB: Input Focus and Textual Presentation Systems (Ed. NB: cf. CMU Sphinx)
      (Ed. NB: cf. UI Accessibility - Guidelines, Advice, Ideal Practices)
    - NB: "Application Logic" and Generally Forms-Oriented Semantics for
      User Input

- Programming Patterns - Callback-Oriented, Generally Asynchronous APIs
    - TBD: General Programming Patterns for Event Loop Catch/Dispatch
      Tasking - Normal Control Flows and Error Case Control Flows -
      Implementation in Single-Threaded and Multi-Threaded Environments
    - **Topic:** Assuming _zero or one_ of an interactive _Forergound
      Task_, registration and sequential dispatching for any _zero or
      more_ active _Background Task_, in a single-threaded manner of
      finite state machine for generalized _Task_ evaluation;
      generalization for support in non-interactive applications;
      generalizations for support in interactive and non-interactive
      multi-threaded applications.
    - NB: Systems Specification and Systems Implementation - Concerns
      for Design, Prototyping, Application, and QA with Portable User
      Interface Definitions


- Concepts of _Environment_ in Computing - Concepts of Definitions of
  "Host," "Task," "System," and Generalized Evaluation for Discrete,
  optionally "Template-Oriented" or "Macro-Like" Procedural Descriptions
    - Formalized System Definitions - A Survey (abbreviated)
    - Concept: Reusable "Task Patterns," Template Systems, and _The
      Literature_ in _Formalized_ Pattern Definitions for Arbitrary
      Systems Programming Environments (Ed. NB: See also, SIMULA)

----

- Concept: IDE as a Task Support Interface for Formalized Tasks

**Ed. NB: The following sections are known to need further clarification**

- TBD: Project Definitions and Project Support
    - Concept: _Project_ as a singularly User-Specific Concept

- TBD: Generalized support for REPL and UNIX shell interaction w/i
  ncurses environments; Generalizations in CommonIDE Terminal Widget API
    - Minimum functionality - non-trivial for portable
      implementation. buffering for strings input (forms, cmds) and
      input history cf. readline/editline, with some assumptions about
      I/O syntax; see also, GTK+
    - TBD: eval w/ Lisp Peer/Eval-over-Wire. e.g fork() and start-IPC or
      fork(), exec(), start-lisp + start-IPC; IPC (UNIX) and
      eval-over-wire protocols - protocol transports, protocol data
      units, and protocol peer models; API generalization, independent
      of any available display environment; NB UNIX signals, e.g used
      generally in manners of shell-oriented methods of process/job
      control (SIGCONT and related) in UNIX console systems, UNIX
      streams I/O; concerns for implementation of sequential, local
      dispatch for synchronous and asynchronous eval-over-wire protocol
      requests with a "Forked Lisp" or "Fork/Exec'd Lisp" process in a
      single-threaded environment (General pattern: send eval-over-write
      request; block for synchronous request, or create a background
      task for asynchronous request, then resuming dispatch-foreground,
      the background task subsequently pollling for receiving an
      asynchronous eval-over-wire response matching the task descriptor
      of the original request; note that the concerns for
      single-threaded evaluation would be limited to the "Foreground
      Lisp", insofar as from a perspective of the "Foreground
      application" - considering that a POSIX/UNIX environment may
      provide for running multiple simultaneous OS processes, regardless
      of any adoption of threading support in any individual process);
      TBD: Functional (non-polymorphic) protocol specification for
      dual-peer systems, with ASN.1 -- function descriptors (??) -- with
      extension onto CLOS/MOP frameworks, and framework support for
      GSS-API in any network-transport implementation; I/O management
      for eval-over-wire (NB: Local streams I/O, representative of eval
      in a remote peered Lisp) (TBD: stdio bindings for peer PTY streams, on
      fork/exec, in UNIX-like shell applications - NB generalized UNIX
      PTY application models, streams and strings _vis a vis_
      shell/subshell exec() interactively) with ncurses, GTK+ (CommonIDE
      terminal widget - GTK+), other display surfaces
   - TBD: Object-Semantic output processing w/ wrapped methods (e.g
      :AROUND onto PRINT-OBJECT) for any single _display surface_ or
      _text to speech schema_
    - TBD: Debugger integration - activation per *DEBUGGER-HOOK*;
      restart handling; integration with REPL streams

**Ed. NB: The main content of this document resumes, subsequently**

----

**Concept:** Towards a Generalized Interface for Development of
Device-Neutral Widgets with Platform-Specific Implementations

- Topic: _**Application** Specification_; Application Implementation;
  Application Support

- Topic: Platform/Framework Specification; Implementation; Application
  Support
    - NB: FFI in Generalized Implementations - Common Lisp implementations
    - TBD: Platform/Framework Modeling for Platform/Framework
      Specifications
        - NB, see also: Knuth's WEB
        - NB, GIR in GNOME
        - NB, compiler-level modeling for C applications with Clang AST,
          LLVM JIT APIs (assuming an absence of Itanium ABI support for
          C++ in most Common lisp implementations)

- Topic: "Beyond the Widget Model" - Application integration for
  platform services in arbitrary application environments (GNOME; other)

- Ed. NB: Juxtapose, in design, to features of CLIM/CLIM2 - Topics e.g
    - CLIM streams model (TBD: CLIM/CLIM2 Streams - applications for
      graphical user interfaces in desktop environments)
    - CLIM input recording - input capture and playback as a generalized "Undo"
    - CLIM abstract widget framework - as a highlighted feature of the
      generalized CLIM/CLIM2 framework
    - Generalized Accept/Present flow in CLIM/CLIM2 (Ed. NB, see also:
      Expect @ UNIX console systems)
    - Fonts in CLIM
    - Pictures in CLIM
    - Geometry and CLIM
    - Portability in CLIM/CLIM2 - Ports, Sheets, Grafts ... and Applications

- Ed. NB: Juxtapose also to Garnet
    - In context, Garnet Gem and Garnet Opal systems - support for
      specific presentational environments with generalized _device_
      definitions
        - X Window System instance as generalized _device_ with
          Gem/Opal; device implementation with XLib/CLX in Garnet (a
          generalized pattern)
        - gworld and the Macintosh PC Desktop [Context:TechArchives]
          (post-NeXT) with Gem/Opal
    - The underlying KR object system in Garnet
    - Fonts in Garnet
    - Pictures and Cursors in Garnet
    - Geometry and Garnet (Ed. NB: cf. _Garnetdraw_ demo)
    - Garnet Gadgets, Aggregadgets, Aggregraphs
    - Interacting with Garnet Applications - Garnet Interactors; Garnet Gestures
    - Adapting Garnet patterns to C++ - the Amulet Project (originally
      at CMU)
    - "Application Devo" Concerns - Garnet Documentation (Scribe Format)

- Topic: Generalizations for Generalized Console Environments - ncurses
    - Topic: ncurses, _circa_ BSD and subsq.
    - Topic: ncurses and readline, editline
    - Topic: Object allocation, initialization/finalization, and
      deallocation - Patterns onto Generalized APIs (ncurses, GTK+,
      other) for External Objects in Common Lisp Programming Systems
        - Ed. NB: "Also" Common Lisp Callback Functions for External
          Object Systems - Definitions; Concerns for debugging (NB:
          semantics for object file linking in C/C++/Obj-C and Common
          Lisp implementation/compiler environments; semantics for call
          stacks/frames in arbitrary architectures)
    - Topic: Generalized presentational models with ncurses - screens,
      tagged face/fonts, streams, [...]
        - **Ed. NB:** UNIX PTY environments, console screen as a display surface
        - Ed. NB, see also: tmux
        - Ed. NB: Concept of "Taggged" fonts is affected after some
          albeit superficial features of GTK+ APis
    - Topic: Generalized Application-Independent APIs and the Host OS
      Environment (Files, I/O, ...)
    - Topic: "Higher-Order" User Interface Concerns - Programmed
      Completion for Symbols in Abtirary Generalized Naming Contexts

- Ed. NB: This topic is directed principally for portable application
  development with GTK+ (PC Desktop) and ncurses (Generalized Console
  Environment) APIs. It may also be considered with regards to
  application development for user interface toolkits on mobile
  platforms -- e.g: Android w/ JNI for user interface APIs in the
  Android Runtime Environment, Android application process environment,
  Android libc (e.g Bionic), and OpenGL for abitrary GPUs on Android
  devices; iOS w/ FFI and Apple Frameworks, such as may be available
  typically in XCode development environments; X with Hildon on Maemo
  devices (cf. Nokia) [Context:TechArchives]. The concept of developing
  a user interface system for singularly audal output and arbitrary
  key/cursor or speech-to-text input may be addressed, in some regards,
  in parallel to the principally visual aspetcs of this design.


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

    - Note concerns for contextual identification and linking between UI
      layout elements, program source elements, and application data
      elements, such as vis a vis:

        - Function names for callback event declarations in the Glade
          interface definition (GTK+ with libglade2 or
          GtkBuilder). These function names would be, in effect, closed
          onto the set of symbols defined within an application's
          distribution -- such that should match the set of symbols
          defined in the application development _environment_

        - Widget names, with similar closure.

        - Stylesheet (??) declarations (GTK+ with ligblade2 or GtkBuilder)

        - Translatable Strings and Translation Specifications

            - See also: The Okapi project

        - Desktop resource definitions, _vis a vis_ effective standards
          under XDG, published at freedesktop.org (FDO) insofar as may
          pertain to development and distribution of applications in
          desktop application/service environments.

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
<!--  LocalWords:  CLIM presentational gworld TechArchives NeXT JNI
 -->
<!--  LocalWords:  Garnetdraw ABI bytecode Aggregadgets Aggregraphs
 -->
<!--  LocalWords:  ncurses toolkits XCode Hildon Maemo
 -->
