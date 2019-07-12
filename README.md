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


----

### CommonIDE - Concepts of Design - Generalized Widgets

**Concept:** Widget Signatures
- **Concept:** Widget Signature Definition
- **Concept:** Widget Signature Implementation
- **Concept:** Widget Presentation Cycle - Allocation, Initialization,
  Finalization, and Deallocation


**Concepts - Misc**

- Input/Editor Focus, Cognitive Focus, and Contextual Interface Definitions
    - NB: "Widget Framing" for input fields (Generally Forms-Oriented Semantics)
    - NB: Input Focus and Textual Presentation Systems (Ed. NB: cf. CMU Sphinx)
      (Ed. NB: cf. UI Accessibility - Guidelines, Advice, Ideal Practices)
    - NB: "Application Logic" and Generally Forms-Oriented Semantics for
      User Input

- Programming Patterns - Callback-Oriented, Generally Asynchronous APIs
    - TBD: General Programming Patterns for Event Loop Catch/Dispatch
      Tasking - Normal Control Flows and Error Case Control Flows -
      Implementation in Single-Threaded and Multi-Threaded Environments
    - **Topic:** Assuming _zero or one_ of an interactive _Foreground
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

- Concepts of _Component_ in Generalized Editor Applications

    - **StarSuite/StarOffice and UNO** - UNO Runtime Environment (URE)
      as an editor component environment (whether installed from
      development source code, or installed at end-user systems); UNO
      IDL as a generalization after CORBA IDL support in GNOME ORBit;
      note support for languages other than C++ in UNO

    - **Netscape and XUL** - under a generalized context, support for
      application development with HTML, JavaScript, and the Browser
      Environment; NB: RDF in XUL application descriptions;  XPCOM as a
      _framework_ for integration, principally, of C++ software components
      in extension to the browser environment; XPIDL for XPCOM as a
      second generalization after GNOME ORBit; XUL at Nestscape -
      subsequently, at Mozilla, to a series of source code revision in
      which XUL was in effect abandoned by Mozilla development

    - **GTK+ and GNOME Object Introspection** (GIR) - note GIR in GNOME
      Vala support; note gtkmm in GNOME components; GNOME VTE as a
      terminal emulator component, available for GNOME applications -
      uses gtkmm, furthermore providing interface declarations via GIR,
      such as may be known to be reusable for linkage from Vala
      applications. Note limitations for C++ applications in some
      programming language environments - in reference to the
      contemporary definition of the Itanium ABI, if not furthermore,
      in reference to definitions of the OS runtime linker for hardware
      architectures supported in individual UNIX kernel environments.

    - **Eclipse RCP** as an architecture for _IDE development_ with
      Eclipse IDE applications in Java(R) platforms; note extensions of
      the Eclipse Platform, in Modelio and other software systems


    - Ed. NB, juxtapose to: **_Project_ Components** -- i.e as may be
      developed by projects using an IDE, rather than _per se_
      developed for the IDE implementation, itself.


- Editor Applications, System Tooling, and the IDE Environment

    - Note that not all tools used in a development environment may be
      bundled with the IDE, itself.

    - Note generalizations for external _shell command_ evaluation, as
      may be represented in components of the Eclipse IDE platform

    - Note generalizations for formalized patterns in shell command
      evaluation, _vis a vis_ Make systems (GNU Make, bmake, CMake, Ant,
      Maven, Ivy, ...)

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
      the background task subsequently polling for receiving an
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
   - NB: LTP-Main - `defportable.lisp`
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
          albeit superficial features of GTK+ APIs
    - Topic: Generalized Application-Independent APIs and the Host OS
      Environment (Files, I/O, ...)
    - Topic: "Higher-Order" User Interface Concerns - Programmed
      Completion for Symbols in Arbitrary Generalized Naming Contexts

- Ed. NB: This topic is directed principally for portable application
  development with GTK+ (PC Desktop) and ncurses (Generalized Console
  Environment) APIs. It may also be considered with regards to
  application development for user interface toolkits on mobile
  platforms -- e.g: Android w/ JNI for user interface APIs in the
  Android Runtime Environment, Android application process environment,
  Android libc (e.g Bionic), and OpenGL for arbitrary GPUs on Android
  devices; iOS w/ FFI and Apple Frameworks, such as may be available
  typically in XCode development environments; X with Hildon on Maemo
  devices (cf. Nokia) [Context:TechArchives]. The concept of developing
  a user interface system for singularly audal output and arbitrary
  key/cursor or speech-to-text input may be addressed, in some regards,
  in parallel to the principally visual aspects of this design.

----

### CommonIDE - Concepts of Design - Source Management Support

- NB: LTP-Main `defportable.lisp` - `DEFSIGNATURE` and
  `DEFIMPLEMENTATION` PROTOTYPES

- NB Usage Cases for Source Management - LTP-Main `BASE-CLASS`
  development

    - Note development of the  WRITE-REFERENCE-EXPRESSION feature
      subset, during prototyping for development of the `BASE-CLASS`
      implementation.

    - Note interaction within the LTP-Main source repository - e.g some
      source forms defined in the `ltp-common` system had found their
      principal origins in other effective components within the
      `LTP-Main` source repository.

- Note concepts of Scrum development philosophy

    - The Scrum Sprint

        - Sprint Planning/Sprint Teams, in Formalized Project Models

        - Source Management and Systems Documentation, Post-Sprint

- Concept: Systems Testing as a Feature of Source Management

- Topic: Infrastructural dependencies of typically "Web-Based" QA Systems

    - e.g GitHub - Outsourced Service Hosting, with Normal Service
      Protocols, _vis a vis_ GitHub HTTP and JSON (entailing, in at
      least, site management and/or personal management of development
      credentials, insofar as pursuant towards interacting with GitHub
      services)

    - e.g Phabricator - Site-Sourced Service Hosting, with Outsourced
      (FOSS) Software Components (also HTTP-based, moreover Web-Oriented
      in general features of service presentation)

    - e.g Generally, "Other" Systems and Infrastructure Dependencies

        - TBD: Visio as generally a Microsoft Infrastructure component,
          not _per se_ a "Web" Infrastructure Component.

    - Note, "Small Site" development projects may ideally be supported
      with QA tooling not requiring a substantial IT management
      infrastructure

        - NB: Opinions may vary, by site, as with regards to a
          hypothetical question: Does an infrastructure based
          principally on HTTP, DBMS, SMTP, and --  as though
          intrinsically -- Web Browser services entail a "Substantial"
          manner of IT Management Infrastructure?

        - NB: "Outsourced Infrastructure" is nonetheless,
          Infrastructure -- e.g in service outsourcing to GitHub, or in
          software development outsourcing, in a manner, using any
          combination of FOSS software components and commercially
          licensed software components. As such, even the adoption of an
          "Outsourced Infrastructure," in any singular regard, may
          entail any number of processes requiring a manner of
          infrastructure management, generally regardless of the nature
          of the, _per se_, "Outsourcing"

        - Note the generally "light" systems footprint of the original
          GNATS QA system - however subsequently displaced with e.g
          Bugzilla. Note also, GNATS uses a manner of a database
          service, albeit typically limited to embedded database
          applications with Berkeley DB (NB: Berkeley DB is now
          principally as a holding of the Oracle company, subsequent of
          the corporate acquisition of Sun Microsystems and the -- in
          effect -- erasure of the Sun Microsystems brand name, by
          Oracle)

        - Note the generalized QA model presented by way of e.g the
          Eclipse Mylyn projects - however supported, singularly, with
          Java software infrastructure components (NB: Thus, once again,
          the Oracle brand name has a presence, subsequent of the
          erasure of the Sun Microsystems brand name, by Oracle. However
          this might be assumed to be regarded in any typically
          corporate circles, a project may adopt a general goal of
          avoiding works now singularly owned -- owned, in, at the
          least, owned in terms of branding and whatsoever with regards
          to formal terms of software systems licensing -- and
          whatsoever managed under the Oracle company. The lawsuit filed
          against Google, by Oracle, may also be considered as a manner
          of incentive towards such a policy..)

        - Thus, in a remark, even a Java software infrastructure is,
          nonetheless, an infrastructure -- in any manner, with regards
          to pedagogic definitions of IT -- an infrastructure, entailing
          some discrete systems management tasks. (**Ed NB:** Moreover,
          at some IT sites, it may be construed as an infrastructure to
          avoid -- perhaps, not only due to the particular management of
          the Java brand name and the effective erasure of the Sun
          Microsystems brand name, subsequent of Sun Microsystems'
          acquisition by Oracle, furthermore due to concerns with regards
          to the very infrastructure-intensive nature of server-side
          Java software applications, at many of a "Web Scale" in
          service development and service application hosting. If only
          due to the memory requirements for Java web service
          applications, juxtaposed to the limits presented in numerous
          cloud hosting options, it may simply not be practical to adopt
          Java web services, at many sites outside of any manner of
          "Enterprise Market." If an infrastructure requiring HTTP, SSL,
          SMTP, DBMS, and PHP or Python services may be considered,
          alternately, but if it may be possible to define an even more
          light-footprint infrastructure, using - by in large - a
          significant number of existing software components, then that
          approach might be most favorable for support of small site
          development -- e.g reusing OpenSSL, Git, and any number of
          data management tools for arbitrary data encoding syntaxes,
          e.g JSON or XML, as with regards to encoding, management, and
          application of project management metadata in automated
          workflow support tools not singularly depending on any lot of
          IT infrastructure)

        - Ideally, an IDE may be developed as to support, principally,
          systems management tasks -- with or without regards to any
          singular models, broadly, models of concepts of systems and
          management, in any singular mode of pedagogy.

        - Thus, perhaps the development of an IDE's support for systems
          management may be addressed, at the outset, with a skeptical
          regard for pedagogic definitions of Systems and Management. It
          may not be assumed, inasmuch, that any single model for
          systems management may be either formally ascertained or
          formally adopted, at every site.

- Topic: Project Support, User Opacity, Data Privacy, and Data Management Systems

    - Design Documentation - Philosophy of Design - Term as an External
      Representation of a Cognitive Concept

        - Consequently, _Term_ as an intrinsically imperfect
          representation of a _Concept_

        - NB: Concerning Social Dynamics, "Terminology as Gesture"

        - Topic: Representation of Terms and Terminological Associations

            - Topic: Notational Representation of Terms and
              Terminological Associations

                - TBD: Formal _Terms_ and _Terminological Associations_ as
                  Generalized Expressions Closed Onto a Formal Data
                  System.

                - Topic: Notation (e.g vis a vis RDF) as Generalized
                  Vocabulary; Model (e.g in application of any single
                  RDF notation) as Generalized Schema; User Data
                  (e.g in application of any form of generalized RDF
                  schema or model) as Schema Reification. Furthermore,
                  User Data -- Generally, "Graph Data" -- as Private
                  Model Data.

                    - Topic: Common Characteristics of Knowledge
                      Representation Languages

                        - Topic: Generally declarative semantics

                        - Topic: General applications of predicate logic

                        - Topic: Knowledge Representation System as an
                          Information System - Towards a Service-Oriented
                          Approach in Applications of Knowledge
                          Representation Systems

                    - NB: The User and RDF - Extending RDF Schema for
                      Applications; Extensibility of Application
                      Schema; Formalisms for Licensing, Distribution,
                      and Accession of Generalized Term Sets; Privacy of
                      Model Data.

                    - TBD: The User and RDF - Privacy, Opacity,
                      Publishing, and Accession for User-Developed RDF
                      Models, User-Governed RDF Graph Data, and
                      User-Developed Analytical and Narrative Content.

                - Topic: Generally Well Known Notations

                    - Topic: Terms and Relations in Common Logic CLIF

                    - Topic: Terms and Relations in RDF Notations

                    - Topic: Terms and Relations  in ISI PowerLoom(R) -
                      PowerLoom KB Expressions

                    - TBD: Terms and Relations in MOF, UML and SysML
                      Applications

            - Topic: Visual Representation of Terms and Terminological
              Associations

                - Topic: Visual Representation of Terms and Relations in
                  Common Logic CGIF

                - Topic: Visual Representation of Terms and Relations in
                  the ODM Metamodel and ODM UML Profile for RDF

                - TBD: Garnet Aggregraphs and PowerLoom(R) - Prototype
                  for a Generalized Diagrammatic, Interactive Knowledge
                  Modeling Application (Desktop Environments vis. CLX)
                  (TBD: Garnet - Garnet Gem and Opal -  device bindings
                  for Mobile Environments)

                - NB: Generally "Diagram-Oriented" Semantics for Visual
                  Representation with UML and SysML; Infrastructural
                  Characteristics of MOF and OCL (see also: VDM, ....)

            - TBD: UML and SysML in Knowledge Representation

                - NB: Visual Representation of UML and SysML Models -
                  Generalized Diagram Semantics in UML and SysML, with
                  Effective Diagram Syntax Constraints

                - TBD: Notational Interoperability for SysML and UML,
                  onto RDF - XML Schema in XMI and RDF Notations;
                  Representation of MOF, SysML and UML Terms onto RDF;
                  SysML and UML Diagrams onto RDF; OCL, XMI, and RDF

            - TBD: Other Diagram Frameworks - SDL and TTCN onto ASN.1;
              ECMA Flowcharts; ....

        - TBD: Procedural Applications for Normal, Declarative
          Terminological/Associative Vocabularies - Systems Management
          with Discrete System Entity Models (Beyond Query Construction)
          in an RDF Notation

            - TBD: Juxtaposition to DMTF CIM and DMTF MOF (NB: Distinct
              to OMG MOF) secondly, DMTF RedFish (NB: JSON Notations),
              _vis a vis_ Known Conventions in Systems Modeling and
              Systems Management.

        - Topic: General Implementation Concerns

            - NB: librdf/redland may not provide support for RDFS
              inference. See also: owlcpp

            - TBD: Common Logic implementations

            - NB ISI PowerLoom(R) - PL STELLA - Implementations (Common
              Lisp, C++, Java, ...)

    - Topic: Authorship, Publishing, and Accession for Applications of
      Modeling Systems - Model Schema, and  Model Data - for Knowledge
      Representation

    - Topic: Towards a Generalized Schema for Knowledge Representation
      in Project Management Support Environments

        - Topic: _Project_ as a Terminological Entity with Social
          Representation

            - TBD: "Defining the Existential Authority" - Project
              Representation in Distributed Environments

        - Topic: _Data System Object_ as a Systematic Construct with
          Tangible Representation in a _Data System_ (NB:
          Generalizations in IT/ICT Domains)

            - TBD: Deriving Terms from Arbitrary Data Objects - Towards a
              Survey of Methodologies

            - NB: File, Database, or Protocol Data Object (NB, Ephemeral
              Request/Response Data, per any Generally Well Known
              Information Systems Protocol) ... as a Data System Object
              Kind, at a certain granularity of model.

        - Topic: _Project Resource_ as a Terminological Entity with one
          or more objective representations, closed onto one or more
          _Project_

        - Topic: Processes and Support - Process Definitions for Portable
          Process Support Tools in Distributed Project Environments

            - TBD: FIPA SL as an intermediate schema/syntax for process
              support tools in distributed information systems; FIPA SL
              in extension/application of knowledge representation and
              inference support in ISI PowerLoom(R), principally towards
              applications in distributed information systems for
              project support environments.

        - Topic: Projects and Archives - Terminological Entity
          definitions, forms of objective representation, and reusable
          tools (editorial/content oriented systems) (Ed. NB: Zotero)


### CommonIDE - Concepts of Design - IDE UI - Editor Views

(...)


### CommonIDE - Concepts of Design - IDE UI - Project Views

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
<!--  LocalWords:  thinkum contrib Gitlab GtkSocket GtkPlug LLVM JIT SL
 -->
<!--  LocalWords:  CLIM presentational gworld TechArchives NeXT JNI CMU
 -->
<!--  LocalWords:  Garnetdraw ABI bytecode Aggregadgets Aggregraphs UNO
 -->
<!--  LocalWords:  ncurses toolkits XCode Hildon Maemo Deallocation URE
 -->
<!--  LocalWords:  SIMULA StarSuite StarOffice IDL CORBA ORBit XUL RDF
 -->
<!--  LocalWords:  XPCOM XPIDL Nestscape gtkmm Itanium RCP Modelio cmds
 -->
<!--  LocalWords:  bmake CMake readline editline eval Eval IPC SIGCONT
 -->
<!--  LocalWords:  Exec'd POSIX ASN CLOS GSS PTY subshell LTP AST XLib
 -->
<!--  LocalWords:  defportable CLX Interactors Devo subsq deallocation
 -->
<!--  LocalWords:  tmux Taggged libc OpenGL GPUs audal DEFSIGNATURE ltp
 -->
<!--  LocalWords:  DEFIMPLEMENTATION JSON Phabricator FOSS Visio SMTP
 -->
<!--  LocalWords:  Microsystems Mylyn SSL OpenSSL syntaxes IDE's XDG
 -->
<!--  LocalWords:  GtkBuilder ligblade Notational Formalisms CLIF ISI
 -->
<!--  LocalWords:  PowerLoom MOF UML SysML CGIF ODM Metamodel OCL VDM
 -->
<!--  LocalWords:  XMI SDL TTCN ECMA DMTF CIM OMG RedFish ICT FIPA
 -->
<!--  LocalWords:  Zotero
 -->
