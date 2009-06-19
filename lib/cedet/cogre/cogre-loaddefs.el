;;; cogre-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cogre) "cogre" "cogre.el" (18977 14170))
;;; Generated autoloads from cogre.el

(eieio-defclass-autoload (quote cogre-base-graph) (quote (eieio-persistent)) "cogre" "A Connected Graph.\na connected graph contains a series of nodes and links which are\nrendered in a buffer, or serialized to disk.")

(eieio-defclass-autoload (quote cogre-graph-element) (quote (eieio-named)) "cogre" "A Graph Element.\nGraph elements are anything that is drawn into a `cogre-base-graph'.\nGraph elements have a method for marking themselves dirty.")

(eieio-defclass-autoload (quote cogre-node) (quote (cogre-graph-element)) "cogre" "Connected Graph node.\nNodes are regions with a fill color, and some amount of text representing\na status, or values.")

(eieio-defclass-autoload (quote cogre-link) (quote (cogre-graph-element)) "cogre" "Connected Graph link.\nLinks are lines drawn between two nodes, or possibly loose in space\nas an intermediate step.  Some links have text describing what they\ndo, and most links have special markers on one end or another, such as\narrows or circles.")

(eieio-defclass-autoload (quote cogre-arrow) (quote (cogre-link)) "cogre" "This type of link is a simple arrow.")

(autoload (quote cogre) "cogre" "\
Create a new graph not associated with a buffer.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create.

\(fn NAME &optional GRAPH-CLASS)" t nil)

;;;***

;;;### (autoloads (cogre-export-ascii) "cogre-ascii" "cogre-ascii.el"
;;;;;;  (18912 13351))
;;; Generated autoloads from cogre-ascii.el

(autoload (quote cogre-export-ascii) "cogre-ascii" "\
Export the current diagram into an ASCII buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-export-utest cogre-export-dot-method cogre-export-dot-postscript-print
;;;;;;  cogre-export-dot-png cogre-export-dot) "cogre-convert" "cogre-convert.el"
;;;;;;  (18912 13859))
;;; Generated autoloads from cogre-convert.el

(autoload (quote cogre-export-dot) "cogre-convert" "\
Export the current COGRE graph to DOT notation.
DOT is a part of GraphViz.

\(fn)" t nil)

(autoload (quote cogre-export-dot-png) "cogre-convert" "\
Export the current COGRE graph to DOT, then convert that to PNG.
The png file is then displayed in an Emacs buffer.
DOT is a part of GraphVis.

\(fn)" t nil)

(autoload (quote cogre-export-dot-postscript-print) "cogre-convert" "\
Print the current graph.
This is done by exporting the current COGRE graph to DOT, then
convert that to Postscript before printing.
DOT is a part of GraphVis.

\(fn)" t nil)

(autoload (quote cogre-export-dot-method) "cogre-convert" "\
Convert G into DOT syntax of semantic tags.

\(fn (G cogre-base-graph))" nil nil)

(autoload (quote cogre-export-utest) "cogre-convert" "\
Run all the COGRE structured export/convert test.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-dot-mode) "cogre-dot-mode" "cogre-dot-mode.el"
;;;;;;  (18906 40825))
;;; Generated autoloads from cogre-dot-mode.el

(autoload (quote cogre-dot-mode) "cogre-dot-mode" "\
Major mode for the dot language.
This is a mini-mode that will first attempt to load and install
`graphviz-dot-mode' in this buffer.  If that fails, it installs
the syntax table, and runs a hook needed to get Semantic working
as a parsing engine.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.dot\\'" . cogre-dot-mode)))

;;;***

;;;### (autoloads (cogre-layout) "cogre-layout" "cogre-layout.el"
;;;;;;  (18906 40988))
;;; Generated autoloads from cogre-layout.el

(autoload (quote cogre-layout) "cogre-layout" "\
Layout the current graph.
This function depends on graphviz `dot' program.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-mode) "cogre-mode" "cogre-mode.el" (18977
;;;;;;  14210))
;;; Generated autoloads from cogre-mode.el

(autoload (quote cogre-mode) "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (cons "\\.cgr\\'" (quote cogre-mode)))

;;;***

;;;### (autoloads (cogre-periodic-utest cogre-periodic) "cogre-periodic"
;;;;;;  "cogre-periodic.el" (18906 45093))
;;; Generated autoloads from cogre-periodic.el

(autoload (quote cogre-periodic) "cogre-periodic" "\
Create a periodic table of COGRE objects.

\(fn)" t nil)

(autoload (quote cogre-periodic-utest) "cogre-periodic" "\
Run the cogre periodic table for unit testing.
Also test various output mechanisms from the periodic table.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-uml-quick-class cogre-export-code cogre-semantic-tag-to-node)
;;;;;;  "cogre-semantic" "cogre-semantic.el" (18927 57417))
;;; Generated autoloads from cogre-semantic.el

(autoload (quote cogre-semantic-tag-to-node) "cogre-semantic" "\
Convert the Semantic tag TAG into a COGRE node.
Only handles data types nodes.
To convert function/variables into methods or attributes in
an existing COGRE node, see @TODO - do that.

\(fn TAG)" nil nil)

(autoload (quote cogre-export-code) "cogre-semantic" "\
Export the current graph into source-code in FILE.
Uses `cogre-export-semantic' to convert into Semantic tags.
Uses `cogre-srecode-setup' to setup SRecode for code generation.

\(fn FILE)" t nil)

(autoload (quote cogre-uml-quick-class) "cogre-semantic" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:dot srecode-semantic-handle-:cogre
;;;;;;  cogre-srecode-setup) "cogre-srecode" "cogre-srecode.el" (18905
;;;;;;  26818))
;;; Generated autoloads from cogre-srecode.el

(autoload (quote cogre-srecode-setup) "cogre-srecode" "\
Update various paths to get SRecode to identify COGRE macros.

\(fn)" nil nil)

(autoload (quote srecode-semantic-handle-:cogre) "cogre-srecode" "\
Add macros to dictionary DICT based on COGRE data.

\(fn DICT)" nil nil)

(eval-after-load "srecode-map" (cogre-srecode-setup))

(autoload (quote srecode-semantic-handle-:dot) "cogre-srecode" "\
Add macros to dictionary DICT based on the current DOT buffer.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (cogre-uml-sort-for-lineage cogre-uml-enable-unicode)
;;;;;;  "cogre-uml" "cogre-uml.el" (18927 57347))
;;; Generated autoloads from cogre-uml.el

(eieio-defclass-autoload (quote cogre-package) (quote (cogre-node)) "cogre-uml" "A Package node.\nPackages represent other class diagrams, and list the major nodes\nwithin them.  They can be linked by dependency links.")

(eieio-defclass-autoload (quote cogre-note) (quote (cogre-node)) "cogre-uml" "An note node.\nNotes are used to add annotations inside a graph.\nNotes are generally linked to some node, and are supposed to look\nlike a little pieces of paper.")

(eieio-defclass-autoload (quote cogre-scoped-node) (quote (cogre-node)) "cogre-uml" "A UML node that has a package specifier within which it is scoped.")

(eieio-defclass-autoload (quote cogre-class) (quote (cogre-scoped-node)) "cogre-uml" "A Class node.\nClass nodes represent a class, and can list the attributes and methods\nwithin them.  Classes can have attribute links, and class hierarchy links.")

(eieio-defclass-autoload (quote cogre-instance) (quote (cogre-scoped-node)) "cogre-uml" "An instance node.\nInstances are used in instance diagrams.\nInstances are linked together with plain links.")

(eieio-defclass-autoload (quote cogre-inherit) (quote (cogre-link)) "cogre-uml" "This type of link indicates that the two nodes reference infer inheritance.\nThe `start' node is the child, and the `end' node is the parent.\nThis is supposed to infer that START inherits from END.")

(eieio-defclass-autoload (quote cogre-aggregate) (quote (cogre-link)) "cogre-uml" "This type of link indicates aggregation.\nThe `start' node is the owner of the aggregation, the `end' node is\nthe item being aggregated.\nThis is supposed to infer that START contains END.")

(autoload (quote cogre-uml-enable-unicode) "cogre-uml" "\
Enable use of UNICODE symbols to create COGRE graphs.
Inheritance uses math triangle on page 25a0.
Aggregation uses math square on edge 25a0.
Line-drawing uses line-drawing codes on page 2500.
See http://unicode.org/charts/symbols.html.

The unicode symbols can be differing widths.  This will make the
cogre chart a little screwy somteims.  Your mileage may vary.

\(fn)" t nil)

(autoload (quote cogre-uml-sort-for-lineage) "cogre-uml" "\
Sort the current graph G for determining inheritance lineage.
Return it as a list of lists.  Each entry is of the form:
  ( NODE PARENT1 PARENT2 ... PARENTN)

\(fn G)" t nil)

;;;***

;;;### (autoloads (cogre-utest-quick-class cogre-utest) "cogre-utest"
;;;;;;  "cogre-utest.el" (18912 16027))
;;; Generated autoloads from cogre-utest.el

(autoload (quote cogre-utest) "cogre-utest" "\
Unit test Various aspects of COGRE.

\(fn)" t nil)

(autoload (quote cogre-utest-quick-class) "cogre-utest" "\
Test the quick-class function.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-picture-insert-rectangle) "picture-hack"
;;;;;;  "picture-hack.el" (18897 57890))
;;; Generated autoloads from picture-hack.el

(autoload (quote cogre-picture-insert-rectangle) "picture-hack" "\
Overlay RECTANGLE with upper left corner at point.
Leaves the region surrounding the rectangle.

\(fn RECTANGLE)" nil nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "wisent-dot.el"
;;;;;;  (18906 40873))
;;; Generated autoloads from wisent-dot.el

(autoload (quote wisent-dot-setup-parser) "wisent-dot" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook (quote graphviz-dot-mode-hook) (quote wisent-dot-setup-parser))

(add-hook (quote cogre-dot-mode-hook) (quote wisent-dot-setup-parser))

;;;***

;;;### (autoloads nil nil ("cogre-load.el" "wisent-dot-wy.el") (19003
;;;;;;  26009 19045))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cogre-loaddefs.el ends here
