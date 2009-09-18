;; css3
;;
;; Copyright (c) 2009 Brian Pattison. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.
;
; based on joshua choi's FnParse and JSON parser
;
(ns parser.css3
  (:use name.choi.joshua.fnparse
        clojure.contrib.error-kit
        [clojure.contrib.seq-utils :only [flatten]]))

;
; CSS3 -- Tokens
;
; wc          ::= #x9 | #xA | #xC | #xD | #x20
; ws          ::= wc*
; unicode     ::= '\' [0-9a-fA-F]{1,6} wc?
; num         ::= [0-9]+ | [0-9]* '.' [0-9]+
; nonascii    ::= [#x80-#xD7FF#xE000-#xFFFD#x10000-#x10FFFF]
; escape      ::= unicode | '\' [#x20-#x7E#x80-#xD7FF#xE000-#xFFFD#x10000-#x10FFFF]
; nmstart     ::= [a-zA-Z] | '_' | nonascii | escape
; nmchar      ::= [a-zA-Z0-9] | '-' | '_' | nonascii | escape
; ident       ::= '-'? nmstart nmchar*
; hash-name   ::= nmchar+
; urlchar     ::= [#x9#x21#x23-#x26#x27-#x7E] | nonascii | escape
; nl          ::= #xA | #xD #xA | #xD | #xC
; stringchar  ::= urlchar | #x20 | '\' nl
; string      ::= '"' (stringchar | "'")* '"' | "'" (stringchar | '"')* "'"
;
; The following productions are the complete list of tokens in CSS3:
;
; IDENT       ::= ident
; ATKEYWORD   ::= '@' ident
; STRING      ::= string
; HASH        ::= '#' name
; NUMBER      ::= num
; PERCENTAGE  ::= num '%'
; DIMENSION   ::= num ident
; URI         ::= "url(" w (string | urlchar* ) w ")"
; UNICODE-RANGE ::= "U+" [0-9A-F?]{1,6} ('-' [0-9A-F]{1,6})?
; CDO         ::= "<!--"
; CDC         ::= "-->"
; S           ::= wc+
; COMMENT     ::= "/*" [^*]* '*'+ ([^/] [^*]* '*'+)* "/"
; FUNCTION    ::= ident '('
; INCLUDES    ::= "~="
; DASHMATCH   ::= "|="
; PREFIXMATCH ::= "^="
; SUFFIXMATCH ::= "$="
; SUBSTRINGMATCH  ::= "*="
; CHAR  ::= any other character not matched by the above rules, except for " or '
; BOM ::= #xFEFF
;
; Style Sheet Grammar
;
; any         : [ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
;               | DELIM | URI | HASH | UNICODE-RANGE | INCLUDES
;               | FUNCTION S* any* ')' | DASHMATCH | '(' S* any* ')'
;               | '[' S* any* ']' ] S*;
; block       : '{' S* [ any | block | ATKEYWORD S* | ';' S* ]* '}' S*;
; at-rule     : ATKEYWORD S* any* [ block | ';' S* ];
; selector    : any+;
; ruleset     : selector? '{' S* declaration? [ ';' S* declaration? ]* '}' S*;
; property    : IDENT S*;
; value       : [ any | block | ATKEYWORD S* ]+;
; declaration : property ':' S* value;
; statement   : ruleset | at-rule;
; stylesheet  : [ CDO | CDC | S | statement ]*;

(defstruct node-s  :type :data) 

(defstruct state-s :remainder :column :line)

(def remainder-a (accessor state-s :remainder))

(def make-node (partial struct node-s))

(def make-scalar-node (partial make-node :scalar))

(def make-ident-node (partial make-node :ident))

(def make-name-node (partial make-node :name))

(def make-block-node (partial make-node :block))

(def make-at-node (partial make-node :at))

(def apply-str (partial apply str))

(defn- vjoin [v i]
  (apply vector (remove nil? (cons i v))))

(defn- nb-char [subrule]
  (invisi-conc subrule (update-info :column inc)))

(def nb-char-lit
  (comp nb-char lit))

(defn- b-char [subrule]
  (invisi-conc subrule (update-info :line inc)))

(deferror parse-error [] [state message message-args]
  {:msg (str (format "CSS3 error at line %s, column %s: "
               (:line state) (:column state))
             (apply format message message-args))
   :unhandled (throw-msg Exception)})

(defn- expectation-error-fn [expectation]
  (fn [remainder state]
    (raise parse-error state "%s expected where \"%s\" is"
      [expectation (or (first remainder) "the end of the file")])))

;; And here are where this parser's rules are defined.

(def string-delimiter
  (nb-char-lit \"))

(def escape-indicator
  (nb-char-lit \\))

(def space (nb-char-lit \space))

(def tab (nb-char-lit \tab))

(def newline-lit (lit \newline))

(def return-lit (lit \return))

(def line-break (b-char (rep+ (alt newline-lit return-lit))))

(def str-char (alt line-break (nb-char anything)))

(def ws (constant-semantics (rep* (alt space tab line-break)) :ws))

(def begin-block
  (constant-semantics (conc ws (nb-char-lit \{) ws) :begin-block))

(def end-block
  (constant-semantics (conc ws (nb-char-lit \}) ws) :end-block))

(def end-stmt
  (constant-semantics (conc ws (nb-char-lit \;) ws) :end-stmt))

(def declaration-separator
  (constant-semantics (conc ws (nb-char-lit \:) ws) :declaration-separator))

(def value-separator
  (constant-semantics (conc ws (nb-char-lit \,) ws) :value-separator))

; uri
(def uri-key
  (constant-semantics
    (conc ws (nb-char-lit \u) (nb-char-lit \r) (nb-char-lit \l) ws (nb-char-lit \()) 
    :url-key))

(def minus-sign (nb-char-lit \-))

(def plus-sign (nb-char-lit \+))

(def decimal-point (nb-char-lit \.))

(def zero-digit (nb-char-lit \0))

(def alpha
  (lit-alt-seq
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    nb-char-lit))

(def nonzero-decimal-digit (lit-alt-seq "123456789" nb-char-lit))

(def decimal-digit (alt zero-digit nonzero-decimal-digit))

(def fractional-part (conc decimal-point (rep* decimal-digit)))

; num ::= [0-9]+ | [0-9]* '.' [0-9]+
(def number
  (complex [m (opt minus-sign)
            a (alt zero-digit (rep+ decimal-digit))
            b (opt fractional-part) ]
    (-> [m a b] flatten apply-str
        Double/parseDouble
        ((if b identity int))
      make-scalar-node)))

; hex digit
(def hexadecimal-digit
  (alt decimal-digit (lit-alt-seq "ABCDEF" nb-char-lit)))

; unicode ::= '\' [0-9a-fA-F]{1,6} wc?
(def unicode-char
  (complex [_ (nb-char-lit \\)
            d (factor= 4
                (failpoint hexadecimal-digit
                  (expectation-error-fn "unicode-char: hexadecimal digit")))]
    (-> d apply-str (Integer/parseInt 16) char)))

(def escaped-characters
  {\\ \\, \/ \/, \b \backspace, \f \formfeed, \n \newline, \r \return, \t \tab})

(def normal-escape
  (semantics (lit-alt-seq (keys escaped-characters) nb-char-lit)
    escaped-characters))

; escape ::= unicode | '\' [#x20-#x7E#x80-#xD7FF#xE000-#xFFFD#x10000-#x10FFFF]
(def escape
  (complex [_ escape-indicator
            c (alt unicode-char normal-escape)] c))

; nmstart ::= [a-zA-Z] | '_' | nonascii | escape
(def nmstart (alt alpha (nb-char-lit \.) (nb-char-lit \_) escape))
 
; nmchar ::= [a-zA-Z0-9] | '-' | '_' | nonascii | escape
(def nmchar (alt alpha decimal-digit (nb-char-lit \.) (nb-char-lit \-) (nb-char-lit \_) escape))

; ident ::= '-'? nmstart nmchar*
(def ident
  (complex [s nmstart
            d (rep* nmchar)
            _ ws]
    (struct node-s :ident (apply-str s d))))

; hash-name ::= nmchar+
(def hash-name
  (complex [fchar nmchar data (rep* nmchar) _ ws]
    (-> (list fchar (apply-str data)) apply-str make-name-node)))

(def unescaped-char
  (except str-char (alt escape-indicator string-delimiter)))

; stringchar ::= urlchar | #x20 | '\' nl
(def string-char (alt escape unescaped-char))

; string ::= '"' (stringchar | "'")* '"' | "'" (stringchar | '"')* "'"
(def string
  (complex [_ string-delimiter
            d (rep* string-char)
            _ string-delimiter]
    (-> d apply-str make-scalar-node)))

; urlchar ::= [#x9#x21#x23-#x26#x27-#x7E] | nonascii | escape
(def urlchar
  (except str-char (alt (nb-char-lit \space) (nb-char-lit \() (nb-char-lit \)) string-delimiter)))

(def urlstring
  (complex [ d (rep* urlchar) ]
    (-> d apply-str make-scalar-node)))

; PERCENTAGE ::= num '%'
(def percentage
  (complex [n number
            _ (nb-char-lit \%)]
    (-> (list (double (/ (:data n) 100))) apply-str make-scalar-node)))

; DIMENSION ::= num ident
(def dimension 
  (complex [n number
            _ ws
            i ident]
    (struct-map node-s :type :dimension :data (apply-str (:data n) (:data i)))))

; URI ::= "url(" w (string | urlchar* ) w ")"
(def uri
  (complex [_ uri-key
            _ ws
            a (alt string urlstring)
            _ ws
            _ (failpoint  (nb-char-lit \))
                (expectation-error-fn
                  (str "uri: expected end paren \")\"")))]
    (struct-map node-s :type :uri :data (:data a))))

; ATKEYWORD   ::= '@' ident
(def at-keyword
  (complex [_ (nb-char-lit \@)
            d ident
            _ ws]
    (-> d apply-str make-at-node)))

; HASH ::= '#' name
(def hash-rule
  (complex [_ (nb-char-lit \#)
            n hash-name
            _ ws]
    (struct-map node-s :type :hash :data (str "#" (:data n)))))

; INCLUDES ::= "~="
(def include-key
  (constant-semantics
    (conc ws (nb-char-lit \~) (nb-char-lit \=)) 
    :include-key))

; INCLUDES ::= "~="
(def include-rule
  (complex [_ include-key 
            _ ws]
    (struct node-s :include)))

; DASHMATCH ::= "|="
(def dashmatch-key
  (constant-semantics
    (conc ws (nb-char-lit \|) (nb-char-lit \=))
    :dashmatch-key))

; DASHMATCH ::= "|="
(def dashmatch
  (complex [_ dashmatch-key 
            _ ws]
    (struct node-s :dashmatch)))

; DELIM
(def delim 
  (complex [_ (nb-char-lit \:)]
    (struct node-s :delimiter ":")))

; any : [ IDENT | NUMBER | PERCENTAGE  | DIMENSION     | STRING
;               | DELIM  | URI | HASH  | UNICODE-RANGE | INCLUDES
;               | FUNCTION S* any* ')' | DASHMATCH     | '(' S* any* ')'
;               | '[' S* any* ']' ] S*;
(def any
  (alt dashmatch include-rule dimension hash-rule
       percentage uri ident string number delim))

; ATKEYWORD ::= '@' ident
(def at-keyword
  (complex [_ (nb-char-lit \@)
            d ident
            _ ws]
    (-> d apply-str make-at-node)))

(def block-data
  (complex [f any 
            r (rep* any)]
    (cons f r)))

; block : '{' S* [ any | block | ATKEYWORD S* | ';' S* ]* '}' S*;
(def block
  (complex [_ begin-block
            d block-data
            _ (failpoint end-block
                (expectation-error-fn
                  (str "block: expected end block \"}\"")))]
    (struct node-s :block (into {} d))))

; at-rule : ATKEYWORD S* any* [ block | ';' S* ];
(def at-rule
  (complex [k at-keyword
            a (rep* any)
            b (alt block
                (failpoint declaration-separator
                  (expectation-error-fn
                    (str "at-rule: expected end statement \";\""))))
            ]
    (struct node-s :at-rule (into {} a))))

; selector : any+;
(def selector (rep+ any))

; property : IDENT S*;
(def property
  (complex [i ident 
            _ ws]
    (struct node-s :property (:data i))))


; value : [ any | block | ATKEYWORD S* ]+;
(def value
  (complex [v (alt any block at-keyword)]
    (struct node-s :value v)))

(def value-block
  (complex [_ (opt value-separator)
            _ ws
            v value] v))

; declaration : property ':' S* value;
(def declaration
  (complex [p property
            _ ws
            _ (failpoint declaration-separator
                (expectation-error-fn
                  (str "declaration: expected seperator \":\"")))
            s (rep+ value-block)]
    (struct-map node-s :type     :declaration
                       :property (:data p)
                       :data     (apply vector (map (fn [i] (:data i)) s)) ) ))

(def decl-block
  (complex [_ end-stmt
            _ ws
            d (opt declaration)] d))

(defn- selector-to-str [eseq]
  (reduce (fn [s e] (str s (:data e))) nil eseq))

; ruleset : selector? '{' S* declaration? [ ';' S* declaration? * '}' S*;
(def ruleset
  (complex [e selector
            _ ws
            _ (failpoint begin-block
                (expectation-error-fn
                  (str "ruleset: expected begin block \"{\"")))
            _ ws
            d (opt declaration)
            _ ws
            s (opt (rep* decl-block))
            _ ws
            _ (failpoint end-block
                (expectation-error-fn
                  (str "ruleset: expected end block \"}\"")))]
    (struct-map node-s :type :ruleset :selector (selector-to-str e) :data (vjoin s d))))

; statement : ruleset | at-rule;
(def statement (alt ruleset at-rule))

; stylesheet : [ CDO | CDC | S | statement ]*;
(def stylesheet (rep* statement ))

(defn css3parse [tokens]
  (binding [*remainder-accessor* remainder-a] ; this is completely optional
    (rule-match stylesheet
      #(raise parse-error "invalid document \"%s\""
         (apply-str (remainder-a %)))
      #(raise parse-error "leftover data after a valid node \"%s\""
         (apply-str (remainder-a %2)))
      (struct state-s tokens 0 0))))

(defn- filter-by-type [t p]
  (filter (fn [i] (= (:type i) t)) p))

(defn- filter-by-regex [re t p]
  (filter (fn [i] (re-find re (t i))) p))

(defn- filter-by-string [s t p]
  (filter (fn [i] (= s (t i))) p))

(defn get-data [a] (:data a))

(defn- what-type? [a & p] (if (number? a) :number (if (string? a) :string :regex)))

(defmulti get-property what-type?)

(defmethod get-property :number [a p]
  (let [props (filter-by-type :declaration p)]
    (apply vector (get-data (nth props a))) ))

(defmethod get-property :regex [a p]
  (let [props (filter-by-type :declaration p)]
    (apply vector (apply get-data (filter-by-regex a :property props))) ))

(defmethod get-property :string [a p]
  (let [props (filter-by-type :declaration p)]
    (apply vector (apply get-data (filter-by-string a :property props))) ))

(defmulti get-ruleset what-type?)

(defmethod get-ruleset :number [a p]
  (let [rs (filter-by-type :ruleset p)]
    (apply vector (get-data (nth rs a))) ))

(defmethod get-ruleset :regex [a p]
  (let [rs (filter-by-type :ruleset p)]
    (apply vector (apply get-data (filter-by-regex a :selector p))) ))

(defmethod get-ruleset :string [a p]
  (let [rs (filter-by-type :ruleset p)]
    (apply vector (apply get-data (filter-by-string a :selector p))) ))
