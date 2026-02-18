(ns

    ^{:doc    "SQL helpers for ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.db.sql

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                   :as                    set]
            [clojure.string                :as                    str]
            [next.jdbc.quoted              :as                 quoted]
            [potemkin                      :as                      p]
            [io.randomseed.utils           :refer                :all]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.var       :as                    var]
            [ferment                     :refer                :all]
            [ferment.app                 :as                    app]
            [ferment.system              :as                 system]
            [ferment.logging             :as                    log]
            [ferment.types.db            :refer                :all])

  (:import  (clojure.lang IPersistentMap
                          PersistentVector
                          Keyword
                          Fn)
            (java.sql     PreparedStatement
                          ParameterMetaData
                          Statement)))

(set! *warn-on-reflection* true)

;; Builder and conversion functions

(p/import-vars [io.randomseed.utils.db
                to-lisp-simple to-snake-simple to-lisp to-snake
                to-lisp-slashed to-snake-slashed])

;; SQL strings preparation

(p/import-vars [io.randomseed.utils.db
                join-col-names braced-join-col-names braced-join-col-names-no-conv
                join-? braced-join-? join-v=? values-? braced-?])

;; SQL helpers

(p/import-vars [next.jdbc.sql
                query update! delete! find-by-keys])

(p/import-vars [io.randomseed.utils.db
                for-replace for-insert-or for-replace-multi for-insert-multi-or
                insert-or! insert-multi-or!
                insert-or-replace-multi! insert-or-ignore-multi!
                insert-or-replace! insert-or-ignore!
                replace! replace-multi!])

;; Database result processing helpers

(p/import-vars [io.randomseed.utils.db get-failed? id-from-db id-to-db])

;; Column and table names processing

(defn quoted
  "Quotes the given string according to MySQL / MariaDB quoting rules."
  ^String [s]
  (if-some [^String s (some-str s)]
    (quoted/mysql s)))

(defn idname
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns the string representation of
  the given object with slashes replaced by dot characters."
  ^String [v]
  (if (ident? v)
    (if-some [^String nsp (namespace v)]
      (str nsp "." (name v))
      (name v))
    (if-some [^String v (some-str v)]
      (str/replace v \/ \.))))

(defn idname-simple
  "If the given value `v` is an ident, it returns its name. Otherwise it returns the
  string representation of the given object or `nil` if the string is empty."
  ^String [v]
  (if (ident? v) (name v) (some-str v)))

(defn dbname
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns a string representation of the
  given object with a first slash replaced by a dot."
  ^String [v]
  (if (ident? v)
    (if-some [^String nsp (namespace v)]
      (strb nsp "." (name v))
      (name v))
    (if-some [^String v (some-str v)]
      (str/replace-first v \/ \.))))

(defn dbname-quoted
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns a string representation of the
  given object with a first slash replaced by a dot. Each part of a name will be
  quoted."
  ^String [v]
  (if (ident? v)
    (if-some [^String nsp (namespace v)]
      (strb (quoted nsp) "." (quoted (name v)))
      (name v))
    (if-some [^String v (some-str v)]
      (if (str/index-of v \/)
        (dbname-quoted (keyword v))
        (if (str/index-of v \.)
          (dbname-quoted (keyword (str/replace-first v \. \/)))
          (quoted v))))))

(defn dbname-kw
  "If the given value `v` is an ident, it returns its keyword representation. Otherwise
  it returns a string representation of the given object with dots replaced by
  slashes."
  ^String [v]
  (if (ident? v)
    (keyword v)
    (if-some [^String v (some-str v)]
      (if (str/index-of v \/)
        (keyword v)
        (keyword (str/replace-first v \. \/))))))

(defn make-kw
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. If the second argument is `nil` then a keyword is created using
  the first argument by simply converting it with the `keyword` function. If both
  `ns` and `name` are given then the following is applied: if `ns` or `name` is a
  qualified ident, its name and namespace will be joined with a dot character before
  producing a keyword; additionally, if `ns` or `name` is a simple ident, any slash
  character in its name will be replaced with a dot. If `ns` or `name` is not an
  ident then any slash character in its string representation will be replaced with a
  dot before creating a keyword."
  (^Keyword [name]
   (if (keyword? name) name (keyword name)))
  (^Keyword [ns name]
   (if name
     (keyword (idname ns) (idname name))
     (if (keyword? ns) ns (keyword ns)))))

(defn make-kw-simple
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. If the second argument is `nil` then a keyword is created using
  the first argument by simply converting it with the `keyword` function. If any
  given ident is namespaced, only its name is used."
  (^Keyword [name]
   (if (keyword? name) name (keyword name)))
  (^Keyword [ns name]
   (if name
     (keyword (idname-simple ns) (idname-simple name))
     (if (keyword? ns) ns (keyword ns)))))

(defn make-kw-snake
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. All hyphen characters will be replaced by underscores. If the
  second argument is `nil` then a keyword is created using the first argument by
  simply converting it with the `keyword` function. If any given ident is namespaced,
  only its name is used."
  (^Keyword [name]
   (keyword (db/to-snake name)))
  (^Keyword [ns name]
   (if name
     (keyword (db/to-snake (idname-simple ns)) (db/to-snake (idname-simple name)))
     (keyword (db/to-snake ns)))))

(defn make-kw-lisp
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. All underscore characters will be replaced by hyphens. If the
  second argument is `nil` then a keyword is created using the first argument by
  simply converting it with the `keyword` function. If any given ident is namespaced,
  only its name is used."
  (^Keyword [name]
   (keyword (db/to-lisp name)))
  (^Keyword [ns name]
   (if name
     (keyword (db/to-lisp (idname-simple ns)) (db/to-lisp (idname-simple name)))
     (keyword (db/to-lisp ns)))))

;; Tables and columns

(defn colspec
  "Converts a `table/column`-formatted identifier `col-spec` into a snake-cased string
  with first slash replaced by a dot character. If `table-id` and `col-id` are given,
  creates a string of those parts joined with a dot character. If single identifier
  is given, it uses its namespace and name.

  Example results: `\"table_name.column_name\"` or `\"simple_name\"`"
  (^String [col-spec]
   (if (ident? col-spec)
     (colspec (namespace col-spec) (name col-spec))
     (if-some [^String col-spec (db/to-snake col-spec)]
       (str/replace-first col-spec \/ \.))))
  (^String [table-id col-id]
   (if-some [^String table-id (some-str table-id)]
     (if-some [^String col-id (some-str col-id)]
       (db/to-snake (strb table-id "." col-id))
       (db/to-snake table-id))
     (if-some [^String col-id (some-str col-id)]
       (db/to-snake col-id)))))

(defn colspec-quoted
  "Converts a `table/column`-formatted identifier `col-spec` into a snake-cased string
  with first slash replaced by a dot character. If `table-id` and `col-id` are given,
  creates a string of those parts joined with a dot character. If identifier is
  given, it uses its namespace and name. Each part of the name will be quoted.

  If the `col-spec` is a string and there is a slash character present in it, it will
  not be checked for a dot character presence.

  Example results: ``\"`table_name`.`column_name`\"`` or ``\"`simple_name`\"``"
  (^String [col-spec]
   (if (ident? col-spec)
     (colspec-quoted (namespace col-spec) (name col-spec))
     (if-some [^String col-spec (some-str col-spec)]
       (if (str/index-of col-spec \/)
         (colspec-quoted (keyword col-spec))
         (if (str/index-of col-spec \.)
           (colspec-quoted (keyword (str/replace-first col-spec \. \/)))
           (quoted (db/to-snake col-spec)))))))
  (^String [table-id col-id]
   (if-some [^String table-id (some-str table-id)]
     (if-some [^String col-id (some-str col-id)]
       (db/to-snake (strb (quoted table-id) "." (quoted col-id)))
       (quoted (db/to-snake table-id)))
     (if-some [^String col-id (some-str col-id)]
       (quoted (db/to-snake col-id))))))

(defn colspec-kw
  "Converts a `table/column` or `table.column`-formatted identifier `table-col` into a
  lisp-cased keyword. If `table-id` and `col-id` are given, it creates a string of
  those parts joined with a dot character. If identifier is given, it uses its
  namespace and name.

  For strings and objects convertable to a string, first slash or dot character will
  be used as a split point. If the `col-spec` is a string and there is a slash
  character present in it, it will not be checked for a dot character presence.

  Example results: `:table-name/column-name` or `:simple-name`"
  (^String [col-spec]
   (if (ident? col-spec)
     (colspec-kw (namespace col-spec) (name col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \/)
         (keyword (db/to-lisp col-spec))
         (if (str/index-of col-spec \.)
           (keyword (db/to-lisp (str/replace-first col-spec \. \/)))
           (keyword (db/to-lisp col-spec)))))))
  (^String [table-id col-id]
   (if-some [^String table-id (some-str table-id)]
     (if-some [^String col-id (some-str col-id)]
       (keyword (db/to-lisp (strb table-id "/" col-id)))
       (keyword (db/to-lisp table-id)))
     (if-some [^String col-id (some-str col-id)]
       (keyword (db/to-lisp col-id))))))

(defn table
  "Extracts table name as a snake-cased string from `col-spec` which may be an
  identifier or a string. If the identifier has a namespace, it will be used,
  otherwise its name will be used. For string, it will look for a slash or dot
  character used as a separator between a table and a column name, to extract
  the table name. If two arguments are given, the second one is ignored.

  Example result: `\"table_name\"`"
  (^String [col-spec]
   (if (ident? col-spec)
     (db/to-snake (or (namespace col-spec) (name col-spec)))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \.)
         (table (keyword (str/replace-first col-spec \. \/)))
         (if (str/index-of col-spec \/)
           (table (keyword col-spec))
           (db/to-snake col-spec))))))
  (^String [col-spec _] (table col-spec)))

(defn column
  "Extracts column name as a snake-cased string from `col-spec` which may be an
  identifier or a string. If the identifier has a name and a namespace, its name will
  be used. For string, it will look for a slash or dot character used as a separator
  between a table and a column name, to extract the column name. If two arguments are
  given, the first one is ignored.

  Example result: `\"column_name\"`"
  (^String [col-spec]
   (if (ident? col-spec)
     (db/to-snake (name col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \.)
         (column (keyword (str/replace-first col-spec \. \/)))
         (if (str/index-of col-spec \/)
           (column (keyword col-spec))
           (db/to-snake col-spec))))))
  (^String [_ col-spec] (column col-spec)))

(def ^{:tag      String
       :arglists '(^String [col-spec] ^String [_ col-spec])}
  col
  "Alias for `column`. Extracts column name as a snake-cased string from `col-spec`
  which may be an identifier or a string. If the identifier has a name, it will be
  used. For string, it will look for a slash or dot character used as a separator
  between a table and a column name, to extract the column name. If two arguments are
  given, the first one is ignored.

  Example result: `\"column_name\"`"
  column)

(defn table-column
  "Extracts table and column names from `col-spec` (which may be an identifier or a
  string) as snake-cased strings of a 2-element vector (first element being a table
  name, second a column name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a table name. If two
  arguments are given, names are extracted separately using `table` and `column`
  functions). If string is given (or an object convertable to a string), a dot or
  slash character will be used as a splitting point to extract table and column name.
  Single string without any separator character will be considered a table name.

  Example results: `[\"table_name\" \"column_name\"]`,  `[\"table_name\" nil]`"
  ([col-spec]
   (if (ident? col-spec)
     (table-column (some-str col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if-some [col-spec (keyword (db/to-snake (str/replace-first col-spec \. \/)))]
         (if-some [n (namespace col-spec)]
           [n (name col-spec)]
           [(name col-spec) nil])))))
  ([col-spec col-id]
   [(table col-spec) (column col-id)]))

(def ^{:arglists '([col-spec] [col-spec col-id])}
  table-col
  "Alias for `table-column`. Extracts table and column names from `col-spec` (which may
  be an identifier or a string) as snake-cased strings of a 2-element vector (first
  element being a table name, second a column name). If `col-spec` is an identifier,
  its namespace and name will be used. If there is no namespace, it will be
  considered a table name. If two arguments are given, names are extracted separately
  using `table` and `column` functions). If string is given (or an object convertable
  to a string), a dot or slash character will be used as a splitting point to extract
  table and column name.  Single string without any separator character will be
  considered a table name.

  Example results: `[\"table_name\" \"column_name\"]`, `[\"table_name\" nil]`"
  table-column)

(defn column-table
  "Extracts column and table names from `col-spec` (which may be an identifier or a
  string) as snake-cased strings of a 2-element vector (first element being a column
  name, second a table name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a column name. If two
  arguments are given, names are extracted separately using `column` and `table`
  functions). If string is given (or an object convertable to a string), a dot or
  slash character will be used as a splitting point to extract table and column name.
  Single string without any separator character will be considered a table name.

  Example results: `[\"column_name\" \"table_name\"]`, `[\"column_name\" nil]`"
  ([col-spec]
   (if (ident? col-spec)
     (column-table (some-str col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if-some [col-spec (keyword (db/to-snake (str/replace-first col-spec \. \/)))]
         [(name col-spec) (namespace col-spec)]))))
  ([col-id col-spec]
   [(column col-id) (table col-spec)]))

(def ^{:arglists '([col-spec] [col-id col-spec])}
  col-table
  "Alias for `column-table`. Extracts column and table names from `col-spec` (which may
  be an identifier or a string) as snake-cased strings of a 2-element vector (first
  element being a column name, second a table name). If `col-spec` is an identifier,
  its namespace and name will be used. If there is no namespace, it will be
  considered a column name. If two arguments are given, names are extracted
  separately using `column` and `table` functions). If string is given (or an object
  convertable to a string), a dot or slash character will be used as a splitting
  point to extract table and column name.  Single string without any separator
  character will be considered a table name.

  Example results: `[\"column_name\" \"table_name\"]`, `[\"column_name\" nil]`"
  column-table)

(defn table-kw
  "Extracts table name as a lisp-cased keyword from `col-spec` which may be an
  identifier or a string. If the identifier has a namespace, it will be used,
  otherwise its name will be used. For strings (or objects convertable to strings),
  it will detect slash and dot characters as separators of a namespace and name to
  pick a table name. If two arguments are given, the second one is ignored.

  Example result: `:table-name`"
  (^Keyword [col-spec]
   (if (ident? col-spec)
     (keyword (db/to-lisp (or (namespace col-spec) (name col-spec))))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \.)
         (table-kw (keyword (str/replace-first col-spec \. \/)))
         (if (str/index-of col-spec \/)
           (table-kw (keyword col-spec))
           (keyword (db/to-lisp table-kw)))))))
  (^Keyword [table-id _] (table-kw table-id)))

(defn column-kw
  "Extracts column name as a lisp-cased keyword from `col-spec` which may be an
  identifier or a string. If the identifier has a name, it will be used. For
  strings (or objects convertable to strings), it will detect slash and dot
  characters as separators of a namespace and name to pick a column name. If two
  arguments are given, the first one is ignored.

  Example result: `:column-name`"
  (^Keyword [col-spec]
   (if (ident? col-spec)
     (keyword (db/to-lisp (name col-spec)))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \.)
         (column-kw (keyword (str/replace-first col-spec \. \/)))
         (if (str/index-of col-spec \/)
           (column-kw (keyword col-spec))
           (keyword (db/to-lisp col-spec)))))))
  (^Keyword [_ col-id] (column-kw col-id)))

(def ^{:tag      Keyword
       :arglists '(^Keyword [col-spec] ^Keyword [_ col-id])}
  col-kw
  "Alias for `column-kw`. Extracts column name as a lisp-cased keyword from `col-spec`
  which may be an identifier or a string. If the identifier has a name, it will be
  used. For strings (or objects convertable to strings), it will detect slash and dot
  characters as separators of a namespace and name to pick a column name. If two
  arguments are given, the first one is ignored.

  Example result: `:column-name`"
  column-kw)

(defn table-column-kw
  "Extracts table and column names from `col-spec` (which may be an identifier or a
  string) as lisp-cased keywords of a 2-element vector (first element being a table
  name, second a column name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a table name. If two
  arguments are given, names are extracted separately using `table-kw` and
  `column-kw` functions). For strings (or objects convertable to strings), it will
  detect slash and dot characters as separators of a namespace and a name.

  Example results: `[:table-name :column-name]`, `[:table-name nil]`"
  ([col-spec]
   (if-some [col-spec (some-str col-spec)]
     (let [k (keyword (db/to-lisp (str/replace-first col-spec \. \/)))]
       (if-some [n (namespace k)]
         [(keyword (namespace k)) (keyword (name k))]
         [(keyword (name k)) nil]))))
  ([col-spec col-id]
   [(table-kw col-spec) (column-kw col-id)]))

(def ^{:tag      Keyword
       :arglists '(^Keyword [col-id] ^Keyword [col-spec col-id])}
  table-col-kw
  "Alias for `table-column-kw`. Extracts table and column names from `col-spec` (which
  may be an identifier or a string) as lisp-cased keywords of a 2-element
  vector (first element being a table name, second a column name). If `col-spec` is
  an identifier, its namespace and name will be used. If there is no namespace, it
  will be considered a table name. If two arguments are given, names are extracted
  separately using `table-kw` and `column-kw` functions). For strings (or objects
  convertable to strings), it will detect slash and dot characters as separators of a
  namespace and a name.

  Example results: `[:table-name :column-name]`, `[:table-name nil]`"
  table-column-kw)

(defn column-table-kw
  "Extracts column and table names from `col-spec` (which may be an identifier or a
  string) as lisp-cased keywords of a 2-element vector (first element being a column
  name, second a table name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a column name. If two
  arguments are given, names are extracted separately using `column-kw` and
  `table-kw` functions). For strings (or objects convertable to strings), it will
  detect slash and dot characters as separators of a namespace and a name.

  Example results: `[:column-name :table-name]`, `[:column-name nil]`"
  ([col-spec]
   (if-some [col-spec (some-str col-spec)]
     (let [k (keyword (db/to-lisp (str/replace-first col-spec \. \/)))]
       [(keyword (name k)) (keyword (namespace k))])))
  ([col-id col-spec]
   [(column-kw col-id) (table-kw col-spec)]))

(def ^{:tag      Keyword
       :arglists '(^Keyword [col-spec] ^Keyword [col-id col-spec])}
  col-table-kw
  "Alias for `column-table-kw`. Extracts column and table names from `col-spec` (which
  may be an identifier or a string) as lisp-cased keywords of a 2-element
  vector (first element being a column name, second a table name). If `col-spec` is
  an identifier, its namespace and name will be used. If there is no namespace, it
  will be considered a column name. If two arguments are given, names are extracted
  separately using `column-kw` and `table-kw` functions). For strings (or objects
  convertable to strings), it will detect slash and dot characters as separators of a
  namespace and a name.

  Example results: `[:column-name :table-name]`, `[:column-name nil]`"
  column-table-kw)

;; SQL query preparation

(defn- interpolate-tag
  "Interpolates tags with extracted values when building query."
  ^String [substitutions [_ quote? ^String modifier ^String tag]]
  (if-some [tag (and tag (get substitutions (some-keyword tag)))]
    (let [msym (and modifier (symbol modifier))
          f    (or (if msym (var/deref-symbol
                             (if (nil? (namespace msym))
                               (symbol "ferment.db.sql" modifier)
                               msym)))
                   identity)]
      (if-some [^String v (some-str (f tag))]
        (if quote? (quoted v) v)
        ""))
    ""))

(defn- interpolate-some?
  "Interpolates `%SOME` tag with the given substitution value when building query."
  [substitutions [_ ^String tag ^String on-true ^String on-false]]
  (if-let [tag (and tag (get substitutions (some-keyword tag)))]
    (str (some-str on-true))
    (str (some-str on-false))))

(defn- quote-tag
  "Quotes tag's value during pattern interpolation when building query."
  ^String [[_ ^String tag]]
  (or (colspec-quoted tag) ""))

(defn- interpolate-tags
  "Tag interpolation for substitution patterns."
  (^String [substitutions ^String q]
   (if substitutions
     (-> q
         (str/replace #"%SOME\? +([^ \:]+): *([^#]*)#(?:([^#]*)#)?" #(interpolate-some? substitutions %))
         (str/replace #"%\[([^\]]+)\]" "%%table{$1}")
         (str/replace #"%\(([^\)]+)\)" "%%column{$1}")
         (str/replace #"%\<([^\>]+)\>" "%colspec-quoted{$1}")
         (str/replace #"%\'([^\']+)\'" quote-tag)
         (str/replace #"%(%)?([^\<\>\{\}\[\]\s]+)?\{([^\}]+)?\}" #(interpolate-tag substitutions %)))
     (interpolate-tags q)))
  (^String [^String q]
   (str/replace q #"%\'([^\']+)\'" quote-tag)))

(def ^{:tag    String
       :no-doc true}
  build-query-core
  "For the given SQL query `q` and substitution map performs pattern
  interpolation. Uses `memoize+` memoization."
  (db/memoize+
   (fn build-query-fn
     (^String []                "")
     (^String [q]               (if q (interpolate-tags q) ""))
     (^String [q substitutions] (if q (interpolate-tags substitutions q) "")))
   2048 1024))

(def ^{:tag    String
       :no-doc true}
  build-query-dynamic-core
  "For the given SQL query `q` and substitution map performs pattern
  interpolation. Uses `memoize+` memoization."
  (db/memoize+
   (fn build-query-fn
     (^String []                "")
     (^String [q]               (if q (interpolate-tags q) ""))
     (^String [q substitutions] (if q (interpolate-tags substitutions q) "")))
   1024 4096))

(defmacro build-query
  "For the given SQL query `q` and substitution map performs pattern interpolation.
  If multiple arguments are given the last one will be treated as substitution map.

  Tries to convert possible literals given as query parts to strings and then trim
  them while squeezing repeated spaces at compile time. If some operation cannot be
  performed in that phase, it generates code which will convert an expression to a
  string at runtime. Then pattern interpolation is performed on the resulting string,
  using the provided `substitutions` map.

  If a source string contains `%{tag-name}` special pattern, `tag-name` will be
  looked up in substitution map and the whole pattern will be replaced by the
  corresponding value.

  If a tag name from pattern cannot be found in a substitution map, the pattern will
  be replaced by an empty string.

  A pattern may have a form of `%%{tag-name}`. In such case any non-`nil` value being
  a result of tag name resolution will be quoted using `ferment.db.sql/quote`.

  A synonym of `%%table{tag-name}` is `%[table-name]`.
  A synonym of `%%column{tag-name}` is `%(column-name)`.
  A synonym of `%%colspec{tag-name}` is `%<column-table-specification>`.

  A pattern may have additional modifier before the opening brace. It will be
  resolved as a symbolic function name to be called in order to transform a value
  associated with a tag name. If the name is not fully-qualified (does not contain a
  namespace part) its default namespace will be set to `ferment.db.sql`.

  There is a special pattern using `%SOME` tag:
  - `%SOME? variable:has-value#no-value` or
  - `%SOME? variable:has-value`

  It performs a substitution with `has-value` string if `variable` exists and is not
  `nil` and not `false`; otherwise it performs a substitution with `no-value` string
  or an empty string if the `no-value` was not given.

  There is also an additional pattern `%'column-table-specification'` which is a
  quotation pattern. It uses `colspec-quoted` function on a given text.

  Example:

  ```
  (build-query \"select %%column{id} from %%table{users}\"
               \"where\" :points '> 100
               {:id    :users/id
                :users :users/id})
  ```

  The above call will generate the following result:

  ```
  \"select `id` from `users` where points > 100\"
  ```

  This is synonymous to:

  ```
  (build-query \"select %(id) from %[users]\"
               \"where\" :points '> 100
               {:id    :users/id
                :users :users/id})
  ```

  This macro can optionally be called with a single literal sequence given as its
  first and only argument. In such cache the sequence should contain all arguments,
  including a substitution map, if applicable.

  This macro should NOT be used to dynamically generate queries having thousands of
  variant substitution parameters as it uses unlimited underlying cache. For such
  purposes please use `build-query-dynamic`, or simply utilize parameters of prepared
  statements.

  WARNING: Interpolation pattern may execute arbitrary code since it allows for any
  function name."
  {:arglists '([] [q] [q substitution-map] [coll] [& query-parts substitution-map])}
  ([] "")
  ([q]
   (if (sequential? q)
     `(build-query ~@q)
     `(build-query-core (strspc-squeezed ~q))))
  ([q substitutions]
   `(build-query-core (strspc-squeezed ~q) ~substitutions))
  ([a b & args]
   (let [v# (vec args)
         l# (peek v#)
         r# (subvec v# 0 (unchecked-dec-int (count v#)))]
     (if (or (const-form? l#) (simple-quote-form? l#))
       `(build-query-core (strspc-squeezed ~a ~b ~@v#))
       `(build-query-core (strspc-squeezed ~a ~b ~@r#) ~l#)))))

(defmacro build-query-dynamic
  "For the given SQL query `q` and substitution map performs pattern interpolation.
  If multiple arguments are given the last one will be treated as substitution map.

  Tries to convert possible literals given as query parts to strings and then trim
  them while squeezing repeated spaces at compile time. If some operation cannot be
  performed in that phase, it generates code which will convert an expression to a
  string at runtime. Then pattern interpolation is performed on the resulting string,
  using the provided `substitutions` map.

  If a source string contains `%{tag-name}` special pattern, `tag-name` will be
  looked up in substitution map and the whole pattern will be replaced by the
  corresponding value.

  If a tag name from pattern cannot be found in a substitution map, the pattern will
  be replaced by an empty string.

  A pattern may have a form of `%%{tag-name}`. In such case any non-`nil` value being
  a result of tag name resolution will be quoted using `ferment.db.sql/quote`.

  A synonym of `%%table{tag-name}` is `%[table-name]`.
  A synonym of `%%column{tag-name}` is `%(column-name)`.
  A synonym of `%%colspec{tag-name}` is `%<column-table-specification>`.

  A pattern may have additional modifier before the opening brace. It will be
  resolved as a symbolic function name to be called in order to transform a value
  associated with a tag name. If the name is not fully-qualified (does not contain a
  namespace part) its default namespace will be set to `ferment.db.sql`.

  There is also additional pattern `%'column-table-specification'` which is a
  quotation pattern. It uses `colspec-quoted` function on a given text.

  Example:

  ```
  (build-query-dynamic \"select %%column{id} from %%table{users}\"
                       \"where\" :points '> 100
                       {:id    :users/id
                        :users :users/id})
  ```

  The above call will generate the following result:

  ```
  \"select `id` from `users` where points > 100\"
  ```

  This is synonymous to:

  ```
  (build-query-dynamic \"select %%column{id} from %[users]\"
                       \"where\" :points '> 100
                       {:users :users/id})
  ```

  This macro can optionally be called with a single literal sequence given as its
  first and only argument. In such cache the sequence should contain all arguments,
  including a substitution map, if applicable.

  This macro should be used to dynamically generate queries having thousands of
  variant substitution parameters.

  WARNING: Interpolation pattern may execute arbitrary code since it allows for any
  function name."
  {:arglists '([] [q] [q substitution-map] [coll] [& query-parts substitution-map])}
  ([] "")
  ([q]
   (if (sequential? q)
     `(build-query ~@q)
     `(build-query-core (strspc-squeezed ~q))))
  ([q substitutions]
   `(build-query-dynamic-core (strspc-squeezed ~q) ~substitutions))
  ([a b & args]
   (let [v# (vec args)
         l# (peek v#)
         r# (subvec v# 0 (unchecked-dec-int (count v#)))]
     (if (or (const-form? l#) (simple-quote-form? l#))
       `(build-query-dynamic-core (strspc-squeezed ~a ~b ~@v#))
       `(build-query-dynamic-core (strspc-squeezed ~a ~b ~@r#) ~l#)))))

;; Grouped sequences processing

(defn groups-inverter
  "Helper function for transforming a map of sequences keyed with keywords into a map
  of elements found in those sequences (as keys) associated with results of calling a
  function on them with additional arguments, including original map's keys.

  In other words: transforms results of `clojure.core/group-by` into a single map,
  changing values found in sequences into keys, and associating values to those keys
  resulting from calling a function.

  Takes a function `f` and additional arguments (zero or more), and returns a
  function which takes a map `m`, identity type `id-type` and a sequence of
  identifiers `ids`, and calls `f` with all arguments and `id-type` passed on the
  sequence. Then it calls `clojure.core/into` to put the result of calling `f` into a
  map `m`.

  Example: `(groups-inverter get-ids db)`

  In this example a function will be returned, similar to the below:

  `(fn [m id-type ids] (into m (get-ids db id-type)))`.

  It is used mainly as a transformer in `reduce-kv` when dealing with multiple user
  identifiers grouped by identity type. Having a map of vectors grouped by identity
  type:

  ```
  {:email [#ferment.Identity {:id-type :email :value \"pw@gnu.org\"}],
   :id    [#ferment.Identity {:id-type :id :value 1}
           #ferment.Identity {:id-type :id, :value 42}]}
  ```

  we can call `(reduce-kv (groups-inverter get-ids db) {})` to get:

  ```
  {#ferment.Identity{:id-type :id, :value 1}               1
   #ferment.Identity{:id-type :id, :value 42}             42
   #ferment.Identity{:id-type :email, :value \"pw@gnu.org\"} 1}
  ```

  The `get-ids` will be called for each identity group, receiving a list of
  identities and passed arguments with identity type. After getting numerical user
  identifiers it will associate them with identity objects in a map."
  ([f]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f id-type) (into m)) m) m)))
  ([f a]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f a id-type) (into m)) m) m)))
  ([f a b]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f a b id-type) (into m)) m) m)))
  ([f a b c]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f a b c id-type) (into m)) m) m)))
  ([f a b c & more]
   (let [fargs (apply vector a b c more)]
     (fn [m ^Keyword id-type ids]
       (if id-type
         (or (some->> (not-empty ids) (conj fargs id-type) (apply f) (into m)) m) m)))))
