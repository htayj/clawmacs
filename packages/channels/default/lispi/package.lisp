(in-package :rplaca)

(deftool execute-read
  :name "read"
  :description "Read a text file. Relative paths use the current tool working directory; absolute and parent-relative paths are allowed. Output is truncated to 2000 lines by default."
  :call-style :raw-args
  :args ((path :type "string"
               :description "Lisp data :path, relative to the tool working directory or absolute.")
         (offset :type "integer"
                 :required nil
                 :description "Lisp data :offset, the 1-indexed line number to start reading from.")
         (limit :type "integer"
                :required nil
                :description "Lisp data :limit, the maximum number of lines to read.")))

(deftool execute-find
  :name "find"
  :description "Search for files by name or glob pattern. Returns paths relative to the tool working directory when possible."
  :call-style :raw-args
  :args ((pattern :type "string"
                  :description "Lisp data :pattern, a filename substring or wildcard pattern such as *.lisp or src/*.lisp.")
         (path :type "string"
               :required nil
               :description "Lisp data :path, the directory or file to search. Default: tool working directory.")
         (limit :type "integer"
                :required nil
                :description "Lisp data :limit, the maximum number of file paths to return.")
         (ignore-case :type "boolean"
                      :required nil
                      :description "Lisp data :ignore-case, true for case-insensitive matching.")))

(deftool execute-grep
  :name "grep"
  :description "Search file contents for a literal pattern. Returns matching lines with file paths and line numbers."
  :call-style :raw-args
  :args ((pattern :type "string"
                  :description "Lisp data :pattern, the literal text to search for.")
         (path :type "string"
               :required nil
               :description "Lisp data :path, the directory or file to search. Default: tool working directory.")
         (glob :type "string"
               :required nil
               :description "Lisp data :glob, optional wildcard pattern limiting searched files, such as *.lisp.")
         (ignore-case :type "boolean"
                      :required nil
                      :description "Lisp data :ignore-case, true for case-insensitive matching.")
         (limit :type "integer"
                :required nil
                :description "Lisp data :limit, the maximum number of matching lines to return.")))

(deftool execute-write
  :name "write"
  :description "Create or overwrite a text file. Relative paths use the tool working directory; absolute and parent-relative paths are allowed. Parent directories are created automatically."
  :call-style :raw-args
  :args ((path :type "string"
               :description "Lisp data :path, relative to the tool working directory or absolute.")
         (content :type "string"
                  :description "Lisp data :content, the complete file content to write. Parentheses must be balanced.")))

(deftool execute-edit
  :name "edit"
  :description "Edit a text file by replacing one exact :old-text occurrence with :new-text. Relative, absolute, and parent-relative paths are allowed."
  :call-style :raw-args
  :args ((path :type "string"
               :description "Lisp data :path, relative to the tool working directory or absolute.")
         (old-text :type "string"
                   :description "Lisp data :old-text, the exact text to find and replace. Must occur exactly once.")
         (new-text :type "string"
                   :description "Lisp data :new-text, the replacement text. Use an empty string to delete :old-text. The resulting file's parentheses must be balanced.")))
