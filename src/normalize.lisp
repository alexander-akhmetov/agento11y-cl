(in-package :sigil-cl)

;;; Message normalization — convert raw LLM API responses (hash-tables)
;;; into sigil-cl CLOS types. Supports Anthropic and OpenAI formats.

(defun normalize-role (role-string)
  "Map an API role string to a sigil-cl keyword."
  (cond
    ((equal role-string "user")      :user)
    ((equal role-string "assistant") :assistant)
    ((equal role-string "tool")      :tool)
    (t :user)))

(defun build-tool-name-map (messages)
  "Scan messages and build a hash-table mapping tool-call-id → tool name.
Handles both Anthropic (content blocks) and OpenAI (tool_calls array) formats."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (msg messages map)
      (when (hash-table-p msg)
        (let ((role (jget msg "role")))
          (when (equal role "assistant")
            ;; Anthropic: content blocks with type=tool_use
            (let ((content (jget msg "content")))
              (when (vectorp content)
                (loop for block across content
                      when (and (hash-table-p block)
                                (equal (jget block "type") "tool_use"))
                      do (let ((id (jget block "id"))
                               (name (jget block "name")))
                           (when (and id name)
                             (setf (gethash id map) name))))))
            ;; OpenAI: tool_calls array
            (let ((tool-calls (jget msg "tool_calls")))
              (when (vectorp tool-calls)
                (loop for call across tool-calls
                      when (hash-table-p call)
                      do (let ((id (jget call "id"))
                               (func (jget call "function")))
                           (when (and id (hash-table-p func))
                             (let ((name (jget func "name")))
                               (when name
                                 (setf (gethash id map) name))))))))))))))

(defun trimmed-string (value)
  "Return VALUE trimmed of surrounding whitespace, or \"\" when it is not a string.
A parsed JSON null is a symbol, and STRING-TRIM accepts a symbol as a string
designator, so without the type check a null would become the string \"NULL\"."
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun media-type-from-data-url (url)
  "Return the lowercased media type of a data: URL, or \"\" when URL is not one."
  (let ((trimmed (trimmed-string url)))
    (if (and (>= (length trimmed) 5)
             (string-equal "data:" trimmed :end2 5))
        (let* ((payload (subseq trimmed 5))
               (stop (position-if (lambda (c) (or (char= c #\;) (char= c #\,))) payload)))
          (if stop
              (string-downcase (trimmed-string (subseq payload 0 stop)))
              ""))
        "")))

(defun make-image-media-part (base64-data media-type source-url)
  "Build an image media-part from a provider image source, or NIL when unusable.
Falls back to a data: URL built from MEDIA-TYPE and BASE64-DATA when SOURCE-URL
is absent. A non-string value counts as absent."
  (let ((url (trimmed-string source-url))
        (mime (string-downcase (trimmed-string media-type))))
    (when (zerop (length mime))
      (setf mime (media-type-from-data-url url)))
    (when (zerop (length url))
      (let ((data (trimmed-string base64-data)))
        (when (or (zerop (length mime)) (zerop (length data)))
          (return-from make-image-media-part nil))
        (setf url (concatenate 'string "data:" mime ";base64," data))))
    (make-media-part :kind "image" :url url :mime-type mime :provider-type "image")))

(defun normalize-content-to-parts (content &optional tool-name-map)
  "Convert raw API content (string or vector of content blocks) into CLOS parts.
Returns a list of part objects (text-part, tool-call-part, etc.)."
  (cond
    ((stringp content)
     (when (plusp (length content))
       (list (make-text-part content))))
    ((vectorp content)
     (let ((parts nil))
       (loop for block across content
             when (hash-table-p block)
             do (let ((type (jget block "type")))
                  (cond
                    ((equal type "text")
                     (let ((text (jget block "text")))
                       (when (and text (stringp text) (plusp (length text)))
                         (push (make-text-part text) parts))))
                    ((equal type "thinking")
                     (let ((text (jget block "thinking")))
                       (when (and text (stringp text) (plusp (length text)))
                         (push (make-thinking-part text) parts))))
                    ((equal type "tool_use")
                     (let ((id (or (jget block "id") ""))
                           (name (or (jget block "name") ""))
                           (input (jget block "input")))
                       (push (make-tool-call-part
                              :id id :name name
                              :input-json (if input
                                              (handler-case (jzon:stringify input)
                                                (error () "{}"))
                                              ""))
                             parts)))
                    ((equal type "tool_result")
                     (let* ((tcid (or (jget block "tool_use_id") ""))
                            (raw-content (jget block "content"))
                            (content-str (if (stringp raw-content) raw-content
                                             (handler-case (jzon:stringify raw-content)
                                               (error () ""))))
                            (tool-name (when tool-name-map
                                         (gethash tcid tool-name-map))))
                       (push (make-tool-result-part
                              :tool-call-id tcid
                              :name tool-name
                              :content content-str
                              :is-error (jget block "is_error"))
                             parts)))
                    ;; Anthropic image block
                    ((equal type "image")
                     (let* ((source (jget block "source"))
                            (part (when (hash-table-p source)
                                    (make-image-media-part (jget source "data")
                                                           (jget source "media_type")
                                                           (jget source "url")))))
                       (when part (push part parts))))
                    ;; OpenAI image_url block
                    ((equal type "image_url")
                     (let* ((image (jget block "image_url"))
                            (url (if (hash-table-p image) (jget image "url") image))
                            (part (make-image-media-part nil nil url)))
                       (when part (push part parts)))))))
       (nreverse parts)))
    (t nil)))

(defun normalize-message (msg &optional tool-name-map)
  "Convert a single API message hash-table to a CLOS message object.
Returns NIL for system messages (handle via extract-system-prompt)."
  (when (hash-table-p msg)
    (let ((role (jget msg "role")))
      (when (equal role "system")
        (return-from normalize-message nil))
      (cond
        ;; OpenAI tool message
        ((equal role "tool")
         (let ((tcid (or (jget msg "tool_call_id") ""))
               (content (or (jget msg "content") ""))
               (tool-name (when tool-name-map
                            (gethash (jget msg "tool_call_id") tool-name-map))))
           (make-message :role :tool
                         :parts (list (make-tool-result-part
                                       :tool-call-id tcid
                                       :name tool-name
                                       :content content)))))
        ;; Regular message (user/assistant)
        (t
         (let ((parts (normalize-content-to-parts (jget msg "content") tool-name-map)))
           ;; OpenAI assistant messages may also have tool_calls
           (when (equal role "assistant")
             (let ((tool-calls (jget msg "tool_calls")))
               (when (vectorp tool-calls)
                 (loop for call across tool-calls
                       when (hash-table-p call)
                       do (let ((func (jget call "function")))
                            (when (hash-table-p func)
                              (push (make-tool-call-part
                                     :id (or (jget call "id") "")
                                     :name (or (jget func "name") "")
                                     :input-json (or (jget func "arguments") ""))
                                    parts)))))))
           (make-message :role (normalize-role role)
                         :parts (nreverse parts))))))))

(defun extract-system-prompt (messages)
  "Extract system prompt text from the first system message. Returns string or NIL."
  (dolist (msg messages)
    (when (and (hash-table-p msg) (equal (jget msg "role") "system"))
      (let ((content (jget msg "content")))
        (return-from extract-system-prompt
          (cond
            ((stringp content) content)
            ((vectorp content)
             (let ((texts nil))
               (loop for block across content
                     when (and (hash-table-p block) (equal (jget block "type") "text"))
                     do (let ((text (jget block "text")))
                          (when text (push text texts))))
               (when texts
                 (format nil "~{~a~^~%~}" (nreverse texts)))))
            (t nil))))))
  nil)

(defun normalize-input-messages (messages)
  "Normalize a list of API message hash-tables to CLOS message objects.
Excludes system messages (use extract-system-prompt for those)."
  (let ((tool-map (build-tool-name-map messages))
        (result nil))
    (dolist (msg messages)
      (let ((normalized (normalize-message msg tool-map)))
        (when normalized
          (push normalized result))))
    (nreverse result)))

(defun build-output-message (&key text reasoning tool-calls)
  "Build an assistant output message from result components.
TEXT: response text string.
REASONING: thinking text string.
TOOL-CALLS: list of plists (:id :name :arguments), where :arguments is a JSON string."
  (let ((parts nil))
    (when (and reasoning (stringp reasoning) (plusp (length reasoning)))
      (push (make-thinking-part reasoning) parts))
    (when tool-calls
      (dolist (tc tool-calls)
        (let ((args (getf tc :arguments)))
          (push (make-tool-call-part
                 :id (or (getf tc :id) "")
                 :name (or (getf tc :name) "")
                 :input-json (cond
                               ((stringp args) args)
                               (args (handler-case (jzon:stringify args)
                                       (error () "{}")))
                               (t "")))
                parts))))
    (when (and text (stringp text) (plusp (length text)))
      (push (make-text-part text) parts))
    (when parts
      (make-message :role :assistant :parts (nreverse parts)))))
