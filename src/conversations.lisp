(in-package :sigil-cl)

;;; Read-only access to Sigil conversations and eval collections.
;;;
;;; Backs DATASET-FROM-COLLECTION: a Sigil collection groups saved
;;; conversations, and each conversation carries the generations (with their
;;; input/output messages) that produced it. Turning a collection into a
;;; dataset means listing its members, then fetching each conversation to
;;; recover the initial user prompt. Endpoints share the eval connection
;;; settings (eval-base-url, eval-path-prefix, eval auth).

(defparameter +user-roles+ '("MESSAGE_ROLE_USER" "user" "USER"))
(defparameter +system-roles+ '("MESSAGE_ROLE_SYSTEM" "system" "SYSTEM"))

(defun %eval-api-url (config path)
  (format nil "~a~a~a"
          (%strip-trailing-slash (eval-base-url config))
          (%ensure-leading-slash (or (config-eval-path-prefix config) "/api/v1"))
          path))

(defun %json-array-p (value)
  (and (vectorp value) (not (stringp value))))

(defun %member-list (body)
  "Normalize a collection-members response (a bare array or a
members/items wrapper) to a list of hash-tables."
  (let ((raw (cond
               ((%json-array-p body) body)
               ((hash-table-p body)
                ;; Prefer a non-empty members array, then items; matches the
                ;; reference SDKs where an empty members list falls through.
                (let* ((members (jget body "members"))
                       (wrapped (if (and (%json-array-p members)
                                         (plusp (length members)))
                                    members
                                    (jget body "items"))))
                  (if (%json-array-p wrapped) wrapped #())))
               (t #()))))
    (loop for m across raw
          when (hash-table-p m)
            collect m)))

(defun list-collection-members (client collection-id)
  "List the saved conversations belonging to a collection.
Returns the raw member objects (saved_id, conversation_id, name, ...) as a
list of hash-tables."
  (let ((cid (%trimmed-text collection-id)))
    (when (zerop (length cid))
      (error 'sigil-validation-error :message "collection_id is required"))
    (let* ((config (client-config client))
           (url (%eval-api-url config
                               (format nil "/eval/collections/~a/members"
                                       (%url-encode cid)))))
      (%member-list (request-eval-json config :get url nil
                                       "collection members list")))))

(defun get-conversation (client conversation-id)
  "Fetch one conversation with all of its generations. Returns a hash-table."
  (let ((cid (%trimmed-text conversation-id)))
    (when (zerop (length cid))
      (error 'sigil-validation-error :message "conversation_id is required"))
    (let* ((config (client-config client))
           (url (%eval-api-url config
                               (format nil "/query/conversations/~a"
                                       (%url-encode cid))))
           (body (request-eval-json config :get url nil "conversation get")))
      (if (hash-table-p body) body (jobj)))))

(defun %conversation-message-text (message)
  "Concatenate the text parts of one conversation message."
  (let ((parts (jget message "parts")))
    (if (%json-array-p parts)
        (%trim (with-output-to-string (out)
                 (loop for p across parts
                       when (hash-table-p p)
                         do (let ((text (jget p "text")))
                              (when (stringp text)
                                (write-string text out))))))
        "")))

(defun %generation-sort-key (generation)
  "Sort key picking the chronologically earliest generation (ISO 8601 sorts
lexically)."
  (let ((started (%trimmed-text (jget generation "started_at"))))
    (if (plusp (length started))
        started
        (let ((created (%trimmed-text (jget generation "created_at"))))
          (if (plusp (length created)) created "~")))))

(defun initial-user-prompt (conversation)
  "Return the initial user prompt from a fetched conversation.
Looks at the chronologically earliest generation and returns the text of its
last user-role input message (skipping any leading system prompt recorded as
a user-role message). Falls back to the first non-system message, then to an
empty string."
  (let ((generations (and (hash-table-p conversation)
                          (jget conversation "generations"))))
    (if (not (and (%json-array-p generations) (plusp (length generations))))
        ""
        (let* ((earliest (let ((best (aref generations 0)))
                           (loop for i from 1 below (length generations)
                                 for g = (aref generations i)
                                 when (string< (%generation-sort-key g)
                                               (%generation-sort-key best))
                                   do (setf best g))
                           best))
               (messages (and (hash-table-p earliest) (jget earliest "input"))))
          (if (not (%json-array-p messages))
              ""
              (let ((last-user "")
                    (first-non-system ""))
                (loop for m across messages
                      when (hash-table-p m)
                        do (let ((role (%text (jget m "role")))
                                 (text (%conversation-message-text m)))
                             (cond
                               ((and (member role +user-roles+ :test #'equal)
                                     (plusp (length text)))
                                (setf last-user text))
                               ((and (zerop (length first-non-system))
                                     (not (member role +system-roles+ :test #'equal))
                                     (plusp (length text)))
                                (setf first-non-system text)))))
                (if (plusp (length last-user)) last-user first-non-system)))))))

(defun dataset-from-collection (client collection-id
                                &key (mode :user-prompt) limit (skip-empty t))
  "Build dataset items for RUN-EXPERIMENT from a Sigil collection.
Lists the collection's saved conversations, fetches each one, and turns it
into an item whose input is the conversation's initial user prompt. Each
item carries collection_id, conversation_id, saved_id, and task_id in its
metadata so the Sigil report groups scores cleanly. LIMIT caps how many
members are pulled; SKIP-EMPTY drops conversations with no recoverable user
prompt. Mode :golden (capturing the original answer as expected) is not
implemented yet."
  (when (eq mode :golden)
    (error 'sigil-validation-error
           :message "dataset mode :golden is not implemented yet; use :user-prompt"))
  (unless (eq mode :user-prompt)
    (error 'sigil-validation-error
           :message (format nil "unknown dataset mode ~s; expected :user-prompt or :golden"
                            mode)))
  (let ((cid (%trimmed-text collection-id)))
    (when (zerop (length cid))
      (error 'sigil-validation-error :message "collection_id is required"))
    (let ((members (list-collection-members client cid)))
      (when limit
        (setf members (subseq members 0 (min (max limit 0) (length members)))))
      (loop for member in members
            for conversation-id = (%trimmed-text (jget member "conversation_id"))
            when (plusp (length conversation-id))
              append (let* ((saved-id (%trimmed-text (jget member "saved_id")))
                            (conversation (get-conversation client conversation-id))
                            (prompt (initial-user-prompt conversation)))
                       (if (and skip-empty (zerop (length prompt)))
                           nil
                           (let* ((item-id (if (plusp (length saved-id))
                                               saved-id
                                               conversation-id))
                                  (metadata (jobj "collection_id" cid
                                                  "conversation_id" conversation-id
                                                  "saved_id" saved-id
                                                  "task_id" item-id
                                                  "source" "collection"))
                                  (name (%trimmed-text (jget member "name"))))
                             (when (plusp (length name))
                               (setf (gethash "saved_name" metadata) name))
                             (list (make-dataset-item :id item-id
                                                      :input prompt
                                                      :metadata metadata)))))))))
