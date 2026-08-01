(in-package :sigil-cl)

;;; --- Token usage ---

(defclass token-usage ()
  ((input-tokens           :initarg :input-tokens           :accessor token-usage-input-tokens           :initform 0)
   (output-tokens          :initarg :output-tokens          :accessor token-usage-output-tokens          :initform 0)
   (total-tokens           :initarg :total-tokens           :accessor token-usage-total-tokens           :initform 0)
   (reasoning-tokens       :initarg :reasoning-tokens       :accessor token-usage-reasoning-tokens       :initform 0)
   (cache-read-tokens      :initarg :cache-read-tokens      :accessor token-usage-cache-read-tokens      :initform 0)
   (cache-creation-tokens  :initarg :cache-creation-tokens  :accessor token-usage-cache-creation-tokens  :initform 0)))

(defun make-token-usage (&key (input 0) (output 0) (total nil)
                              (reasoning 0) (cache-read 0) (cache-creation 0))
  (make-instance 'token-usage
    :input-tokens input
    :output-tokens output
    :total-tokens (or total (+ input output))
    :reasoning-tokens reasoning
    :cache-read-tokens cache-read
    :cache-creation-tokens cache-creation))

;;; --- Message parts ---

(defclass text-part ()
  ((text :initarg :text :accessor text-part-text :initform "")))

(defun make-text-part (text)
  (make-instance 'text-part :text text))

(defclass thinking-part ()
  ((text :initarg :text :accessor thinking-part-text :initform "")))

(defun make-thinking-part (text)
  (make-instance 'thinking-part :text text))

(defclass tool-call-part ()
  ((id         :initarg :id         :accessor tool-call-part-id         :initform "")
   (name       :initarg :name       :accessor tool-call-part-name       :initform "")
   (input-json :initarg :input-json :accessor tool-call-part-input-json :initform "")))

(defun make-tool-call-part (&key id name input-json)
  (make-instance 'tool-call-part :id (or id "") :name (or name "") :input-json (or input-json "")))

(defclass tool-result-part ()
  ((tool-call-id :initarg :tool-call-id :accessor tool-result-part-tool-call-id :initform "")
   (name         :initarg :name         :accessor tool-result-part-name         :initform nil)
   (content      :initarg :content      :accessor tool-result-part-content      :initform "")
   (content-json :initarg :content-json :accessor tool-result-part-content-json :initform nil)
   (is-error     :initarg :is-error     :accessor tool-result-part-is-error     :initform nil)))

(defun make-tool-result-part (&key tool-call-id name content content-json is-error)
  (make-instance 'tool-result-part
    :tool-call-id (or tool-call-id "")
    :name name
    :content (or content "")
    :content-json content-json
    :is-error is-error))

(defclass media-part ()
  ((kind          :initarg :kind          :accessor media-part-kind          :initform "")
   (url           :initarg :url           :accessor media-part-url           :initform "")
   (mime-type     :initarg :mime-type     :accessor media-part-mime-type     :initform "")
   (name          :initarg :name          :accessor media-part-name          :initform "")
   (provider-type :initarg :provider-type :accessor media-part-provider-type :initform nil)))

(defun make-media-part (&key kind url mime-type name provider-type)
  (make-instance 'media-part
    :kind (or kind "")
    :url (or url "")
    :mime-type (or mime-type "")
    :name (or name "")
    :provider-type provider-type))

;;; --- Message ---

(defclass message ()
  ((role  :initarg :role  :accessor message-role  :initform :user)
   (name  :initarg :name  :accessor message-name  :initform nil)
   (parts :initarg :parts :accessor message-parts :initform nil)))

(defun make-message (&key (role :user) name parts)
  (make-instance 'message :role role :name name :parts (or parts nil)))
