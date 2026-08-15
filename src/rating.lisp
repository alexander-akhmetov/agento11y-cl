(in-package :agento11y-cl)

(defun submit-conversation-rating (client conversation-id rating
                                   &key rating-id feedback user-id)
  "Submit a conversation rating.
RATING is :good or :bad. RATING-ID is required for idempotency.
Synchronous (not queued)."
  (let* ((config (client-config client))
         ;; Scheme and host, not the configured endpoint as given: the
         ;; generation endpoint ends in /api/v1/generations:export, and the
         ;; rating path appended to that reaches no route.
         (base-url (%resolve-api-base-url config)))
    (unless base-url
      (agento11y-log config :warn "rating" "no api or generation endpoint configured")
      (return-from submit-conversation-rating nil))
    (let* ((url (format nil "~a/api/v1/conversations/~a/ratings"
                        base-url conversation-id))
           (rid (or rating-id (generate-id)))
           (payload (jobj "rating" (case rating
                                     (:good "CONVERSATION_RATING_VALUE_GOOD")
                                     (:bad "CONVERSATION_RATING_VALUE_BAD")
                                     (t (princ-to-string rating)))
                          "rating_id" rid))
           (auth-headers (build-auth-headers config)))
      ;; FEEDBACK is caller content, so a redacting capture mode drops it. The
      ;; POST still succeeds, so warn: a caller who set :metadata-only on the
      ;; client would otherwise see the text disappear without a signal. A
      ;; rating has no recorder, so a per-call mode never reaches it.
      (when feedback
        (let ((capture (config-content-capture-mode config)))
          (if (capture-keeps-payload-content-p capture)
              (setf (gethash "comment" payload) feedback)
              (agento11y-log config :warn "rating"
                        (format nil "content capture mode ~a withholds the rating comment"
                                capture)))))
      (when user-id
        (setf (gethash "rater_id" payload) (princ-to-string user-id)))
      (handler-case
          (post-with-retry config url (jzon:stringify payload)
                           auth-headers "rating" 1)
        (error (e)
          (agento11y-log config :warn "rating"
                    (format nil "failed to submit rating: ~a" (princ-to-string e)))
          nil)))))
