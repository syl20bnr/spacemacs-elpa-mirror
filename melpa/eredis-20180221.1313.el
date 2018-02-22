;;; eredis.el --- eredis, a Redis client in emacs lisp
;; Copyright 2012 Justin Heyes-Jones
 
;; Author: Justin Heyes-Jones
;; URL: http://code.google.com/p/eredis/
;; Package-Version: 20180221.1313
;; Version: 0.6.00
;; Package-Requires: 

;; See for info on the protocol http://redis.io/topics/protocol
;; This is released under the Gnu License v3. See http://www.gnu.org/licenses/gpl.txt

;; Usage: 
;; (eredis-connect "localhost" "6379")
;; ...
;; (eredis-set "key" "value") "ok"
;; (eredis-get "key") "value"
;; ...
;; (eredis-disconnect)

;; wiki http://code.google.com/p/eredis/wiki/wiki 
;; videos http://code.google.com/p/eredis/wiki/demovideos

(require 'org-table)
(require 'cl-lib)

(defvar eredis-process nil "Current Redis client process")
(defvar eredis-response nil "Stores response of last Redis command")

;; UTILS

(defun eredis-set-timeout(redis-process seconds)
  "set how long emacs will wait for a response from redit, pay attention to this if using blocking 
commands like blpop which also have a timeout" 
  (process-put redis-process 'eredis-timeout seconds))

(defun eredis--two-lists-to-map(key-list value-list)
  "take a list of keys LST1 and a list of values LST2 and make a hashmap, not particularly efficient
as it first constructs a list of key value pairs then uses that to construct the hashmap"
  (let ((retmap (make-hash-table :test 'equal)))
    (cl-mapc (lambda (k v)
               (puthash k v retmap))
             key-list value-list)
    retmap))

(defun eredis--unflatten-map-worker(in keys values)
  (if (null in)
      (eredis--two-lists-to-map keys values)
    (eredis--unflatten-map-worker (cddr in) (cons (first in) keys) (cons (second in) values))))

(defun eredis--unflatten-map(l)
  "take a list of value1 key1 ... valuen keyn and return a map"
  (let ((len (length l)))
    (if (/= (mod len 2) 0)
        (error "list must be even length"))
    (eredis--unflatten-map-worker l nil nil)))

(defun eredis--flatten-map(m)
  "flatten the key values of map M to a list of the form key1 value1 key2 value2..."
  (let ((key-values nil))
    (maphash (lambda (k v)
               (push k key-values)
               (push v key-values))
             m)
    (reverse key-values)))

(defun eredis-parse-map-or-list-arg(a)
  "handle when an argument can be passed as a hash table or a list of key values"
  (if (hash-table-p a)
      (eredis--flatten-map a)
    a))

(defun eredis--insert-map(m)
  "insert a map M of key value pairs into the current buffer"
  (maphash (lambda (a b) (insert (format "%s,%s\n" a b))) m))

;; TODO random macro would be nice; dolist with a different body to execute for the first 
;; or last item 
(defun eredis--insert-list(l)
  "insert a list L into the current buffer"
  (let ((str (mapconcat #'identity l ",")))
    (insert str)))
                                        ;    (insert (cl-subseq str 0 ))))

(defun eredis--stringify-numbers-and-symbols(item)
  (cond 
   ((numberp item)
    (number-to-string item))
   ((symbolp item)
    (symbol-name item))
   ((stringp item)
    item)
   (t
    (error "unsupported type: %s" item))))

(defun eredis-construct-unified-request(command &rest arguments)
  "all redis commands are sent using this protocol"
  (let ((num-args (+ 1 (length arguments))))
    (if (> num-args 0)
        (let ((req (format "*%d\r\n$%d\r\n%s\r\n" num-args (length command) command)))
          (dolist (item arguments)
            (setf item (eredis--stringify-numbers-and-symbols item))
            (setf req (concat req (format "$%d\r\n%s\r\n" (string-bytes item) item))))
          req)
      nil)))

(defun eredis-map-keys(key-expr)
  "take a glob expression like \"user.id.*\" and return the key/values of matching keys"
  (let ((keys (eredis-keys key-expr)))
    (if keys
        (let ((values (eredis-mget keys)))
          (eredis--two-lists-to-map keys values))
      nil)))

(defun eredis-get-response(&optional requested-timeout)
  "await response from redis and store it in eredis-response. If it times out it will return nil"
  (let ((timeout (or requested-timeout
                     (process-get eredis-process 'eredis-timeout)
                     3))
        (parsed-response))
    (accept-process-output eredis-process timeout 0 t)
    (condition-case resp
        (progn
          (setq eredis-response (process-get eredis-process 'eredis-response-str))
          (setq parsed-response (eredis-parse-response eredis-response))
          (setf (process-get eredis-process 'eredis-response-str) ""))
      (eredis-incomplete-response-error (eredis-get-response requested-timeout)))
    parsed-response))

(defun eredis-response-type-of (response)
  (let ((chr (elt response 0))
        (chr-type-alist '((?- . error)
                          (?* . multi-bulk)
                          (?$ . single-bulk)
                          (?: . integer)
                          (?+ . status))))
    (cdr (assoc chr chr-type-alist))))

(defun eredis-parse-response (response)
  (let ((response-type (eredis-response-type-of response)))
    (cond ((eq response-type 'error)
           (eredis-parse-error-response response))
          ((eq response-type 'multi-bulk)
           (eredis-parse-multi-bulk-response response))
          ((eq response-type 'single-bulk)
           (eredis-parse-bulk-response response))
          ((eq response-type 'integer)
           (eredis-parse-integer-response response))
          ((eq response-type 'status)
           (eredis-parse-status-response response))
          (t (error "unkown response-type:%s" response)))))

(define-error 'eredis-incomplete-response-error
  "The response is incomplete"
  'user-error)

(defun eredis-response-basic-check (resp)
  (when resp
    (unless (string-suffix-p "\r\n" resp)
      (signal 'eredis-incomplete-response-error resp))
    resp))

(defun eredis-trim-status-response(resp)
  "strip the leading character +/- and the final carriage returns"
  (let ((len (length resp)))
    (cl-subseq resp 1 (- len 2))))

(defun eredis-parse-integer-response(resp)
  "parse integer response type"
  (when (eredis-response-basic-check resp)
    (string-to-number (cl-subseq resp 1))))

(defun eredis-parse-error-response (resp)
  (when (eredis-response-basic-check resp)
    (error "redis error: %s" (eredis-trim-status-response resp))))

(defun eredis-parse-status-response (resp)
  (when (eredis-response-basic-check resp)
    (eredis-trim-status-response resp)))

(defun eredis-parse-bulk-response--inner (resp)
  "parse the redis bulk response RESP and return the result and rest unparsed resp"
  (when (eredis-response-basic-check resp)
    (condition-case nil
        (progn
          (unless (string-match "^$\\([-0-9]+\\)\r\n" resp)
            (signal 'eredis-incomplete-response-error resp))
          (let ((count (string-to-number (match-string 1 resp)))
                (body-start (match-end 0)))
            (if (> count 0)
                (cons (substring resp body-start (+ count body-start))
                      (substring resp (+ count body-start 2)))
              (cons nil
                    (substring resp (+ count body-start))))))
      (error  (signal 'eredis-incomplete-response-error resp)))))

(defun eredis-parse-bulk-response (resp)
  "parse the redis bulk response RESP and return the result"
  (car (eredis-parse-bulk-response--inner resp)))

(defun eredis-parse-multi-bulk-response (resp)
  "parse the redis multi bulk response RESP and return the list of results. handles null entries when
length is -1 as per spec"
  (when (eredis-response-basic-check resp)
    (condition-case nil
        (progn
          (unless (string-match "^*\\([0-9]+\\)\r\n" resp)
            (signal 'eredis-incomplete-response-error resp))
          (let ((num-values (string-to-number (match-string 1 resp)))
                (return-list nil)
                (parse-pos (match-end 0)))
            (let ((resp (substring resp parse-pos)))
              (dotimes (n num-values)
                (let ((result (eredis-parse-bulk-response--inner resp)))
                  (push (car result) return-list)
                  (setq resp (cdr result)))))
            (reverse return-list)))
      (error  (signal 'eredis-incomplete-response-error resp)))))

(defun eredis-command-returning (command &rest args)
  "Send a command that has the status code return type"
  (if (and eredis-process (eq (process-status eredis-process) 'open))
      (progn 
        (process-send-string eredis-process (apply #'eredis-construct-unified-request command args))
        (let* ((ret-val (eredis-get-response)))
          (when (called-interactively-p 'any)
            (message ret-val))
          ret-val))
    (error "redis not connected")))

(defun eredis-buffer-message(process message)
  "append a message to the redis process buffer"
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert message)
    (goto-char (point-max))))

(defun eredis-sentinel(process event)
  "sentinel function for redis network process which monitors for events"
  (eredis-buffer-message process (format "sentinel event %s" event))
  (when (eq 'closed (process-status process))
    (delete-process process)
    (setq eredis-process nil)))

(defun eredis-filter(process string)
  "filter function for redis network process, which receives output"
  (process-put process 'eredis-response-str (concat (or (process-get process 'eredis-response-str)
                                                    "")
                                                string)))

(defun eredis-delete-process()
  (when eredis-process
    (delete-process eredis-process)
    (setq eredis-process nil)))

;; Connect and disconnect functionality

(defun eredis-connect(host port &optional no-wait)
  "connect to Redis on HOST PORT. NO-WAIT can be set to true to make the connection asynchronously
but that's not supported on windows and doesn't make much difference"
  (interactive (list (read-string "Host: ") (read-number "Port: " 6379)))
  (eredis-delete-process)
  (setq eredis-process
        (make-network-process :name "redis"
                              :host host
                              :service port
                              :type nil
                              :nowait no-wait
                              :filter #'eredis-filter
                              :keepalive t
                              :linger t
                              :sentinel #'eredis-sentinel
                              :buffer (get-buffer-create "*redis*"))))
(defalias 'eredis-hai 'eredis-connect)

     
(defun eredis-disconnect()
  "Close the connection to Redis"
  (interactive)
  (eredis-delete-process))

(defalias 'eredis-kthxbye 'eredis-disconnect)

(defun eredis-get-map(keys)
  "given a map M of key/value pairs, go to Redis to retrieve the values and set the value to whatever it is in Redis (or nil if not found)"
  (let* ((m (make-hash-table))
         (num-args (1+ (hash-table-count m)))
         (command (format "*%d\r\n$4\r\nMGET\r\n" num-args))
         (key-value-string ""))
    (maphash (lambda (k v)
               (setf key-value-string (concat key-value-string (format "$%d\r\n%s\r\n" (length k) k))))
             m)
    (process-send-string eredis-process (concat command key-value-string))
    (eredis-get-response)))

;; all the redis commands are documented at http://redis.io/commands
;; key commands

(defun eredis-del(key &rest keys)
  (apply #'eredis-command-returning "del" key keys))  

(defun eredis-exists(key)
  "Returns 1 if key exists and 0 otherwise"
  (eredis-command-returning "exists" key))

(defun eredis-expire(key seconds)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "expire" key seconds))

(defun eredis-expireat(key unix-time)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "expireat" key unix-time))

(defun eredis-keys(pattern)
  "returns a list of keys where the key matches the provided
pattern. see the link for the style of patterns"
  (eredis-command-returning "keys" pattern))

(defun eredis-move(key db)
  "moves KEY to DB and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "move" key db))

(defun eredis-object(subcommand &rest args)
  "inspect the internals of Redis Objects associated with keys,
  best see the docs for this one. http://redis.io/commands/object"
  (if (eq t (compare-strings "encoding" nil nil subcommand nil nil t))
      (apply #'eredis-command-returning "object" subcommand args)
    (apply #'eredis-command-returning "object" subcommand args)))

(defun eredis-persist(key)
  "Remove the existing timeout on KEY and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "persist" key))

(defun eredis-randomkey()
  "get a random key from the redis db"
  (eredis-command-returning "randomkey"))

(defun eredis-rename(key newkey)
  "renames KEY as NEWKEY"
  (eredis-command-returning "rename" key newkey))

(defun eredis-renamenx(key newkey)
  "renames KEY as NEWKEY only if NEWKEY does not yet exist"
  (eredis-command-returning "renamenx" key newkey))

(defun eredis-sort(key &rest args)
  "call the redis sort command with the specified KEY and ARGS"
  (apply #'eredis-command-returning "sort" key args))

(defun eredis-ttl(key)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "ttl" key))

(defun eredis-type(key)
  "Get the type of KEY"
  (eredis-command-returning "type" key))

;; string commands

(defun eredis-append(key value)
  "Append VALUE to value of KEY"
  (eredis-command-returning "append" key value))

(defun eredis-decr(key)
  "decrement value of KEY"
  (eredis-command-returning "decr" key))

(defun eredis-decrby(key decrement)
  "decrement value of KEY by DECREMENT"
  (eredis-command-returning "decrby" key decrement))

(defun eredis-get(key)
  "redis get"
  (eredis-command-returning "get" key))

(defun eredis-getbit(key offset)
  "redis getbit"
  (eredis-command-returning "getbit" key offset))

(defun eredis-getrange(key start end)
  "redis getrange"
  (eredis-command-returning "getrange" key start end))

(defun eredis-getset(key value)
  "redis atomic set and get old value"
  (eredis-command-returning "getset" key value))

(defun eredis-incr(key)
  "increment value of KEY"
  (eredis-command-returning "incr" key))

(defun eredis-incrby(key increment)
  "increment value of KEY by INCREMENT"
  (eredis-command-returning "incrby" key increment))

(defun eredis-mget(keys)
  "return the values of the specified keys, or nil if not present"
  (apply #'eredis-command-returning "mget" keys))

(defun eredis-mset(m)
  "set the keys and values of the map M in Redis using mset"
  (apply #'eredis-command-returning "mset" (eredis-parse-map-or-list-arg m)))

(defun eredis-msetnx(m)
  "set the keys and values of the map M in Redis using msetnx (only if all are not existing)"
  (apply #'eredis-command-returning "msetnx" (eredis-parse-map-or-list-arg m)))

(defun eredis-set(k v)
  "set the key K and value V in Redis"
  (eredis-command-returning "set" k v))

(defun eredis-setbit(key offset value)
  "redis setbit"
  (eredis-command-returning "setbit" key offset value))

(defun eredis-setex(key seconds value)
  "eredis setex"
  (eredis-command-returning "setex" key seconds value))

(defun eredis-setnx(k v)
  "set if not exist"
  (eredis-command-returning "setnx" k v))

(defun eredis-setrange(key offset value)
  "redis setrange"
  (eredis-command-returning "setrange" key offset value))

(defun eredis-strlen(key)
  "redis strlen"
  (eredis-command-returning "strlen" key))

;; hash commands

(defun eredis-hget(key field)
  "redis hget"
  (eredis-command-returning "hget" key field))

(defun eredis-hset(key field value)
  "redis hset"
  (eredis-command-returning "hset" key field value))

(defun eredis-hsetnx(key field value)
  "redis hsetnx"
  (eredis-command-returning "hsetnx" key field value))

(defun eredis-hmget(key field &rest fields)
  "redis hmget"
  (apply #'eredis-command-returning "hmget" key field fields))

(defun eredis-hmset(key m)
  "redis hmset set multiple key values on the key KEY using an emacs lisp map M or list of key values"
  (apply #'eredis-command-returning "hmset" key (eredis-parse-map-or-list-arg m)))

(defun eredis-hincrby(key field integer)
  "increment FIELD on KEY by INTEGER"
  (eredis-command-returning "hincrby" key field integer))

(defun eredis-hexists(key field)
  "redis hexists"
  (eredis-command-returning "hexists" key field))

(defun eredis-hdel(key field)
  "redis hdel"
  (eredis-command-returning "hdel" key field))

(defun eredis-hlen(key)
  "redis hlen"
  (eredis-command-returning "hlen" key))

(defun eredis-hkeys(key)
  "redis hkeys"
  (eredis-command-returning "hkeys" key))

(defun eredis-hvals(key)
  "redis hvals"
  (eredis-command-returning "hvals" key))

(defun eredis-hgetall(key)
  "redis hgetall"
  (eredis-command-returning "hgetall" key))

;; hyperloglog commands
(defun eredis-pfadd(key value &rest values)
  "add the elements to the named HyperLogLog"
  (eredis-command-returning "pfadd" key value values))

(defun eredis-pfcount(key &rest keys)
  "return the approx cardinality of the HyperLogLog(s)"
  (eredis-command-returning "pfcount" key keys))

(defun eredis-pfmerge(dest src &rest srcs)
  "merge all source keys into dest HyperLogLog"
  (eredis-command-returning "pfmerge" dest src srcs))

;; list commands

(defun eredis-llen(key)
  "length of list"
  (eredis-command-returning "llen" key))

(defun eredis-lpop(key)
  "list pop first element"
  (eredis-command-returning "lpop" key))

(defun eredis-lpush(key value &rest values)
  "Prepend value(s) to a list stored by KEY"
  (apply #'eredis-command-returning "lpush" key value values))

(defun eredis-rpush(key value &rest values)
  "Append value(s) to a list stored by KEY"
  (apply #'eredis-command-returning "rpush" key value values))

(defun eredis-lpushx(key value)
  "Prepend value(s) to a list stored by KEY if it doesn't exist already"
  (eredis-command-returning "lpushx" key value))

(defun eredis-rpushx(key value)
  "Append value(s) to a list stored by KEY if it doesn't exist already"
  (eredis-command-returning "rpushx" key value))

(defun eredis-lindex(key index)
  "list element INDEX to a list stored by KEY"
  (eredis-command-returning "lindex" key index))

(defun eredis-blpop(key &rest rest)
  "blocking left pop of multiple lists, rest is actually as many keys as you want and a timeout"
  (apply #'eredis-command-returning "blpop" key rest))

(defun eredis-brpop(key &rest rest)
  "blocking right pop of multiple lists, rest is actually as many keys as you want and a timeout"
  (apply #'eredis-command-returning "brpop" key rest))

(defun eredis-lrange(key start stop)
  "redis lrange"
  (eredis-command-returning "lrange" key start stop))

(defun eredis-linsert(key position pivot value)
  "redis linsert"
  (eredis-command-returning "linsert" key position pivot value))

(defun eredis-brpoplpush(source destination timeout)
  "redis brpoplpush"
  (eredis-command-returning "brpoplpush" source destination timeout))

(defun eredis-rpoplpush(source destination timeout)
  "redis rpoplpush"
  (eredis-command-returning "rpoplpush" source destination))

(defun eredis-lrem(key count value)
  "redis lrem"
  (eredis-command-returning "lrem" key count value))

(defun eredis-lset(key index value)
  "redis lset"
  (eredis-command-returning "lset" key index value))

(defun eredis-ltrim(key start stop)
  "redis ltrim"
  (eredis-command-returning "ltrim" key start stop))

(defun eredis-rpop(key)
  "right pop of list"
  (eredis-command-returning "rpop" key))

;; set commands

(defun eredis-sadd(key member &rest members)
  "redis add to set"
  (apply #'eredis-command-returning "sadd" key member members))

(defun eredis-scard(key)
  "redis scard"
  (eredis-command-returning "scard" key))

(defun eredis-sdiff(key &rest keys)
  "redis sdiff"
  (apply #'eredis-command-returning "sdiff" key keys))

(defun eredis-sdiffstore(destination key &rest keys)
  "redis sdiffstore"
  (apply #'eredis-command-returning "sdiffstore" destination key keys))

(defun eredis-sinter(key &rest keys)
  "redis sinter"
  (apply #'eredis-command-returning "sinter" key keys))

(defun eredis-sinterstore(destination key &rest keys)
  "redis sinterstore"
  (apply #'eredis-command-returning "sinterstore" destination key keys))

(defun eredis-sismember(key member)
  "redis sdiffstore"
  (eredis-command-returning "sismember" key member))

(defun eredis-smembers(key)
  "redis smembers"
  (eredis-command-returning "smembers" key))

(defun eredis-smove(source destination member)
  "redis smove"
  (eredis-command-returning "smove" source destination member))

(defun eredis-spop(key)
  "redis spop"
  (eredis-command-returning "spop" key))

(defun eredis-srandmember(key)
  "redis srandmember"
  (eredis-command-returning "srandmember" key))

(defun eredis-srem(key member &rest members)
  "redis srem"
  (apply #'eredis-command-returning "srem" key member members))

(defun eredis-sunion(key &rest keys)
  "redis sunion"
  (apply #'eredis-command-returning "sunion" key keys))

(defun eredis-sunionstore(destination key &rest keys)
  "redis sunionstore"
  (apply #'eredis-command-returning "sunionstore" destination key keys))


;; sorted set commands

(defun eredis-zadd(key score member)
  "redis zadd"
  (eredis-command-returning "zadd" key score member))

(defun eredis-zcard(key)
  "redis zcard"
  (eredis-command-returning "zcard" key))

(defun eredis-zcount(key min max)
  "redis zcount"
  (eredis-command-returning "zcount" key min max))

(defun eredis-zincrby(key increment member)
  "redis zincrby"
  (eredis-command-returning "zincrby" key increment member))

(defun eredis-zinterstore(destination numkeys key &rest rest)
  "redis zinterstore"
  (apply #'eredis-command-returning "zinterstore" destination numkeys key rest))

(defun eredis-zrange(key start stop &optional withscores)
  "eredis zrange. withscores can be the string \"withscores\", the symbol 'withscores"
  (if (null withscores)
      (eredis-command-returning "zrange" key start stop)
    (eredis-command-returning "zrange" key start stop withscores)))

(defun eredis-zrangebyscore(key min max &rest rest)
  "eredis zrangebyscore"
  (apply #'eredis-command-returning "zrangebyscore" key min max rest))

(defun eredis-zrank(key member)
  "redis zrank"
  (eredis-command-returning "zrank" key member))

(defun eredis-zrem(key member)
  "redis zrem"
  (eredis-command-returning "zrem" key member))

(defun eredis-zremrangebyrank(key start stop)
  "redis zremrangebyrank"
  (eredis-command-returning "zremrangebyrank" key start stop))

(defun eredis-zremrangebyscore(key min max)
  "redis zremrangebyscore"
  (eredis-command-returning "zremrangebyscore" key min max))

(defun eredis-zrevrange(key start stop &optional withscores)
  "eredis zrevrange. withscores can be the string \"withscores\", the symbol 'withscores"
  (if (null withscores)
      (eredis-command-returning "zrevrange" key start stop)
    (eredis-command-returning "zrevrange" key start stop withscores)))

(defun eredis-zrevrangebyscore(key min max &rest rest)
  "eredis zrevrangebyscore"
  (apply #'eredis-command-returning "zrevrangebyscore" key min max rest))

(defun eredis-zrevrank(key member)
  "redis zrevrank"
  (eredis-command-returning "zrevrank" key member))

(defun eredis-zscore(key member)
  "redis zscore"
  (eredis-command-returning "zscore" key member))

(defun eredis-zunionstore(destination numkeys key &rest rest)
  "redis zunionstore"
  (apply #'eredis-command-returning destination numkeys key rest))

;; pub/sub commands

;; Warning: these aren't working very well yet. Need to write a custom response handler 
;; to handle replies from the publish subscribe commands. They have differences, for 
;; example multiple bulk messages come at once. 

(defun eredis-publish(channel message)
  "eredis publish"
  (eredis-command-returning "publish" channel message))

(defun eredis-subscribe(channel &rest channels)
  "eredis subscribe"
  (apply #'eredis-command-returning "subscribe" channel channels))

(defun eredis-psubscribe(pattern &rest patterns)
  "eredis psubscribe"
  (apply #'eredis-command-returning "psubscribe" pattern patterns))

(defun eredis-unsubscribe(channel &rest channels)
  "eredis unsubscribe"
  (apply #'eredis-command-returning "unsubscribe" channel channels))

(defun eredis-punsubscribe(pattern &rest patterns)
  "eredis punsubscribe"
  (apply #'eredis-command-returning "punsubscribe" pattern patterns))

(defun eredis-await-message()
  "Not a redis command. After subscribe or psubscribe, call this
to poll each message and call unsubscribe or punsubscribe when
done. Other commands will fail with an error until then"
  (eredis-get-response))

;; transaction commands

(defun eredis-discard()
  "eredis discard"
  (eredis-command-returning "discard"))

(defun eredis-multi()
  "eredis multi"
  (eredis-command-returning "multi"))

;; TODO this returns a multibulk which in turn will contain a sequence of responses to commands
;; executed. Best way to handle this is probably to return a list of responses
;; Also need to fix the parser to handle numeric results in a multibulk response
;; which is the same issue I'm seeing with publish/subscribe results
(defun eredis-exec()
  "eredis exec"
  (eredis-command-returning "exec"))

(defun eredis-watch(key &rest keys)
  "redis watch"
  (apply #'eredis-command-returning "watch" key keys))

(defun eredis-unwatch()
  "redis unwatch"
  (eredis-command-returning "unwatch"))

;; connection commands

(defun eredis-auth(password)
  "eredis auth"
  (eredis-command-returning "auth" password))

(defun eredis-echo(message)
  "eredis echo"
  (eredis-command-returning "echo" message))

(defun eredis-ping()
  "redis ping"
  (interactive)
  (eredis-command-returning "ping"))

(defun eredis-quit()
  "redis ping"
  (interactive)
  (eredis-command-returning "quit"))

(defun eredis-select(index)
  "redis select db with INDEX"
  (interactive)
  (eredis-command-returning "select" index))

;; server commands 

(defun eredis-bgrewriteaof()
  (eredis-command-returning "bgrewriteaof"))

(defun eredis-bgsave()
  (eredis-command-returning "bgsave"))

(defun eredis-config-get(parameter)
  (eredis-command-returning "config" "get" parameter))

(defun eredis-config-set(parameter value)
  (eredis-command-returning "config" "set" parameter value))

(defun eredis-config-resetstat()
  (eredis-command-returning "config" "resetstat"))

(defun eredis-dbsize()
  (eredis-command-returning "dbsize"))

(defun eredis-debug-object(key)
  (eredis-command-returning "debug" "object" key))

(defun eredis-debug-segfault()
  (eredis-command-returning "debug" "segfault"))

(defun eredis-flushall()
  (eredis-command-returning "flushall"))

(defun eredis-flushdb()
  (eredis-command-returning "flushdb"))

;; TODO the response from this is a single bulk response but it could be further parsed into a map
;; It uses : to delimit the keys from values
(defun eredis-info()
  (eredis-command-returning "info"))

(defun eredis-lastsave()
  (eredis-command-returning "lastsave"))

;; TODO monitor opens up the *redis-buffer* and shows commands streaming 
;; but it does not yet follow along, they just go off the screen, so I need
;; to fix that
(defun eredis-monitor()
  (if (and eredis-process (eq (process-status eredis-process) 'open))
      (unwind-protect
          (progn
            (switch-to-buffer "*redis*")
            (goto-char (point-max))
            (eredis-buffer-message eredis-process "C-g to exit\n")
            (process-send-string eredis-process "monitor\r\n")
            (let ((resp nil))
              (while t
                (redisplay t)
                (sleep-for 1)
                ;;(recenter-top-bottom 'top)
                (let ((resp (eredis-get-response 5)))
                  (when resp
                    (eredis-buffer-message eredis-process eredis-response))))))
        ;; when the user hits C-g we send the quit command to exit
        ;; monitor mode
        (progn
          (eredis-quit)
          (eredis-kthxbye)))))


(defun eredis-save()
  (eredis-command-returning "save"))

(defun eredis-shutdown()
  "shutdown redis server"
  (interactive)
  ;; Note that this just sends the command and does not wait for or parse the response
  ;; since there shouldn't be one
  (if (and eredis-process (eq (process-status eredis-process) 'open))
      (progn 
        (process-send-string eredis-process (eredis-construct-unified-request "shutdown"))
        (eredis-kthxbye))))

(defun eredis-slaveof(host port)
  (eredis-command-returning "slaveof" host port))

(defun eredis-slowlog-len()
  (eredis-command-returning "slowlog" "len"))

(defun eredis-slowlog-get(&optional depth)
  (eredis-command-returning "slowlog" "get" depth))

(defun eredis-sync()
  (eredis-command-returning "sync"))

;; Helpers 

(defun eredis-mset-region(beg end delimiter) 
  "Parse the current region using DELIMITER to split each line into a key value pair which
is then sent to redis using mset"
  (interactive "*r\nsDelimiter: ")
  (let ((done nil)
        (mset-param (make-hash-table :test 'equal)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (save-excursion
        (while (not done)
          (let ((split-line 
                 (split-string  
                  (buffer-substring (point-at-bol) (point-at-eol)) 
                  delimiter)))
            (let ((key (first split-line))
                  (value (second split-line)))
              (if (or (null key) (null value))
                  (setf done t)
                (progn
                  (puthash key value mset-param)
                  (forward-line))))))))
    (if (> (hash-table-count mset-param) 0)
        (eredis-mset mset-param)
      nil)))

(defun eredis-org-table-from-keys(keys)
  "for each of KEYS lookup their type in redis and populate an org table 
containing a row for each one"
  (eredis--org-table-from-list  '("Key" "Type" "Values"))
  (dolist (key keys)
    (let ((type (eredis-type key)))	 
      (cond
       ((string= "string" type)
        (eredis-org-table-from-string key))
       ((string= "zset" type)
        (eredis-org-table-from-zset key 'withscores))
       ((string= "hash" type)
        (eredis-org-table-from-hash key))
       ((string= "list" type)
        (eredis-org-table-from-list key))
       ((string= "set" type)
        (eredis-org-table-from-set key))
       ((string= "none" type)
        nil) ; silently skip missing keys
       (t
        (insert (format "| %s | unknown type %s |\n" key type)))))))

(defun eredis-org-table-from-list(key)
  "create an org table populated with the members of the list KEY"
  (let ((items (eredis-lrange key 0 -1)))
    (when items
      (eredis--org-table-from-list (apply #' list key "list" items)))))

(defun eredis-org-table-from-zset(key &optional withscores)
  "create an org table populated with the members of the zset KEY"
  (let ((items (eredis-zrange key 0 -1 withscores)))
    (when items
      (eredis--org-table-from-list (apply #'list key "zset" items)))))

(defun eredis-org-table-from-set(key)
  "create an org table populated with the members of the set KEY"
  (let ((members (eredis-smembers key)))
    (when members
      (eredis--org-table-from-list (apply #'list key "set" members)))))

(defun eredis-org-table-from-hash(key)
  "org table populated with the hash of KEY"
  (let ((m (eredis-hgetall key)))
    (when m
      (setf m (eredis--unflatten-map m))
      (eredis--org-table-from-map m))))

(defun eredis-org-table-from-string(key)
  "create a small org table from the key, and it's string value"
  (let ((val (eredis-get key)))
    (when val
      (eredis--org-table-from-list (list key "string" val)))))

(defun eredis-org-table-from-pattern(pattern)
  "Search Redis for the pattern of keys and create an org table from the results"
  (let ((keys (eredis-keys pattern)))
    (if keys
        (eredis-org-table-from-keys keys))))

(defun eredis--org-table-from-list(l)
  "Create an org-table from a list"
  (if (listp l)
      (let ((beg (point)))
        (eredis--insert-list l)
        (org-table-convert-region beg (point) '(4))
        (forward-line))))

(defun eredis--org-table-from-map(m)
  "Create an org-table from a map of key value pairs"
  (let ((beg (point)))
    (if (hash-table-p m)
        (progn
          (eredis--insert-map m)
          (org-table-convert-region beg (point))))))

(defun eredis-org-table-get-field-clean(col)
  "Get a field in org table at column COL and strip any leading or
trailing whitespace using `string-trim'. Also strip text properties"
  (let ((field (org-table-get-field col)))
    (let ((chomped (string-trim field)))
      (set-text-properties 0 (length chomped) nil chomped)
      chomped)))

(defun eredis-org-table-to-map()
  "Walk an org table and convert the first column to keys and the second 
column to values in an elisp map"
  (let ((retmap (make-hash-table :test 'equal)))
    (save-excursion
      (let ((beg (org-table-begin))
            (end (org-table-end)))
        (goto-char beg)
        (while (> end (point))
          (let ((key (eredis-org-table-get-field-clean 1))
                (value (eredis-org-table-get-field-clean 2)))
            (when (and key value)
              (puthash key value retmap)))
          (forward-line))))
    retmap))

(defun eredis-org-table-row-to-key-value-pair()
  "When point is in an org table convert the first column to a key and the second 
column to a value, returning the result as a dotted pair"
  (let ((beg (org-table-begin))
        (end (org-table-end)))
    (if (and (>= (point) beg)
             (<= (point) end))
        (let ((key (eredis-org-table-get-field-clean 1))
              (value (eredis-org-table-get-field-clean 2)))
          (if (and key value)
              (cons key value)
            nil))
      nil)))

(defun eredis-org-table-mset()
  "with point in an org table convert the table to a map and send it to redis with mset"
  (interactive)
  (let ((m (eredis-org-table-to-map)))
    (eredis-mset m)))

(defun eredis-org-table-msetnx()
  "with point in an org table convert the table to a map and send it to redis with msetnx"
  (interactive)
  (let ((m (eredis-org-table-to-map)))
    (eredis-msetnx m)))

(defun eredis-org-table-row-set()
  "with point in an org table set the key and value"
  (interactive)
  (let ((keyvalue (eredis-org-table-row-to-key-value-pair)))
    (eredis-set (car keyvalue) (cdr keyvalue))))

(provide 'eredis)

;;; eredis.el ends here
