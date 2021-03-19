(ns maximoplus.net.protocols)

;;abstract away get post and server push receiving for nodejs, react native and browser
(defprotocol INet
  (-send-get
    [this url callback error-callback]
    [this url data callback error-callback])
  (-send-post
    [this url data callback error-callback]
    [this url data callback error-callback progress-callback])
  (-start-server-push-receiving
    [this sse-path callback error-callback])
  (-stop-server-push-receiving
    [this])
  (-get-tabsess;;tabsess handling will be done by the implemntation (browser or node)
    [this])
  (-set-tabsess!
    [this tabsess])
  (-send-get-with-timeout
    [this url callback error-callback timeout]
    )
  (-get-offilne-db-from-server;;works just for native, browser implementation is for the testing only, it has to return the promise
    [this post-data])
  (-get-offline-db-from-server
    [this dburl postqry])
  )
