;; ------------------------------------------------------------
;; dao-core.clar
;; Minimal DAO governance core for Stacks projects
;; ------------------------------------------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-NOT-MEMBER u101)
(define-constant ERR-PROPOSAL-NOT-FOUND u102)
(define-constant ERR-VOTING-CLOSED u103)
(define-constant ERR-ALREADY-VOTED u104)
(define-constant ERR-PROPOSAL-ACTIVE u105)
(define-constant ERR-QUORUM-NOT-REACHED u106)
(define-constant ERR-PROPOSAL-NOT-PASSED u107)
(define-constant ERR-INVALID-EXECUTION u108)

;; DAO parameters
(define-data-var owner (optional principal) none)
(define-data-var voting-period uint u43200) ;; ~12 hours (in blocks)
(define-data-var quorum-percent uint u20)   ;; 20% required
(define-data-var proposal-count uint u0)

;; Members registry
(define-map members
  { addr: principal }
  { active: bool })

;; Proposals
(define-map proposals
  { id: uint }
  {
    creator: principal,
    description: (string-ascii 200),
    start-block: uint,
    yes: uint,
    no: uint,
    executed: bool,
    target: principal,
    function-name: (string-ascii 40)
  })

;; Track who voted on what
(define-map votes
  { id: uint, voter: principal }
  { voted: bool })

;; Initialization (one-time)
(define-public (initialize (admin principal))
  (if (is-some (var-get owner))
      (err ERR-NOT-OWNER)
      (if (is-eq admin tx-sender)
          (begin
            (var-set owner (some admin))
            (map-set members { addr: admin } { active: true })
            (ok admin)
          )
          (err ERR-NOT-OWNER)
      )
  )
)

;; Member management (owner only)
(define-public (add-member (user principal))
  (if (is-some (var-get owner))
      (if (is-eq (unwrap-panic (var-get owner)) tx-sender)
          (if (not (is-eq user tx-sender))
              (begin
                (map-set members { addr: user } { active: true })
                (ok user)
              )
              (err ERR-NOT-OWNER)
          )
          (err ERR-NOT-OWNER)
      )
      (err ERR-NOT-OWNER)
  )
)

;; Helper: Check if user is a member (read-only)
(define-read-only (is-member (user principal))
  (default-to false (get active (map-get? members { addr: user })))
)

;; Create a DAO proposal
(define-public (create-proposal 
    (description (string-ascii 200))
    (target principal)
    (function-name (string-ascii 40))
)
  (if (not (is-member tx-sender))
      (err ERR-NOT-MEMBER)
      (if (is-eq (len description) u0)
          (err ERR-NOT-MEMBER)
          (if (and (not (is-eq target tx-sender)) (> (len function-name) u0))
              (let ((id (+ (var-get proposal-count) u1)))
                (begin
              (var-set proposal-count id)
              (map-set proposals 
                { id: id }
                {
                  creator: tx-sender,
                  description: description,
                  start-block: burn-block-height,
                  yes: u0,
                  no: u0,
                  executed: false,
                  target: target,
                  function-name: function-name
                }
              )
              (ok id)
                )
              )
              (err ERR-NOT-MEMBER)
          )
      )
  )
)

;; Vote on a proposal
(define-public (vote (id uint) (support bool))
  (if (not (is-member tx-sender))
      (err ERR-NOT-MEMBER)
      (if (is-eq id u0)
          (err ERR-PROPOSAL-NOT-FOUND)
          (let ((proposal-opt (map-get? proposals { id: id })))
            (if (is-none proposal-opt)
                (err ERR-PROPOSAL-NOT-FOUND)
                (let ((some-p (unwrap-panic proposal-opt)))
                  (let ((end (+ (get start-block some-p) (var-get voting-period))))
                    (if (> burn-block-height end)
                        (err ERR-VOTING-CLOSED)
                        (if (is-some (map-get? votes { id: id, voter: tx-sender }))
                            (err ERR-ALREADY-VOTED)
                            (begin
                              (map-set votes 
                                { id: id, voter: tx-sender }
                                { voted: true }
                              )
                              (if support
                                  (map-set proposals { id: id }
                                    { 
                                      creator: (get creator some-p),
                                      description: (get description some-p),
                                      start-block: (get start-block some-p),
                                      yes: (+ (get yes some-p) u1),
                                      no: (get no some-p),
                                      executed: (get executed some-p),
                                      target: (get target some-p),
                                      function-name: (get function-name some-p)
                                    })
                                  (map-set proposals { id: id }
                                    { 
                                      creator: (get creator some-p),
                                      description: (get description some-p),
                                      start-block: (get start-block some-p),
                                      yes: (get yes some-p),
                                      no: (+ (get no some-p) u1),
                                      executed: (get executed some-p),
                                      target: (get target some-p),
                                      function-name: (get function-name some-p)
                                    })
                              )
                              (ok true)
                            )
                        )
                    )
                  )
                )
            )
          )
      )
  )
)

;; Execute a passed proposal
(define-public (execute (id uint))
  (if (is-eq id u0)
      (err ERR-PROPOSAL-NOT-FOUND)
      (let ((proposal-opt (map-get? proposals { id: id })))
        (if (is-none proposal-opt)
            (err ERR-PROPOSAL-NOT-FOUND)
            (let ((some-p (unwrap-panic proposal-opt)))
              (let ((total (+ (get yes some-p) (get no some-p)))
                    (quorum-needed (if (is-eq total u0) u1 (/ (* (var-get quorum-percent) total) u100))))
                (if (get executed some-p)
                    (err ERR-INVALID-EXECUTION)
                    (if (< (get yes some-p) quorum-needed)
                        (err ERR-QUORUM-NOT-REACHED)
                        (if (<= (get yes some-p) (get no some-p))
                            (err ERR-PROPOSAL-NOT-PASSED)
                            (begin
                              (map-set proposals { id: id }
                                { creator: (get creator some-p),
                                  description: (get description some-p),
                                  start-block: (get start-block some-p),
                                  yes: (get yes some-p),
                                  no: (get no some-p),
                                  executed: true,
                                  target: (get target some-p),
                                  function-name: (get function-name some-p)
                                }
                              )
                              (ok true)
                            )
                        )
                    )
                )
              )
            )
        )
      )
  )
)

;; Views
(define-read-only (get-proposal (id uint))
  (map-get? proposals { id: id })
)

(define-read-only (get-total-proposals)
  (ok (var-get proposal-count))
)
