(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PORT (err u101))
(define-constant ERR-ALREADY-REPORTED (err u102))
(define-constant ERR-INVALID-REPORT (err u103))
(define-constant ERR-NOT-FOUND (err u104))

(define-data-var admin principal tx-sender)

(define-map ports
    { port-id: uint }
    {
        name: (string-ascii 50),
        country: (string-ascii 2),
        reputation-score: int,
        total-reports: uint,
        is-verified: bool
    }
)

(define-map reports
    { report-id: uint }
    {
        reporter: principal,
        port-id: uint,
        timestamp: uint,
        barrier-type: (string-ascii 20),
        description: (string-ascii 500),
        evidence-hash: (optional (buff 32)),
        upvotes: uint,
        downvotes: uint
    }
)

(define-map user-votes
    { user: principal, report-id: uint }
    { voted: bool }
)

(define-data-var next-report-id uint u1)
(define-data-var next-port-id uint u1)

(define-public (add-port (name (string-ascii 50)) (country (string-ascii 2)))
    (let ((port-id (var-get next-port-id)))
        (if (is-eq tx-sender (var-get admin))
            (begin
                (map-set ports
                    { port-id: port-id }
                    {
                        name: name,
                        country: country,
                        reputation-score: 0,
                        total-reports: u0,
                        is-verified: false
                    }
                )
                (var-set next-port-id (+ port-id u1))
                (ok port-id))
            ERR-NOT-AUTHORIZED)
    )
)

(define-public (submit-report 
    (port-id uint)
    (barrier-type (string-ascii 20))
    (description (string-ascii 500))
    (evidence-hash (optional (buff 32))))
    (let ((report-id (var-get next-report-id)))
        (match (map-get? ports { port-id: port-id })
            port
            (begin
                (map-set reports
                    { report-id: report-id }
                    {
                        reporter: tx-sender,
                        port-id: port-id,
                        timestamp: burn-block-height,
                        barrier-type: barrier-type,
                        description: description,
                        evidence-hash: evidence-hash,
                        upvotes: u0,
                        downvotes: u0
                    }
                )
                (map-set ports
                    { port-id: port-id }
                    (merge port {
                        total-reports: (+ (get total-reports port) u1),
                        reputation-score: (- (get reputation-score port) 1)
                    })
                )
                (var-set next-report-id (+ report-id u1))
                (ok report-id))
            ERR-INVALID-PORT)
    )
)

(define-public (vote-on-report (report-id uint) (is-upvote bool))
    (match (map-get? reports { report-id: report-id })
        report
        (match (map-get? user-votes { user: tx-sender, report-id: report-id })
            vote-info
            ERR-ALREADY-REPORTED
            (begin
                (map-set user-votes
                    { user: tx-sender, report-id: report-id }
                    { voted: true }
                )
                (map-set reports
                    { report-id: report-id }
                    (merge report {
                        upvotes: (if is-upvote (+ (get upvotes report) u1) (get upvotes report)),
                        downvotes: (if is-upvote (get downvotes report) (+ (get downvotes report) u1))
                    })
                )
                (ok true)))
        ERR-NOT-FOUND)
)

(define-public (verify-port (port-id uint))
    (if (is-eq tx-sender (var-get admin))
        (match (map-get? ports { port-id: port-id })
            port
            (begin
                (map-set ports
                    { port-id: port-id }
                    (merge port { is-verified: true })
                )
                (ok true))
            ERR-INVALID-PORT)
        ERR-NOT-AUTHORIZED)
)

(define-read-only (get-port-details (port-id uint))
    (map-get? ports { port-id: port-id })
)

(define-read-only (get-report-details (report-id uint))
    (map-get? reports { report-id: report-id })
)

(define-read-only (get-analytics-summary)
    {
        total-ports: (- (var-get next-port-id) u1),
        total-reports: (- (var-get next-report-id) u1),
        active-ports: (count-active-ports)
    }
)

(define-read-only (get-port-analytics (port-id uint))
    (match (map-get? ports { port-id: port-id })
        port 
        (some {
            port-id: port-id,
            name: (get name port),
            country: (get country port),
            reputation-score: (get reputation-score port),
            total-reports: (get total-reports port),
            is-verified: (get is-verified port),
            risk-level: (categorize-risk (get total-reports port))
        })
        none)
)

(define-read-only (get-report-analytics (report-id uint))
    (match (map-get? reports { report-id: report-id })
        report
        (let ((total-votes (+ (get upvotes report) (get downvotes report))))
            (some {
                report-id: report-id,
                reporter: (get reporter report),
                port-id: (get port-id report),
                barrier-type: (get barrier-type report),
                total-votes: total-votes,
                approval-rate: (if (> total-votes u0)
                                  (/ (* (get upvotes report) u100) total-votes)
                                  u0),
                timestamp: (get timestamp report)
            }))
        none)
)

(define-read-only (get-verification-status (port-id uint))
    (match (map-get? ports { port-id: port-id })
        port (get is-verified port)
        false)
)

(define-read-only (get-port-reputation (port-id uint))
    (match (map-get? ports { port-id: port-id })
        port (get reputation-score port)
        0)
)

(define-private (count-active-ports)
    (fold count-port-helper (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
)

(define-private (count-port-helper (port-id uint) (acc uint))
    (match (map-get? ports { port-id: port-id })
        port (+ acc u1)
        acc)
)

(define-private (categorize-risk (report-count uint))
    (if (>= report-count u5)
        "high"
        (if (>= report-count u2)
            "medium"
            "low"))
)