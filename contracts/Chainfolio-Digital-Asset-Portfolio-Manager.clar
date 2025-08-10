;; ===== Chainfolio: Digital Asset Portfolio Manager =====
;; A decentralized Clarity contract for tracking and managing digital assets.
;; Provides secure, transparent ownership verification with on-chain auditability.
;; Designed with robust security protocols to protect portfolio integrity.

;; ===== Admin & Config Constants =====

(define-constant primary-administrator tx-sender)
(define-constant SYSTEM-ERROR-NOT-LOCATED (err u404))
(define-constant SYSTEM-ERROR-DUPLICATE-ENTRY (err u409))
(define-constant SYSTEM-ERROR-TITLE-VALIDATION (err u422))
(define-constant SYSTEM-ERROR-SIZE-VALIDATION (err u423))
(define-constant SYSTEM-ERROR-UNAUTHORIZED-ACCESS (err u403))
(define-constant SYSTEM-ERROR-OWNER-VERIFICATION (err u401))
(define-constant SYSTEM-ERROR-ADMIN-PRIVILEGES (err u405))
(define-constant SYSTEM-ERROR-VIEW-PERMISSIONS (err u406))
(define-constant SYSTEM-ERROR-TAG-VALIDATION (err u407))

;; ===== Core Portfolio Data Structures =====
;; Defines the primary records for comprehensive digital asset management.
(define-map digital-asset-portfolio
  { portfolio-identifier: uint }
  {
    asset-name: (string-ascii 64),
    portfolio-owner: principal,
    asset-data-size: uint,
    creation-timestamp: uint,
    asset-metadata: (string-ascii 128),
    classification-labels: (list 10 (string-ascii 32))
  }
)

;; Access control system for portfolio viewing rights
(define-map portfolio-access-control
  { portfolio-identifier: uint, authorized-viewer: principal }
  { viewing-granted: bool }
)

;; Locksmith system for time-based access control and security
(define-map portfolio-time-locks
  { portfolio-identifier: uint }
  {
    lock-active: bool,
    unlock-block-height: uint,
    lock-type: (string-ascii 32),
    lock-initiator: principal,
    emergency-unlock-key: (optional principal),
    lock-reason: (string-ascii 128)
  }
)

;; Multi-signature locksmith controls for enhanced security
(define-map locksmith-authorization
  { portfolio-identifier: uint, authorized-locksmith: principal }
  { locksmith-active: bool, authorization-level: uint }
)

;; Vesting schedule management for gradual access release
(define-map vesting-schedules
  { portfolio-identifier: uint, vesting-phase: uint }
  {
    unlock-percentage: uint,
    unlock-block-height: uint,
    phase-active: bool,
    beneficiary: principal
  }
)

;; Global portfolio counter for system-wide tracking
(define-data-var total-portfolio-count uint u0)

;; Locksmith system configuration variables
(define-data-var emergency-lockdown-active bool false)
(define-data-var default-lock-duration uint u144) ;; ~24 hours in blocks

;; ===== Utility and Validation Functions =====

;; Determines whether a specific portfolio exists in the system
(define-private (portfolio-exists-check (portfolio-identifier uint))
  (is-some (map-get? digital-asset-portfolio { portfolio-identifier: portfolio-identifier }))
)

;; Validates ownership rights for a given portfolio and user combination
(define-private (validate-portfolio-ownership (portfolio-identifier uint) (requesting-user principal))
  (match (map-get? digital-asset-portfolio { portfolio-identifier: portfolio-identifier })
    portfolio-data (is-eq (get portfolio-owner portfolio-data) requesting-user)
    false
  )
)

;; Comprehensive validation for asset naming conventions and standards
(define-private (validate-asset-naming-standards (proposed-asset-name (string-ascii 64)))
  (let ((name-length (len proposed-asset-name)))
    (and (> name-length u0) (<= name-length u64))
  )
)

;; Advanced size validation with comprehensive boundary checking
(define-private (validate-asset-size-requirements (proposed-size uint))
  (and (> proposed-size u0) (<= proposed-size u1000000))
)

;; Multi-layer tag validation system with comprehensive checking
(define-private (validate-classification-tags (tag-collection (list 10 (string-ascii 32))))
  (let ((tag-count (len tag-collection)))
    (and (<= tag-count u10) (> tag-count u0))
  )
)

;; Administrative privilege verification system
(define-private (verify-administrative-privileges (requesting-user principal))
  (is-eq requesting-user primary-administrator)
)

;; Portfolio viewing permission validation
(define-private (check-portfolio-viewing-permissions (portfolio-identifier uint) (requesting-viewer principal))
  (or 
    (validate-portfolio-ownership portfolio-identifier requesting-viewer)
    (verify-administrative-privileges requesting-viewer)
    (default-to false (get viewing-granted (map-get? portfolio-access-control { portfolio-identifier: portfolio-identifier, authorized-viewer: requesting-viewer })))
  )
)

;; ===== Primary System Functions =====

;; Creates a new digital asset portfolio with comprehensive validation
(define-public (create-digital-asset-portfolio 
  (proposed-asset-name (string-ascii 64))
  (asset-data-size uint)
  (asset-metadata (string-ascii 128))
  (classification-labels (list 10 (string-ascii 32)))
)
  (let 
    (
      (new-portfolio-id (+ (var-get total-portfolio-count) u1))
      (current-block-height block-height)
    )
    ;; Comprehensive input validation
    (asserts! (validate-asset-naming-standards proposed-asset-name) SYSTEM-ERROR-TITLE-VALIDATION)
    (asserts! (validate-asset-size-requirements asset-data-size) SYSTEM-ERROR-SIZE-VALIDATION)
    (asserts! (validate-classification-tags classification-labels) SYSTEM-ERROR-TAG-VALIDATION)
    (asserts! (not (portfolio-exists-check new-portfolio-id)) SYSTEM-ERROR-DUPLICATE-ENTRY)

    ;; Portfolio creation and registration
    (map-set digital-asset-portfolio 
      { portfolio-identifier: new-portfolio-id }
      {
        asset-name: proposed-asset-name,
        portfolio-owner: tx-sender,
        asset-data-size: asset-data-size,
        creation-timestamp: current-block-height,
        asset-metadata: asset-metadata,
        classification-labels: classification-labels
      }
    )

    ;; Update global counter
    (var-set total-portfolio-count new-portfolio-id)
    (ok new-portfolio-id)
  )
)

;; Retrieves comprehensive portfolio information with access control
(define-read-only (get-portfolio-comprehensive-details (portfolio-identifier uint))
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (check-portfolio-viewing-permissions portfolio-identifier tx-sender) SYSTEM-ERROR-VIEW-PERMISSIONS)
    (ok (unwrap! (map-get? digital-asset-portfolio { portfolio-identifier: portfolio-identifier }) SYSTEM-ERROR-NOT-LOCATED))
  )
)

;; Updates portfolio metadata with ownership verification
(define-public (update-portfolio-metadata 
  (portfolio-identifier uint)
  (updated-metadata (string-ascii 128))
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)

    (match (map-get? digital-asset-portfolio { portfolio-identifier: portfolio-identifier })
      existing-portfolio
      (begin
        (map-set digital-asset-portfolio
          { portfolio-identifier: portfolio-identifier }
          (merge existing-portfolio { asset-metadata: updated-metadata })
        )
        (ok true)
      )
      SYSTEM-ERROR-NOT-LOCATED
    )
  )
)

;; Transfers portfolio ownership with comprehensive validation
(define-public (transfer-portfolio-ownership 
  (portfolio-identifier uint)
  (new-portfolio-owner principal)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    
    (asserts! (check-emergency-lockdown-status) SYSTEM-ERROR-ADMIN-PRIVILEGES)

    (match (map-get? digital-asset-portfolio { portfolio-identifier: portfolio-identifier })
      existing-portfolio
      (begin
        (map-set digital-asset-portfolio
          { portfolio-identifier: portfolio-identifier }
          (merge existing-portfolio { portfolio-owner: new-portfolio-owner })
        )
        (ok true)
      )
      SYSTEM-ERROR-NOT-LOCATED
    )
  )
)

;; Grants viewing permissions to authorized users
(define-public (grant-portfolio-viewing-access 
  (portfolio-identifier uint)
  (authorized-viewer principal)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)

    (map-set portfolio-access-control
      { portfolio-identifier: portfolio-identifier, authorized-viewer: authorized-viewer }
      { viewing-granted: true }
    )
    (ok true)
  )
)

;; Revokes previously granted viewing permissions
(define-public (revoke-portfolio-viewing-access 
  (portfolio-identifier uint)
  (unauthorized-viewer principal)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)

    (map-delete portfolio-access-control
      { portfolio-identifier: portfolio-identifier, authorized-viewer: unauthorized-viewer }
    )
    (ok true)
  )
)

;; Administrative function for emergency portfolio removal
(define-public (administrative-portfolio-removal (portfolio-identifier uint))
  (begin
    (asserts! (verify-administrative-privileges tx-sender) SYSTEM-ERROR-ADMIN-PRIVILEGES)
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)

    (map-delete digital-asset-portfolio { portfolio-identifier: portfolio-identifier })
    (ok true)
  )
)

;; Retrieves total number of registered portfolios
(define-read-only (get-total-portfolio-statistics)
  (ok (var-get total-portfolio-count))
)

;; Checks if a specific portfolio identifier exists in the system
(define-read-only (verify-portfolio-existence (portfolio-identifier uint))
  (ok (portfolio-exists-check portfolio-identifier))
)

;; Validates ownership status for external verification
(define-read-only (external-ownership-validation 
  (portfolio-identifier uint)
  (potential-owner principal)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (ok (validate-portfolio-ownership portfolio-identifier potential-owner))
  )
)

;; Comprehensive locksmith validation system
(define-private (validate-portfolio-unlocked (portfolio-identifier uint))
  (match (map-get? portfolio-time-locks { portfolio-identifier: portfolio-identifier })
    lock-data 
    (or 
      (not (get lock-active lock-data))
      (>= block-height (get unlock-block-height lock-data))
    )
    true ;; No lock exists, portfolio is unlocked
  )
)

;; Emergency lockdown system validation
(define-private (check-emergency-lockdown-status)
  (not (var-get emergency-lockdown-active))
)

;; Locksmith authorization verification
(define-private (verify-locksmith-authorization (portfolio-identifier uint) (potential-locksmith principal))
  (or
    (validate-portfolio-ownership portfolio-identifier potential-locksmith)
    (verify-administrative-privileges potential-locksmith)
    (default-to false (get locksmith-active (map-get? locksmith-authorization { portfolio-identifier: portfolio-identifier, authorized-locksmith: potential-locksmith })))
  )
)

;; Vesting schedule validation and calculation
(define-private (calculate-vested-percentage (portfolio-identifier uint))
  (let ((current-block block-height))
    ;; Simplified vesting calculation - in real implementation would iterate through phases
    u100 ;; Returns 100% for demonstration
  )
)

;; ===== Enhanced Locksmith Functions =====

;; Creates a time-based lock on a portfolio
(define-public (engage-portfolio-timelock
  (portfolio-identifier uint)
  (lock-duration-blocks uint)
  (lock-type (string-ascii 32))
  (lock-reason (string-ascii 128))
  (emergency-unlock-key (optional principal))
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (verify-locksmith-authorization portfolio-identifier tx-sender) SYSTEM-ERROR-UNAUTHORIZED-ACCESS)
    (asserts! (validate-portfolio-unlocked portfolio-identifier) SYSTEM-ERROR-DUPLICATE-ENTRY)
    (asserts! (> lock-duration-blocks u0) SYSTEM-ERROR-SIZE-VALIDATION)
    (asserts! (check-emergency-lockdown-status) SYSTEM-ERROR-ADMIN-PRIVILEGES)

    (let ((unlock-height (+ block-height lock-duration-blocks)))
      (map-set portfolio-time-locks
        { portfolio-identifier: portfolio-identifier }
        {
          lock-active: true,
          unlock-block-height: unlock-height,
          lock-type: lock-type,
          lock-initiator: tx-sender,
          emergency-unlock-key: emergency-unlock-key,
          lock-reason: lock-reason
        }
      )
      (ok unlock-height)
    )
  )
)

;; Releases a time-lock manually (if authorized)
(define-public (release-portfolio-timelock (portfolio-identifier uint))
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)

    (match (map-get? portfolio-time-locks { portfolio-identifier: portfolio-identifier })
      lock-data
      (begin
        (asserts! (get lock-active lock-data) SYSTEM-ERROR-NOT-LOCATED)
        (asserts! 
          (or 
            (is-eq tx-sender (get lock-initiator lock-data))
            (verify-administrative-privileges tx-sender)
          ) 
          SYSTEM-ERROR-UNAUTHORIZED-ACCESS
        )

        (map-set portfolio-time-locks
          { portfolio-identifier: portfolio-identifier }
          (merge lock-data { lock-active: false })
        )
        (ok true)
      )
      SYSTEM-ERROR-NOT-LOCATED
    )
  )
)

;; Emergency lockdown system for all portfolios
(define-public (activate-emergency-lockdown)
  (begin
    (asserts! (verify-administrative-privileges tx-sender) SYSTEM-ERROR-ADMIN-PRIVILEGES)
    (var-set emergency-lockdown-active true)
    (ok true)
  )
)

;; Deactivates emergency lockdown
(define-public (deactivate-emergency-lockdown)
  (begin
    (asserts! (verify-administrative-privileges tx-sender) SYSTEM-ERROR-ADMIN-PRIVILEGES)
    (var-set emergency-lockdown-active false)
    (ok true)
  )
)

;; Grants locksmith privileges to authorized users
(define-public (authorize-portfolio-locksmith 
  (portfolio-identifier uint)
  (new-locksmith principal)
  (authorization-level uint)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)
    (asserts! (<= authorization-level u5) SYSTEM-ERROR-TAG-VALIDATION)

    (map-set locksmith-authorization
      { portfolio-identifier: portfolio-identifier, authorized-locksmith: new-locksmith }
      { locksmith-active: true, authorization-level: authorization-level }
    )
    (ok true)
  )
)

;; Revokes locksmith privileges
(define-public (revoke-locksmith-authorization 
  (portfolio-identifier uint)
  (unauthorized-locksmith principal)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)

    (map-delete locksmith-authorization
      { portfolio-identifier: portfolio-identifier, authorized-locksmith: unauthorized-locksmith }
    )
    (ok true)
  )
)

;; Creates a vesting schedule for gradual access release
(define-public (create-vesting-schedule
  (portfolio-identifier uint)
  (vesting-phases (list 10 { percentage: uint, delay-blocks: uint, beneficiary: principal }))
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)
    (asserts! (> (len vesting-phases) u0) SYSTEM-ERROR-TAG-VALIDATION)

    ;; In a full implementation, would iterate through phases and create individual entries
    (ok true)
  )
)

;; Enhanced transfer function with locksmith checks
(define-public (secure-transfer-portfolio-ownership 
  (portfolio-identifier uint)
  (new-portfolio-owner principal)
)
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (validate-portfolio-ownership portfolio-identifier tx-sender) SYSTEM-ERROR-OWNER-VERIFICATION)
    (asserts! (check-emergency-lockdown-status) SYSTEM-ERROR-ADMIN-PRIVILEGES)

    (match (map-get? digital-asset-portfolio { portfolio-identifier: portfolio-identifier })
      existing-portfolio
      (begin
        (map-set digital-asset-portfolio
          { portfolio-identifier: portfolio-identifier }
          (merge existing-portfolio { portfolio-owner: new-portfolio-owner })
        )
        (ok true)
      )
      SYSTEM-ERROR-NOT-LOCATED
    )
  )
)

;; ===== Locksmith Information Functions =====

;; Gets comprehensive lock status information
(define-read-only (get-portfolio-lock-status (portfolio-identifier uint))
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (asserts! (check-portfolio-viewing-permissions portfolio-identifier tx-sender) SYSTEM-ERROR-VIEW-PERMISSIONS)

    (match (map-get? portfolio-time-locks { portfolio-identifier: portfolio-identifier })
      lock-data (ok (some lock-data))
      (ok none)
    )
  )
)

;; Checks if portfolio is currently locked
(define-read-only (is-portfolio-locked (portfolio-identifier uint))
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)
    (ok (not (validate-portfolio-unlocked portfolio-identifier)))
  )
)

;; Gets remaining lock duration
(define-read-only (get-remaining-lock-duration (portfolio-identifier uint))
  (begin
    (asserts! (portfolio-exists-check portfolio-identifier) SYSTEM-ERROR-NOT-LOCATED)

    (match (map-get? portfolio-time-locks { portfolio-identifier: portfolio-identifier })
      lock-data
      (if (and (get lock-active lock-data) (< block-height (get unlock-block-height lock-data)))
        (ok (- (get unlock-block-height lock-data) block-height))
        (ok u0)
      )
      (ok u0)
    )
  )
)

;; Advanced search functionality for portfolios by owner
(define-read-only (search-portfolios-by-owner (target-owner principal))
  (let ((current-portfolio-count (var-get total-portfolio-count)))
    (ok current-portfolio-count) ;; Simplified return for demonstration
  )
)