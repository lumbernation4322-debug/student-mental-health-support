;; Student Mental Health Support Smart Contract
;; Manages counseling services, appointments, crisis intervention, and resource referrals

;; Define constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-NOT-AUTHORIZED (err u103))
(define-constant ERR-INVALID-STATUS (err u104))
(define-constant ERR-INVALID-TIME (err u105))

;; Data variables
(define-data-var next-appointment-id uint u1)
(define-data-var next-crisis-id uint u1)
(define-data-var next-resource-id uint u1)

;; Define data maps
;; Counselor profiles
(define-map counselors
  { counselor: principal }
  {
    name: (string-ascii 50),
    specialization: (string-ascii 100),
    license-number: (string-ascii 30),
    contact-info: (string-ascii 100),
    active: bool,
    max-daily-appointments: uint
  }
)

;; Student profiles (anonymous)
(define-map student-profiles
  { student: principal }
  {
    grade-level: uint,
    emergency-contact: (string-ascii 100),
    medical-notes: (string-ascii 200),
    consent-given: bool,
    risk-level: (string-ascii 20)
  }
)

;; Appointment scheduling
(define-map appointments
  { appointment-id: uint }
  {
    student: principal,
    counselor: principal,
    scheduled-time: uint,
    duration-minutes: uint,
    type: (string-ascii 30),
    status: (string-ascii 20),
    notes: (optional (string-ascii 500)),
    outcome: (optional (string-ascii 200))
  }
)

;; Crisis interventions
(define-map crisis-reports
  { crisis-id: uint }
  {
    student: principal,
    reported-by: principal,
    severity: (string-ascii 20),
    description: (string-ascii 500),
    timestamp: uint,
    response-actions: (string-ascii 300),
    status: (string-ascii 30),
    resolved-by: (optional principal)
  }
)

;; Mental health resources
(define-map resources
  { resource-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    category: (string-ascii 50),
    contact-info: (string-ascii 100),
    availability: (string-ascii 100),
    added-by: principal,
    active: bool
  }
)

;; Resource referrals
(define-map referrals
  { student: principal, resource-id: uint }
  {
    referred-by: principal,
    referral-date: uint,
    status: (string-ascii 30),
    follow-up-date: (optional uint),
    outcome-notes: (optional (string-ascii 200))
  }
)

;; School administrators
(define-map administrators
  { admin: principal }
  {
    school-name: (string-ascii 100),
    role: (string-ascii 50),
    authorized: bool
  }
)

;; Public functions

;; Register counselor (administrators only)
(define-public (register-counselor
    (counselor principal)
    (name (string-ascii 50))
    (specialization (string-ascii 100))
    (license-number (string-ascii 30))
    (contact-info (string-ascii 100))
    (max-daily-appointments uint))
  (let
    (
      (admin-data (map-get? administrators { admin: tx-sender }))
    )
    (asserts! (is-some admin-data) ERR-NOT-AUTHORIZED)
    (asserts! (get authorized (unwrap! admin-data ERR-NOT-AUTHORIZED)) ERR-NOT-AUTHORIZED)
    
    (map-set counselors
      { counselor: counselor }
      {
        name: name,
        specialization: specialization,
        license-number: license-number,
        contact-info: contact-info,
        active: true,
        max-daily-appointments: max-daily-appointments
      }
    )
    (ok true)
  )
)

;; Register student profile
(define-public (register-student
    (grade-level uint)
    (emergency-contact (string-ascii 100))
    (medical-notes (string-ascii 200)))
  (begin
    (map-set student-profiles
      { student: tx-sender }
      {
        grade-level: grade-level,
        emergency-contact: emergency-contact,
        medical-notes: medical-notes,
        consent-given: true,
        risk-level: "low"
      }
    )
    (ok true)
  )
)

;; Schedule appointment
(define-public (schedule-appointment
    (counselor principal)
    (scheduled-time uint)
    (duration-minutes uint)
    (appointment-type (string-ascii 30)))
  (let
    (
      (appointment-id (var-get next-appointment-id))
      (counselor-data (unwrap! (map-get? counselors { counselor: counselor }) ERR-NOT-FOUND))
      (student-data (unwrap! (map-get? student-profiles { student: tx-sender }) ERR-NOT-AUTHORIZED))
    )
    (asserts! (get active counselor-data) ERR-INVALID-STATUS)
    (asserts! (get consent-given student-data) ERR-NOT-AUTHORIZED)
    (asserts! (> scheduled-time burn-block-height) ERR-INVALID-TIME)
    
    (map-set appointments
      { appointment-id: appointment-id }
      {
        student: tx-sender,
        counselor: counselor,
        scheduled-time: scheduled-time,
        duration-minutes: duration-minutes,
        type: appointment-type,
        status: "scheduled",
        notes: none,
        outcome: none
      }
    )
    
    (var-set next-appointment-id (+ appointment-id u1))
    (ok appointment-id)
  )
)

;; Report crisis (students, staff, or counselors)
(define-public (report-crisis
    (student principal)
    (severity (string-ascii 20))
    (description (string-ascii 500)))
  (let
    (
      (crisis-id (var-get next-crisis-id))
    )
    (map-set crisis-reports
      { crisis-id: crisis-id }
      {
        student: student,
        reported-by: tx-sender,
        severity: severity,
        description: description,
        timestamp: burn-block-height,
        response-actions: "",
        status: "reported",
        resolved-by: none
      }
    )
    
    (var-set next-crisis-id (+ crisis-id u1))
    (ok crisis-id)
  )
)

;; Update crisis response (counselors only)
(define-public (update-crisis-response
    (crisis-id uint)
    (response-actions (string-ascii 300))
    (new-status (string-ascii 30)))
  (let
    (
      (crisis (unwrap! (map-get? crisis-reports { crisis-id: crisis-id }) ERR-NOT-FOUND))
      (counselor-data (unwrap! (map-get? counselors { counselor: tx-sender }) ERR-NOT-AUTHORIZED))
    )
    (asserts! (get active counselor-data) ERR-NOT-AUTHORIZED)
    
    (map-set crisis-reports
      { crisis-id: crisis-id }
      (merge crisis {
        response-actions: response-actions,
        status: new-status,
        resolved-by: (some tx-sender)
      })
    )
    (ok true)
  )
)

;; Add mental health resource (counselors and administrators)
(define-public (add-resource
    (title (string-ascii 100))
    (description (string-ascii 300))
    (category (string-ascii 50))
    (contact-info (string-ascii 100))
    (availability (string-ascii 100)))
  (let
    (
      (resource-id (var-get next-resource-id))
      (is-counselor (is-some (map-get? counselors { counselor: tx-sender })))
      (is-admin (is-some (map-get? administrators { admin: tx-sender })))
    )
    (asserts! (or is-counselor is-admin) ERR-NOT-AUTHORIZED)
    
    (map-set resources
      { resource-id: resource-id }
      {
        title: title,
        description: description,
        category: category,
        contact-info: contact-info,
        availability: availability,
        added-by: tx-sender,
        active: true
      }
    )
    
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

;; Create resource referral (counselors only)
(define-public (create-referral
    (student principal)
    (resource-id uint))
  (let
    (
      (counselor-data (unwrap! (map-get? counselors { counselor: tx-sender }) ERR-NOT-AUTHORIZED))
      (resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR-NOT-FOUND))
    )
    (asserts! (get active counselor-data) ERR-NOT-AUTHORIZED)
    (asserts! (get active resource) ERR-INVALID-STATUS)
    
    (map-set referrals
      { student: student, resource-id: resource-id }
      {
        referred-by: tx-sender,
        referral-date: burn-block-height,
        status: "active",
        follow-up-date: none,
        outcome-notes: none
      }
    )
    (ok true)
  )
)

;; Update appointment outcome (counselors only)
(define-public (update-appointment-outcome
    (appointment-id uint)
    (notes (string-ascii 500))
    (outcome (string-ascii 200)))
  (let
    (
      (appointment (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get counselor appointment)) ERR-NOT-AUTHORIZED)
    
    (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment {
        status: "completed",
        notes: (some notes),
        outcome: (some outcome)
      })
    )
    (ok true)
  )
)

;; Authorize administrator (contract owner only)
(define-public (authorize-admin
    (admin principal)
    (school-name (string-ascii 100))
    (role (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-set administrators
      { admin: admin }
      {
        school-name: school-name,
        role: role,
        authorized: true
      }
    )
    (ok true)
  )
)

;; Read-only functions

;; Get counselor profile
(define-read-only (get-counselor (counselor principal))
  (map-get? counselors { counselor: counselor })
)

;; Get appointment details
(define-read-only (get-appointment (appointment-id uint))
  (map-get? appointments { appointment-id: appointment-id })
)

;; Get crisis report
(define-read-only (get-crisis (crisis-id uint))
  (map-get? crisis-reports { crisis-id: crisis-id })
)

;; Get resource
(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

;; Get referral
(define-read-only (get-referral (student principal) (resource-id uint))
  (map-get? referrals { student: student, resource-id: resource-id })
)

;; Get student profile (students can only view their own)
(define-read-only (get-student-profile (student principal))
  (if (is-eq student tx-sender)
    (map-get? student-profiles { student: student })
    none
  )
)

;; Get next IDs
(define-read-only (get-next-appointment-id)
  (var-get next-appointment-id)
)

(define-read-only (get-next-crisis-id)
  (var-get next-crisis-id)
)

(define-read-only (get-next-resource-id)
  (var-get next-resource-id)
)


;; title: counseling-services
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

