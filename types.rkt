#lang racket

(require racket/string
         json
         "utils.rkt")

(provide make-host-config
         create-container-config)

(define-syntax unless-null
  (syntax-rules ()
    [(unless-null val body)
  		(unless (or (null? val) (not val)) body)]
    [(unless-null val body otherwise)
     	(if (not (or (null? val) (not val)))
          body
          otherwise)]))

(define make-host-config 
  (lambda (version
           #:binds [binds null]
           #:port-bindings [port-bindings null]
           #:publish-all-ports [publish-all-ports #f]
           #:links [links null]
           #:privileged [privileged #f]
           #:dns [dns null]
           #:dns-search [dns-search null]
           #:volumes-from [volumes-from null]
           #:network-mode [network-mode null]
           #:restart-policy [restart-policy null]
           #:cap-add [cap-add null]
           #:cap-drop [cap-drop null]
           #:devices [devices null]
           #:extra-hosts [extra-hosts null]
           #:read-only [read-only null]
           #:pid-mode [pid-mode null]
           #:ipc-mode [ipc-mode null]
           #:security-opt [security-opt null]
           #:ulimits [ulimits null]
           #:log-config [log-config null]
           #:mem-limit [mem-limit null]
           #:memswap-limit [memswap-limit null]
           #:mem-reservation [mem-reservation null]
           #:kernel-memory [kernel-memory null]
           #:mem-swappiness [mem-swappiness null]
           #:cgroup-parent [cgroup-parent null]
           #:group-add [group-add null]
           #:cpu-quota [cpu-quota null]
           #:cpu-period [cpu-period null]
           #:blkio-weight [blkio-weight null]
           #:blkio-weight-device [blkio-weight-device null]
           #:device-read-bps [device-read-bps null]
           #:device-write-bps [device-write-bps null]
           #:device-read-iops [device-read-iops null]
           #:device-write-iops [device-write-iops null]
           #:oom-kill-disable [oom-kill-disable #f]
           #:shm-size [shm-size null]
           #:sysctls [sysctls null]
           #:tmpfs [tmpfs null]
           #:oom-score-adj [oom-score-adj null]
           #:dns-opt [dns-opt null]
           #:cpu-shares [cpu-shares null]
           #:cpuset-cpus [cpuset-cpus null]
           #:userns-mode [userns-mode null]
           #:pids-limit [pids-limit null]
           #:isolation [isolation null]
           #:auto-remove [auto-remove #f]
           #:storage-opt [storage-opt null]
           #:init [init null]
           #:init-path [init-path null]
           #:volume-driver [volume-driver null]
           #:cpu-count [cpu-count null]
           #:cpu-percent [cpu-percent null]
           #:nano-cpus [nano-cpus null]
           #:cpuset-mems [cpuset-mems null]
           #:runtime [runtime null])
    (define host-config (make-hash))
    (unless-null binds
    	(hash-set! host-config 'Binds binds))
    (unless-null port-bindings
      (hash-set! host-config 'PortBindings port-bindings))
    (unless-null publish-all-ports
      (hash-set! host-config 'PublishAllPorts publish-all-ports))
    (unless-null links
      (hash-set! host-config 'Links (normalize-links links)))
    (unless-null privileged
      (hash-set! host-config 'Privileged privileged))
    (unless-null dns
      (hash-set! host-config 'Dns dns))
    (unless-null dns-search
      (hash-set! host-config 'DnsSearch dns-search))
    (unless-null volumes-from
      (hash-set! host-config 'VolumesFrom volumes-from))
    (unless-null network-mode
      (hash-set! host-config 'NetworkMode network-mode)
      (when (version-gte version "1.20")
        (hash-set! host-config 'NetworkMode "default")))
    (unless-null restart-policy
      (hash-set! host-config 'RestartPolicy restart-policy))
    (unless-null cap-add
      (hash-set! host-config 'CapAdd cap-add))
    (unless-null cap-drop
      (hash-set! host-config 'CapDrop cap-drop))
    (unless-null devices
      (hash-set! host-config 'Devices (parse-devices devices)))
    (unless-null extra-hosts
      (hash-set! host-config 'ExtraHosts extra-hosts))
    (unless-null read-only
      (hash-set! host-config 'ReadonlyRootfs read-only))
    (unless-null pid-mode
      (if (and (version-lt version "1.24")
               (not (eq? pid-mode "host")))
          (error "host-config value error,
                 pid-mode " pid-mode)
      		(hash-set! host-config 'PidMode pid-mode)))
    (unless-null ipc-mode
      (hash-set! host-config 'IpcMode ipc-mode))
    (unless-null security-opt
      (hash-set! host-config 'SecurityOpt security-opt))
    (unless-null ulimits
      (hash-set! host-config 'Ulimits ulimits))
    (unless-null log-config
      (hash-set! host-config 'LogConfig log-config))
    (unless-null mem-limit
      (hash-set! host-config 'Memory mem-limit))
    (unless-null memswap-limit
      (hash-set! host-config 'MemorySwap memswap-limit))
    (unless-null mem-reservation
      (hash-set! host-config 'MemoryReservation mem-reservation))
    (unless-null kernel-memory
      (hash-set! host-config 'KernelMemory kernel-memory))
    (unless-null mem-swappiness
      (hash-set! host-config 'MemorySwappiness mem-swappiness))
    (unless-null cgroup-parent
      (hash-set! host-config 'CgroupParent cgroup-parent))
    (unless-null group-add
      (hash-set! host-config 'GroupAdd group-add))
    (unless-null cpu-quota
      (hash-set! host-config 'CpuQuota cpu-quota))
    (unless-null cpu-period
      (hash-set! host-config 'CpuPeriod cpu-period))
    (unless-null blkio-weight
      (hash-set! host-config 'BlkioWeight blkio-weight))
    (unless-null blkio-weight-device
      (hash-set! host-config 'BlkioWeightDevice blkio-weight-device))
    (unless-null device-read-bps
      (hash-set! host-config 'DeviceReadBps device-read-bps))
    (unless-null device-write-bps
      (hash-set! host-config 'DeviceWriteBps device-write-bps))
    (unless-null device-read-iops
      (hash-set! host-config 'DeviceReadIops device-read-iops))
    (unless-null device-write-iops
      (hash-set! host-config 'DeviceWriteIOps device-write-iops))
    (unless-null oom-kill-disable
      (hash-set! host-config 'OomKillDisable oom-kill-disable))
    (unless-null shm-size
      (hash-set! host-config 'ShmSize shm-size))
    (unless-null sysctls
      (hash-set! host-config 'Sysctls sysctls))
    (unless-null tmpfs
      (hash-set! host-config 'Tmpfs tmpfs))
    (unless-null oom-score-adj
      (hash-set! host-config 'OomScoreAdj oom-score-adj))
    (unless-null dns-opt
      (hash-set! host-config 'DnsOptions dns-opt))
    (unless-null cpu-shares
      (hash-set! host-config 'CpuShares cpu-shares))
    (unless-null cpuset-cpus
      (hash-set! host-config 'CpusetCpus cpuset-cpus))
    (unless-null userns-mode
      (hash-set! host-config 'UsernsMode userns-mode))
    (unless-null pids-limit
      (hash-set! host-config 'PidsLimit pids-limit))
    (unless-null isolation
      (hash-set! host-config 'Isolation isolation))
    (unless-null auto-remove
      (hash-set! host-config 'AutoRemove auto-remove))
    (unless-null storage-opt
      (hash-set! host-config 'StorageOpt storage-opt))
    (unless-null init
      (hash-set! host-config 'Init init))
    (unless-null init-path
      (hash-set! host-config 'InitPath init-path))
    (unless-null volume-driver
      (hash-set! host-config 'VolumeDriver volume-driver))
    (unless-null cpu-count
      (hash-set! host-config 'CpuCount cpu-count))
    (unless-null cpu-percent
      (hash-set! host-config 'CpuPercent cpu-percent))
    (unless-null nano-cpus
      (hash-set! host-config 'NanoCpus nano-cpus))
    (unless-null cpuset-mems
      (hash-set! host-config 'CpusetMems cpuset-mems))
    (unless-null runtime
      (hash-set! host-config 'Runtime runtime))
    host-config))

(define create-container-config
  (lambda (version image command hostname user detach
            stdin-open tty mem-limit ports
            environment dns volumes volumes-from
            network-disabled entrypoint cpu-shares
            working-dir domainname memswap-limit
            cpuset host-config mac-address labels
            volume-driver stop-signal networking-config
            healthcheck stop-timeout runtime)
    (let ([container-config (make-hash)])
      (when (version-gte version "1.10")
        		(begin
          		(unless-null dns
            		(raise "Invalid version: dns parameter has no effect on create-container"))
              (unless-null volumes-from
                (raise "Invalid version: volumes-from parameter has no effect on create-container"))))
      (when (version-lt version "1.18")
          	(unless-null labels
            	(raise "Invalid version: labels were introduced in API 1.18")))
      (if (version-lt version "1.19")
          (begin
            (unless-null volume-driver
              (raise "Invalid version: volume drivers were introduced in API 1.19"))
            (when (null? mem-limit)
              (set! mem-limit 0))
            (when (null? memswap-limit)
              (set! memswap-limit 0)))
          (begin
            (unless-null mem-limit
              (raise "mem-limit has been moved to host-config since API 1.19"))
            (unless-null memswap-limit
              (raise "memswap-limit has been moved to host-config since API 1.19"))))
      (when (version-lt version "1.21")
        (unless-null stop-signal
          (raise "Invalid version: stop signal was introduced in API 1.21")))
      (unless-null stop-timeout
        (when (version-lt version "1.25")
          (raise "Invalid version: stop-timeout only introduced in version 1.25")))
      (unless-null healthcheck
        (begin
	        (when (version-lt version "1.24")
	          (raise "Invalid verison: health options were only introduced in API 1.24"))
	        (when (and (version-lt version "1.29") (hash-has-key? healthcheck 'StartPeriod))
	          (raise "Invalid version: healthcheck start period was introduced in API version 1.29"))))
      (when (hash? environment)
        (set! environment (format-hashenv environment)))
      (when (list? labels)
        (set! labels (let ([hsh (make-hash)])
                       (foldl (lambda (label tbl) (hash-set! tbl label "") tbl)
                              hsh
                              labels))))
      (when (list? ports)
        (let ([exposed-ports (make-hash)])
          (for ([port-def ports]
                #:when (and (not (null? port-def))
                            (pair? port-def)))
            (let ([formatted 
                   (string->symbol
                     (format "~a/~a" (car port-def) (cdr port-def)))])
              (hash-set! exposed-ports formatted (make-hash)))
          (set! ports exposed-ports))))
      (when (string? volumes)
        (set! volumes (list volumes)))
      (when (list? volumes)
        (let ([volume-hash (make-hash)]
              [vols (map string->symbol volumes)])
          (for ([v vols])
            (hash-set! volume-hash v (make-hash)))
          (set! volumes volume-hash)))
      (let ([attach-stdin #f]
            [attach-stdout #f]
            [attach-stderr #f]
            [stdin-once #f])
        (unless detach
          (set! attach-stdout #t)
          (set! attach-stderr #t)
          (when stdin-open
            (set! attach-stdin #t)
            (set! stdin-once #t)))
        (hash-set! container-config 'Hostname hostname)
        (hash-set! container-config 'Domainname domainname)
        (hash-set! container-config 'ExposedPorts ports)
        (hash-set! container-config 'User user)
        (hash-set! container-config 'Tty tty)
        (hash-set! container-config 'OpenStdin stdin-open)
        (hash-set! container-config 'StdinOnce stdin-once)
        (hash-set! container-config 'Memory mem-limit)
        (hash-set! container-config 'AttachStdin attach-stdin)
        (hash-set! container-config 'AttachStdout attach-stdout)
        (hash-set! container-config 'AttachStderr attach-stderr)
        (hash-set! container-config 'Env environment)
        (hash-set! container-config 'Cmd command)
        (hash-set! container-config 'Dns dns)
        (hash-set! container-config 'Image image)
        (hash-set! container-config 'Volumes volumes)
        (hash-set! container-config 'NetworkDisabled network-disabled)
        (hash-set! container-config 'Entrypoint entrypoint)
        (hash-set! container-config 'CpuShares cpu-shares)
        (hash-set! container-config 'Cpuset cpuset)
        (hash-set! container-config 'CpusetCpus cpuset)
        (hash-set! container-config 'WorkingDir working-dir)
        (hash-set! container-config 'MemorySwap memswap-limit)
        (hash-set! container-config 'HostConfig host-config)
        (hash-set! container-config 'NetworkingConfig networking-config)
        (hash-set! container-config 'MacAddress mac-address)
        (hash-set! container-config 'Labels labels)
        (hash-set! container-config 'StopSignal stop-signal)
        (hash-set! container-config 'StopTimeout stop-timeout)
        (hash-set! container-config 'Runtime runtime))
      container-config)))