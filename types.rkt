#lang racket

(require racket/string
         json
         "utils.rkt")

(provide make-host-config)

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
      (hash-set! host-config 'ReadonlyRootfs read-only)
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