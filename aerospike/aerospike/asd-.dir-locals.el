((nil . ((compilation-read-command . nil)
         ;; TODO(laurynas): consider setting compile-command too so that M-x compile DTRT
         (projectile-generic-command . "(cd ce && git ls-files -zco --exclude-standard --recurse-submodules | gsed -z 's/^/ce\\//g'); (cd ee && git ls-files -zco --exclude-standard --recurse-submodules | gsed -z 's/^/ee\\//g')")))
 (c-mode . ((c-file-style . "aerospike"))))
