function hh --description "Display colorized hex-dump of first 256 bytes of files"
    hexyl --border=none --bytes=256 $argv
end
