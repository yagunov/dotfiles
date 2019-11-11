function lsblk --wraps lsblk
    if not count $argv > /dev/null
        command lsblk -T -o NAME,TYPE,SIZE,SCHED,MOUNTPOINT,FSTYPE,UUID
    else
        command lsblk $argv
    end
end
