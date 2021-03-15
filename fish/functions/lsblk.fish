function lsblk --wraps lsblk
    if not count $argv > /dev/null
        command lsblk -T -o NAME,TYPE,SIZE,FSTYPE,SCHED,UUID,MOUNTPOINT
    else
        command lsblk $argv
    end
end
