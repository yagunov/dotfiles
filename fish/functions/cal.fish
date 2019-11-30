function cal --wraps cal
    if command -sq gcal
        gcal -q RU --iso-week-number=yes --starting-day=1 $argv
    else if command -sq ncal
        ncal -Mwb $argv
    else
        echo "No calendar command found"
    end
end
