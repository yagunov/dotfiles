function p --description "Start most appropriate python's REPL (based on current directory)"

    function run_system_repl
        # Use system's python

        type -q ptipython
        and ptipython && return

        type -q ipython
        and ipython && return

        type -q ptpython
        and ptpython && return

        python
    end

    if test -e ./manage.py
        # Start Django shell if possible
        set -l py_packages (pip list --disable-pip-version-check | cut -d' ' -f1)

        if not contains Django $py_packages
            run_system_repl
        else if contains django-extensions $py_packages
            ./manage.py shell_plus
        else
            ./manage.py shell
        end
        or run_system_repl
    else if set -q VIRTUAL_ENV
        # Virtual environment (both pyenv and python -m venv)
        if test -x $VIRTUAL_ENV/bin/ptipython
            $VIRTUAL_ENV/bin/ptipython
        else if test -x $VIRTUAL_ENV/bin/ipython
            $VIRTUAL_ENV/bin/ipython
        else
            $VIRTUAL_ENV/bin/python
        end
        or run_system_repl
    else
        run_system_repl
    end
end
