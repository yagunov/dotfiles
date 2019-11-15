if status is-interactive
    bind \eo '__fish_copy_pwd'
    bind \en '__fish_sink_output'
    bind -M insert \eo '__fish_copy_pwd'
    bind -M insert \en '__fish_sink_output'
end
