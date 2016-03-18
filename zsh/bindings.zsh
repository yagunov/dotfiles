bindkey -e

bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey '^[^[[C' emacs-forward-word
bindkey '^[^[[D' emacs-backward-word

bindkey -s '^X^Z' '%-^M'
bindkey '^[e' expand-cmd-path
bindkey '^[^I' reverse-menu-complete
bindkey '^X^N' accept-and-infer-next-history
bindkey '^W' kill-region
bindkey '^I' complete-word
# Fix weird sequence that rxvt produces
bindkey -s '^[[Z' '\t'

bindkey "^[h" .backward-kill-word

bindkey "^[u" up-case-word
bindkey "^[l" down-case-word

# command history search
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey "^X^Z" predict-on # C-x C-z
bindkey "^Z" predict-off # C-z
bindkey "^p" history-beginning-search-backward
bindkey "^n" history-beginning-search-forward
