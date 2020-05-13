if [[ "${DESKTOP_SESSION,,}" =~ gnome ]]; then
    trap "gnome-session-quit --logout --force" EXIT
    emacs --eval "(exwm-enable)"
else
    exec emacs --eval "(exwm-enable)"
fi
