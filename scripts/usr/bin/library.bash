set -u

function is_on_ac() {
    if [[ $(pmset -g ps | head -1) =~ "AC Power" ]]; then
        true
    else
        false
    fi
}
