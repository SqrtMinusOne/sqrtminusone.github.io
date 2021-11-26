#!/usr/bin/env bash
ROOT=$(git rev-parse --show-toplevel)
DOTFILES_REPO=$(git rev-parse --show-toplevel)/repos/dotfiles
DATA_ROOT=$(git rev-parse --show-toplevel)/__data

echo $ROOT
echo $DOTFILES_REPO
echo $DATA_ROOT

if [ ! -d "$ROOT/static/stats" ]; then
    mkdir "$ROOT/static/stats"
fi

if [ ! -d "$DATA_ROOT" ]; then
    mkdir "$DATA_ROOT"
fi

declare -A paths

keys=("Emacs.org" "init.el" "init.vim" "Desktop.org" "Console.org" "Mail.org" "Guix.org")

paths["Emacs.org"]="Emacs.org;.emacs.d/emacs.org;config/.emacs.d/emacs.org"
paths["init.el"]=".emacs.d/init.el;config/.emacs.d/init.el"
paths["init.vim"]=".config/nvim/init.vim;config/nvim/init.vim;nvim/init.vim"
paths["Desktop.org"]="Desktop.org"
paths["Console.org"]="Console.org"
paths["Guix.org"]="Guix.org"
paths["Mail.org"]="Mail.org"

get_lengths () {
    while IFS=' ' read commit date; do

        result="$commit,$date"

        for key in "${keys[@]}"
        do
            val=0
            IFS=';' read -r -a files <<< "${paths[$key]}"
            for file in "${files[@]}"
            do
                if (( val == 0 )); then
                    val=$(git -C $DOTFILES_REPO show $commit:$file 2>/dev/null | wc -l || 0)
                fi
            done
            result+=",$val"
        done
        # result=${result%,*}
        echo $result
    done
}

header="commit,date"
for key in "${keys[@]}"
do
    header+=",$key"
done
echo $header > $DATA_ROOT/lengths.csv
git -C $DOTFILES_REPO log --pretty="%H %cI" | get_lengths >> $DATA_ROOT/lengths.csv
