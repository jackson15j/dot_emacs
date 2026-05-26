#! /bin/sh
# emacs -Q --batch --load '~/org/dot_emacs/.emacs_agenda_export'

docker run  -v ~/org:/root/org -v .:/config -v /tmp:/tmp silex/emacs:alpine emacs -Q --batch --load '/config/.emacs_agenda_export'
