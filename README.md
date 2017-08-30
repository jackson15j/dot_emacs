dot_emacs
=========

My main `.emacs` file.

Ansible
-------

Install via [ansible](http://docs.ansible.com/ansible/) with the following
command, which will prompt for sudo password (pacman package installation) due
to the `-K` or `--ask-become-pass` flag:

```bash
cd ansible/
 ansible-playbook main.yml -K
```

You can add `-i hosts` if you want to run against remote hosts instead of
locally.

**Note:** Hard-coded to ArchLinux and my own needs.
